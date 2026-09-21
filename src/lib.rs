//! flowsom_operator — FlowSOM clustering for Tercen, as a drop-in for the R operator.
//!
//! Rows are channels, columns are cells, y is the value. The result is one row per cell with
//! its SOM node and its metacluster, plus a table describing the map.
//!
//! **The shape of the problem is a transpose.** The crosstab arrives as scattered
//! `(.ri, .ci, .y)` triples and a map needs each cell's whole vector across channels. There is
//! no order to rely on — R's own client scatters by index rather than assuming one — so the
//! operator gathers the matrix itself, `n_cells × n_channels` of `f32`, and the memory model
//! declares that cost rather than hiding it. `f32` because the R pipeline is `f32` anyway
//! (flowCore stores expressions as 32-bit floats), so the second half of the mantissa was never
//! real.
//!
//! One pass to gather, one map, one assignment, one write.
pub mod context;
pub mod input;
pub mod output;
pub mod pagecache;
pub mod progress;
pub mod props;
pub mod tson;
pub mod upload;

use std::sync::Arc;
use std::time::Instant;

use anyhow::{Context, Result, anyhow};
use tercen_rs::context::ContextBase;
use tercen_rs::{DevContext, TercenClient};

use progress::Reporter;
use props::Clusters;
use tson::TsonWriter;

const CHUNK: usize = 1_000_000;

pub fn init_tracing() {
    let filter = tracing_subscriber::EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("info"));
    let _ = tracing_subscriber::fmt().with_env_filter(filter).try_init();
}

pub fn require_env(name: &str) -> Result<String> {
    std::env::var(name).map_err(|_| anyhow!("{name} is not set"))
}

pub async fn run(task_id: &str) -> Result<()> {
    tracing::info!("flowsom_operator starting (task_id={task_id})");
    let client = build_client().await?;
    let ctx = context::from_task_id(client, task_id).await?;
    execute(
        &ctx,
        Mode::Production {
            task_id: task_id.to_string(),
        },
    )
    .await
}

pub async fn run_dev(workflow_id: &str, step_id: &str) -> Result<()> {
    tracing::info!("flowsom_operator starting in dev mode ({workflow_id} / {step_id})");
    let client = build_client().await?;
    let ctx = DevContext::from_workflow_step(client, workflow_id, step_id)
        .await
        .map_err(|e| anyhow!("load workflow {workflow_id} / step {step_id}: {e}"))?;
    execute(
        &ctx,
        Mode::Dev {
            workflow_id: workflow_id.to_string(),
            step_id: step_id.to_string(),
        },
    )
    .await
}

enum Mode {
    Production {
        task_id: String,
    },
    Dev {
        workflow_id: String,
        step_id: String,
    },
}

async fn build_client() -> Result<Arc<TercenClient>> {
    let client = TercenClient::from_env()
        .await
        .map_err(|e| anyhow!("connect to Tercen: {e}"))?;
    tracing::info!("connected to Tercen");
    Ok(Arc::new(client))
}

async fn execute(ctx: &ContextBase, mode: Mode) -> Result<()> {
    let t_start = Instant::now();
    tracing::info!(
        workflow = ctx.workflow_id(),
        step = ctx.step_id(),
        namespace = ctx.namespace(),
        "context loaded"
    );
    let rep = match &mode {
        Mode::Production { task_id } => Reporter::spawn(Arc::clone(ctx.client()), task_id.clone()),
        Mode::Dev { .. } => Reporter::silent(),
    };
    let s = props::read(ctx)?;

    let n_values = input::cell_count(ctx).await?;
    let channels = input::row_labels(ctx).await?;
    let p = channels.len();
    if p == 0 {
        anyhow::bail!("the projection has no rows: put the channels on rows");
    }
    let n_cells = n_values / p;
    tracing::info!(n_values, n_cells, channels = p, "projection");

    let ncodes = s.xdim * s.ydim;
    if n_cells < ncodes {
        anyhow::bail!(
            "the projection has {n_cells} cells and the map has {ncodes} nodes ({}x{}). A map \
             needs at least one cell per node; use a smaller xdim/ydim.",
            s.xdim,
            s.ydim
        );
    }

    rep.at(0, "Reading the crosstab");
    let data = gather(ctx, n_values, n_cells, p, &rep).await?;
    // Widen once, then free the narrow copy: the peak is 12 bytes a value, not 12 twice over.
    let mut wide: Vec<f64> = data.iter().map(|v| *v as f64).collect();
    drop(data);
    if s.scale {
        // `FlowSOM(scale = TRUE)`, which the R operator gets by not passing anything. It runs
        // after the f32 rounding, as `ReadInput` does, and in double precision.
        flowsom::metacluster::scale_columns(&mut wide, n_cells, p);
    }

    rep.at(progress::FIT.0, "Training the map");
    let t = Instant::now();
    let params = flowsom::flowsom::Params {
        xdim: s.xdim,
        ydim: s.ydim,
        clusters: match s.clusters {
            Clusters::Fixed(k) => flowsom::flowsom::Clusters::Fixed(k),
            Clusters::UpTo(m) => flowsom::flowsom::Clusters::UpTo(m),
        },
        rlen: s.rlen,
        seed: s.seed,
    };
    let fsom = flowsom::flowsom::fit(&wide, n_cells, p, &params);
    let n_meta = fsom.metaclustering.iter().copied().max().unwrap_or(0);
    tracing::info!(
        secs = format!("{:.1}", t.elapsed().as_secs_f64()),
        nodes = ncodes,
        metaclusters = n_meta,
        "map trained"
    );
    rep.info(format!(
        "FlowSOM: {n_cells} cells x {p} channels into {ncodes} nodes and {n_meta} metaclusters"
    ));

    let metacluster = fsom.metacluster_of(&wide, n_cells, p);
    drop(wide);

    rep.at(progress::WRITE.0, "Writing the result");
    let work_root = std::env::temp_dir().join(format!(
        "flowsom_op_{}_{}",
        ctx.workflow_id(),
        ctx.step_id()
    ));
    std::fs::create_dir_all(&work_root)
        .with_context(|| format!("create {}", work_root.display()))?;
    let _guard = TempDirGuard(work_root.clone());
    let result_path = work_root.join("result.tson");
    {
        let f = std::fs::File::create(&result_path)
            .with_context(|| format!("create {}", result_path.display()))?;
        let w = std::io::BufWriter::with_capacity(4 << 20, pagecache::Releasing::new(f, 256 << 20));
        let mut w = TsonWriter::new(w)?;
        output::write_cells(
            &mut w,
            &table_name(ctx),
            ctx.namespace(),
            &fsom.node,
            &metacluster,
        )?;
        output::write_footer(&mut w)?;
    }
    let bytes = std::fs::metadata(&result_path)?.len();
    tracing::info!(bytes, "result written");
    pagecache::release_path(&result_path);

    rep.at(progress::UPLOAD.0, "Uploading the result");
    match mode {
        Mode::Production { task_id } => {
            upload::save_production(ctx, &task_id, &result_path, &rep).await?
        }
        Mode::Dev {
            workflow_id,
            step_id,
        } => {
            let saved = upload::save_dev(ctx, &workflow_id, &step_id, &result_path).await?;
            tracing::info!(task_id = saved.task_id, "dev result saved");
        }
    }
    rep.at(100, "Done");
    tracing::info!(
        total_secs = format!("{:.1}", t_start.elapsed().as_secs_f64()),
        peak_rss_kb = peak_rss_kb().unwrap_or(0),
        "done"
    );
    Ok(())
}

/// Gather the crosstab into a cell-by-channel matrix.
///
/// Scattered by `(.ri, .ci)`, so no ordering is assumed.
///
/// **`f32` is a parity decision, not only a memory one.** The R operator does
/// `flowCore::flowFrame(as.matrix(data))`, and flowCore stores expressions as 32-bit floats, so
/// every value it clusters has already been rounded — by up to 2.4e-7. Rounding here too is what
/// makes the two agree; keeping full precision would be more accurate and would not be FlowSOM.
/// It also halves the peak, which is the part the memory model books.
async fn gather(
    ctx: &ContextBase,
    n_values: usize,
    n_cells: usize,
    p: usize,
    rep: &Reporter,
) -> Result<Vec<f32>> {
    let mut data = vec![f32::NAN; n_cells * p];
    let mut seen = 0usize;
    let mut out_of_range = 0usize;
    input::for_each_chunk(ctx, &[".ri", ".ci", ".y"], n_values, CHUNK, |c| {
        for k in 0..c.y.len() {
            let (ri, ci) = (c.ri[k] as usize, c.ci[k] as usize);
            if ri >= p || ci >= n_cells {
                out_of_range += 1;
                continue;
            }
            data[ci + ri * n_cells] = c.y[k] as f32;
        }
        seen += c.len();
        rep.at(
            progress::band(progress::READ, seen, n_values.max(1)),
            format!("Read {seen} of {n_values}"),
        );
        Ok(())
    })
    .await?;
    if out_of_range > 0 {
        anyhow::bail!(
            "{out_of_range} values fell outside the {n_cells} x {p} crosstab the schema \
             described; the projection changed under the run"
        );
    }
    let missing = data.iter().filter(|v| v.is_nan()).count();
    if missing > 0 {
        anyhow::bail!(
            "{missing} of {} cell-channel pairs have no value. FlowSOM needs a complete matrix: \
             every cell must have every channel. Filter the projection, or fill the gaps \
             upstream.",
            data.len()
        );
    }
    Ok(data)
}

struct TempDirGuard(std::path::PathBuf);
impl Drop for TempDirGuard {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn table_name(ctx: &ContextBase) -> String {
    format!("{}_{}", ctx.step_id(), ctx.qt_hash())
}

fn peak_rss_kb() -> Option<u64> {
    std::fs::read_to_string("/proc/self/status")
        .ok()
        .and_then(|s| {
            s.lines()
                .find(|l| l.starts_with("VmHWM:"))
                .and_then(|l| l.split_whitespace().nth(1)?.parse().ok())
        })
}
