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

    // Which cells train the map. Empty `train_factor` = every cell, the R operator's behaviour;
    // otherwise the cells whose factor value is `train_value` (CytoNorm's batch controls), at most
    // `train_cells` of them by a seeded draw, and every cell is assigned to that map afterwards.
    let train: Option<Vec<usize>> = if s.train_factor.is_empty() {
        None
    } else {
        let labels = input::column_labels(ctx, &s.train_factor).await?;
        if labels.len() != n_cells {
            anyhow::bail!(
                "column factor '{}' has {} values for {n_cells} cells",
                s.train_factor,
                labels.len()
            );
        }
        let mut idx: Vec<usize> = (0..n_cells)
            .filter(|&i| labels[i] == s.train_value)
            .collect();
        if idx.is_empty() {
            anyhow::bail!(
                "no cell has {} = '{}'; the values present are {:?}",
                s.train_factor,
                s.train_value,
                {
                    let mut v: Vec<&String> = labels.iter().collect();
                    v.sort();
                    v.dedup();
                    v.into_iter().take(8).cloned().collect::<Vec<_>>()
                }
            );
        }
        let n_matching = idx.len();
        if s.train_cells > 0 && idx.len() > s.train_cells {
            idx = draw(&idx, s.train_cells, s.seed as u64);
        }
        if idx.len() < ncodes {
            anyhow::bail!(
                "{} training cells ({} = '{}'{}) for a map of {ncodes} nodes; a map needs at least \
                 one cell per node. Use a smaller xdim/ydim or more training cells.",
                idx.len(),
                s.train_factor,
                s.train_value,
                if s.train_cells > 0 {
                    format!(", train_cells {}", s.train_cells)
                } else {
                    String::new()
                }
            );
        }
        tracing::info!(n_matching, n_train = idx.len(), "training subset");
        Some(idx)
    };

    rep.at(0, "Reading the crosstab");
    let data = gather(ctx, n_values, n_cells, p, &rep).await?;
    // Widen once, then free the narrow copy: the peak is 12 bytes a value, not 12 twice over.
    let wide: Vec<f64> = data.iter().map(|v| *v as f64).collect();
    drop(data);

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
    let fitted = fit_and_assign(wide, n_cells, p, train.as_deref(), s.scale, &params);
    let n_meta = fitted.n_metaclusters;
    tracing::info!(
        secs = format!("{:.1}", t.elapsed().as_secs_f64()),
        nodes = ncodes,
        metaclusters = n_meta,
        "map trained"
    );
    rep.info(match &train {
        None => format!(
            "FlowSOM: {n_cells} cells x {p} channels into {ncodes} nodes and {n_meta} metaclusters"
        ),
        Some(idx) => format!(
            "FlowSOM: map trained on {} cells ({} = '{}') x {p} channels into {ncodes} nodes and \
             {n_meta} metaclusters; all {n_cells} cells assigned to it",
            idx.len(),
            s.train_factor,
            s.train_value
        ),
    });
    let (node, metacluster) = (fitted.node, fitted.metacluster);

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
            &node,
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

/// A trained map applied to every cell.
pub struct Fitted {
    /// SOM node of every cell (1-based, as FlowSOM numbers them).
    pub node: Vec<usize>,
    /// Metacluster of every cell (1-based).
    pub metacluster: Vec<usize>,
    pub n_metaclusters: usize,
}

/// Train the map and assign every cell. `wide` is column-major (`n` cells per channel, `p`
/// channels). With `train` = `None` this is `FlowSOM(scale = scale)` on all cells. With a
/// subset it is what R does for `NewData`: the scaling is computed on the training cells and
/// applied to everyone, the map is trained on the (scaled) training cells, and every cell is
/// mapped to its nearest code and that code's metacluster.
pub fn fit_and_assign(
    mut wide: Vec<f64>,
    n: usize,
    p: usize,
    train: Option<&[usize]>,
    scale: bool,
    params: &flowsom::flowsom::Params,
) -> Fitted {
    let (fit_data, n_fit): (Vec<f64>, usize) = match train {
        None => {
            if scale {
                flowsom::metacluster::scale_columns(&mut wide, n, p);
            }
            (wide.clone(), n)
        }
        Some(idx) => {
            if scale {
                let raw_train = take_rows(&wide, n, p, idx);
                let (center, sd) = flowsom::metacluster::column_scaling(&raw_train, idx.len(), p);
                flowsom::metacluster::scale_columns_with(&mut wide, n, p, &center, &sd);
            }
            (take_rows(&wide, n, p, idx), idx.len())
        }
    };
    let fsom = flowsom::flowsom::fit(&fit_data, n_fit, p, params);
    drop(fit_data);
    let n_metaclusters = fsom.metaclustering.iter().copied().max().unwrap_or(0);
    let metacluster = fsom.metacluster_of(&wide, n, p);
    let node: Vec<usize> = match train {
        None => fsom.node,
        Some(_) => flowsom::som::map_data_to_codes(
            &wide,
            &fsom.codes,
            n,
            p,
            fsom.ncodes(),
            flowsom::som::Dist::Euclidean,
        )
        .iter()
        .map(|m| m.node)
        .collect(),
    };
    Fitted {
        node,
        metacluster,
        n_metaclusters,
    }
}

/// Rows `idx` of a column-major `n × p` matrix, as a column-major `idx.len() × p` matrix.
fn take_rows(wide: &[f64], n: usize, p: usize, idx: &[usize]) -> Vec<f64> {
    let m = idx.len();
    let mut out = vec![0.0; m * p];
    for c in 0..p {
        for (k, &r) in idx.iter().enumerate() {
            out[c * m + k] = wide[c * n + r];
        }
    }
    out
}

/// A seeded draw of `k` of `idx`, sorted. ChaCha8 on the operator seed, so the same seed gives
/// the same training cells run after run.
fn draw(idx: &[usize], k: usize, seed: u64) -> Vec<usize> {
    use rand::SeedableRng;
    use rand::seq::SliceRandom;
    let mut v = idx.to_vec();
    let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(seed ^ 0x005e_ed0f_f10a_504d);
    v.shuffle(&mut rng);
    v.truncate(k);
    v.sort_unstable();
    v
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
