//! Operator properties.
//!
//! Names follow the R `flowsom_operator` exactly, so a workflow can swap one step for the other
//! without re-typing anything: `nclust`, `maxMeta`, `seed`, `xdim`, `ydim`, `rlen`, `mst`,
//! `alpha_1`, `alpha_2`, `distf`.
use anyhow::{Result, bail};
use flowsom::som::Dist;
use tercen_rs::PropertyReader;
use tercen_rs::context::ContextBase;

/// How many metaclusters to produce.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Clusters {
    /// `nclust`: a fixed number, and the reproducible choice.
    Fixed(usize),
    /// `maxMeta`: let FlowSOM pick by the elbow of the within-cluster sum of squares, trying
    /// every k up to this one.
    UpTo(usize),
}

#[derive(Debug, Clone)]
pub struct Settings {
    pub clusters: Clusters,
    /// `seed` used for this run. A negative property means "random", as in the R operator: a
    /// seed is drawn per run and `seed_random` is set so the log can name it (and the model
    /// records it), which keeps any run repeatable by entering that seed.
    pub seed: u32,
    pub seed_random: bool,
    pub xdim: usize,
    pub ydim: usize,
    pub rlen: usize,
    /// `mst`. Only 1 is supported: above that FlowSOM retrains on distances taken from a
    /// minimum spanning tree of the codes, which is not ported.
    pub mst: usize,
    pub alpha: (f64, f64),
    pub distf: Dist,
    /// `scale`. Off by default since 0.1.4. FlowSOM 1.22's `FlowSOM()` defaulted to TRUE (which
    /// the R operator inherited by not passing it), but FlowSOM 2.x and the Python port do not
    /// scale, and on an asinh-transformed panel unit-variance scaling gives every dim state
    /// marker the weight of CD3: measured against a reference clustering it cost 0.2 of ARI.
    pub scale: bool,
    /// `train_factor`: a column factor naming which cells train the map; empty trains on every
    /// cell (the R operator's behaviour). With it set, the map is trained on the cells whose
    /// value is `train_value` — CytoNorm's batch controls, typically — and every cell is then
    /// assigned to the trained map, as `FlowSOM::NewData` does.
    pub train_factor: String,
    pub train_value: String,
    /// `train_cells`: at most this many training cells, a seeded draw from the matching ones;
    /// 0 uses all of them. cytonormpy's `run_clustering(n_cells = 6000)`.
    pub train_cells: usize,
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            // `if (is.null(maxMeta) & is.null(n.clust)) maxMeta <- 10`
            clusters: Clusters::UpTo(10),
            seed: 42,
            seed_random: false,
            xdim: 10,
            ydim: 10,
            rlen: 10,
            mst: 1,
            alpha: (0.05, 0.01),
            distf: Dist::Euclidean,
            scale: false,
            train_factor: String::new(),
            train_value: "Train".to_string(),
            train_cells: 0,
        }
    }
}

/// Read the properties off the task's `CubeQueryTask` snapshot.
pub fn read(ctx: &ContextBase) -> Result<Settings> {
    let pr = PropertyReader::from_operator_settings(ctx.operator_settings());
    let d = Settings::default();

    // The R operator writes the string "NULL" when a number is unset, and so does the property
    // panel; treat that and an empty string alike.
    let opt_usize = |name: &str| -> Result<Option<usize>> {
        let raw = pr.get_string(name, "").trim().to_string();
        if raw.is_empty() || raw.eq_ignore_ascii_case("null") {
            return Ok(None);
        }
        match raw.parse::<f64>() {
            Ok(v) if v >= 1.0 => Ok(Some(v as usize)),
            _ => bail!("property '{name}' should be a whole number of at least 1, got '{raw}'"),
        }
    };
    let num = |name: &str, dflt: f64| -> Result<f64> {
        let raw = pr.get_string(name, "").trim().to_string();
        if raw.is_empty() || raw.eq_ignore_ascii_case("null") {
            return Ok(dflt);
        }
        raw.parse::<f64>()
            .map_err(|_| anyhow::anyhow!("property '{name}' should be a number, got '{raw}'"))
    };

    let nclust = opt_usize("nclust")?;
    let max_meta = opt_usize("maxMeta")?;
    // The R operator: maxMeta wins if both are given, and 10 is the fallback when neither is.
    let clusters = match (max_meta, nclust) {
        (Some(m), _) => Clusters::UpTo(m),
        (None, Some(n)) => Clusters::Fixed(n),
        (None, None) => d.clusters,
    };

    let (seed, seed_random) = seed_from(num("seed", d.seed as f64)?, random_seed);

    let dim = |name: &str, dflt: usize| -> Result<usize> {
        let v = num(name, dflt as f64)?;
        if v < 1.0 {
            bail!("property '{name}' must be at least 1, got {v}");
        }
        Ok(v as usize)
    };

    let mst = dim("mst", d.mst)?;
    if mst != 1 {
        bail!(
            "property 'mst' is {mst}; only 1 is supported. Above 1 FlowSOM retrains on distances \
             taken from a minimum spanning tree of the codes, which is not ported."
        );
    }

    let distf = match num("distf", 2.0)? as i64 {
        1 => Dist::Manhattan,
        2 => Dist::Euclidean,
        3 => Dist::Chebyshev,
        4 => Dist::Cosine,
        other => bail!("property 'distf' is {other}; FlowSOM defines 1, 2, 3 and 4"),
    };

    let s = Settings {
        clusters,
        seed,
        seed_random,
        xdim: dim("xdim", d.xdim)?,
        ydim: dim("ydim", d.ydim)?,
        rlen: dim("rlen", d.rlen)?,
        mst,
        alpha: (num("alpha_1", d.alpha.0)?, num("alpha_2", d.alpha.1)?),
        distf,
        scale: pr.get_string("scale", "false").trim().to_lowercase() == "true",
        train_factor: pr.get_string("train_factor", "").trim().to_string(),
        train_value: {
            let v = pr.get_string("train_value", "").trim().to_string();
            if v.is_empty() {
                d.train_value.clone()
            } else {
                v
            }
        },
        train_cells: num("train_cells", 0.0)?.max(0.0) as usize,
    };
    tracing::info!(?s, "properties");
    Ok(s)
}

/// `seed` as the R operator reads it: a negative value means "random". A fresh seed is then
/// drawn for this run and returned with `true`, so the caller can say which seed was used.
fn seed_from(v: f64, draw: impl FnOnce() -> u32) -> (u32, bool) {
    if v < 0.0 {
        (draw(), true)
    } else {
        (v as u32, false)
    }
}

/// A seed the property panel can hold exactly and a user can type back in.
fn random_seed() -> u32 {
    rand::random::<u32>() >> 1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_negative_seed_means_random_and_says_so() {
        assert_eq!(seed_from(-1.0, || 7), (7, true));
        assert_eq!(seed_from(-3.0, || 9), (9, true));
        assert_eq!(seed_from(0.0, || 7), (0, false));
        assert_eq!(seed_from(123.0, || 7), (123, false));
        // Drawn seeds fit a property value exactly, so a user can type one back in.
        assert!((0..1000).all(|_| random_seed() <= i32::MAX as u32));
    }

    #[test]
    fn defaults_match_the_r_operator_except_scale() {
        let d = Settings::default();
        assert_eq!(d.clusters, Clusters::UpTo(10));
        assert_eq!((d.xdim, d.ydim, d.rlen, d.mst), (10, 10, 10, 1));
        assert_eq!(d.alpha, (0.05, 0.01));
        assert_eq!(d.distf, Dist::Euclidean);
        assert!(
            !d.scale,
            "off since 0.1.4: FlowSOM 2.x and the Python port do not scale; 1.22 did"
        );
    }
}
