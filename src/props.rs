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
    /// `seed`. The R operator treats a negative seed as "random"; here it is always set,
    /// because an operator that cannot be re-run to the same answer is not much use in a
    /// workflow. A user who wants variation changes the number.
    pub seed: u32,
    pub xdim: usize,
    pub ydim: usize,
    pub rlen: usize,
    /// `mst`. Only 1 is supported: above that FlowSOM retrains on distances taken from a
    /// minimum spanning tree of the codes, which is not ported.
    pub mst: usize,
    pub alpha: (f64, f64),
    pub distf: Dist,
    /// `scale`. The R operator does not pass it, so it gets `FlowSOM()`'s default of TRUE and
    /// every channel is centred and divided by its standard deviation. Matching that is the
    /// point of the default here.
    pub scale: bool,
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            // `if (is.null(maxMeta) & is.null(n.clust)) maxMeta <- 10`
            clusters: Clusters::UpTo(10),
            seed: 42,
            xdim: 10,
            ydim: 10,
            rlen: 10,
            mst: 1,
            alpha: (0.05, 0.01),
            distf: Dist::Euclidean,
            scale: true,
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

    let seed = {
        let v = num("seed", d.seed as f64)?;
        if v < 0.0 {
            // The R operator makes this a clock seed. Refuse instead of quietly producing an
            // unrepeatable clustering: a workflow that cannot be re-run is a bug, not a feature.
            bail!(
                "property 'seed' is {v}; this operator needs a seed it can repeat. Use any \
                 non-negative number."
            );
        }
        v as u32
    };

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
        xdim: dim("xdim", d.xdim)?,
        ydim: dim("ydim", d.ydim)?,
        rlen: dim("rlen", d.rlen)?,
        mst,
        alpha: (num("alpha_1", d.alpha.0)?, num("alpha_2", d.alpha.1)?),
        distf,
        scale: pr.get_string("scale", "true").trim().to_lowercase() != "false",
    };
    tracing::info!(?s, "properties");
    Ok(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn defaults_match_the_r_operator() {
        let d = Settings::default();
        assert_eq!(d.clusters, Clusters::UpTo(10));
        assert_eq!((d.xdim, d.ydim, d.rlen, d.mst), (10, 10, 10, 1));
        assert_eq!(d.alpha, (0.05, 0.01));
        assert_eq!(d.distf, Dist::Euclidean);
        assert!(
            d.scale,
            "FlowSOM()'s own default, which the R operator does not override"
        );
    }
}
