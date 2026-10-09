//! The trained map, written out so a later step can draw it or apply it.
//!
//! FlowSOM's tree is the minimum spanning tree of the map's **codes**, and its stars are the
//! nodes' **median** values. Neither can be rebuilt downstream from the per-cell labels alone:
//! node medians differ from the codes enough to give a different tree (22 of 35 edges shared on
//! a 6x6 test map), and an empty node has no median at all. So the operator writes the map as a
//! small JSON document, the same on every channel row, in a table keyed by `.ri`. One row per
//! channel joins one to one, so the per-cell stream downstream does not grow, and a step that
//! projects the column on rows gets the whole map whichever channels it keeps.
//!
//! Format (`"format": "flowsom_model"`, `"version": 1`):
//! - `xdim`, `ydim`; node k (1-based) sits at grid column `(k-1) % xdim + 1`, row
//!   `(k-1) / xdim + 1`, FlowSOM's `expand.grid(1:xdim, 1:ydim)`.
//! - `markers`: the channels, in row order; every per-node vector follows it.
//! - `codes`: one vector per node, at full precision, in the space the map was trained in (the
//!   scaled space when `scale` is true).
//! - `medians`: one vector per node of its cells' median value, in the data's own units (the
//!   scaling undone), `null` for a node no cell maps to. This is `fsom$map$medianValues`.
//! - `counts`: cells per node, over every cell of the projection.
//! - `metaclustering`: the metacluster of each node, 1-based.
//! - `scale`, `scale_center`, `scale_sd`: the per-channel centring and scaling applied before
//!   the map (`null` when `scale` is false), so new cells can be put in the codes' space.
//! - `seed`, `rlen`, `n_cells`, `n_train`: how the map was made.
use serde_json::{Value, json};

pub struct Model<'a> {
    pub xdim: usize,
    pub ydim: usize,
    pub markers: &'a [String],
    /// Column-major, `ncodes` rows per channel, as `flowsom::flowsom::FlowSom::codes`.
    pub codes: &'a [f64],
    pub metaclustering: &'a [usize],
    pub counts: &'a [usize],
    /// Column-major `ncodes x p` in data units; NaN for an empty node.
    pub medians: &'a [f64],
    pub scaling: Option<(&'a [f64], &'a [f64])>,
    pub seed: u32,
    pub rlen: usize,
    pub n_cells: usize,
    pub n_train: usize,
}

impl Model<'_> {
    pub fn to_json(&self) -> String {
        let ncodes = self.xdim * self.ydim;
        let p = self.markers.len();
        let by_node =
            |m: &[f64], k: usize| -> Vec<f64> { (0..p).map(|c| m[c * ncodes + k]).collect() };
        let codes: Vec<Vec<f64>> = (0..ncodes).map(|k| by_node(self.codes, k)).collect();
        let medians: Vec<Value> = (0..ncodes)
            .map(|k| {
                if self.counts[k] == 0 {
                    Value::Null
                } else {
                    json!(by_node(self.medians, k))
                }
            })
            .collect();
        let (center, sd) = match self.scaling {
            Some((c, s)) => (json!(c), json!(s)),
            None => (Value::Null, Value::Null),
        };
        json!({
            "format": "flowsom_model",
            "version": 1,
            "xdim": self.xdim,
            "ydim": self.ydim,
            "markers": self.markers,
            "codes": codes,
            "medians": medians,
            "counts": self.counts,
            "metaclustering": self.metaclustering,
            "scale": self.scaling.is_some(),
            "scale_center": center,
            "scale_sd": sd,
            "seed": self.seed,
            "rlen": self.rlen,
            "n_cells": self.n_cells,
            "n_train": self.n_train,
        })
        .to_string()
    }
}

/// Cells per node (1-based nodes).
pub fn counts(node: &[usize], ncodes: usize) -> Vec<usize> {
    let mut c = vec![0; ncodes];
    for &k in node {
        c[k - 1] += 1;
    }
    c
}

/// Median of each channel over each node's cells, R's `median` (the mean of the two middle
/// values for an even count). `data` is column-major `n x p`; the result is column-major
/// `ncodes x p`, NaN where a node has no cells.
pub fn node_medians(data: &[f64], n: usize, p: usize, node: &[usize], ncodes: usize) -> Vec<f64> {
    let cnt = counts(node, ncodes);
    // Cells grouped by node: a counting sort, so each node's cells are one contiguous run.
    let mut start = vec![0usize; ncodes + 1];
    for k in 0..ncodes {
        start[k + 1] = start[k] + cnt[k];
    }
    let mut order = vec![0usize; n];
    let mut fill = start.clone();
    for (i, &k) in node.iter().enumerate() {
        order[fill[k - 1]] = i;
        fill[k - 1] += 1;
    }
    let mut out = vec![f64::NAN; ncodes * p];
    let mut buf: Vec<f64> = Vec::new();
    for c in 0..p {
        let col = &data[c * n..(c + 1) * n];
        for k in 0..ncodes {
            let cells = &order[start[k]..start[k + 1]];
            if cells.is_empty() {
                continue;
            }
            buf.clear();
            buf.extend(cells.iter().map(|&i| col[i]));
            out[c * ncodes + k] = median(&mut buf);
        }
    }
    out
}

fn median(v: &mut [f64]) -> f64 {
    let m = v.len();
    let cmp = |a: &f64, b: &f64| a.total_cmp(b);
    let (_, hi, _) = v.select_nth_unstable_by(m / 2, cmp);
    let hi = *hi;
    if m % 2 == 1 {
        hi
    } else {
        let lo = v[..m / 2].iter().copied().fold(f64::NEG_INFINITY, f64::max);
        (lo + hi) / 2.0
    }
}

/// Undo `(v - center) / sd` on a column-major `rows x p` matrix, in place. NaN stays NaN.
pub fn unscale(m: &mut [f64], rows: usize, center: &[f64], sd: &[f64]) {
    for (c, (ce, s)) in center.iter().zip(sd).enumerate() {
        for v in &mut m[c * rows..(c + 1) * rows] {
            *v = *v * s + ce;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn medians_follow_r() {
        // 5 cells, 2 channels, 3 nodes; node 3 is empty.
        let data = [
            1.0, 4.0, 2.0, 10.0, 3.0, /* ch2 */ 5.0, 6.0, 7.0, 8.0, 9.0,
        ];
        let node = [1, 2, 1, 2, 1];
        let m = node_medians(&data, 5, 2, &node, 3);
        assert_eq!(m[0], 2.0); // node 1, ch1: median(1, 2, 3)
        assert_eq!(m[1], 7.0); // node 2, ch1: mean of 4 and 10
        assert!(m[2].is_nan());
        assert_eq!(m[3], 7.0); // node 1, ch2: median(5, 7, 9)
        assert_eq!(m[4], 7.0); // node 2, ch2: mean of 6 and 8
        assert_eq!(counts(&node, 3), vec![3, 2, 0]);
    }

    #[test]
    fn json_is_per_node_and_round_trips_exactly() {
        let markers = vec!["CD3".to_string(), "CD4".to_string()];
        let codes = [0.1, 1.0 / 3.0, -2.5, 1e-17]; // 2 nodes x 2 channels, column-major
        let medians = [0.0, f64::NAN, 1.0, f64::NAN];
        let m = Model {
            xdim: 2,
            ydim: 1,
            markers: &markers,
            codes: &codes,
            metaclustering: &[1, 2],
            counts: &[4, 0],
            medians: &medians,
            scaling: None,
            seed: 42,
            rlen: 10,
            n_cells: 4,
            n_train: 4,
        };
        let v: Value = serde_json::from_str(&m.to_json()).unwrap();
        assert_eq!(
            v["codes"][0][1].as_f64().unwrap().to_bits(),
            (-2.5f64).to_bits()
        );
        assert_eq!(
            v["codes"][1][0].as_f64().unwrap().to_bits(),
            (1.0f64 / 3.0).to_bits()
        );
        assert_eq!(
            v["codes"][1][1].as_f64().unwrap().to_bits(),
            1e-17f64.to_bits()
        );
        assert_eq!(v["medians"][0], json!([0.0, 1.0]));
        assert!(v["medians"][1].is_null());
        assert!(v["scale_center"].is_null());
        assert_eq!(v["metaclustering"], json!([1, 2]));
    }
}
