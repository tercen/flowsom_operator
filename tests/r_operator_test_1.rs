//! The R `flowsom_operator`'s own unit test (`tests/test_1.json` in its 1.x history: the crabs
//! data, 200 cells × 5 channels, seed 42, every other property at its default), run through this
//! operator's pipeline. The expected labels are R's `test_1_out_1.csv`, unchanged.
//!
//! The R test also expected a serialized R FlowSOM object as a second table; 2.0 does not emit
//! one (no R consumer of it exists outside R), so only the labels are compared.
use flowsom::flowsom::{Clusters, Params};
use flowsom_operator::{fit_and_assign, output};
use std::collections::BTreeMap;

fn fixture(name: &str) -> String {
    let p = format!(
        "{}/fixtures/r_operator_test_1/{name}",
        env!("CARGO_MANIFEST_DIR")
    );
    std::fs::read_to_string(&p).unwrap_or_else(|e| panic!("{p}: {e}"))
}

fn unquote(s: &str) -> &str {
    s.trim().trim_matches('"')
}

fn run(scale: bool) -> (Vec<String>, Vec<String>) {
    // Long input: "y_values","variable","observation","color". The crosstab has the channels on
    // rows (sorted by name, as Tercen sorts a factor) and the cells on columns.
    let text = fixture("test_1_in.csv");
    let mut cells: BTreeMap<String, BTreeMap<i64, f64>> = BTreeMap::new();
    for l in text.lines().skip(1).filter(|l| !l.trim().is_empty()) {
        let f: Vec<&str> = l.split(',').collect();
        let y: f64 = unquote(f[0]).parse().unwrap();
        let obs: i64 = unquote(f[2]).parse().unwrap();
        cells
            .entry(unquote(f[1]).to_string())
            .or_default()
            .insert(obs, y);
    }
    let p = cells.len();
    let n = cells.values().next().unwrap().len();
    let mut wide = Vec::with_capacity(n * p);
    for col in cells.values() {
        assert_eq!(col.len(), n);
        // flowCore holds expressions as 32-bit floats; the operator reproduces that.
        wide.extend(col.values().map(|v| *v as f32 as f64));
    }
    let params = Params {
        xdim: 10,
        ydim: 10,
        clusters: Clusters::UpTo(10),
        rlen: 10,
        seed: 42,
    };
    let fit = fit_and_assign(wide, n, p, None, scale, &params);
    let nw = output::label_width(fit.node.iter().copied().max().unwrap());
    let mw = output::label_width(fit.n_metaclusters);
    (
        fit.node.iter().map(|&k| output::label(k, nw)).collect(),
        fit.metacluster
            .iter()
            .map(|&k| output::label(k, mw))
            .collect(),
    )
}

fn expected() -> (Vec<String>, Vec<String>) {
    let text = fixture("test_1_out_1.csv");
    let mut rows: Vec<(i64, String, String)> = text
        .lines()
        .skip(1)
        .filter(|l| !l.trim().is_empty())
        .map(|l| {
            let f: Vec<&str> = l.split(',').collect();
            (
                unquote(f[3]).parse().unwrap(),
                unquote(f[0]).to_string(),
                unquote(f[1]).to_string(),
            )
        })
        .collect();
    rows.sort_by_key(|r| r.0);
    (
        rows.iter().map(|r| r.1.clone()).collect(),
        rows.iter().map(|r| r.2.clone()).collect(),
    )
}

#[test]
fn the_r_operators_own_test_gives_the_same_labels() {
    let (want_node, want_meta) = expected();
    let (node, meta) = run(false);
    let differ = |a: &[String], b: &[String]| a.iter().zip(b).filter(|(x, y)| x != y).count();
    assert_eq!(node.len(), want_node.len());
    assert_eq!(
        differ(&node, &want_node),
        0,
        "cluster_id differs from the R operator"
    );
    assert_eq!(
        differ(&meta, &want_meta),
        0,
        "metacluster_id differs from the R operator"
    );
}
