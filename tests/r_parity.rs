//! The operator's own pipeline against the R `flowsom_operator`, on the same synthetic data.
//!
//! Not just the crate: this goes through the f32 rounding the R operator inherits from
//! `flowCore::flowFrame`, then FlowSOM's column scaling, then the map — the three steps in the
//! order the operator does them.
use flowsom::flowsom::{self as fs, Clusters, Params};
use flowsom::metacluster;
use flowsom_operator::output;

fn read_matrix(path: &str) -> (Vec<f64>, usize, usize) {
    let s = std::fs::read_to_string(format!("{}/fixtures/{path}", env!("CARGO_MANIFEST_DIR")))
        .unwrap_or_else(|e| panic!("{path}: {e}"));
    let rows: Vec<Vec<f64>> = s
        .lines()
        .skip(1)
        .filter(|l| !l.trim().is_empty())
        .map(|l| {
            l.split(',')
                .map(|v| v.trim().trim_matches('"').parse().unwrap())
                .collect()
        })
        .collect();
    let (n, px) = (rows.len(), rows[0].len());
    let mut col = vec![0.0; n * px];
    for (i, r) in rows.iter().enumerate() {
        for (j, v) in r.iter().enumerate() {
            col[i + j * n] = *v;
        }
    }
    (col, n, px)
}

fn read_strings(path: &str) -> (Vec<Vec<String>>, usize) {
    let s = std::fs::read_to_string(format!("{}/fixtures/{path}", env!("CARGO_MANIFEST_DIR")))
        .unwrap_or_else(|e| panic!("{path}: {e}"));
    let mut lines = s.lines();
    let ncol = lines.next().unwrap().split(',').count();
    let mut cols = vec![Vec::new(); ncol];
    for l in lines.filter(|l| !l.trim().is_empty()) {
        for (j, v) in l.split(',').enumerate() {
            cols[j].push(v.trim().trim_matches('"').to_string());
        }
    }
    let n = cols[0].len();
    (cols, n)
}

/// What the operator does to the crosstab before the map sees it.
fn as_the_operator_reads_it(raw: &[f64], n: usize, p: usize, scale: bool) -> Vec<f64> {
    // flowCore holds expressions as 32-bit floats, so the R operator clusters rounded values.
    let mut wide: Vec<f64> = raw.iter().map(|v| *v as f32 as f64).collect();
    if scale {
        metacluster::scale_columns(&mut wide, n, p);
    }
    wide
}

#[test]
fn the_f32_rounding_and_scaling_match_what_flowsom_saw() {
    let (raw, n, p) = read_matrix("som_input.csv");
    let (want, n2, p2) = read_matrix("op_input_scaled.csv");
    assert_eq!((n, p), (n2, p2));
    let got = as_the_operator_reads_it(&raw, n, p, true);
    // Bit equality, not a tolerance: a few ulp here moves a quarter of the cells to a different
    // SOM node, because the nearest-node decision is chaotic under training.
    for (k, (g, w)) in got.iter().zip(&want).enumerate() {
        assert_eq!(
            g.to_bits(),
            w.to_bits(),
            "into the map at {k}: got {g:.17e}, R says {w:.17e}"
        );
    }
}

#[test]
fn the_clusters_match_the_r_operator() {
    let (raw, n, p) = read_matrix("som_input.csv");
    let wide = as_the_operator_reads_it(&raw, n, p, true);
    let fsom = fs::fit(
        &wide,
        n,
        p,
        &Params {
            xdim: 10,
            ydim: 10,
            clusters: Clusters::Fixed(5),
            rlen: 10,
            seed: 42,
        },
    );

    let (want_codes, ncodes, _) = read_matrix("op_codes.csv");
    assert_eq!(ncodes, 100);
    for (k, (g, w)) in fsom.codes.iter().zip(&want_codes).enumerate() {
        assert_eq!(
            g.to_bits(),
            w.to_bits(),
            "code {k}: got {g:.17e}, R says {w:.17e}"
        );
    }

    let (want_meta, _, _) = read_matrix("op_metaclustering.csv");
    let want_meta: Vec<usize> = want_meta.iter().map(|v| *v as usize).collect();
    assert_eq!(fsom.metaclustering, want_meta, "metacluster per node");

    // And the labels the operator actually writes, string for string.
    let (cols, rows) = read_strings("op_cells.csv");
    assert_eq!(rows, n);
    let metacluster = fsom.metacluster_of(&wide, n, p);
    let node_width = output::label_width(fsom.node.iter().copied().max().unwrap());
    let meta_width = output::label_width(metacluster.iter().copied().max().unwrap());
    for i in 0..n {
        assert_eq!(
            output::label(fsom.node[i], node_width),
            cols[0][i],
            "cluster_id at cell {i}"
        );
        assert_eq!(
            output::label(metacluster[i], meta_width),
            cols[1][i],
            "metacluster_id at cell {i}"
        );
    }
}

/// The `operatorSpec` vocabulary is not free-form, and inventing a kind is not caught until
/// install, which is the worst place to find it: `0.1.0` shipped with `JoinSpec`,
/// `RelationSpec` and `AttributeSpec` — all made up — and the platform refused it with
/// `Invalid argument (bad kind error): "JoinSpec"` after pulling the image.
///
/// The accepted set below is taken from the manifests the platform has actually installed:
/// `cytonorm_rust_operator`, `asinh_rust_operator` and `read_fcs_rust_operator`. A kind outside
/// it is either a typo or an invention.
#[test]
fn the_spec_uses_only_kinds_the_platform_knows() {
    const KNOWN: &[&str] = &[
        "OperatorSpec",
        "CrosstabSpec",
        "MetaFactor",
        "AxisSpec",
        // Output: a plain OperatorJoinSpec, or a ConditionalJoinSpec of OutputAlternatives each
        // carrying one. There is no bare "JoinSpec".
        "OperatorJoinSpec",
        "ConditionalJoinSpec",
        "OutputAlternative",
        "JoinOperator",
        "ColumnPair",
        "TableRelation",
        "Attribute",
        "Pair",
    ];
    let manifest = concat!(env!("CARGO_MANIFEST_DIR"), "/operator.json");
    let spec: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(manifest).unwrap()).unwrap();

    fn kinds(v: &serde_json::Value, out: &mut Vec<String>) {
        match v {
            serde_json::Value::Object(m) => {
                if let Some(serde_json::Value::String(k)) = m.get("kind") {
                    out.push(k.clone());
                }
                for x in m.values() {
                    kinds(x, out);
                }
            }
            serde_json::Value::Array(a) => a.iter().for_each(|x| kinds(x, out)),
            _ => {}
        }
    }
    let mut found = Vec::new();
    kinds(&spec["operatorSpec"], &mut found);
    assert!(!found.is_empty(), "the spec has no kinds at all");
    for k in &found {
        assert!(
            KNOWN.contains(&k.as_str()),
            "operatorSpec uses kind '{k}', which no installed Tercen operator uses. The platform              rejects it at install with `bad kind error`."
        );
    }
}

/// What the spec promises must be what the writer emits.
#[test]
fn the_spec_declares_the_columns_the_writer_writes() {
    let manifest = concat!(env!("CARGO_MANIFEST_DIR"), "/operator.json");
    let spec: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(manifest).unwrap()).unwrap();
    let joins = spec["operatorSpec"]["outputSpecsV2"][0]["joinOperators"]
        .as_array()
        .expect("outputSpecsV2[0].joinOperators");
    // One relation. A second one joined on nothing is a cross join: 0.1.1 shipped the map that
    // way and every event came back carrying every row of it, so colouring by metacluster
    // coloured nothing. phenograph_operator and the R flowsom_operator both emit one table.
    assert_eq!(joins.len(), 1, "one per-cell relation, and no second one");

    let names = |j: &serde_json::Value| -> Vec<String> {
        j["rightRelation"]["attributes"]
            .as_array()
            .unwrap()
            .iter()
            .map(|a| a["name"].as_str().unwrap().to_string())
            .collect()
    };
    assert_eq!(names(&joins[0]), ["cluster_id", "metacluster_id"]);
    // One row per observation, joined on the observation factor — the name phenograph and the
    // R flowsom_operator both use, and the name this spec gives its column MetaFactor.
    for side in ["lColumns", "rColumns"] {
        assert_eq!(
            joins[0]["leftPair"][side].as_array().unwrap(),
            &vec![serde_json::json!("Observation")],
            "{side}"
        );
    }
    let obs = spec["operatorSpec"]["inputSpecs"][0]["metaFactors"]
        .as_array()
        .unwrap()
        .iter()
        .find(|m| m["ontologyMapping"] == "observation")
        .expect("an observation MetaFactor");
    assert_eq!(obs["name"], "Observation", "the join names this factor");
}

/// Every property the code reads must be declared, or a user cannot set it; and the defaults
/// must agree, or the panel shows one number and the operator uses another.
#[test]
fn the_manifest_matches_the_code() {
    let manifest = concat!(env!("CARGO_MANIFEST_DIR"), "/operator.json");
    let spec: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(manifest).unwrap()).unwrap();
    let props = spec["properties"].as_array().unwrap();
    let names: Vec<&str> = props.iter().map(|p| p["name"].as_str().unwrap()).collect();
    for n in [
        "nclust", "maxMeta", "seed", "xdim", "ydim", "rlen", "mst", "alpha_1", "alpha_2", "distf",
        "scale",
    ] {
        assert!(
            names.contains(&n),
            "property '{n}' is read but not declared"
        );
    }
    let default_of = |name: &str| -> f64 {
        props.iter().find(|p| p["name"] == name).unwrap()["defaultValue"]
            .as_f64()
            .unwrap_or_else(|| panic!("property '{name}' has no numeric default"))
    };
    let d = flowsom_operator::props::Settings::default();
    for (name, code) in [
        ("seed", d.seed as f64),
        ("xdim", d.xdim as f64),
        ("ydim", d.ydim as f64),
        ("rlen", d.rlen as f64),
        ("mst", d.mst as f64),
        ("alpha_1", d.alpha.0),
        ("alpha_2", d.alpha.1),
        ("distf", 2.0),
    ] {
        assert_eq!(default_of(name), code, "default for '{name}'");
    }
    // `nclust` and `maxMeta` are strings because the R operator writes "NULL" into them.
    for name in ["nclust", "maxMeta"] {
        let p = props.iter().find(|p| p["name"] == name).unwrap();
        assert_eq!(p["kind"], "StringProperty", "{name}");
        assert_eq!(p["defaultValue"], "NULL", "{name}");
    }
}
