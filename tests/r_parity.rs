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
    // The per-cell relation, and the model keyed by channel. Never a relation joined on nothing:
    // 0.1.1 shipped the map that way and every event came back carrying every row of it.
    assert_eq!(joins.len(), 2, "per-cell relation and per-channel model");
    for j in joins {
        assert!(
            !j["leftPair"]["lColumns"].as_array().unwrap().is_empty(),
            "an empty join key is a cross join"
        );
    }
    assert_eq!(
        joins[1]["leftPair"]["lColumns"],
        serde_json::json!(["Variable"])
    );
    assert_eq!(
        joins[1]["rightRelation"]["attributes"][0]["name"],
        "flowsom_model"
    );
    let names = |j: &serde_json::Value| -> Vec<String> {
        j["rightRelation"]["attributes"]
            .as_array()
            .unwrap()
            .iter()
            .map(|a| a["name"].as_str().unwrap().to_string())
            .collect()
    };
    assert_eq!(
        names(&joins[0]),
        ["cluster_id", "metacluster_id", "som_x", "som_y"]
    );
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

/// `train_factor`: the map trained on every fifth cell and every cell mapped to it must be what
/// FlowSOM 1.22.0 gives for `FlowSOM(train rows)` + `NewData(all rows)` — node and metacluster
/// per cell, and the scaling parameters the new cells are scaled with (`fsom$scaled.center`,
/// `$scaled.scale`). Golden: `fixtures/gen_train.R`, same image as the other fixtures.
#[test]
fn train_on_a_subset_and_map_all_matches_newdata() {
    let (raw, n, p) = read_matrix("som_input.csv");
    let wide: Vec<f64> = raw.iter().map(|v| *v as f32 as f64).collect();
    let train: Vec<usize> = (0..n).filter(|i| i % 5 == 0).collect();
    assert_eq!(train.len(), 600);

    // Scaling parameters of the training cells, as R stores them on the trained map.
    let train_wide = {
        let m = train.len();
        let mut out = vec![0.0; m * p];
        for c in 0..p {
            for (k, &r) in train.iter().enumerate() {
                out[c * m + k] = wide[c * n + r];
            }
        }
        out
    };
    let (center, scale) = metacluster::column_scaling(&train_wide, train.len(), p);
    let (sc, _) = read_strings("op_scaling_train.csv");
    for c in 0..p {
        let r_center: f64 = sc[c][0].parse().unwrap();
        let r_scale: f64 = sc[c][1].parse().unwrap();
        assert!(
            (center[c] - r_center).abs() <= 1e-12,
            "center[{c}] {} vs R {r_center}",
            center[c]
        );
        assert!(
            (scale[c] - r_scale).abs() <= 1e-12,
            "scale[{c}] {} vs R {r_scale}",
            scale[c]
        );
    }

    let params = Params {
        xdim: 10,
        ydim: 10,
        clusters: Clusters::Fixed(5),
        rlen: 10,
        seed: 42,
    };
    let fitted = flowsom_operator::fit_and_assign(wide, n, p, Some(&train), true, &params);
    let (cells, nr) = read_strings("op_cells_train.csv");
    assert_eq!(nr, n);
    let label = |s: &str| -> usize { s.trim_start_matches('c').parse().unwrap() };
    let mut node_mismatch = 0;
    let mut meta_mismatch = 0;
    for (i, (node, meta)) in fitted.node.iter().zip(&fitted.metacluster).enumerate() {
        if *node != label(&cells[0][i]) {
            node_mismatch += 1;
        }
        if *meta != label(&cells[1][i]) {
            meta_mismatch += 1;
        }
    }
    assert_eq!(
        node_mismatch, 0,
        "nodes differ from R's NewData on {node_mismatch} of {n} cells"
    );
    assert_eq!(
        meta_mismatch, 0,
        "metaclusters differ from R's NewData on {meta_mismatch} of {n} cells"
    );
    assert_eq!(fitted.n_metaclusters, 5);
}

/// The model the operator writes (2.2.0) must be FlowSOM's own map: the codes bit for bit, and
/// the node medians and counts `fsom$map$medianValues` (scaling undone) and `mapping` give.
/// Golden: `fixtures/gen_model.R`, same image and map as `gen_operator.R`.
#[test]
fn the_model_is_flowsoms_map() {
    let (raw, n, p) = read_matrix("som_input.csv");
    let wide: Vec<f64> = raw.iter().map(|v| *v as f32 as f64).collect();
    let params = Params {
        xdim: 10,
        ydim: 10,
        clusters: Clusters::Fixed(5),
        rlen: 10,
        seed: 42,
    };
    let fitted = flowsom_operator::fit_and_assign(wide, n, p, None, true, &params);
    let markers: Vec<String> = (1..=p).map(|i| format!("m{i}")).collect();
    let counts = flowsom_operator::model::counts(&fitted.node, 100);
    let (c, sd) = fitted
        .scaling
        .as_ref()
        .expect("scale = true keeps its scaling");
    let json = flowsom_operator::model::Model {
        xdim: 10,
        ydim: 10,
        markers: &markers,
        codes: &fitted.codes,
        metaclustering: &fitted.metaclustering,
        counts: &counts,
        medians: &fitted.medians,
        scaling: Some((c, sd)),
        seed: 42,
        rlen: 10,
        n_cells: n,
        n_train: n,
    }
    .to_json();
    let m: serde_json::Value = serde_json::from_str(&json).unwrap();

    let (want_codes, ncodes, _) = read_matrix("op_codes.csv");
    for k in 0..ncodes {
        for j in 0..p {
            let got = m["codes"][k][j].as_f64().unwrap();
            assert_eq!(
                got.to_bits(),
                want_codes[k + j * ncodes].to_bits(),
                "code of node {k}, channel {j}"
            );
        }
    }
    let (want_meta, _, _) = read_matrix("op_metaclustering.csv");
    for k in 0..ncodes {
        assert_eq!(m["metaclustering"][k].as_f64().unwrap(), want_meta[k]);
    }

    let (med, rows) = read_strings("op_medians.csv");
    assert_eq!(rows, ncodes);
    let mut empty = 0;
    for k in 0..ncodes {
        let want_count: usize = med[p][k].parse().unwrap();
        assert_eq!(
            m["counts"][k].as_u64().unwrap() as usize,
            want_count,
            "count of node {k}"
        );
        if want_count == 0 {
            empty += 1;
            assert!(m["medians"][k].is_null(), "node {k} is empty");
            continue;
        }
        for j in 0..p {
            let want: f64 = med[j][k].parse().unwrap();
            let got = m["medians"][k][j].as_f64().unwrap();
            // R takes the median of the scaled values and the test unscales it in R's order;
            // the operator does the same in its own. A few ulp, not a different median.
            assert!(
                (got - want).abs() <= 1e-12 * want.abs().max(1.0),
                "median of node {k}, channel {j}: {got} vs R {want}"
            );
        }
    }
    assert_eq!(empty, 14, "the fixture map has 14 empty nodes");
}
