//! The result: one row per **column** of the crosstab — one per cell — carrying the SOM node
//! and the metacluster it landed in.
//!
//! Shape follows the R `flowsom_operator`, which emits `cluster_id` and `metacluster_id` as
//! zero-padded strings (`c001`) keyed by `.ci`. Strings rather than numbers because they are
//! labels: a downstream step colours or facets by them, and a number invites arithmetic on a
//! cluster index. The zero padding is what makes them sort correctly in the UI.
//!
//! **One table, and only one.** The map used to go out as a second relation joined on nothing,
//! which is a cross join: every event got every one of the map's rows, so colouring by
//! metacluster coloured nothing. `phenograph_operator` and the R `flowsom_operator` both emit a
//! single per-cell table, and that is the shape that works. The R operator's serialised model
//! object has no Rust equivalent and is simply not produced.
use std::io::Write;

use anyhow::{Result, anyhow};

use crate::tson::TsonWriter;

pub struct ColSpec<'a> {
    pub name: &'a str,
    pub ty: &'a str, // "double" | "int32" | "string"
}

/// `sprintf("c%0Nd", i)` — the width comes from the largest label, as the R operator does it.
pub fn label(i: usize, width: usize) -> String {
    format!("c{i:0width$}")
}

/// Width the R operator uses: the digits of the largest value present.
pub fn label_width(max: usize) -> usize {
    max.max(1).to_string().len()
}

pub fn write_header<W: Write>(
    w: &mut TsonWriter<W>,
    table_name: &str,
    n_rows: usize,
    cols: &[ColSpec],
    n_tables: usize,
) -> Result<()> {
    w.map(3)?;
    w.key("kind")?;
    w.str("OperatorResult")?;
    w.key("tables")?;
    w.list(n_tables)?;

    w.map(4)?;
    w.key("kind")?;
    w.str("Table")?;
    w.key("nRows")?;
    w.i32(i32::try_from(n_rows).map_err(|_| {
        anyhow!(
            "the result would have {n_rows} rows, more than a Tercen table can hold (i32::MAX). \
             Project fewer cells, or split the step."
        )
    })?)?;
    w.key("properties")?;
    w.map(4)?;
    w.key("kind")?;
    w.str("TableProperties")?;
    w.key("name")?;
    w.str(table_name)?;
    w.key("sortOrder")?;
    w.list(0)?;
    w.key("ascending")?;
    w.bool(false)?;
    w.key("columns")?;
    w.list(cols.len())?;
    Ok(())
}

pub fn write_column_header<W: Write>(
    w: &mut TsonWriter<W>,
    c: &ColSpec,
    n_rows: usize,
) -> Result<()> {
    w.map(6)?;
    w.key("kind")?;
    w.str("Column")?;
    w.key("name")?;
    w.str(c.name)?;
    w.key("type")?;
    w.str(c.ty)?;
    w.key("nRows")?;
    w.i32(n_rows as i32)?;
    w.key("size")?;
    w.i32(n_rows as i32)?;
    w.key("values")?;
    Ok(())
}

/// The per-cell table: `.ci`, the node and the metacluster.
pub fn write_cells<W: Write>(
    w: &mut TsonWriter<W>,
    table_name: &str,
    namespace: &str,
    node: &[usize],
    metacluster: &[usize],
    xdim: usize,
) -> Result<()> {
    let n = node.len();
    let cluster_name = format!("{namespace}.cluster_id");
    let meta_name = format!("{namespace}.metacluster_id");
    let x_name = format!("{namespace}.som_x");
    let y_name = format!("{namespace}.som_y");
    let cols = [
        ColSpec {
            name: &cluster_name,
            ty: "string",
        },
        ColSpec {
            name: &meta_name,
            ty: "string",
        },
        ColSpec {
            name: &x_name,
            ty: "double",
        },
        ColSpec {
            name: &y_name,
            ty: "double",
        },
        ColSpec {
            name: ".ci",
            ty: "int32",
        },
    ];
    write_header(w, table_name, n, &cols, 1)?;

    // `str_list`, not a generic list of strings: the server reads a column as a typed list and
    // rejects anything else with "expected type as LSTSTR,LSTU8, …".
    // Width from the largest value actually present, which is what R does:
    // `sprintf("c%0*d", max(nchar(as.character(clust))), clust)`. The two columns are widened
    // independently, as they are there.
    let node_width = label_width(node.iter().copied().max().unwrap_or(1));
    write_column_header(w, &cols[0], n)?;
    w.str_list(
        &node
            .iter()
            .map(|v| label(*v, node_width))
            .collect::<Vec<_>>(),
    )?;

    let meta_width = label_width(metacluster.iter().copied().max().unwrap_or(1));
    write_column_header(w, &cols[1], n)?;
    w.str_list(
        &metacluster
            .iter()
            .map(|v| label(*v, meta_width))
            .collect::<Vec<_>>(),
    )?;

    // The node's place on the SOM grid, 1-based, laid out as FlowSOM's `expand.grid(1:xdim,
    // 1:ydim)`: node k is at column (k-1) % xdim + 1, row (k-1) / xdim + 1. For grid plots.
    let (gx, gy) = grid_xy(node, xdim);
    write_column_header(w, &cols[2], n)?;
    w.f64_list(&gx)?;
    write_column_header(w, &cols[3], n)?;
    w.f64_list(&gy)?;

    write_column_header(w, &cols[4], n)?;
    w.i32_list(&(0..n as i32).collect::<Vec<_>>())?;
    Ok(())
}

/// Grid column and row (1-based) of each 1-based node on an `xdim`-wide map.
pub fn grid_xy(node: &[usize], xdim: usize) -> (Vec<f64>, Vec<f64>) {
    node.iter()
        .map(|&k| (((k - 1) % xdim + 1) as f64, ((k - 1) / xdim + 1) as f64))
        .unzip()
}

/// Close the result. One relation, so no joins to declare.
pub fn write_footer<W: Write>(w: &mut TsonWriter<W>) -> Result<()> {
    w.key("joinOperators")?;
    w.list(0)?;
    w.flush()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn grid_positions_follow_expand_grid() {
        let (x, y) = grid_xy(&[1, 10, 11, 100], 10);
        assert_eq!(x, vec![1.0, 10.0, 1.0, 10.0]);
        assert_eq!(y, vec![1.0, 1.0, 2.0, 10.0]);
    }

    #[test]
    fn labels_are_zero_padded_to_the_widest() {
        assert_eq!(label_width(9), 1);
        assert_eq!(label_width(10), 2);
        assert_eq!(label_width(100), 3);
        assert_eq!(label(7, 3), "c007");
        assert_eq!(label(100, 3), "c100");
    }

    #[test]
    fn a_result_bigger_than_an_i32_is_an_error_not_a_panic() {
        let mut w = TsonWriter::new(Vec::new()).unwrap();
        let cols = [ColSpec {
            name: ".ci",
            ty: "int32",
        }];
        let e = write_header(&mut w, "cells", i32::MAX as usize + 1, &cols, 1).unwrap_err();
        assert!(e.to_string().contains("more than a Tercen table can hold"));
    }
}
