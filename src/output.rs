//! The result: one row per **column** of the crosstab — one per cell — carrying the SOM node
//! and the metacluster it landed in.
//!
//! Shape follows the R `flowsom_operator`, which emits `cluster_id` and `metacluster_id` as
//! zero-padded strings (`c001`) keyed by `.ci`. Strings rather than numbers because they are
//! labels: a downstream step colours or facets by them, and a number invites arithmetic on a
//! cluster index. The zero padding is what makes them sort correctly in the UI.
//!
//! The R operator also serialises the FlowSOM model object. A Rust operator cannot write an R
//! object, so the map itself goes out as a second table instead: one row per node, its
//! metacluster, and its codes.
use std::io::Write;

use anyhow::{Result, anyhow};

use crate::tson::TsonWriter;

pub struct ColSpec<'a> {
    pub name: &'a str,
    pub ty: &'a str, // "double" | "int32" | "string"
}

/// Name of the second output relation: the trained map.
pub const MAP: &str = "Map";

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
    namespace: &str,
    node: &[usize],
    metacluster: &[usize],
) -> Result<()> {
    let n = node.len();
    let cluster_name = format!("{namespace}.cluster_id");
    let meta_name = format!("{namespace}.metacluster_id");
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
            name: ".ci",
            ty: "int32",
        },
    ];
    write_header(w, "cells", n, &cols, 2)?;

    let node_width = label_width(node.iter().copied().max().unwrap_or(1));
    write_column_header(w, &cols[0], n)?;
    w.list(n)?;
    for v in node {
        w.str(&label(*v, node_width))?;
    }

    let meta_width = label_width(metacluster.iter().copied().max().unwrap_or(1));
    write_column_header(w, &cols[1], n)?;
    w.list(n)?;
    for v in metacluster {
        w.str(&label(*v, meta_width))?;
    }

    write_column_header(w, &cols[2], n)?;
    w.i32_list_header(n)?;
    let mut buf = Vec::with_capacity(n * 4);
    for i in 0..n {
        buf.extend_from_slice(&(i as i32).to_le_bytes());
    }
    w.raw(&buf)?;
    Ok(())
}

/// The map: one row per node, its metacluster, and its codes — the replacement for the R
/// operator's serialised model object. It is what lets someone see *why* a cell was clustered
/// where it was, and it is the thing to keep if the map is to be applied to another dataset.
pub fn write_map<W: Write>(
    w: &mut TsonWriter<W>,
    channels: &[String],
    codes: &[f64],
    ncodes: usize,
    metaclustering: &[usize],
) -> Result<()> {
    let p = channels.len();
    let mut cols: Vec<ColSpec> = vec![
        ColSpec {
            name: "node",
            ty: "string",
        },
        ColSpec {
            name: "metacluster",
            ty: "string",
        },
    ];
    for c in channels {
        cols.push(ColSpec {
            name: c,
            ty: "double",
        });
    }

    w.map(4)?;
    w.key("kind")?;
    w.str("Table")?;
    w.key("nRows")?;
    w.i32(ncodes as i32)?;
    w.key("properties")?;
    w.map(4)?;
    w.key("kind")?;
    w.str("TableProperties")?;
    w.key("name")?;
    w.str(MAP)?;
    w.key("sortOrder")?;
    w.list(0)?;
    w.key("ascending")?;
    w.bool(false)?;
    w.key("columns")?;
    w.list(cols.len())?;

    let node_width = label_width(ncodes);
    write_column_header(w, &cols[0], ncodes)?;
    w.list(ncodes)?;
    for i in 1..=ncodes {
        w.str(&label(i, node_width))?;
    }

    let meta_width = label_width(metaclustering.iter().copied().max().unwrap_or(1));
    write_column_header(w, &cols[1], ncodes)?;
    w.list(ncodes)?;
    for v in metaclustering {
        w.str(&label(*v, meta_width))?;
    }

    for j in 0..p {
        write_column_header(w, &cols[2 + j], ncodes)?;
        w.f64_list_header(ncodes)?;
        let mut buf = Vec::with_capacity(ncodes * 8);
        for i in 0..ncodes {
            buf.extend_from_slice(&codes[i + j * ncodes].to_le_bytes());
        }
        w.raw(&buf)?;
    }
    Ok(())
}

/// Close the result: the map is a standalone relation beside the per-cell one.
pub fn write_footer<W: Write>(w: &mut TsonWriter<W>) -> Result<()> {
    w.key("joinOperators")?;
    w.list(1)?;
    w.map(4)?;
    w.key("kind")?;
    w.str("JoinOperator")?;
    w.key("joinType")?;
    w.str("")?;
    w.key("leftPair")?;
    write_column_pair(w)?;
    w.key("rightRelation")?;
    write_simple_relation(w, MAP)?;
    w.flush()?;
    Ok(())
}

fn write_column_pair<W: Write>(w: &mut TsonWriter<W>) -> Result<()> {
    w.map(3)?;
    w.key("kind")?;
    w.str("ColumnPair")?;
    w.key("lColumns")?;
    w.list(0)?;
    w.key("rColumns")?;
    w.list(0)?;
    Ok(())
}

fn write_simple_relation<W: Write>(w: &mut TsonWriter<W>, name: &str) -> Result<()> {
    w.map(3)?;
    w.key("kind")?;
    w.str("SimpleRelation")?;
    w.key("id")?;
    w.str(name)?;
    w.key("inMemoryRelation")?;
    w.bool(false)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

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
