# flowsom_rust_operator

FlowSOM clustering for Tercen, in Rust. A **drop-in for the R `flowsom_operator`**: the same
projection, the same property names, the same output columns, and the same clusters — bit for
bit — without the R runtime.

| | |
|---|---|
| projection | rows = channels, columns = cells, y = value |
| output | one row per cell: `cluster_id` (SOM node) and `metacluster_id` — one table, as phenograph and the R operator emit |
| image | `ghcr.io/tercen/flowsom_rust_operator` |

## Properties

The R operator's names, unchanged, so a workflow can swap one step for the other.

| property | default | meaning |
|---|---|---|
| `nclust` | NULL | Number of metaclusters. Setting it is the reproducible and the faster choice. |
| `maxMeta` | NULL | Largest number to consider; the elbow of the within-cluster sum of squares picks one. Ignored when `nclust` is set; 10 is used if neither is. |
| `seed` | 42 | Must be non-negative. The R operator treats a negative seed as "random"; this refuses, because a step that cannot be re-run to the same answer is a bug. |
| `xdim`, `ydim` | 10, 10 | The SOM grid. |
| `rlen` | 10 | Passes over the data while training. |
| `mst` | 1 | Only 1 is supported — see below. |
| `alpha_1`, `alpha_2` | 0.05, 0.01 | Learning rate at the start and the end. |
| `distf` | 2 | 1 Manhattan, 2 Euclidean, 3 Chebyshev, 4 cosine. |
| `scale` | true | Centre each channel and divide by its standard deviation. `FlowSOM()`'s own default, which the R operator inherits by not passing anything. Turn it off when the channels are already comparable, as after an asinh transform with per-channel cofactors. |

## Parity

The clustering lives in [`flowsom-rs`](https://github.com/tercen/flowsom-rs), which reproduces
FlowSOM 1.22.0 on R 4.0.4 bit for bit — R's random stream, `hclust`'s tie-breaking, and
ConsensusClusterPlus's hundred resamples included.

Two layers. `tests/test.json` is the platform's own `OperatorUnitTest`: it projects
`flowsom_golden_long.csv` (channels on rows, cells on columns), runs the operator with
`nclust = 5` on a 10×10 map, and diffs the **assembled relations** — `table1.csv` is the per-cell
result, `table2.csv` the column table, `table3.csv` the row table. Its expected labels were
checked against the R operator's on the same data before they were committed: 3,000 of 3,000
`cluster_id` and `metacluster_id` agree. This is the test that sees a join, which nothing in
`cargo test` can.

`tests/r_parity.rs` tests the **operator's** pipeline, not only the crate's: `tests/r_parity.rs`
takes the same synthetic data through the three steps in the order the operator does them and
compares with what the R operator emits. On 3,000 cells and a 10×10 map it agrees on every code,
every node's metacluster, and all 3,000 `cluster_id` / `metacluster_id` strings.

Two details it took measurements to get right, both worth knowing if you ever compare a
clustering against R:

- **flowCore stores expressions as 32-bit floats.** `flowCore::flowFrame(as.matrix(data))`, which
  the R operator calls, rounds every value by up to 2.4e-7. This operator rounds to `f32` and back
  for exactly that reason. Keeping full precision would be more accurate and would not be FlowSOM.
- **A few ulp in the scaling moves a quarter of the cells.** R accumulates `colMeans` and `sum`
  in 80-bit long double; a plain `f64` loop lands a few ulp away, and because the nearest-node
  test is chaotic under training, 26% of cells landed on a *different node* (their metaclusters
  all agreed, which says something about which of the two outputs to trust). Compensated
  summation is correctly rounded and matches R exactly, so the node ids match too.

## Memory

The crosstab arrives as scattered `(.ri, .ci, .y)` triples and a map needs each **cell's** vector
across channels, which is the transpose. There is no order to rely on — R's own client scatters
by index — so the operator gathers the matrix itself and the memory model declares that cost:
roughly **16 bytes per value** plus 90 MB. That is the honest shape of the problem; it is not
hidden in a subsample.

## What is not here

- **`mst` above 1.** FlowSOM then retrains on distances taken from a minimum spanning tree of the
  codes. Not ported; the operator refuses rather than silently ignoring it.
- **The serialised FlowSOM model.** A Rust operator cannot write an R object, and it is not
  replaced by a second table either: `0.1.1` shipped a `Map` relation joined on nothing, which
  against a crosstab is a cartesian product — every event came back carrying every row of it.
  One relation, per cell, is the shape that works.
- **A negative seed.** See `seed` above.

## Licence

GPL-2.0-only. See `LICENSING.md` — it is not a free choice.
