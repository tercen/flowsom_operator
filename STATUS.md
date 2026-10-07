# flowsom_operator (Rust, 2.x; developed as tercen/flowsom_rust_operator) — status, 2026-09-27

## Where it got to

A drop-in for the R `flowsom_operator`, agreeing with it bit for bit on the reference data:
every code, every node's metacluster, and all 3,000 `cluster_id` / `metacluster_id` strings
(`tests/r_parity.rs`, 10×10 map, `nclust = 5`, `scale = TRUE`). The clustering itself is
[`flowsom-rs`](https://github.com/tercen/flowsom-rs) 0.1.3.

## 0.1.4: `scale` defaults to false (2026-09-29)

The R operator inherits FlowSOM 1.22's `scale = TRUE`; FlowSOM 2.x and the Python `flowsom` package
never scale. Reproducing a Python-FlowSOM clustering on Tercen showed what scaling costs on an
asinh-transformed panel: same cells, same map, same k, ARI against the reference 0.938 unscaled vs 0.727
scaled with the lineage markers, and 0.879 vs 0.575 with all markers. Default flipped; the platform tests
pin `scale = true` explicitly so their goldens (made with 1.22 semantics) stay valid; pass `scale = true`
to reproduce the R operator.

## 0.1.3: train on a subset, map all (2026-09-27)

CytoNorm needs FlowSOM trained on the pooled reference samples with every sample's cells then
assigned to that map. Three properties: `train_factor` (a column factor of the projection),
`train_value` (the label of the training cells, default `Train`) and `train_cells` (a seeded cap
on how many, 0 = all; cytonormpy uses 6,000). Scaling is taken from the training cells only and
applied to everyone, which is what `NewData()` does with `scaled.center` / `scaled.scale`.
`flowsom-rs` 0.1.3 exposes that split (`column_scaling` / `scale_columns_with`).

Golden: `fixtures/gen_train.R` on the same FlowSOM 1.22.0 image, training on every fifth of the
3,000 reference cells (600) and mapping all; 87 of 100 nodes used, 5 metaclusters. The parity
test matches all 3,000 nodes and metaclusters and the scaling parameters to 1e-12. Second
platform test `tests/flowsom_train.json` on a long table with a `type` column (Train/Test).

Without `train_factor` the operator is unchanged, and `tests/test.json` still passes.

## 0.1.2: one relation, and a test that can see a join

`0.1.1` emitted a second `Map` relation joined on nothing. Against a crosstab that is a cartesian
product, and every event came back carrying all hundred rows of the map — colouring by
metacluster coloured nothing. Nothing in `cargo test` could have seen it: every test stopped at
the bytes the operator writes, and the join happens afterwards, inside Tercen.

Fixed by emitting one per-cell relation, as `phenograph_operator` and the R `flowsom_operator`
do. Verified through a Studio run on the R fixture: the assembled relation has 3,000 rows, one
per cell, and all 3,000 `cluster_id` / `metacluster_id` labels match the R operator's.
`tests/test.json` now carries that as the platform's own unit test, with the expected files
taken from that run after their content was checked against R.

The same `[]` join declares `asinh_rust_operator`'s Cofactors table in `auto` mode. Being checked.

## Measured on a Studio dev run (2026-09-21)

| projection | wall | peak RSS |
|---|---|---|
| 1,200 cells x 4 channels, 5x5 map, nclust 4 | 0.2 s | 12 MB |
| 100,000 cells x 20 channels, 10x10 map, nclust 6 | 3.4 s | 80 MB |

That is **34 bytes per value** between the two points: 12 for the gathered `f32` matrix and the
`f64` copy the map trains on, and the rest a constant — one decoded chunk of a million rows,
plus the binary and tonic. The model books `0.000014 x n_main + 90 MB`, which is 118 MB where
80 was used.

Two things the run found, both now fixed and both invisible until a real instance saw them: a
column has to be written as a **typed** list (`str_list`, not a list of strings, or the server
says `expected type as LSTSTR,LSTU8, ...`), and `SimpleRelation` needs an **`index`** field or
the task fails with `missing field \`index\`` long after the operator has exited cleanly.

## The scale limit, stated plainly

The booking is linear in the crosstab, so a cohort-scale projection asks for a cohort-scale
machine: 19.5 M cells x 43 channels is 838 M values, which is **about 12 GB**. That is the real
cost of gathering a transpose, and the R operator does not do better — it holds doubles and
copies them.

The fix, when it is needed, is not more passes: reading the crosstab once takes about a second
per 2 M values, so assigning in 480 MB blocks would be 21 passes and two and a half hours.
Gather once to a spill file (838 M values is 3.4 GB of `f32` on disk), then stream it back. The
operator already spills nothing today, so this is unwritten.

Until then: subsample upstream, which is what CytoNorm itself does — `prepareFlowSOM` trains on
at most a million cells.

- **Speed of the metaclustering** is quadratic in the number of nodes: a 15x15 map (225 nodes)
  costs about five times a 10x10. The 10x10 above took 2.3 s of the 3.4.

## Deliberately absent

- `mst > 1` — refuses, rather than ignoring the property.
- A negative `seed` — refuses, rather than clustering unrepeatably. The R operator allows it.
- The serialised FlowSOM model object; the `Map` table replaces it.
- `maxMeta` has **no reproducible R reference to test against**: `FlowSOM:::consensus` calls
  ConsensusClusterPlus without a seed, so R seeds it from the clock and gives a different number
  of clusters on every run. The parts — `SSE`, `findElbow`, the consensus itself — are each
  tested against R; the composition cannot be.

## Next

1. A Studio dev run, then refit the memory model from the measurement.
2. Chain it into CytoNorm through `cluster_factor` and check a two-cluster normalisation end to
   end. That is the reason this operator exists.
