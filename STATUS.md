# flowsom_rust_operator — status, 2026-09-21

## Where it got to

A drop-in for the R `flowsom_operator`, agreeing with it bit for bit on the reference data:
every code, every node's metacluster, and all 3,000 `cluster_id` / `metacluster_id` strings
(`tests/r_parity.rs`, 10×10 map, `nclust = 5`, `scale = TRUE`). The clustering itself is
[`flowsom-rs`](https://github.com/tercen/flowsom-rs) 0.1.2.

## Not measured yet

- **The memory model is a calculation, not a measurement**: 16 bytes a value plus 90 MB, from
  4 bytes for the gathered `f32` matrix, 8 for the `f64` copy the map trains on, and the result.
  It needs a dev run on a real crosstab and a refit, as every other operator in this family did.
- **Speed at scale.** The hundred consensus resamples are quadratic in the number of nodes, so a
  15×15 map (225 nodes) costs about five times a 10×10. Not yet timed on a cohort.
- **No Studio dev run.** The parity tests need no Tercen instance; the read and write path does.

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
