# flowsom_rust_operator — notes for whoever works on this next

Built 2026-09-21 with the `create-rust-operator` skill. Read `README.md` first; this is the
part that is not obvious from the code.

## The shape of the problem is a transpose

A crosstab arrives as scattered `(.ri, .ci, .y)` triples. A self-organising map needs each
**cell's** vector across channels. There is no arrival order to rely on — R's own Tercen client
scatters by index rather than assuming one — so the operator gathers the whole matrix and the
memory model declares the cost, 16 bytes a value plus 90 MB.

Two alternatives were considered and rejected. Accumulating a distance to every node as the
values stream costs `n_cells × n_nodes`, which is worse than the data for a 10×10 map. Reading
the crosstab once per block of cells costs one pass per block — about 25 passes on a
19.5 M-cell study, which is 7 minutes of re-reading to save memory the booking can simply
declare.

## Three things that decide whether the clusters match R

1. **`f32`.** `flowCore::flowFrame()` stores expressions as 32-bit floats, so the R operator
   clusters values already rounded by up to 2.4e-7. Rounding here is what makes the two agree.
   It is a parity decision that happens to halve the peak, not the other way round.
2. **Compensated summation in the scaling.** R accumulates `colMeans` and `sum` in 80-bit long
   double. A plain `f64` loop is a few ulp away, and that moved **26% of cells to a different
   SOM node** on the reference data — the nearest-node test is chaotic under training. Their
   metaclusters all still agreed, which is a fair hint about which output to trust, but
   `cluster_id` is an output too. `flowsom::metacluster::scale_columns` uses Neumaier.
3. **Everything in `flowsom-rs`**: R's Mersenne-Twister and rejection sampler, `hclust`'s scan
   order, and the integer `abs` in FlowSOM's C. See that repository.

## Train mode and its fixture

`train_factor` / `train_value` / `train_cells` train on a labelled subset and map every cell, R's
`FlowSOM(subset)` then `NewData(all)`. Scaling parameters come from the training cells alone
(`flowsom::metacluster::column_scaling` on the training rows, `scale_columns_with` on all).
Goldens for this come from `fixtures/gen_train.R`, run in the same image as the others —
the image's entrypoint is the R operator, so override it:

```
docker run --rm -v "$PWD/fixtures:/w" -w /w --entrypoint Rscript lucas501/cytonorm_docker:1.1.9 gen_train.R
```

Never regenerate goldens with a local R: FlowSOM 2.x is not bit-compatible with 1.22.0.

## Deliberate differences from the R operator

- A **negative seed** is refused. The R operator reads it as "use a random seed", which makes
  the step unrepeatable.
- **`mst > 1`** is refused rather than ignored.
- The **serialised FlowSOM model** is replaced by a `Map` table — one row per node, its
  metacluster and its codes — because a Rust operator cannot write an R object, and because a
  table is readable by anything.
- **`maxMeta` is reproducible here and is not in R**: `FlowSOM:::consensus` calls
  ConsensusClusterPlus with no seed, so R reseeds from the clock. There is therefore no fixture
  for the whole `maxMeta` path, only for its parts.

## A second relation joined on nothing is a cross join

`0.1.1` shipped a `Map` table with `lColumns: [] / rColumns: []`, copied from `read_fcs`. There
it is right: an import operator has no input crosstab, so an unkeyed relation is just a separate
table. Here the left side is every cell, so an empty key is a cartesian product and every event
carries every row of the map. `phenograph_operator` emits one table; so does the R FlowSOM
operator; so does this one now.

No unit test could have caught it — the join happens inside Tercen, after the operator's bytes.
The platform's `OperatorUnitTest` (`tests/test.json`) can, because it diffs the assembled
relations. Every operator that returns anything to a crosstab needs one.

## The operatorSpec vocabulary is fixed, and inventing a kind fails at install

`0.1.0` shipped with `JoinSpec`, `RelationSpec` and `AttributeSpec` in `outputSpecsV2`. All three
were invented; the platform pulled the image, then refused the operator with
`Invalid argument (bad kind error): "JoinSpec"`. The real vocabulary, taken from the manifests
the platform has accepted:

```
outputSpecsV2: [ OperatorJoinSpec { joinOperators: [ JoinOperator {
    joinType, leftPair: ColumnPair { lColumns, rColumns },
    rightRelation: TableRelation { attributes: [ Attribute { name, type } ],
                                   meta_data: [ Pair { key, value } ] } } ] } ]
```

and, when the output shape depends on a property, `ConditionalJoinSpec { alternatives: [
OutputAlternative { condition, joinSpec: OperatorJoinSpec } ] }`. There is no bare `JoinSpec`.
`tests/r_parity.rs` now asserts every kind against that list — copy the shape from an operator
that installs rather than reading it off the model definition, which differs between versions.

## Two things only a live instance finds

Both cost a failed task each, after the operator had exited cleanly:

- A column must be a **typed** list. `w.list(n)` followed by `w.str(...)` per row looks right and
  is rejected with `Tbl deser failed -- expected type as LSTSTR,LSTU8, ... ,LSTF64`. Use
  `str_list`, `i32_list`, `f64_list`.
- `SimpleRelation` needs an **`index`** field. Without it the worker fails with
  `missing field \`index\`` and nothing in the operator's own log hints at it.

## Licence

GPL-2.0-only and not by choice: see `LICENSING.md`. It constrains what may link this.
