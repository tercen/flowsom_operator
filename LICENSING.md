# Licensing

`flowsom_rust_operator` is **GPL-2.0-only**, and that is forced, not chosen.

The metaclustering comes from [`flowsom-rs`](https://github.com/tercen/flowsom-rs), which ports
ConsensusClusterPlus. ConsensusClusterPlus is licensed **"GPL version 2"** with no "or later"
clause, so nothing that links it can be GPL-3, AGPL-3, or anything else incompatible with GPL-2.

| what | licence |
|---|---|
| this operator | GPL-2.0-only |
| `flowsom-rs` | GPL-2.0-only (pinned by ConsensusClusterPlus) |
| FlowSOM, R's `hclust` | GPL (>= 2) — would have allowed GPL-3 |
| tercen-rs | Apache-2.0 (since 2026-10-08; it said MIT with no licence text) |
| rustson | none yet (tercen/rustson#1 proposes Apache-2.0) |

The sibling operators: `cytonorm_rust_operator` moved from AGPL-3 to **GPL-2-or-later** on
2026-09-21 so that it could link `flowsom-rs`; it can also consume this operator's labels through
a `cluster_factor` instead, which needs no linking at all. `asinh_rust_operator` is GPL-2.0-or-later since 2026-10-08. `read_fcs_rust_operator` is Apache-2.0.

## Known gap: Apache-2.0-only dependencies (2026-10-08)

Every Rust operator binary links a few crates that are **Apache-2.0 only**, notably `prost`
(gRPC protobuf), `sync_wrapper`, `sqlparser` and `polars-arrow-format`. The FSF considers
Apache-2.0 incompatible with GPL-2.0-only, so the distributed FlowSOM image is not cleanly
licensed while `flowsom-rs` is GPL-2.0-only. GPL-2.0-or-later operators are fine: the combination
can be distributed under GPL-3. The fix is either permission from the ConsensusClusterPlus
authors to use "or later", or a clean-room reimplementation of the consensus step.
