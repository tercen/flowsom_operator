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
| tercen-rs, rustson | MIT |

The sibling operators: `cytonorm_rust_operator` moved from AGPL-3 to **GPL-2-or-later** on
2026-09-21 so that it could link `flowsom-rs`; it can also consume this operator's labels through
a `cluster_factor` instead, which needs no linking at all. `asinh_rust_operator` is AGPL-3 and
cannot link `flowsom-rs` as it stands. `read_fcs_rust_operator` is Apache-2.0.
