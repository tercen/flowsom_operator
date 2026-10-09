# Golden for the model output (2.2.0): FlowSOM 1.22.0's own node medians, with the scaling
# undone, for the same map as gen_operator.R. Run as gen_operator.R is:
#   docker run --rm -v "$PWD:/out" --entrypoint Rscript lucas501/cytonorm_docker:1.1.9 /out/gen_model.R
suppressMessages({library(FlowSOM); library(flowCore)})
out <- "/out/"
data <- as.matrix(read.csv(paste0(out, "som_input.csv")))
flow.dat <- flowCore::flowFrame(as.matrix(data))
set.seed(42)
fsom <- FlowSOM(input = flow.dat, compensate = FALSE, colsToUse = 1:ncol(flow.dat),
                nClus = 5, maxMeta = NULL, seed = 42,
                xdim = 10, ydim = 10, rlen = 10, mst = 1, alpha = c(0.05, 0.01), distf = 2)
f <- fsom$FlowSOM
med <- f$map$medianValues
med <- sweep(sweep(med, 2, f$scaled.scale, "*"), 2, f$scaled.center, "+")
df <- as.data.frame(lapply(as.data.frame(med), function(c) sprintf("%.17g", c)))
df$count <- tabulate(f$map$mapping[, 1], nrow(f$map$codes))
write.csv(df, paste0(out, "op_medians.csv"), row.names = FALSE, quote = FALSE)
cat("empty nodes:", sum(df$count == 0), "\n")
