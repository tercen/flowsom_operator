# Exactly what the R flowsom_operator computes, on synthetic data: FlowSOM with the operator's
# own defaults (scale = TRUE, inherited from FlowSOM()), a fixed nclust so it is reproducible.
suppressMessages({library(FlowSOM); library(flowCore)})
out <- "/out/"
w17 <- function(m, file) {
  df <- as.data.frame(lapply(as.data.frame(m), function(c)
    if (is.numeric(c)) sprintf("%.17g", c) else as.character(c)))
  write.csv(df, paste0(out, file), row.names = FALSE, quote = FALSE)
}
data <- as.matrix(read.csv(paste0(out, "som_input.csv")))
flow.dat <- flowCore::flowFrame(as.matrix(data))
set.seed(42)
fsom <- FlowSOM(input = flow.dat, compensate = FALSE, colsToUse = 1:ncol(flow.dat),
                nClus = 5, maxMeta = NULL, seed = 42,
                xdim = 10, ydim = 10, rlen = 10, mst = 1, alpha = c(0.05, 0.01), distf = 2)
cluster_num <- GetClusters(fsom)
metacluster_num <- GetMetaclusters(fsom)
w17(data.frame(
  cluster_id = sprintf(paste0("c%0", max(nchar(as.character(cluster_num))), "d"), cluster_num),
  metacluster_id = sprintf(paste0("c%0", max(nchar(as.character(metacluster_num))), "d"), metacluster_num)
), "op_cells.csv")
w17(fsom$FlowSOM$map$codes, "op_codes.csv")
w17(matrix(as.integer(fsom$metaclustering), ncol = 1), "op_metaclustering.csv")
w17(fsom$FlowSOM$data, "op_input_scaled.csv")
cat("nodes used:", length(unique(cluster_num)), " metaclusters:", length(unique(metacluster_num)), "\n")
