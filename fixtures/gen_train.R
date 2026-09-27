# Train-on-a-subset golden: the map is trained on every fifth row of som_input.csv (600 of 3000,
# the "Train" cells), then every row is mapped to it with NewData — what the operator does with
# train_factor/train_value. Same FlowSOM 1.22.0 / R 4.0.4 image and parameters as gen_operator.R.
suppressMessages({library(FlowSOM); library(flowCore)})
out <- "/out/"
w17 <- function(m, file) {
  df <- as.data.frame(lapply(as.data.frame(m), function(c)
    if (is.numeric(c)) sprintf("%.17g", c) else as.character(c)))
  write.csv(df, paste0(out, file), row.names = FALSE, quote = FALSE)
}
data <- as.matrix(read.csv(paste0(out, "som_input.csv")))
train_rows <- which(((seq_len(nrow(data)) - 1) %% 5) == 0)
set.seed(42)
fsom <- FlowSOM(input = flowCore::flowFrame(data[train_rows, , drop = FALSE]), compensate = FALSE,
                colsToUse = 1:ncol(data), nClus = 5, maxMeta = NULL, seed = 42,
                xdim = 10, ydim = 10, rlen = 10, mst = 1, alpha = c(0.05, 0.01), distf = 2)
fnew <- NewData(fsom$FlowSOM, flowCore::flowFrame(data))
cluster_num <- fnew$map$mapping[, 1]
metacluster_num <- as.integer(fsom$metaclustering)[cluster_num]
w17(data.frame(
  cluster_id = sprintf(paste0("c%0", max(nchar(as.character(cluster_num))), "d"), cluster_num),
  metacluster_id = sprintf(paste0("c%0", max(nchar(as.character(metacluster_num))), "d"), metacluster_num),
  is_train = as.integer(seq_len(nrow(data)) %in% train_rows)
), "op_cells_train.csv")
w17(fsom$FlowSOM$map$codes, "op_codes_train.csv")
w17(rbind(center = fsom$FlowSOM$scaled.center, scale = fsom$FlowSOM$scaled.scale), "op_scaling_train.csv")
cat("train rows:", length(train_rows), " nodes used:", length(unique(cluster_num)),
    " metaclusters:", length(unique(metacluster_num)), "\n")
