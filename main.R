suppressPackageStartupMessages({
  library(tercen)
  library(tercenApi)
  library(dplyr)
  library(flowCore)
  library(FlowSOM)
  library(tim)
})

ctx <- tercenCtx()

seed <- NULL
if(!ctx$op.value('seed') < 0) seed <- as.integer(ctx$op.value('seed'))
if(seed > 0) set.seed(seed)

n.clust <- NULL
if(!is.null(ctx$op.value('nclust')) && !ctx$op.value('nclust') == "NULL") n.clust <- as.integer(ctx$op.value('nclust'))

data <- ctx %>% 
  as.matrix() %>%
  t()

colnames(data) <- ctx$rselect()[[1]]

flow.dat <- flowCore::flowFrame(as.matrix(data))

xdim   = ifelse(is.null(ctx$op.value('xdim')), 10, as.integer(ctx$op.value('xdim')))
ydim   = ifelse(is.null(ctx$op.value('ydim')), 10, as.integer(ctx$op.value('ydim')))
rlen   = ifelse(is.null(ctx$op.value('rlen')), 10, as.integer(ctx$op.value('rlen')))
mst    = ifelse(is.null(ctx$op.value('mst')), 1, as.integer(ctx$op.value('mst')))
alpha  = c(
  ifelse(is.null(ctx$op.value('alpha_1')), 0.05, as.double(ctx$op.value('alpha_1'))),
  ifelse(is.null(ctx$op.value('alpha_2')), 0.01, as.double(ctx$op.value('alpha_2')))
)
distf  = ifelse(is.null(ctx$op.value('distf')), 2, as.integer(ctx$op.value('distf')))

# parameters for cluster labelling
minPercent  = ifelse(is.null(ctx$op.value('minPercent')), 0.05, as.double(ctx$op.value('minPercent')))
labelQuantile  = ifelse(is.null(ctx$op.value('labelQuantile')), 0.95, as.double(ctx$op.value('labelQuantile')))

maxMeta <- NULL
if(!is.null(ctx$op.value('maxMeta')) && !ctx$op.value('maxMeta') == "NULL") maxMeta <- as.integer(ctx$op.value('maxMeta'))

if(is.null(maxMeta) & is.null(n.clust)) maxMeta <- 10

fsom <- FlowSOM(
  input = flow.dat,
  compensate = FALSE,
  colsToUse = 1:ncol(flow.dat),
  nClus = n.clust,
  maxMeta = maxMeta,
  seed = seed,
  xdim = xdim,
  ydim = ydim, 
  rlen = rlen, 
  mst = mst, 
  alpha = alpha,
  distf = distf
)
fsom$data <- NULL
cluster_num = GetClusters(fsom)
metacluster_num = GetMetaclusters(fsom)

df_out <- data.frame(
  cluster_id = sprintf(paste0("c%0", max(nchar(as.character(cluster_num))), "d"), cluster_num),
  metacluster_id = sprintf(paste0("c%0", max(nchar(as.character(metacluster_num))), "d"), metacluster_num)
)

results <- list(df_out, fsom)

df_out <- results[[1]] %>%
  as_tibble() %>%
  mutate(.ci = seq_len(nrow(.)) - 1L)

model <- results[[2]]
res <- get_serialized_result(
  df = df_out,
  object = model,
  object_name = "flowsom_model", ctx = ctx
)

ctx$save(res)
