library(tercen)
library(dplyr)
library(flowCore)
# remotes::install_github("saeyslab/FlowSOM")
library(FlowSOM)
# remotes::install_github("tercen/tim")
library(tim)


get_FlowSOM_Clusters <- function(data, ctx) {
  colnames(data) <- ctx$rselect()[[1]]
  
  flow.dat <- flowCore::flowFrame(as.matrix(data))
  
  n.clust <- NULL
  if(!is.null(ctx$op.value('nclust')) && !ctx$op.value('nclust') == "NULL") n.clust <- as.integer(ctx$op.value('nclust'))
  
  seed <- NULL
  if(!is.null(ctx$op.value('seed')) && !ctx$op.value('seed') == "NULL") seed <- as.integer(ctx$op.value('seed'))
  
  xdim   = ifelse(is.null(ctx$op.value('xdim')), 10, as.integer(ctx$op.value('xdim')))
  ydim   = ifelse(is.null(ctx$op.value('ydim')), 10, as.integer(ctx$op.value('ydim')))
  rlen   = ifelse(is.null(ctx$op.value('rlen')), 10, as.integer(ctx$op.value('rlen')))
  mst    = ifelse(is.null(ctx$op.value('mst')), 1, as.integer(ctx$op.value('mst')))
  alpha  = c(
    ifelse(is.null(ctx$op.value('alpha_1')), 0.05, as.double(ctx$op.value('alpha_1'))),
    ifelse(is.null(ctx$op.value('alpha_2')), 0.01, as.double(ctx$op.value('alpha_2')))
  )
  distf  = ifelse(is.null(ctx$op.value('distf')), 2, as.integer(ctx$op.value('distf')))
  
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
  cluster_num = fsom$metaclustering[GetClusters(fsom)]
  df_out <- data.frame(
    cluster_id = sprintf(paste0("c%0", nchar(as.character(cluster_num)), "d"), cluster_num)
    #cluster_id = as.character(fsom$metaclustering[GetClusters(fsom)])
  )
  return(list(df_out, fsom))
}

ctx <- tercenCtx()

seed <- NULL
if(!ctx$op.value('seed') < 0) seed <- as.integer(ctx$op.value('seed'))

set.seed(seed)

nclust <- NULL
if(!is.null(ctx$op.value('nclust')) && !ctx$op.value('nclust') == "NULL") nclust <- as.integer(ctx$op.value('nclust'))

results <- ctx %>% 
  as.matrix() %>%
  t() %>%
  get_FlowSOM_Clusters(., ctx, nclust, seed)

df_out <- results[[1]] %>%
  as_tibble() %>%
  mutate(.ci = seq_len(nrow(.)) - 1)

model <- results[[2]]
res <- get_serialized_result(
  df = df_out,
  object = model,
  object_name = "flowsom_model", ctx = ctx
)

ctx$save(res)
