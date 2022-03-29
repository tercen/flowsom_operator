library(tercen)
library(dplyr)
library(flowCore)
# remotes::install_github("saeyslab/FlowSOM")
library(FlowSOM)
# remotes::install_github("tercen/tim")
library(tim)
library(MetaCyto)


get_FlowSOM_Clusters <- function(data, ctx,n.clust, seed) {
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
  cluster_num = fsom$metaclustering[GetClusters(fsom)]
  df_out <- data.frame(
    cluster_id = sprintf(paste0("c%0", max(nchar(as.character(cluster_num))), "d"), cluster_num)
  )
  
  clust_ids <- unique(df_out$cluster_id)
  clusters_as_list <- lapply(
    clust_ids,
    function(x) which(df_out$cluster_id == x)
  )
  names(clusters_as_list) <- clust_ids

  cluster_labels <- MetaCyto::labelCluster(
    fcsFrame = flow.dat,
    clusterList = clusters_as_list,
    minPercent = 0.05,
    labelQuantile = 0.95
  )

  df_out2 <- as_tibble(cluster_labels["clusterLabel"]) %>% 
    mutate(cluster_id = names(clusters_as_list)) %>%
    right_join(df_out, by = "cluster_id")
  
  return(list(df_out2, fsom))
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
