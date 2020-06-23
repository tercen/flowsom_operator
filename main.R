library(tercen)
library(dplyr)
library(flowCore)
library(FlowSOM)

get_FlowSOM_Clusters <- function(data) {
  
  colnames(data) <- ctx$rselect()[[1]]
  
  flow.dat <- flowCore::flowFrame(as.matrix(data))
  
  n.clust <- NULL
  if(!ctx$op.value('nclust') == "NULL") n.clust <- as.integer(ctx$op.value('nclust'))
  
  fsom <- FlowSOM(
    flow.dat,
    colsToUse = 1:ncol(flow.dat),
    nClus = n.clust,
    seed = as.integer(ctx$op.value('seed')),
    xdim   = as.integer(ctx$op.value('xdim')),
    ydim   = as.integer(ctx$op.value('ydim')), 
    rlen   = as.integer(ctx$op.value('rlen')), 
    mst    = as.integer(ctx$op.value('mst')), 
    alpha  = c(as.integer(ctx$op.value('alpha_start')),(as.double(ctx$op.value('alpha_end')))),
    distf  = as.integer(ctx$op.value('distf'))
  )
  
  cluster <- data.frame(
    cluster = as.character(fsom[[2]][fsom[[1]]$map$mapping[, 1]])
  )
  
  return(cluster)
}

ctx <- tercenCtx()

ctx %>% 
  as.matrix() %>%
  t() %>%
  get_FlowSOM_Clusters() %>%
  as_tibble() %>%
  mutate(.ci = seq_len(nrow(.)) - 1) %>%
  ctx$addNamespace() %>%
  ctx$save()
