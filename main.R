library(tercen)
library(dplyr)
library(flowCore)
library(FlowSOM)

get_FlowSOM_Clusters <- function(data) {
  
  colnames(data) <- ctx$rselect()[[1]]
  
  flow.dat <- flowCore::flowFrame(as.matrix(data))
  
  fsom <- FlowSOM(
    dat,
    colsToUse = 1:ncol(flow.dat),
    nClus = as.integer(ctx$op.value('nclust')),
    seed = 1
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
