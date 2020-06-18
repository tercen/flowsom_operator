library(tercen)
library(dplyr)
library(flowCore)
library(FlowSOM)

options("tercen.workflowId" = "7eee20aa9d6cc4eb9d7f2cc2430313b6")
options("tercen.stepId"     = "d9da8ab3-ea4f-4da6-88d2-2d8e566040fe")

getOption("tercen.workflowId")
getOption("tercen.stepId")

get_FlowSOM_Clusters <- function(data) {
  colnames(data) <- ctx$rselect()[[1]]
  dat <- flowCore::flowFrame(as.matrix(data))
  fsom <- FlowSOM(
    dat,
    colsToUse = 1:ncol(dat),
    nClus = 20,
    seed = 1
  )
  cluster <- data.frame(cluster=as.character(fsom[[2]][fsom[[1]]$map$mapping[, 1]]))
  return(cluster)
}

ctx <- tercenCtx()

ctx %>% 
  as.matrix() %>%
  t() %>%
  get_FlowSOM_Clusters() %>%
  as_tibble() %>%
  mutate(.ci = seq_len(nrow(.))-1) %>%
  ctx$addNamespace() %>%
  ctx$save()
