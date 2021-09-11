library(tercen)
library(dplyr)
library(flowCore)
library(FlowSOM)

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
  maxMeta  = ifelse(is.null(ctx$op.value('maxMeta')), 10, as.integer(ctx$op.value('maxMeta')))
  
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
  
  df_out <- data.frame(
    cluster_id = as.character(fsom$metaclustering)
  )
  return(list(df_out, fsom))
}

# here the key for the join start by "." (.model) so it wont be displayed to the user
serialize_to_string <- function(object) {
  con <- rawConnection(raw(0), "r+")
  saveRDS(object, con)
  str64 <- base64enc::base64encode(rawConnectionValue(con))
  close(con)
  return(str64)
}
serialise_to_string <- serialize_to_string

deserialize_from_string <- function(str64) {
  con <- rawConnection(base64enc::base64decode(str64), "r+")
  object <- readRDS(con)
  close(con)
  return(object)
}
deserialise_from_string <- deserialize_from_string

get_serialized_result <- function(df, object, object_name, ctx) {
  
  df$.object <- object_name
  
  # explicitly create relation with .ci and input data
  columnTable <- ctx$cselect() %>%
    mutate(.ci = 0:(nrow(.) - 1))
  
  leftTable <- data.frame(df) %>%
    ctx$addNamespace() %>%
    left_join(columnTable, by = ".ci") %>%
    select(-.ci) %>%
    tercen::dataframe.as.table()
  leftTable$properties$name = 'left'
  
  leftRelation <- SimpleRelation$new()
  leftRelation$id <- leftTable$properties$name
  
  # the factor where the binary data base64 encoded is stored MUST start by a "." character so it wont be displayed to the user
  # the factor used in the join relation MUST have a different name then the one used in the leftTable 
  rightTable <- data.frame(
    model = object_name,
    .base64.serialized.r.model = c(serialize.to.string(object))
  ) %>%
    ctx$addNamespace() %>%
    tercen::dataframe.as.table()
  
  rightTable$properties$name <- 'right'
  rightRelation <- SimpleRelation$new()
  rightRelation$id <- rightTable$properties$name
  
  pair <- ColumnPair$new()
  pair$lColumns <- list(".object") # column name of the leftTable to use for the join
  pair$rColumns = list(rightTable$columns[[1]]$name) # column name of the rightTable to use for the join (note : namespace has been added)
  # pair
  
  join.model = JoinOperator$new()
  join.model$rightRelation = rightRelation
  join.model$leftPair = pair
  
  # create the join relationship using a composite relation (think at a start schema)
  compositeRelation = CompositeRelation$new()
  compositeRelation$id = "compositeRelation"
  compositeRelation$mainRelation = leftRelation
  compositeRelation$joinOperators = list(join.model)
  
  # finally return a JoinOperator to tercen with the composite relation
  join = JoinOperator$new()
  join$rightRelation = compositeRelation
  
  result = OperatorResult$new()
  result$tables = list(leftTable, rightTable)
  result$joinOperators = list(join)
  
  return(result)
}

ctx <- tercenCtx()

results <- ctx %>% 
  as.matrix() %>%
  t() %>%
  get_FlowSOM_Clusters(., ctx)

df_out <- results[[1]] %>%
  as_tibble() %>%
  mutate(.ci = seq_len(nrow(.)) - 1)

model <- results[[2]]
res <- get_serialized_result(df = df_out, object = model, object_name = "flowsom_model", ctx = ctx)

ctx$save(res)