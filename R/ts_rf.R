#'@title Time Series Random Forest
#'@description The function receives as arguments the variables preprocess, input_size, nodesize and ntree
#'@param preprocess normalization
#'@param input_size input size for machine learning model
#'@param nodesize node size
#'@param ntree number of trees
#'@param mtry number of attributes to build tree
#'@return a `ts_rf` object.
#'@examples trans <- dal_transform()
#'@export
ts_rf <- function(preprocess=NA, input_size=NA, nodesize = 1, ntree = 10, mtry = NULL) {
  obj <- ts_regsw(preprocess, input_size)

  obj$nodesize <- nodesize
  obj$ntree <- ntree
  obj$mtry <- mtry

  class(obj) <- append("ts_rf", class(obj))
  return(obj)
}


#'@importFrom randomForest randomForest
#'@export
do_fit.ts_rf <- function(obj, x, y) {
  if (is.null(obj$mtry))
    obj$mtry <- ceiling(obj$input_size/3)
  obj$model <- randomForest::randomForest(x = as.data.frame(x), y = as.vector(y), mtry=obj$mtry, nodesize = obj$nodesize, ntree=obj$ntree)
  return(obj)
}

#'@importFrom stats predict
#'@export
do_predict.ts_rf <- function(obj, x) {
  prediction <- stats::predict(obj$model, as.data.frame(x))
  return(prediction)
}
