#'@title Time Series Random Forest
#'@description The function receives as arguments the variables preprocess, input_size, nodesize and ntree
#'@param preprocess normalization
#'@param input_size input size for machine learning model
#'@param nodesize node size
#'@param ntree number of trees
#'@return a `ts_rf` object.
#'@examples trans <- dal_transform()
#'@export
ts_rf <- function(preprocess=NA, input_size=NA, nodesize = 5, ntree = 20) {
  obj <- tsreg_sw(preprocess, input_size)

  obj$nodesize <- nodesize
  obj$ntree <- ntree

  class(obj) <- append("ts_rf", class(obj))
  return(obj)
}


#'@title Allows parameters of a ts_rf object to be updated with new values
#'@description It receives as input a ts_rf object (obj) and a set of parameters (params)
#'@param obj object
#'@param params parameters
#'@return The ts_rf object updated with the new parameter values
#'@export
set_params.ts_rf <- function(obj, params) {
  if (!is.null(params$nodesize))
    obj$nodesize <- params$nodesize
  if (!is.null(params$ntree))
    obj$ntree <- params$ntree

  return(obj)
}

#'@title Fits a random forest model to time series
#'@description It receives as input a ts_rf object (obj), an input dataset (x) and an output dataset (y)
#'@param obj object
#'@param x input variable
#'@param y output variable
#'@return The updated ts_rf object
#'@importFrom randomForest randomForest
#'@export
do_fit.ts_rf <- function(obj, x, y) {
  obj$model <- randomForest::randomForest(x = as.data.frame(x), y = as.vector(y), mtry=ceiling(obj$input_size/3), nodesize = obj$nodesize, ntree=obj$ntree)
  return(obj)
}

#'@title Make predictions on a new dataset (x) using the fitted random forest model
#'@description It takes as input a ts_rf object (obj) and an input dataset (x)
#'@param obj object
#'@param x input variable
#'
#'@return The prediction vector
#'@importFrom stats predict
#'@export
do_predict.ts_rf <- function(obj, x) {
  prediction <- stats::predict(obj$model, as.data.frame(x))
  return(prediction)
}
