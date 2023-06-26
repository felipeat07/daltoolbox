#'@title Time Series Extreme Learning Machine (ELM)
#'@description Machine learning technique used for time series forecasting.
#' ELM is a type of feedforward neural network that uses a single hidden layer
#' of randomly generated neurons.
#'@param preprocess normalization
#'@param input_size input size for machine learning model
#'@param nhid ensemble size
#'@param actfun string: defines the type to use, possible values: 'sig',
#' 'radbas', 'tribas', 'relu', 'purelin' (default).
#'@return a `ts_elm` object.
#'@examples trans <- dal_transform()
#'@export
ts_elm <- function(preprocess=NA, input_size=NA, nhid=NA, actfun='purelin') {
  obj <- ts_regsw(preprocess, input_size)
  if (is.na(nhid))
    nhid <- input_size/3
  obj$nhid <- nhid
  obj$actfun <- as.character(actfun)

  class(obj) <- append("ts_elm", class(obj))
  return(obj)
}

#'@import elmNNRcpp
#'@export
do_fit.ts_elm <- function(obj, x, y) {
  obj$model <- elmNNRcpp::elm_train(x, y, nhid = obj$nhid, actfun = obj$actfun, init_weights = "uniform_positive", bias = FALSE, verbose = FALSE)
  return(obj)
}

#'@import elmNNRcpp
#'@export
do_predict.ts_elm <- function(obj, x) {
  if (is.data.frame(x))
    x <- as.matrix(x)
  prediction <- elmNNRcpp::elm_predict(obj$model, x)
  return(prediction)
}
