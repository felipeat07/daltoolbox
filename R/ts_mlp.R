#'@title Time Series Multilayer Perceptron (MLP)
#'@description Type of artificial neural network used to make predictions and
#' forecasts based on time series data. Consists of multiple layers of
#' interconnected nodes or neurons, using a supervised learning algorithm, which
#' processes the input data and passes it on. The output of the final layer
#' provides the predicted values for the future.
#'@param preprocess normalization
#'@param input_size input size for machine learning model
#'@param size number of neurons inside hidden layer
#'@param decay decay parameter for MLP
#'@param maxit maximum number of iterations
#'@return a `ts_mlp` object.
#'@examples trans <- dal_transform()
#'@export
ts_mlp <- function(preprocess=NA, input_size=NA, size=NA, decay=0.01, maxit=1000) {
  obj <- ts_regsw(preprocess, input_size)
  if (is.na(size))
    size <- ceiling(input_size/3)

  obj$size <- size
  obj$decay <- decay
  obj$maxit <- maxit

  class(obj) <- append("ts_mlp", class(obj))
  return(obj)
}


#'@import nnet
#'@export
do_fit.ts_mlp <- function(obj, x, y) {
  obj$model <- nnet::nnet(x = x, y = y, size = obj$size, decay=obj$decay, maxit = obj$maxit, linout=TRUE, trace = FALSE)
  return(obj)
}
