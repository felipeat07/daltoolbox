# DAL Library
# version 2.1

# depends dal_transform.R

# mlp_nnet
#loadlibrary("nnet")

#'@title Classification using Artificial Neural Network (ANN)
#'@description Performs classification using Artificial Neural Network (ANN) algorithm
#'@details This function creates an object of class "cla_mlp" that can be used for classification of a target attribute using the ANN algorithm. The number of nodes in the hidden layer can be specified using the size parameter. The decay parameter controls how quickly the gradient descent decreases, and the maxit parameter sets the maximum number of iterations.
#'
#'@param attribute - name of the attribute used as target classification
#'@param slevels - possible values for the target classification
#'@param size - number of nodes that will be used in the hidden layer
#'@param decay - how quickly it decreases in gradient descent
#'@param maxit - maximun interations
#'@return a classification object
#'@examples
#'@export
cla_mlp <- function(attribute, slevels=NULL, size=NULL, decay=seq(0, 1, 0.0335), maxit=1000) {
  obj <- classification(attribute, slevels)
  obj$maxit <- maxit
  obj$size <- size
  obj$decay <- decay

  class(obj) <- append("cla_mlp", class(obj))
  return(obj)
}

#'@title Set parameters values for cla_mlp
#'@description It receives as input a cla_mlp object (obj) and a set of parameters (params)
#'@details
#'@param obj
#'@param params
#'@return The cla_mlp object updated with the new parameter values
#'@export
set_params.cla_mlp <- function(obj, params) {
  if (!is.null(params$size))
    obj$size <- params$size
  if (!is.null(params$decay))
    obj$decay <- params$decay
  if (!is.null(params$maxit))
    obj$maxit <- params$maxit

  return(obj)
}



#'@import nnet
#'@export
fit.cla_mlp <- function(obj, data) {
  data <- adjust_data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- fit.classification(obj, data)

  if (is.null(obj$size))
    obj$size <- ceiling(sqrt(ncol(data)))

  x <- data[,obj$x, drop = FALSE]
  y <- data[,obj$attribute]

  obj$model <- nnet::nnet(x = x, y = adjustClassLabels(y), size=obj$size, decay=obj$decay, maxit=obj$maxit, trace=FALSE)

  msg <- sprintf("size=%d,decay=%.2f", obj$size, obj$decay)
  obj <- register_log(obj, msg)
  return(obj)
}

#'@export
predict.cla_mlp  <- function(obj, x) {
  x <- adjust_data.frame(x)
  x <- x[,obj$x, drop = FALSE]

  prediction <- predict(obj$model, x, type="raw")

  return(prediction)
}
