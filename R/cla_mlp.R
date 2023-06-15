#'@title Classification using Artificial Neural Network (ANN)
#'@description Performs classification using Artificial Neural Network (ANN) algorithm
#'@param attribute attribute target to model building
#'@param slevels - possible values for the target classification
#'@param size - number of nodes that will be used in the hidden layer
#'@param decay - how quickly it decreases in gradient descent
#'@param maxit - maximun interations
#'@return a classification object
#'@examples trans <- dal_transform()
#'@export
cla_mlp <- function(attribute, slevels, size=NULL, decay=0.1, maxit=1000) {
  obj <- classification(attribute, slevels)
  obj$maxit <- maxit
  obj$size <- size
  obj$decay <- decay

  class(obj) <- append("cla_mlp", class(obj))
  return(obj)
}

#'@title Set parameters values for cla_mlp
#'@description It receives as input a cla_mlp object (obj) and a set of parameters (params)
#'@param obj object
#'@param params parameters
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



#'@title fit mlp model
#'@description fit mlp model
#'@param obj object
#'@param data dataset
#'@param ... optional arguments
#'@return fitted obj
#'@import nnet
#'@export
fit.cla_mlp <- function(obj, data, ...) {
  data <- adjust_data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- fit.classification(obj, data)

  if (is.null(obj$size))
    obj$size <- ceiling(sqrt(ncol(data)))

  x <- data[,obj$x, drop = FALSE]
  y <- data[,obj$attribute]

  obj$model <- nnet::nnet(x = x, y = adjustClassLabels(y), size=obj$size, decay=obj$decay, maxit=obj$maxit, trace=FALSE)

  if (obj$log) {
    msg <- sprintf("size=%d,decay=%.2f", obj$size, obj$decay)
    obj <- register_log(obj, msg)
  }
  return(obj)
}

#'@title predict data from input
#'@description predict data from input
#'@param object object
#'@param x input variable
#'@param ... optional arguments
#'@return predicted values
#'@export
predict.cla_mlp  <- function(object, x, ...) {
  x <- adjust_data.frame(x)
  x <- x[,object$x, drop = FALSE]

  prediction <- predict(object$model, x, type="raw")

  return(prediction)
}
