#'@title K Nearest Neighbor Classification
#'@description Classifies using the K-Nearest Neighbor algorithm.
#'@param attribute attribute target to model building.
#'@param slevels Possible values for the target classification.
#'@param k A vector of integers indicating the number of neighbors to be considered.
#'@return A classification object.
#'@examples trans <- dal_transform()
#'@export
cla_knn <- function(attribute, slevels, k=1) {
  obj <- classification(attribute, slevels)
  obj$k <- k
  class(obj) <- append("cla_knn", class(obj))
  return(obj)
}

#'@title Set parameters values for cla_knn
#'@description It receives as input a ts_rf object (obj) and a set of parameters (params)
#'@param obj object
#'@param params parameters
#'@return The cla_knn object updated with the new parameter values
#'@export
set_params.cla_knn <- function(obj, params) {
  if (!is.null(params$k))
    obj$k <- params$k

  return(obj)
}


#'@title fit knn model
#'@description fit knn model
#'@param obj object
#'@param data dataset
#'@param ... optional arguments
#'@return fitted obj
#'@import class
#'@export
fit.cla_knn <- function(obj, data, ...) {

  data <- adjust_data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- fit.classification(obj, data)

  x <- data[,obj$x, drop = FALSE]
  y <- data[,obj$attribute]

  obj$model <-list(x=x, y=y, k=obj$k)

  if (obj$log) {
    msg <- sprintf("k=%d", obj$k)
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
#'@import class
#'@export
predict.cla_knn  <- function(object, x, ...) {
  x <- adjust_data.frame(x)
  x <- x[,object$x, drop=FALSE]

  prediction <- class::knn(train=object$model$x, test=x, cl=object$model$y, prob=TRUE)
  prediction <- adjustClassLabels(prediction)

  return(prediction)
}


