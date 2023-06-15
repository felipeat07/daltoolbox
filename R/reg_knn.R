#'@title K-Nearest Neighbors (KNN) Regression
#'@description K-Nearest Neighbors (KNN) Regression
#'@param attribute attribute target to model building
#'@param k number of k neighbors
#'@return obj
#'@examples trans <- dal_transform()
#'@export
reg_knn <- function(attribute, k) {
  obj <- regression(attribute)
  obj$k <- k

  class(obj) <- append("reg_knn", class(obj))
  return(obj)
}

#'@title Set parameters values for reg_knn
#'@description It receives as input a ts_rf object (obj) and a set of parameters (params)
#'@param obj object
#'@param params parameters
#'@return The reg_knn object updated with the new parameter values
#'@export
set_params.reg_knn <- function(obj, params) {
  if (!is.null(params$k))
    obj$k <- params$k

  return(obj)
}

#'@importFrom FNN knn.reg
#'@export
fit.reg_knn <- function(obj, data, ...) {
  data <- adjust_data.frame(data)
  obj <- fit.regression(obj, data)

  x <- as.matrix(data[,obj$x])
  y <- data[,obj$attribute]

  obj$model <- list(x=x, y=y, k=obj$k)

  if (obj$log) {
    msg <- sprintf("k=%d", obj$model$k)
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
#'@importFrom FNN knn.reg
#'@export
predict.reg_knn  <- function(object, x, ...) {
  #develop from FNN https://daviddalpiaz.github.io/r4sl/knn-reg.html
  x <- adjust_data.frame(x)
  x <- as.matrix(x[,object$x])
  prediction <- FNN::knn.reg(train = object$model$x, test = x, y = object$model$y, k = object$model$k)
  return(prediction$pred)
}
