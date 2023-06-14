# DAL Library
# version 2.1

# depends dal_transform.R
#loadlibrary("FNN")

# reg_knn
#'@title K-Nearest Neighbors (KNN) Regression
#'@description
#'@details
#'
#'@param attribute
#'@param k
#'@return
#'@examples
#'@export
reg_knn <- function(attribute, k) {
  obj <- regression(attribute)
  obj$k <- k

  class(obj) <- append("reg_knn", class(obj))
  return(obj)
}

#'@title Set parameters values for reg_knn
#'@description It receives as input a ts_rf object (obj) and a set of parameters (params)
#'@details If the parameter set contains an entry for nodesize, the corresponding value is assigned to the ts_rf object. Likewise, if the parameter set contains an entry for ntree, the corresponding value is assigned to the ts_rf object
#'@param obj
#'@param params
#'@return The reg_knn object updated with the new parameter values
#'@export
set_params.reg_knn <- function(obj, params) {
  if (!is.null(params$k))
    obj$k <- params$k

  return(obj)
}

#'@importFrom FNN knn.reg
#'@export
fit.reg_knn <- function(obj, data) {
  data <- adjust_data.frame(data)
  obj <- fit.regression(obj, data)

  x <- as.matrix(data[,obj$x])
  y <- data[,obj$attribute]

  obj$model <- list(x=x, y=y, k=obj$k)

  msg <- sprintf("k=%d", obj$model$k)
  obj <- register_log(obj, msg)
  return(obj)
}

#'@importFrom FNN knn.reg
#'@export
predict.reg_knn  <- function(obj, x) {
  #develop from FNN https://daviddalpiaz.github.io/r4sl/knn-reg.html
  x <- adjust_data.frame(x)
  x <- as.matrix(x[,obj$x])
  prediction <- FNN::knn.reg(train = obj$model$x, test = x, y = obj$model$y, k = obj$model$k)
  return(prediction$pred)
}
