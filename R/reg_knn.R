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
reg_knn <- function(attribute, k=1:30) {
  obj <- regression(attribute)
  obj$k <- k

  class(obj) <- append("reg_knn", class(obj))
  return(obj)
}

#'@importFrom class knn.reg
#'@export
fit.reg_knn <- function(obj, data) {
  internal_fit.reg_knn <- function (x, y, k, ...) {
    model <- list(x=x, y=y, k=k)
    return (model)
  }

  internal_predict.reg_knn <- function(model, x) {
    prediction <- class::knn.reg(train = model$x, test = x, y = model$y, k = model$k)
    return(prediction$pred)
  }
  data <- adjust_data.frame(data)
  obj <- fit.regression(obj, data)

  x <- as.matrix(data[,obj$x])
  y <- data[,obj$attribute]

  ranges <- list(k = obj$k, stub = 0)
  obj$model <- tune.regression(obj, x = x, y = y, ranges = ranges, fit.func = internal_fit.reg_knn, pred.fun = internal_predict.reg_knn)

  params <- attr(obj$model, "params")
  msg <- sprintf("k=%d", params$k)
  obj <- register_log(obj, msg)
  return(obj)
}

#'@importFrom class knn.reg
#'@export
predict.reg_knn  <- function(obj, x) {
  #develop from FNN https://daviddalpiaz.github.io/r4sl/knn-reg.html
  x <- adjust_data.frame(x)
  x <- as.matrix(x[,obj$x])
  prediction <- class::knn.reg(train = obj$model$x, test = x, y = obj$model$y, k = obj$model$k)
  return(prediction$pred)
}
