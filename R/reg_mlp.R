# DAL Library
# version 2.1

# depends dal_transform.R

# mlp_nnet
# loadlibrary("nnet")

#'@title Multi-Layer Perceptron (MLP) Regression
#'@description
#'@details
#'
#'@param attribute
#'@param size
#'@param decay
#'@param maxit
#'@return
#'@examples
#'@export
reg_mlp <- function(attribute, size=NULL, decay=seq(0, 1, 0.0335), maxit=1000) {
  obj <- regression(attribute)
  obj$maxit <- maxit
  obj$size <- size
  obj$decay <- decay
  class(obj) <- append("reg_mlp", class(obj))
  return(obj)
}

#'@export
fit.reg_mlp <- function(obj, data) {
  data <- adjust.data.frame(data)
  obj <- fit.regression(obj, data)

  if (is.null(obj$size))
    obj$size <- ceiling(ncol(data)/3)

  x <- data[,obj$x]
  y <- data[,obj$attribute]
  ranges <- list(size = obj$size, decay = obj$decay, maxit=obj$maxit, linout=TRUE, trace = FALSE)
  obj$model <- tune.regression(obj, x = x, y = y, ranges = ranges, fit.func = nnet)

  params <- attr(obj$model, "params")
  msg <- sprintf("size=%d,decay=%.2f", params$size, params$decay)
  obj <- register_log(obj, msg)
  return(obj)
}

#'@export
predict.reg_mlp  <- function(obj, x) {
  x <- adjust.data.frame(x)
  x <- x[,obj$x]
  prediction <- predict(obj$model, x)
  return(prediction)
}
