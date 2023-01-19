# DAL Library
# version 2.1

# depends dal_transform.R


# random forest
#loadlibrary("randomForest")

#'@title
#'@description
#'@details
#'
#'@param attribute
#'@param mtry
#'@param ntree
#'@return
#'@examples
#'@export
reg_rf <- function(attribute, mtry = NULL, ntree = seq(5, 50, 5)) {
  obj <- regression(attribute)

  obj$mtry <- mtry
  obj$ntree <- ntree

  class(obj) <- append("reg_rf", class(obj))
  return(obj)
}

#'@export
fit.reg_rf <- function(obj, data) {
  data <- adjust.data.frame(data)
  obj <- fit.regression(obj, data)

  if (is.null(obj$mtry))
    obj$mtry <- ceiling(c(1,1.5,2)*ncol(data)/3)

  x <- data[,obj$x]
  y <- data[,obj$attribute]

  ranges <- list(mtry=obj$mtry, ntree=obj$ntree)
  obj$model <- tune.regression(obj, x = x, y = y, ranges = ranges, fit.func = randomForest)

  params <- attr(obj$model, "params")
  msg <- sprintf("mtry=%d,ntree=%d", params$mtry, params$ntree)
  obj <- register_log(obj, msg)
  return(obj)
}

#'@export
predict.reg_rf  <- function(obj, x) {
  x <- adjust.data.frame(x)
  x <- x[,obj$x]
  prediction <- predict(obj$model, x)
  return(prediction)
}
