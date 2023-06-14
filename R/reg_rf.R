# DAL Library
# version 2.1

# depends dal_transform.R


# random forest
#loadlibrary("randomForest")

#'@title Random Forest Regression
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

#'@title Set parameters values for reg_rf
#'@description It receives as input a reg_rf object (obj) and a set of parameters (params)
#'@details
#'@param obj
#'@param params
#'@return The reg_rf object updated with the new parameter values
#'@export
set_params.reg_rf <- function(obj, params) {
  if (!is.null(params$mtry))
    obj$mtry <- params$mtry
  if (!is.null(params$ntree))
    obj$ntree <- params$ntree

  return(obj)
}

#'@importFrom randomForest randomForest
#'@export
fit.reg_rf <- function(obj, data) {
  data <- adjust_data.frame(data)
  obj <- fit.regression(obj, data)

  if (is.null(obj$mtry))
    obj$mtry <- ceiling(ncol(data)/3)

  x <- data[,obj$x]
  y <- data[,obj$attribute]

  obj$model <- randomForest::randomForest(x = x, y = y, mtry=obj$mtry, ntree=obj$ntree)

  if (obj$log) {
    msg <- sprintf("mtry=%d,ntree=%d", obj$mtry, obj$ntree)
    obj <- register_log(obj, msg)
  }
  return(obj)
}

#'@export
predict.reg_rf  <- function(obj, x) {
  x <- adjust_data.frame(x)
  x <- x[,obj$x]
  prediction <- predict(obj$model, x)
  return(prediction)
}
