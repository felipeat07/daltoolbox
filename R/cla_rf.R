# DAL Library
# version 2.1

# depends dal_transform.R

# classif

# random_forest
# loadlibrary("randomForest")

#'@title Random Forest Classifier
#'@description
#'@details
#'
#'@param attribute - name of the attribute used as target classification
#'@param slevels - possible values for the target classification
#'@param mtry
#'@param ntree
#'@return
#'@examples
#'@export
cla_rf <- function(attribute, slevels, mtry = NULL, ntree = 10) {
  obj <- classification(attribute, slevels)

  obj$ntree <- ntree
  obj$mtry <- mtry

  class(obj) <- append("cla_rf", class(obj))
  return(obj)
}

#'@title Set parameters values for cla_rf
#'@description It receives as input a cla_rf object (obj) and a set of parameters (params)
#'@details
#'@param obj
#'@param params
#'@return The cla_rf object updated with the new parameter values
#'@export
set_params.cla_rf <- function(obj, params) {
  if (!is.null(params$mtry))
    obj$mtry <- params$mtry
  if (!is.null(params$ntree))
    obj$ntree <- params$ntree

  return(obj)
}

#'@importFrom randomForest randomForest
#'@export
fit.cla_rf <- function(obj, data) {
  data <- adjust_data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- fit.classification(obj, data)

  if (is.null(obj$mtry))
    obj$mtry <- ceiling(sqrt(ncol(data)))

  x <- data[,obj$x, drop=FALSE]
  y <- data[,obj$attribute]

  obj$model <- randomForest::randomForest(x = x, y = y, mtry=obj$mtry, ntree=obj$ntree)

  if (obj$log) {
    msg <- sprintf("mtry=%d,ntree=%d", obj$mtry, obj$ntree)
    obj <- register_log(obj, msg)
  }
  return(obj)
}

#'@export
predict.cla_rf  <- function(obj, x) {
  x <- adjust_data.frame(x)
  x <- x[,obj$x, drop = FALSE]

  prediction <- predict(obj$model, x, type="prob")
  return(prediction)
}
