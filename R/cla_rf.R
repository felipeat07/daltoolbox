#'@title Random Forest Classifier
#'@description Random Forest Classifier
#'@param attribute attribute target to model building
#'@param slevels - possible values for the target classification
#'@param mtry number of attributes to build tree
#'@param ntree number of trees
#'@return obj
#'@examples trans <- dal_transform()
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
#'@param obj object
#'@param params parameters
#'@return The cla_rf object updated with the new parameter values
#'@export
set_params.cla_rf <- function(obj, params) {
  if (!is.null(params$mtry))
    obj$mtry <- params$mtry
  if (!is.null(params$ntree))
    obj$ntree <- params$ntree

  return(obj)
}

#'@title fit rf model
#'@description fit rf model
#'@param obj object
#'@param data dataset
#'@param ... optional arguments
#'@return fitted obj
#'@importFrom randomForest randomForest
#'@export
fit.cla_rf <- function(obj, data, ...) {
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

#'@title predict data from input
#'@description predict data from input
#'@param object object
#'@param x input variable
#'@param ... optional arguments
#'@return predicted values
#'@export
predict.cla_rf  <- function(object, x, ...) {
  x <- adjust_data.frame(x)
  x <- x[,object$x, drop = FALSE]

  prediction <- predict(object$model, x, type="prob")
  return(prediction)
}
