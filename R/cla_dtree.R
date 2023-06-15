#'@title Decision Tree Classification
#'@description Creates a classification object that uses the Decision Tree algorithm for classification.
#'@param attribute attribute target to model building.
#'@param slevels The possible values for the target classification.
#'@return A classification object that uses the Decision Tree algorithm for classification.
#'@examples trans <- dal_transform()
#'@export
cla_dtree <- function(attribute, slevels) {
  obj <- classification(attribute, slevels)

  class(obj) <- append("cla_dtree", class(obj))
  return(obj)
}

#'@title fit dtree model
#'@description fit dtree model
#'@param obj object
#'@param data dataset
#'@param ... optional arguments
#'@return fitted obj
#'@import tree
#'@export
fit.cla_dtree <- function(obj, data, ...) {
  data <- adjust_data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- fit.classification(obj, data)

  regression <- formula(paste(obj$attribute, "  ~ ."))
  obj$model <- tree::tree(regression, data)

  obj <- register_log(obj)
  return(obj)
}

#'@title predict data from input
#'@description predict data from input
#'@param object object
#'@param x input variable
#'@param ... optional arguments
#'@return predicted values
#'@export
predict.cla_dtree <- function(object, x, ...) {
  x <- adjust_data.frame(x)
  x <- x[,object$x, drop=FALSE]

  prediction <- predict(object$model, x, type="vector")

  return(prediction)
}





