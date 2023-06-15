#'@title Naive Bayes Classifier
#'@description Classification using the Naive Bayes algorithm
#'@param attribute attribute target to model building.
#'@param slevels Possible values for the target classification.
#'@return A classification object.
#'@examples trans <- dal_transform()
#'@export
cla_nb <- function(attribute, slevels) {
  obj <- classification(attribute, slevels)

  class(obj) <- append("cla_nb", class(obj))
  return(obj)
}

#'@title fit nb model
#'@description fit nb model
#'@param obj object
#'@param data dataset
#'@param ... optional arguments
#'@return fitted obj
#'@import e1071
#'@export
fit.cla_nb <- function(obj, data, ...) {
  data <- adjust_data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- fit.classification(obj, data)

  regression <- formula(paste(obj$attribute, "  ~ ."))
  obj$model <- e1071::naiveBayes(regression, data, laplace=0)

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
predict.cla_nb  <- function(object, x, ...) {
  x <- adjust_data.frame(x)
  x <- x[,object$x, drop=FALSE]

  prediction <- predict(object$model, x, type="raw")

  return(prediction)
}
