#'@title Support Vector Machine Classification
#'@description Classification using Support Vector Machine (SVM) algorithm
#'@param attribute attribute target to model building
#'@param slevels - possible values for the target classification
#'@param epsilon - parameter that controls the width of the margin around the separating hyperplane
#'@param cost - parameter that controls the trade-off between having a wide margin and correctly classifying training data points
#'@param kernel - the type of kernel function to be used in the SVM algorithm (linear, radial, polynomial, sigmoid)
#'@return classification object
#'@examples trans <- dal_transform()
#'@export
cla_svm <- function(attribute, slevels, epsilon=0.1, cost=10, kernel="radial") {
  #kernel: linear, radial, polynomial, sigmoid
  #studio: https://rpubs.com/Kushan/296706
  obj <- classification(attribute, slevels)
  obj$kernel <- kernel
  obj$epsilon <- epsilon
  obj$cost <- cost

  class(obj) <- append("cla_svm", class(obj))
  return(obj)
}

#'@title fit svm model
#'@description fit svm model
#'@param obj object
#'@param data dataset
#'@param ... optional arguments
#'@return fitted obj
#'@import e1071
#'@export
fit.cla_svm <- function(obj, data, ...) {
  data <- adjust_data.frame(data)
  data[,obj$attribute] <- adjust_factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- fit.predictor(obj, data)

  x <- data[,obj$x, drop=FALSE]
  y <- data[,obj$attribute]

  obj$model <- e1071::svm(x, y, probability=TRUE, epsilon=obj$epsilon, cost=obj$cost, kernel=obj$kernel)

  return(obj)
}

#'@title predict data from input
#'@description predict data from input
#'@param object object
#'@param x input variable
#'@param ... optional arguments
#'@return predicted values
#'@export
predict.cla_svm  <- function(object, x, ...) {
  x <- adjust_data.frame(x)
  x <- x[,object$x, drop = FALSE]

  prediction <- predict(object$model, x, probability = TRUE)
  prediction <- attr(prediction, "probabilities")
  prediction <- prediction[,object$slevels]

  return(prediction)
}
