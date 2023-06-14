# DAL Library
# version 2.1

# depends dal_transform.R

# cla_svm
# loadlibrary("e1071")

#'@title Support Vector Machine Classification
#'@description Classification using Support Vector Machine (SVM) algorithm
#'@details This function creates a classification object using the Support Vector Machine (SVM) algorithm.
#'The SVM is a popular machine learning algorithm used in classification tasks.
#'It works by finding the optimal hyperplane that separates data points belonging to different classes.
#'
#'@param attribute - name of the attribute used as target classification
#'@param slevels - possible values for the target classification
#'@param epsilon - parameter that controls the width of the margin around the separating hyperplane
#'@param cost - parameter that controls the trade-off between having a wide margin and correctly classifying training data points
#'@param kernel - the type of kernel function to be used in the SVM algorithm (linear, radial, polynomial, sigmoid)
#'@return classification object
#'@examples
#'@export
cla_svm <- function(attribute, slevels=NULL, epsilon=0.1, cost=10, kernel="radial") {
  #kernel: linear, radial, polynomial, sigmoid
  #studio: https://rpubs.com/Kushan/296706
  obj <- classification(attribute, slevels)
  obj$kernel <- kernel
  obj$epsilon <- epsilon
  obj$cost <- cost

  class(obj) <- append("cla_svm", class(obj))
  return(obj)
}

#'@title Set parameters values for reg_svm
#'@description It receives as input a reg_svm object (obj) and a set of parameters (params)
#'@details
#'@param obj
#'@param params
#'@return The reg_svm object updated with the new parameter values
#'@export
set_params.cla_svm <- function(obj, params) {
  if (!is.null(params$kernel))
    obj$kernel <- params$kernel
  if (!is.null(params$epsilon))
    obj$epsilon <- params$epsilon
  if (!is.null(params$cost))
    obj$cost <- params$cost

  return(obj)
}


#'@import e1071
#'@export
fit.cla_svm <- function(obj, data) {
  data <- adjust_data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- fit.classification(obj, data)

  x <- data[,obj$x, drop=FALSE]
  y <- data[,obj$attribute]

  obj$model <- e1071::svm(x, y, probability=TRUE, epsilon=obj$epsilon, cost=obj$cost, kernel=obj$kernel)
  attr(model, "slevels")  <- levels(y)

  msg <- sprintf("epsilon=%.1f,cost=%.3f", obj$epsilon, obj$cost)
  obj <- register_log(obj, msg)
  return(obj)
}

#'@export
predict.cla_svm  <- function(obj, x) {
  x <- adjust_data.frame(x)
  x <- x[,obj$x, drop = FALSE]

  prediction <- predict(obj$model, x, probability = TRUE)
  prediction <- attr(prediction, "probabilities")
  slevels <- attr(obj$model, "slevels")
  prediction <- prediction[,slevels]

  return(prediction)
}
