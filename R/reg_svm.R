# DAL Library
# version 2.1

# depends dal_transform.R

# reg_svm
# loadlibrary("e1071")

#'@title Support Vector Machine (SVM) Regression
#'@description
#'@details
#'
#'@param attribute
#'@param epsilon
#'@param cost
#'@param kernel
#'@return
#'@examples
#'@export
reg_svm <- function(attribute, epsilon=0.1, cost=10, kernel="radial") {
  #kernel: linear, radial, polynomial, sigmoid
  #analisar: https://rpubs.com/Kushan/296706
  obj <- regression(attribute)
  obj$kernel <- kernel
  obj$epsilon <- epsilon
  obj$cost <- cost

  class(obj) <- append("reg_svm", class(obj))
  return(obj)
}

#'@title Set parameters values for reg_svm
#'@description It receives as input a reg_svm object (obj) and a set of parameters (params)
#'@details
#'@param obj
#'@param params
#'@return The reg_svm object updated with the new parameter values
#'@export
set_params.reg_svm <- function(obj, params) {
  if (!is.null(params$kernel))
    obj$kernel <- params$kernel
  if (!is.null(params$epsilon))
    obj$epsilon <- params$epsilon
  if (!is.null(params$cost))
    obj$cost <- params$cost

  return(obj)
}


#'@export
fit.reg_svm <- function(obj, data) {
  data <- adjust_data.frame(data)
  obj <- fit.regression(obj, data)

  x <- data[,obj$x]
  y <- data[,obj$attribute]

  obj$model <- svm(x = x, y = y, epsilon=obj$epsilon, cost=obj$cost, kernel=obj$kernel)

  if (obj$log) {
    msg <- sprintf("epsilon=%.1f,cost=%.3f", obj$epsilon, obj$cost)
    obj <- register_log(obj, msg)
  }
  return(obj)
}

#'@export
predict.reg_svm  <- function(obj, x) {
  x <- adjust_data.frame(x)
  x <- x[,obj$x]
  prediction <- predict(obj$model, x)
  return(prediction)
}
