#'@title Support Vector Machine (SVM) Regression
#'@description Support Vector Machine (SVM) Regression
#'@param attribute attribute target to model building
#'@param epsilon error threshold
#'@param cost cost
#'@param kernel SVM kernel (linear, radial, polynomial, sigmoid)
#'@return obj
#'@examples trans <- dal_transform()
#'@export
reg_svm <- function(attribute, epsilon=0.1, cost=10, kernel="radial") {
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
#'@param obj object
#'@param params parameters
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
fit.reg_svm <- function(obj, data, ...) {
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

#'@title predict data from input
#'@description predict data from input
#'@param object object
#'@param x input variable
#'@param ... optional arguments
#'@return predicted values
#'@export
predict.reg_svm  <- function(object, x, ...) {
  x <- adjust_data.frame(x)
  x <- x[,object$x]
  prediction <- predict(object$model, x)
  return(prediction)
}
