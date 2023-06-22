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

#'@export
fit.reg_svm <- function(obj, data, ...) {
  data <- adjust_data.frame(data)
  obj <- fit.predictor(obj, data)

  x <- data[,obj$x]
  y <- data[,obj$attribute]

  obj$model <- svm(x = x, y = y, epsilon=obj$epsilon, cost=obj$cost, kernel=obj$kernel)

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
