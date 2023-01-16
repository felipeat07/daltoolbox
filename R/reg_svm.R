# DAL Library
# version 2.1

# depends dal_transform.R

# reg_svm 
# loadlibrary("e1071")

#'@title
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
reg_svm <- function(attribute, epsilon=seq(0,1,0.2), cost=seq(20,100,20), kernel="radial") {
  #kernel: linear, radial, polynomial, sigmoid
  #analisar: https://rpubs.com/Kushan/296706  
  obj <- regression(attribute)
  obj$kernel <- kernel
  obj$epsilon <- epsilon
  obj$cost <- cost
  
  class(obj) <- append("reg_svm", class(obj))    
  return(obj)
}

#'@export
fit.reg_svm <- function(obj, data) {
  data <- adjust.data.frame(data)
  obj <- fit.regression(obj, data)  
  
  x <- data[,obj$x]
  y <- data[,obj$attribute]
  
  ranges <- list(epsilon=obj$epsilon, cost=obj$cost, kernel=obj$kernel)
  obj$model <- tune.regression(obj, x = x, y = y, ranges = ranges, fit.func = svm)

  params <- attr(obj$model, "params") 
  msg <- sprintf("epsilon=%.1f,cost=%.3f", params$epsilon, params$cost)
  obj <- register_log(obj, msg)
  return(obj)
}

#'@export
predict.reg_svm  <- function(obj, x) {
  x <- adjust.data.frame(x)
  x <- x[,obj$x]   
  prediction <- predict(obj$model, x) 
  return(prediction)
}
