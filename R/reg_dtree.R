# DAL Library
# version 2.1

# depends dal_transform.R

# decision_tree
# loadlibrary("tree")
#'@title
#'@description
#'@details
#'
#'@param attribute
#'@return
#'@examples
#'@export
reg_dtree <- function(attribute) {
  obj <- regression(attribute)
  
  class(obj) <- append("reg_dtree", class(obj))    
  return(obj)
}

#'@export
fit.reg_dtree <- function(obj, data) {
  data <- adjust.data.frame(data)
  obj <- fit.regression(obj, data)  
  
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  obj$model <- tree(regression, data)
  
  obj <- register_log(obj)
  return(obj)
}

#'@export
predict.reg_dtree <- function(obj, x) {
  x <- adjust.data.frame(x)
  x <- x[,obj$x]   
  prediction <- predict(obj$model, x, type="vector")  
  return(prediction)
}
