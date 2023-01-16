# DAL Library
# version 2.1

# depends dal_transform.R
# depends cla_classification.R

# decision_tree
# loadlibrary("tree")

#'@title
#'@description
#'@details
#'
#'@param attribute
#'@param slevels
#'@return
#'@examples
#'@export
cla_dtree <- function(attribute, slevels=NULL) {
  obj <- classification(attribute, slevels)
  
  class(obj) <- append("cla_dtree", class(obj))    
  return(obj)
}

#'@export
fit.cla_dtree <- function(obj, data) {
  data <- adjust.data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- fit.classification(obj, data)
  
  loadlibrary("tree")
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  obj$model <- tree(regression, data)
  
  obj <- register_log(obj)  
  return(obj)
}

#'@export
predict.cla_dtree <- function(obj, x) {
  x <- adjust.data.frame(x)
  x <- x[,obj$x]   
  
  loadlibrary("tree")
  prediction <- predict(obj$model, x, type="vector")  
  
  return(prediction)
}
