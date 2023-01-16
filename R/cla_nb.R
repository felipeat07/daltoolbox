# DAL Library
# version 2.1

# depends dal_transform.R

# naive_bayes

# loadlibrary("e1071")

#'@title
#'@description
#'@details
#'
#'@param attribute
#'@param slevels
#'@return
#'@examples
#'@export
cla_nb <- function(attribute, slevels=NULL) {
  obj <- classification(attribute, slevels)
  
  class(obj) <- append("cla_nb", class(obj))    
  return(obj)
}

#'@export
fit.cla_nb <- function(obj, data) {
  data <- adjust.data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- fit.classification(obj, data)
  
  regression <- formula(paste(obj$attribute, "  ~ ."))  
  obj$model <- naiveBayes(regression, data, laplace=0)
  
  obj <- register_log(obj)
  return(obj)
}

#'@export
predict.cla_nb  <- function(obj, x) {
  x <- adjust.data.frame(x)
  x <- x[,obj$x]
  
  prediction <- predict(obj$model, x, type="raw")
  
  return(prediction)
}
