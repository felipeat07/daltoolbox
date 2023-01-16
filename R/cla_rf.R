# DAL Library
# version 2.1

# depends dal_transform.R

# classif

# random_forest
# loadlibrary("randomForest")

#'@title
#'@description
#'@details
#'
#'@param attribute
#'@param slevels
#'@param mtry
#'@param ntree
#'@return
#'@examples
#'@export
cla_rf <- function(attribute, slevels=NULL, mtry = NULL, ntree = seq(5, 50, 5)) {
  obj <- classification(attribute, slevels)
  
  obj$ntree <- ntree
  obj$mtry <- mtry
  
  class(obj) <- append("cla_rf", class(obj))
  return(obj)
}

#'@export
fit.cla_rf <- function(obj, data) {
  
  internal_predict.cla_rf <- function(model, x) {
    prediction <- predict(model, x, type="prob")  
    return(prediction)
  }  
  
  data <- adjust.data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- fit.classification(obj, data)
  
  if (is.null(obj$mtry))
    obj$mtry <- ceiling(c(1,1.5,2)*sqrt(ncol(data)))
  
  x <- data[,obj$x]
  y <- data[,obj$attribute]
  
  ranges <- list(mtry=obj$mtry, ntree=obj$ntree)
  obj$model <- tune.classification(obj, x = x, y = y, ranges = ranges, fit.func = randomForest, pred.fun = internal_predict.cla_rf)
  
  params <- attr(obj$model, "params") 
  msg <- sprintf("mtry=%d,ntree=%d", params$mtry, params$ntree)
  obj <- register_log(obj, msg)
  return(obj)
}

#'@export
predict.cla_rf  <- function(obj, x) {
  x <- adjust.data.frame(x)
  x <- x[,obj$x]   
  
  prediction <- predict(obj$model, x, type="prob")  
  return(prediction)
}
