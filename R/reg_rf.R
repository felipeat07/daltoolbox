#'@title Random Forest Regression
#'@description Random Forest Regression
#'@param attribute attribute target to model building
#'@param mtry number of attributes to build trees
#'@param ntree number of trees
#'@return obj
#'@examples trans <- dal_transform()
#'@export
reg_rf <- function(attribute, mtry = NULL, ntree = 10) {
  obj <- regression(attribute)

  obj$mtry <- mtry
  obj$ntree <- ntree

  class(obj) <- append("reg_rf", class(obj))
  return(obj)
}

#'@importFrom randomForest randomForest
#'@export
fit.reg_rf <- function(obj, data, ...) {
  data <- adjust_data.frame(data)
  obj <- fit.predictor(obj, data)

  if (is.null(obj$mtry))
    obj$mtry <- ceiling(ncol(data)/3)

  x <- data[,obj$x]
  y <- data[,obj$attribute]

  obj$model <- randomForest::randomForest(x = x, y = y, mtry=obj$mtry, ntree=obj$ntree)

  return(obj)
}

#'@title predict data from input
#'@description predict data from input
#'@param object object
#'@param x input variable
#'@param ... optional arguments
#'@return predicted values
#'@export
predict.reg_rf  <- function(object, x, ...) {
  x <- adjust_data.frame(x)
  x <- x[,object$x]
  prediction <- predict(object$model, x)
  return(prediction)
}
