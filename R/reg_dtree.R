#'@title Decision Tree for Regression
#'@description Decision Tree for Regression
#'@param attribute attribute target to model building
#'@return obj
#'@examples trans <- dal_transform()
#'@export
reg_dtree <- function(attribute) {
  obj <- regression(attribute)

  class(obj) <- append("reg_dtree", class(obj))
  return(obj)
}

#'@import tree
#'@export
fit.reg_dtree <- function(obj, data, ...) {
  data <- adjust_data.frame(data)
  obj <- fit.regression(obj, data)

  regression <- formula(paste(obj$attribute, "  ~ ."))
  obj$model <- tree::tree(regression, data)

  if (obj$log)
    obj <- register_log(obj)
  return(obj)
}

#'@title predict data from input
#'@description predict data from input
#'@param object object
#'@param x input variable
#'@param ... optional arguments
#'@return predicted values
#'@export
predict.reg_dtree <- function(object, x, ...) {
  x <- adjust_data.frame(x)
  x <- x[,object$x]
  prediction <- predict(object$model, x, type="vector")
  return(prediction)
}
