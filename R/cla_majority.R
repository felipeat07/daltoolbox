#'@title Majority Classification
#'@description This function creates a classification object that uses the majority vote strategy to predict the target attribute. Given a target attribute, the function counts the number of occurrences of each value in the dataset and selects the one that appears most often.
#'@param attribute attribute target to model building.
#'@param slevels Possible values for the target classification.
#'@return Returns a classification object.
#'@examples trans <- dal_transform()
#'@export
cla_majority <- function(attribute, slevels) {
  obj <- classification(attribute, slevels)

  class(obj) <- append("cla_majority", class(obj))
  return(obj)
}

#'@title fit majority model
#'@description fit majority model
#'@param obj object
#'@param data dataset
#'@param ... optional arguments
#'@return fitted obj
#'@export
fit.cla_majority <- function(obj, data, ...) {
  data <- adjust_data.frame(data)
  data[,obj$attribute] <- adjust_factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- fit.prediction(obj, data)

  y <- adjust_class_label(data[,obj$attribute])
  cols <- apply(y, 2, sum)
  col <- match(max(cols),cols)
  obj$model <- list(cols=cols, col=col)


  return(obj)
}

#'@title predict data from input
#'@description predict data from input
#'@param object object
#'@param x input variable
#'@param ... optional arguments
#'@return predicted values
#'@export
predict.cla_majority <- function(object, x, ...) {
  rows <- nrow(x)
  cols <- length(object$model$cols)
  prediction <- matrix(rep.int(0, rows*cols), nrow=rows, ncol=cols)
  prediction[,object$model$col] <- 1
  colnames(prediction) <- names(object$model$cols)
  prediction <- as.matrix(prediction)
  return(prediction)
}


