#'@title Create an object of class "ts_reg" which is a subclass of object dal_learner
#'@description The function sets some object attributes, such as log, debug and reproduce, to TRUE or FALSE. Then it adds the "ts_reg" class to the object and returns the created object
#'@return An object of the class "ts_reg"
#'@examples trans <- dal_transform()
#'@export
ts_reg <- function() {
  obj <- dal_learner()
  class(obj) <- append("ts_reg", class(obj))
  return(obj)
}

#'@title Action implementation for prediction
#'@description A default function that defines the action to proxy predict method
#'@param obj object
#'@param ... optional arguments
#'@return It simply returns NULL, which indicates that no transforms are applied
#'@examples trans <- dal_transform()
#'@export
action.ts_reg <- function(obj, ...) {
  thiscall <- match.call(expand.dots = TRUE)
  thiscall[[1]] <- as.name("predict")
  result <- eval.parent(thiscall)
  return(result)
}

#'@title predict data from input
#'@description predict data from input
#'@param object object
#'@param x input variable
#'@param ... optional arguments
#'@return predicted values
#'@examples trans <- dal_transform()
#'@export
predict.ts_reg <- function(object, x, ...) {
  return(x[,ncol(x)])
}

#'@title Delegate the task of adjusting the model to the specific methods of each class that implements it.
#'@description This function receives the obj, x and y variables as parameters
#'@param obj object
#'@param x input variable
#'@param y output variable
#'@return fitted object
#'@examples trans <- dal_transform()
#'@export
do_fit <- function(obj, x, y = NULL) {
  UseMethod("do_fit")
}

#'@title Define a generic method and delegate the implementation
#'@description This function defines the "do_predict" method for a generic object "obj" and a dataset "x"
#'@param obj object
#'@param x input variable
#'@return predicted values
#'@examples trans <- dal_transform()
#'@export
do_predict <- function(obj, x) {
  UseMethod("do_predict")
}

#'@title Calculate the mean squared error (MSE) between actual values and forecasts of a time series
#'@description The function receives two variables as a parameter, which are actual and prediction
#'@param actual real observations
#'@param prediction predicted observations
#'@return A number, which is the calculated MSE
#'@export
MSE.ts <- function (actual, prediction) {
  if (length(actual) != length(prediction))
    stop("actual and prediction have different lengths")
  n <- length(actual)
  res <- mean((actual - prediction)^2)
  res
}

#'@title Calculate the symmetric mean absolute percent error (sMAPE)
#'@description The function receives two variables as a parameter, which are actual and prediction
#'@param actual real observations
#'@param prediction predicted observations
#'@return The sMAPE between the actual and prediction vectors
#'@export
sMAPE.ts <- function (actual, prediction) {
  if (length(actual) != length(prediction))
    stop("actual and prediction have different lengths")
  n <- length(actual)
  res <- (1/n) * sum(abs(actual - prediction)/((abs(actual) +
                                                  abs(prediction))/2))
  res
}

#'@title Calculate the Mean Squared Error (MSE) error metric and the Symmetric Mean Absolute Percentage Error (sMAPE) error metric
#'@description The function receives two variables as a parameter, which are values and prediction
#'@param obj object
#'@param values real observations
#'@param prediction predicted observations
#'@param ... optional arguments.
#'@return An object that contains these metrics and their values, stored in a data frame
#'@export
evaluate.ts_reg <- function(obj, values, prediction, ...) {
  result <- list(values=values, prediction=prediction)

  result$smape <- sMAPE.ts(values, prediction)
  result$mse <- MSE.ts(values, prediction)

  result$metrics <- data.frame(mse=result$mse, smape=result$smape)

  return(result)
}

