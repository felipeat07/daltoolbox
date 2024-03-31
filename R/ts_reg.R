#'@title TSReg
#'@description Time Series Regression directly from time series
#'Ancestral class for non-sliding windows implementation.
#'@return A `ts_reg` object
#'@examples
#'#See ?ts_arima for an example using Auto-regressive Integrated Moving Average
#'@export
ts_reg <- function() {
  obj <- predictor()
  class(obj) <- append("ts_reg", class(obj))
  return(obj)
}

#'@export
action.ts_reg <- function(obj, ...) {
  thiscall <- match.call(expand.dots = TRUE)
  thiscall[[1]] <- as.name("predict")
  result <- eval.parent(thiscall)
  return(result)
}

#'@export
predict.ts_reg <- function(object, x, ...) {
  return(x[,ncol(x)])
}

#'@title do fit for time series
#'@description The actual time series model fitting.
#'This method should be override by descendants.
#'@param obj object
#'@param x input variable
#'@param y output variable
#'@return fitted object
#'@export
do_fit <- function(obj, x, y = NULL) {
  UseMethod("do_fit")
}

#'@title do predict for time series
#'@description The actual time series model prediction.
#'This method should be override by descendants.
#'@param obj object
#'@param x input variable
#'@return predicted values
#'@export
do_predict <- function(obj, x) {
  UseMethod("do_predict")
}

#'@title MSE
#'@description Compute the mean squared error (MSE) between actual values and forecasts of a time series
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

#'@title sMAPE
#'@description Compute the symmetric mean absolute percent error (sMAPE)
#'@param actual real observations
#'@param prediction predicted observations
#'@return The sMAPE between the actual and prediction vectors
#'@export
sMAPE.ts <- function (actual, prediction) {
  if (length(actual) != length(prediction))
    stop("actual and prediction have different lengths")
  n <- length(actual)
  num <- abs(actual - prediction)
  denom <- (abs(actual) + abs(prediction))/2
  i <- denom != 0
  num <- num[i]
  denom <- denom[i]
  res <- (1/n) * sum(num/denom)
  res
}

#'@title R2
#'@description Compute the R-squared (R2) between actual values and forecasts of a time series
#'@param actual real observations
#'@param prediction predicted observations
#'@return A number, which is the calculated R2
#'@export
R2.ts <- function (actual, prediction) {
  if (length(actual) != length(prediction))
    stop("actual and prediction have different lengths")
  res <-  1 - sum((prediction - actual)^2)/sum((mean(actual) - actual)^2)
  res
}


#'@export
evaluate.ts_reg <- function(obj, values, prediction, ...) {
  result <- list(values=values, prediction=prediction)

  result$smape <- sMAPE.ts(values, prediction)
  result$mse <- MSE.ts(values, prediction)
  result$R2 <- R2.ts(values, prediction)

  result$metrics <- data.frame(mse=result$mse, smape=result$smape, R2 = result$R2)

  return(result)
}

