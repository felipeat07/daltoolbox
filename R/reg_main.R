# DAL Library
# version 2.1

# depends dal_transform.R

# regression
#'@title Regression
#'@description
#'@details
#'
#'@param attribute
#'@return
#'@examples
#'@export
regression <- function(attribute) {
  obj <- dal_transform()
  obj$attribute <- attribute
  class(obj) <- append("regression", class(obj))
  return(obj)
}

#'@export
fit.regression <- function(obj, data) {
  obj <- start_log(obj)
  obj$x <- setdiff(colnames(data), obj$attribute)
  return(obj)
}


# evaluate.regression
#'@export
evaluate.regression <- function(obj, values, prediction) {
  MSE <- function (actual, prediction) {
    if (length(actual) != length(prediction))
      stop("actual and prediction have different lengths")
    n <- length(actual)
    res <- mean((actual - prediction)^2)
    res
  }

  sMAPE <- function (actual, prediction) {
    if (length(actual) != length(prediction))
      stop("actual and prediction have different lengths")
    n <- length(actual)
    res <- (1/n) * sum(abs(actual - prediction)/((abs(actual) +
                                                    abs(prediction))/2))
    res
  }

  result <- list(values=values, prediction=prediction)

  result$smape <- sMAPE(values, prediction)
  result$mse <- MSE(values, prediction)

  result$metrics <- data.frame(mse=result$mse, smape=result$smape)

  return(result)
}
