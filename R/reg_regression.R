#'@title Regression base class
#'@description Regression base class
#'@param attribute attribute target to model building
#'@return obj
#'@examples trans <- dal_transform()
#'@export
regression <- function(attribute) {
  obj <- prediction()
  obj$attribute <- attribute
  class(obj) <- append("regression", class(obj))
  return(obj)
}

#'@title Regression evaluation
#'@description Evaluate major regression metrics for trained model
#'@param obj object
#'@param values real observations
#'@param prediction predicted observations
#'@param ... optional arguments.
#'@return Computed metrics
#'@examples trans <- dal_transform()
#'@export
evaluate.regression <- function(obj, values, prediction, ...) {
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
