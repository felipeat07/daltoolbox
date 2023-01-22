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

### tune
#'@export
tune.regression <- function (obj, x, y, ranges, folds=3, fit.func, pred.fun = predict) {
  ranges <- expand.grid(ranges)
  n <- nrow(ranges)
  errors <- rep(0,n)

  data <- data.frame(i = 1:nrow(x), idx = 1:nrow(x))
  folds <- k_fold(sample_random(), data, folds)

  i <- 1
  if (n > 1) {
    for (i in 1:n) {
      for (j in 1:length(folds)) {
        if (obj$reproduce)
          set.seed(1)
        tt <- train_test_from_folds(folds, j)

        params <- append(list(x = x[tt$train$i,], y = y[tt$train$i]), as.list(ranges[i,]))
        model <- do.call(fit.func, params)
        prediction <- pred.fun(model, x[tt$test$i,])
        errors[i] <- errors[i] + evaluation.regression(y[tt$test$i], prediction)$mse
      }
    }
    i <- which.min(errors)
  }
  params <- append(list(x = x, y = y), as.list(ranges[i,]))
  if (obj$reproduce)
    set.seed(1)
  model <- do.call(fit.func, params)
  attr(model, "params") <- as.list(ranges[i,])
  return(model)
}

### evaluation
#'@export
MSE.regression <- function (actual, prediction) {
  if (length(actual) != length(prediction))
    stop("actual and prediction have different lengths")
  n <- length(actual)
  res <- mean((actual - prediction)^2)
  res
}

#'@export
sMAPE.regression <- function (actual, prediction) {
  if (length(actual) != length(prediction))
    stop("actual and prediction have different lengths")
  n <- length(actual)
  res <- (1/n) * sum(abs(actual - prediction)/((abs(actual) +
                                                  abs(prediction))/2))
  res
}

# evaluation.regression
#'@export
evaluation.regression <- function(values, prediction) {
  obj <- list(values=values, prediction=prediction)

  obj$smape <- sMAPE.regression(values, prediction)
  obj$mse <- MSE.regression(values, prediction)

  obj$metrics <- data.frame(mse=obj$mse, smape=obj$smape)

  attr(obj, "class") <- "evaluation.regression"
  return(obj)
}
