# DAL Library
# version 2.1

#'@title Time Series Tune
#'@description Time Series Tune
#'@param input_size input size for machine learning model
#'@param base_model base model for tuning
#'@param folds number of folds for cross-validation
#'@return a `ts_tune` object.
#'@examples trans <- dal_transform()
#'@export
ts_tune <- function(input_size, base_model, folds=10) {
  obj <- dal_base()
  obj$input_size <- input_size
  obj$base_model <- base_model
  obj$folds <- folds
  obj$name <- ""
  class(obj) <- append("ts_tune", class(obj))
  return(obj)
}


#'@importFrom stats predict
#'@export
fit.ts_tune <- function(obj, x, y, ranges, ...) {

  build_model <- function(obj, ranges, x, y) {
    model <- obj$base_model
    model$input_size <- ranges$input_size
    model <- set_params(model, ranges)
    model <- fit(model, x, y)
    return(model)
  }

  prepare_ranges <- function(obj, ranges) {
    ranges <- append(list(input_size = obj$input_size), ranges)
    ranges <- expand.grid(ranges)
    ranges$key <- 1:nrow(ranges)
    obj$ranges <- ranges
    return(obj)
  }

  evaluate_error <- function(model, i, x, y) {
    x <- x[i,]
    y <- as.vector(y[i,])
    prediction <- as.vector(stats::predict(model, x))
    error <- evaluate(model, y, prediction)$mse
    return(error)
  }

  if (obj$base_model$reproduce)
    set.seed(1)

  obj <- prepare_ranges(obj, ranges)
  ranges <- obj$ranges

  n <- nrow(ranges)
  i <- 1
  hyperparameters <- NULL
  if (n > 1) {
    data <- data.frame(i = 1:nrow(x), idx = 1:nrow(x))
    folds <- k_fold(sample_random(), data, obj$folds)
    nfolds <- length(folds)
    if (obj$base_model$debug)
      print(sprintf("%d-%d", nfolds, n))
    for (j in 1:nfolds) {
      tt <- train_test_from_folds(folds, j)
      error <- rep(0, n)
      msg <- rep("", n)
      for (i in 1:n) {
        err <- tryCatch(
          {
            model <- build_model(obj, ranges[i,], x[tt$train$i,], y[tt$train$i,])
            error[i] <- evaluate_error(model, tt$test$i, x, y)
            ""
          },
          error = function(cond) {
            err <- sprintf("tune: %s", as.character(cond))
          }
        )
        if (err != "") {
          msg[i] <- err
          if (obj$base_model$debug)
            print(err)
        }
      }
      if (obj$base_model$debug)
        print(sprintf("%d/%d-%d", j, nfolds, n))
      hyperparameters <- rbind(hyperparameters, cbind(ranges, error, msg))
    }
    hyperparameters$error[hyperparameters$msg != ""] <- NA
    i <- select_hyper(obj, hyperparameters)
  }

  model <- build_model(obj, ranges[i,], x, y)
  if (n == 1) {
    prediction <- stats::predict(model, x)
    error <- evaluate(model, y, prediction)$mse
    hyperparameters <- cbind(ranges, error)
  }

  attr(model, "params") <- as.list(ranges[i,])
  attr(model, "hyperparameters") <- hyperparameters

  return(model)
}

#'@title selection of hyperparameters (time series)
#'@description selection of hyperparameters (minimizing error)
#'@param obj object
#'@param hyperparameters hyperparameters dataset
#'@return optimized key number of hyperparameters
#'@importFrom dplyr filter summarise group_by
#'@export
select_hyper.ts_tune <- function(obj, hyperparameters) {
  msg <- error <- 0
  hyper_summary <- hyperparameters |> dplyr::filter(msg == "") |>
    dplyr::group_by(key) |> dplyr::summarise(error = mean(error, na.rm=TRUE))

  mim_error <- hyper_summary |> dplyr::summarise(error = min(error))

  key <- which(hyper_summary$error == mim_error$error)
  i <- min(hyper_summary$key[key])
  return(i)
}
