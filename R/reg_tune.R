# DAL Library
# version 2.1

#'@title Regression Tune
#'@description Regression Tune
#'@param base_model base model for tuning
#'@param folds number of folds for cross-validation
#'@return a `reg_tune` object.
#'@examples trans <- dal_transform()
#'@export
reg_tune <- function(base_model, folds=10) {
  obj <- dal_base()
  obj$base_model <- base_model
  obj$folds <- folds
  obj$name <- ""
  class(obj) <- append("reg_tune", class(obj))
  return(obj)
}


#'@title tune hyperparameters of ml model (regression)
#'@description tune hyperparameters of ml model for regression
#'@param obj object
#'@param data dataset
#'@param ranges hyperparamters ranges
#'@param ... optional arguments
#'@return fitted obj
#'@importFrom stats predict
#'@export
fit.reg_tune <- function(obj, data, ranges, ...) {

  build_model <- function(obj, ranges, data) {
    model <- obj$base_model
    model <- set_params(model, ranges)
    model <- fit(model, data)
    return(model)
  }

  prepare_ranges <- function(obj, ranges) {
    ranges <- expand.grid(ranges)
    ranges$key <- 1:nrow(ranges)
    obj$ranges <- ranges
    return(obj)
  }

  evaluate_error <- function(model, data) {
    x <- as.matrix(data[,model$x])
    y <- data[,model$attribute]
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
    ref <- data.frame(i = 1:nrow(data), idx = 1:nrow(data))
    folds <- k_fold(sample_random(), ref, obj$folds)
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
            model <- build_model(obj, ranges[i,], data[tt$train$i,])
            error[i] <- evaluate_error(model, data[tt$test$i,])
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

  model <- build_model(obj, ranges[i,], data)
  if (n == 1) {
    error <- evaluate_error(model, data)
    hyperparameters <- cbind(ranges, error)
  }
  attr(model, "params") <- as.list(ranges[i,])
  attr(model, "hyperparameters") <- hyperparameters

  return(model)
}


#'@title selection of hyperparameters
#'@description selection of hyperparameters (minimizing regression error)
#'@param obj object
#'@param hyperparameters hyperparameters dataset
#'@return optimized key number of hyperparameters
#'@importFrom dplyr filter summarise group_by
#'@export
select_hyper.reg_tune <- function(obj, hyperparameters) {
  msg <- error <- 0
  hyper_summary <- hyperparameters |> dplyr::filter(msg == "") |>
    dplyr::group_by(key) |> dplyr::summarise(error = mean(error, na.rm=TRUE))

  mim_error <- hyper_summary |> dplyr::summarise(error = min(error))

  key <- which(hyper_summary$error == mim_error$error)
  i <- min(hyper_summary$key[key])
  return(i)
}
