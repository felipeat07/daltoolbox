# DAL Library
# version 2.1

#'@title Regression Tune
#'@description
#'@details
#'
#'@param input_size
#'@param base_model
#'@param folds
#'@return a `cla_tune` object.
#'@examples
#'@export
cla_tune <- function(base_model, folds=10, metric="accuracy") {
  obj <- dal_base()
  obj$base_model <- base_model
  obj$folds <- folds
  obj$name <- ""
  obj$metric <- metric
  class(obj) <- append("cla_tune", class(obj))
  return(obj)
}


#'@importFrom stats predict
#'@export
fit.cla_tune <- function(obj, data, ranges) {

  build_model <- function(obj, ranges, data) {
    model <- obj$base_model
    model$log <- FALSE
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

  evaluate_metric <- function(model, data) {
    x <- as.matrix(data[,model$x])
    y <- adjustClassLabels(data[,model$attribute])
    prediction <- stats::predict(model, x)
    metric <- evaluate(model, y, prediction)$metrics[1,obj$metric]
    return(metric)
  }

  obj <- start_log(obj)
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
      metric <- rep(0, n)
      msg <- rep("", n)
      for (i in 1:n) {
        err <- tryCatch(
          {
            model <- build_model(obj, ranges[i,], data[tt$train$i,])
            metric[i] <- evaluate_metric(model, data[tt$test$i,])
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
      hyperparameters <- rbind(hyperparameters, cbind(ranges, metric, msg))
    }
    hyperparameters$error[hyperparameters$msg != ""] <- NA
    i <- select_hyper(obj, hyperparameters)
  }

  model <- build_model(obj, ranges[i,], data)
  if (n == 1) {
    metric <- evaluate_metric(model, data)
    hyperparameters <- cbind(ranges, metric)
  }
  attr(model, "params") <- as.list(ranges[i,])
  attr(model, "hyperparameters") <- hyperparameters

  msg <- sprintf("%s-%s", describe(obj), describe(model))
  if (obj$base_model$log)
    obj <- register_log(obj, msg)
  return(model)
}


#'@import dplyr
#'@export
select_hyper.cla_tune <- function(obj, hyperparameters) {
  hyper_summary <- hyperparameters |> dplyr::filter(msg == "") |>
    dplyr::group_by(key) |> dplyr::summarise(metric = mean(metric, na.rm=TRUE))

  max_metric <- hyper_summary |> dplyr::summarise(metric = max(metric))

  key <- which(hyper_summary$metric == max_metric$metric)
  i <- min(hyper_summary$key[key])
  return(i)
}

#'@export
tune.classification <- function (obj, x, y, ranges, folds=3, fit.func, pred.fun = predict) {


  ranges <- expand.grid(ranges)
  n <- nrow(ranges)
  accuracies <- rep(0,n)

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
        accuracies[i] <- accuracies[i] + evaluation.classification(adjustClassLabels(y[tt$test$i]), prediction)$accuracy
      }
    }
    i <- which.max(accuracies)
  }
  if (obj$reproduce)
    set.seed(1)
  params <- append(list(x = x, y = y), as.list(ranges[i,]))
  model <- do.call(fit.func, params)
  attr(model, "params") <- as.list(ranges[i,])
  return(model)
}


