# DAL Library
# version 2.1

#'@title Regression Tune
#'@description Regression Tune
#'@param base_model base model for tuning
#'@param folds number of folds for cross-validation
#'@param metric metric used to optimize
#'@return a `cla_tune` object.
#'@examples
#'# preparing dataset for random sampling
#'set.seed(1)
#'sr <- sample_random()
#'sr <- train_test(sr, iris)
#'train <- sr$train
#'test <- sr$test
#'
#'# hyper parameter optimization
#'tune <- cla_tune(cla_svm("Species", levels(iris$Species)))
#'ranges <- list(epsilon=seq(0,1,0.25), cost=seq(25,100,25), kernel = c("radial"))
#'model <- fit(tune, train, ranges)
#'train_prediction <- predict(model, train)
#'
#'# testing optimization
#'test_prediction <- predict(model, test)
#'test_predictand <- adjust_class_label(test[,"Species"])
#'test_eval <- evaluate(model, test_predictand, test_prediction)
#'test_eval$metrics
#'@export
cla_tune <- function(base_model, folds=10, metric="accuracy") {
  obj <- dal_tune(base_model, folds)
  obj$name <- ""
  obj$metric <- metric
  class(obj) <- append("cla_tune", class(obj))
  return(obj)
}


#'@title tune hyperparameters of ml model
#'@description tune hyperparameters of ml model for classification
#'@param obj object
#'@param data dataset
#'@param ranges hyperparameters ranges
#'@param ... optional arguments
#'@return fitted obj
#'@importFrom stats predict
#'@export
fit.cla_tune <- function(obj, data, ranges, ...) {

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

  evaluate_metric <- function(model, data) {
    x <- as.matrix(data[,model$x])
    y <- adjust_class_label(data[,model$attribute])
    prediction <- stats::predict(model, x)
    metric <- evaluate(model, y, prediction)$metrics[1,obj$metric]
    return(metric)
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
      }
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

  return(model)
}


#'@title selection of hyperparameters
#'@description selection of hyperparameters (maximizing classification metric)
#'@param obj object
#'@param hyperparameters hyperparameters dataset
#'@return optimized key number of hyperparameters
#'@importFrom dplyr filter summarise group_by
#'@export
select_hyper.cla_tune <- function(obj, hyperparameters) {
  msg <- metric <- 0
  hyper_summary <- hyperparameters |> dplyr::filter(msg == "") |>
    dplyr::group_by(key) |> dplyr::summarise(metric = mean(metric, na.rm=TRUE))

  max_metric <- hyper_summary |> dplyr::summarise(metric = max(metric))

  key <- which(hyper_summary$metric == max_metric$metric)
  i <- min(hyper_summary$key[key])
  return(i)
}


