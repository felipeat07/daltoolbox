# DAL Library
# version 2.1

# depends dal_transform.R
# depends dal_sample.R
# depends ts_data.R
# depends ts_regression.R
# depends ts_preprocessing.R
# depends ts_augmentation.R

# class ts_maintune
# loadlibrary("dplyr")

#'@title
#'@description
#'@details
#'
#'@param preprocess
#'@param input_size
#'@param base_model
#'@param augment
#'@param folds
#'@return
#'@examples
#'@export
ts_maintune <- function(preprocess, input_size, base_model, augment = ts_augment(), folds=10) {
  obj <- tsreg_sw(preprocess, input_size)
  obj$base_model <- base_model
  obj$input_size <- input_size
  obj$preprocess <- preprocess
  obj$augment <- augment
  obj$folds <- folds
  obj$name <- ""
  class(obj) <- append("ts_maintune", class(obj))
  return(obj)
}

#'@title
#'@description
#'@details
#'
#'@param obj
#'@param name
#'@return
#'@examples
#'@export
get_preprocess <- function(obj, name) {
  i <- which(obj$names_preprocess == name)
  return(obj$preprocess[[i]])
}

#'@title
#'@description
#'@details
#'
#'@param obj
#'@param name
#'@return
#'@examples
#'@export
get_augment <- function(obj, name) {
  i <- which(obj$names_augment == name)
  return(obj$augment[[i]])
}

#'@title
#'@description
#'@details
#'
#'@param obj
#'@param ranges
#'@param x
#'@param y
#'@return
#'@examples
#'@export
build_model <- function(obj, ranges, x, y) {
  augment_data <- function(augment, x, y) {
    data <- cbind(x, y)
    data <- adjust.ts_data(data)
    data <- transform(augment, data)
    data <- adjust.ts_data(data)

    io <- ts_projection(data)

    return(list(x=io$input, y=io$output))
  }

  model <- obj$base_model
  model$log <- FALSE
  model$input_size <- ranges$input_size
  model$preprocess <- get_preprocess(obj, ranges$preprocess)
  model <- set_params(model, ranges)

  augment <- get_augment(obj, ranges$augment)
  data <- augment_data(augment, x, y)

  model <- fit(model, data$x, data$y)
  attr(model, "augment") <- augment

  return(model)
}

#'@title
#'@description
#'@details
#'
#'@param obj
#'@param ranges
#'@return
#'@examples
#'@export
prepare_ranges <- function(obj, ranges) {
  obj$names_preprocess <- sapply(obj$preprocess, function(x) { as.character(class(x)[1]) })
  obj$names_augment <- sapply(obj$augment, function(x) { as.character(class(x)[1]) })

  ranges <- append(list(input_size = obj$input_size, preprocess = obj$names_preprocess, augment = obj$names_augment), ranges)
  ranges <- expand.grid(ranges)
  ranges$preprocess <- as.character(ranges$preprocess)
  ranges$augment <- as.character(ranges$augment)
  ranges$key <- 1:nrow(ranges)

  obj$ranges <- ranges
  return(obj)
}

#'@title
#'@description
#'@details
#'
#'@param obj
#'@param model
#'@param i
#'@param x
#'@param y
#'@return
#'@examples
#'@export
evaluate_error <- function(obj, model, i, x, y) {
  x <- x[i,]
  y <- as.vector(y[i,])
  prediction <- as.vector(predict(model, x))
  error <- evaluation.tsreg(y, prediction)$mse
  return(error)
}

#'@export
fit_augment.ts_maintune <- function(obj, x, y) {
  data <- cbind(x, y)
  data <- adjust.ts_data(data)
  for (i in 1:length(obj$augment)) {
    augment <- obj$augment[[i]]
    obj$augment[[i]] <- fit(augment, data)
  }
  return(obj)
}

#'@export
fit.ts_maintune <- function(obj, x, y, ranges) {
  obj <- start_log(obj)
  if (obj$base_model$reproduce)
    set.seed(1)

  obj <- prepare_ranges(obj, ranges)
  ranges <- obj$ranges

  obj <- fit_augment.ts_maintune(obj, x, y)

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
            error[i] <- evaluate_error(obj, model, tt$test$i, x, y)
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
    i <- optimize(obj, hyperparameters)
  }

  model <- build_model(obj, ranges[i,], x, y)
  if (n == 1) {
    prediction <- predict(model, x)
    error <- evaluation.tsreg(y, prediction)$mse
    hyperparameters <- cbind(ranges, error)
  }

  attr(model, "params") <- as.list(ranges[i,])
  attr(model, "hyperparameters") <- hyperparameters
  augment <- attr(model, "augment")

  msg <- sprintf("%s-%s-%s", describe(obj), describe(model), describe(augment))
  if (obj$base_model$log)
    obj <- register_log(obj, msg)
  return(model)
}

#'@import dplyr
#'@export
optimize.ts_maintune <- function(obj, hyperparameters) {
  hyper_summary <- hyperparameters |> dplyr::filter(msg == "") |>
    dplyr::group_by(key) |> dplyr::summarise(error = mean(error, na.rm=TRUE))

  mim_error <- hyper_summary |> dplyr::summarise(error = min(error))

  key <- which(hyper_summary$error == mim_error$error)
  i <- min(hyper_summary$key[key])
  return(i)
}
