# DAL Library
# version 2.1

#'@title Time Series Tune
#'@description
#'@details
#'
#'@param preprocess
#'@param input_size
#'@param base_model
#'@param folds
#'@return a `ts_tune` object.
#'@examples
#'@export
clu_tune <- function(base_model) {
  obj <- dal_base()
  obj$base_model <- base_model
  obj$name <- ""
  class(obj) <- append("clu_tune", class(obj))
  return(obj)
}

#'@importFrom stats predict
#'@export
fit.clu_tune <- function(obj, data, ranges) {

  build_cluster <- function(obj, ranges, data) {
    model <- obj$base_model
    model$log <- FALSE
    model <- set_params(model, ranges)
    result <- cluster(model, data)
    return(result)
  }

  prepare_ranges <- function(obj, ranges) {
    ranges <- expand.grid(ranges)
    ranges$key <- 1:nrow(ranges)
    obj$ranges <- ranges
    return(obj)
  }

  obj <- start_log(obj)
  if (obj$base_model$reproduce)
    set.seed(1)

  obj <- prepare_ranges(obj, ranges)
  ranges <- obj$ranges
  ranges$metric <- NA

  n <- nrow(ranges)
  i <- 1
  if (n > 1) {
    for (i in 1:n) {
      err <- tryCatch(
        {
          clu <- build_cluster(obj, ranges[i,], data)
          ranges$metric[i] <- attr(clu, "metric")
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
    myfit <- fit_curvature_max()
    res <- transform(myfit, ranges$metric)
    i <- res$x
  }
  model <- set_params(obj$base_model, ranges[i,])
  msg <- sprintf("%s-%s", describe(obj), describe(model))
  if (obj$base_model$log)
    obj <- register_log(obj, msg)
  return(model)
}

