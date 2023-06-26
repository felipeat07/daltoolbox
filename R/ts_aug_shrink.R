#'@title Time Series Augmentation ts_augshrink
#'@description Augmentation is a technique used to increase the size and
#' diversity of a time series dataset by creating new instances of the original
#' data through transformations or modifications. The goal is to improve the
#' performance of machine learning models trained on time series data by
#' reducing overfitting and improving generalization.Time Series Augmentation ts_augshrink
#'@param factor a real value (default = 0.8) define the degree of distortion applied.
#'@return a `ts_augshrink` object.
#'@examples trans <- dal_transform()
#'@export
ts_augshrink <- function(factor = 0.8) {
  obj <- dal_transform()
  obj$preserve_data <- TRUE
  obj$factor <- factor
  class(obj) <- append("ts_augshrink", class(obj))
  return(obj)
}

#'@export
transform.ts_augshrink <- function(obj, data, ...) {
  add.ts_augshrink <- function(obj, data) {
    an <- apply(data, 1, mean)
    x <- data - an
    x <- x * obj$factor
    x[,ncol(data)] <- 0
    data <- data + x
    attr(data, "idx") <- 1:nrow(data)
    return(data)
  }
  result <- add.ts_augshrink(obj, data)
  if (obj$preserve_data) {
    idx <- c(1:nrow(data), attr(result, "idx"))
    result <- rbind(data, result)
    result <- adjust_ts_data(result)
    attr(result, "idx") <- idx
  }
  return(result)
}

