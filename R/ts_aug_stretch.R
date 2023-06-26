#'@title Time Series Augmentation ts_augstretch
#'@description Augmentation is a technique used to increase the size and
#' diversity of a time series dataset by creating new instances of the original
#' data through transformations or modifications. The goal is to improve the
#' performance of machine learning models trained on time series data by
#' reducing overfitting and improving generalization.Apply temporal distortion to the time axis of the data.
#'@param factor a real value (default = 1.2) define the degree of distortion applied.
#'@return a `ts_augstretch` object.
#'@examples trans <- dal_transform()
#'@export
ts_augstretch <- function(factor=1.2) {
  obj <- dal_transform()
  obj$preserve_data <- TRUE
  obj$factor <- factor
  class(obj) <- append("ts_augstretch", class(obj))
  return(obj)
}

#'@export
transform.ts_augstretch <- function(obj, data, ...) {
  add.ts_augstretch <- function(obj, data) {
    an <- apply(data, 1, mean)
    x <- data - an
    x <- x * obj$factor
    x[,ncol(data)] <- 0
    data <- data + x
    attr(data, "idx") <- 1:nrow(data)
    return(data)
  }
  result <- add.ts_augstretch(obj, data)
  if (obj$preserve_data) {
    idx <- c(1:nrow(data), attr(result, "idx"))
    result <- rbind(data, result)
    result <- adjust_ts_data(result)
    attr(result, "idx") <- idx
  }
  return(result)
}

