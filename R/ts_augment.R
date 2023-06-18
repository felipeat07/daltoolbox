#'@title Time Series Augmentation
#'@description Augmentation is a technique used to increase the size and
#' diversity of a time series dataset by creating new instances of the original
#' data through transformations or modifications. The goal is to improve the
#' performance of machine learning models trained on time series data by
#' reducing overfitting and improving generalization.
#'@return a `ts_augment` object
#'@examples trans <- dal_transform()
#'@export
ts_augment <- function() {
  obj <- ts_transform()
  obj$preserve_data <- TRUE
  class(obj) <- append("ts_augment", class(obj))
  return(obj)
}

#'@export
fit.ts_augment <- function(obj, data, ...) {
  return(obj)
}

#'@title Time Series Augmentation tsaug_jitter
#'@description tsaug_jitter adds random "noise" to each data point in the time series.
#'@return a `tsaug_jitter` object
#'@examples trans <- dal_transform()
#'@export
tsaug_jitter <- function() {
  obj <- ts_augment()
  class(obj) <- append("tsaug_jitter", class(obj))
  return(obj)
}

#'@importFrom stats sd
#'@export
fit.tsaug_jitter <- function(obj, data, ...) {
  an <- apply(data, 1, mean)
  x <- data - an
  obj$sd <- stats::sd(x)
  return(obj)
}

#'@importFrom stats rnorm
#'@export
transform.tsaug_jitter <- function(obj, data, ...) {
  add.tsaug_jitter <- function(obj, data) {
    x <- stats::rnorm(length(data), mean = 0, sd = obj$sd)
    x <- matrix(x, nrow=nrow(data), ncol=ncol(data))
    x[,ncol(data)] <- 0
    data <- data + x
    attr(data, "idx") <- 1:nrow(data)
    return(data)
  }
  result <- add.tsaug_jitter(obj, data)
  if (obj$preserve_data) {
    idx <- c(1:nrow(data), attr(result, "idx"))
    result <- rbind(data, result)
    result <- adjust_ts_data(result)
    attr(result, "idx") <- idx
  }
  return(result)
}

#'@title Time Series Augmentation tsaug_stretch
#'@description Apply temporal distortion to the time axis of the data.
#'@param factor a real value (default = 1.2) define the degree of distortion applied.
#'@return a `tsaug_stretch` object.
#'@examples trans <- dal_transform()
#'@export
tsaug_stretch <- function(factor=1.2) {
  obj <- ts_augment()
  obj$factor <- factor
  class(obj) <- append("tsaug_stretch", class(obj))
  return(obj)
}

#'@export
transform.tsaug_stretch <- function(obj, data, ...) {
  add.tsaug_stretch <- function(obj, data) {
    an <- apply(data, 1, mean)
    x <- data - an
    x <- x * obj$factor
    x[,ncol(data)] <- 0
    data <- data + x
    attr(data, "idx") <- 1:nrow(data)
    return(data)
  }
  result <- add.tsaug_stretch(obj, data)
  if (obj$preserve_data) {
    idx <- c(1:nrow(data), attr(result, "idx"))
    result <- rbind(data, result)
    result <- adjust_ts_data(result)
    attr(result, "idx") <- idx
  }
  return(result)
}

#'@title Time Series Augmentation tsaug_shrink
#'@description Time Series Augmentation tsaug_shrink
#'@param factor a real value (default = 0.8) define the degree of distortion applied.
#'@return a `tsaug_shrink` object.
#'@examples trans <- dal_transform()
#'@export
tsaug_shrink <- function(factor = 0.8) {
  obj <- ts_augment()
  obj$factor <- factor
  class(obj) <- append("tsaug_shrink", class(obj))
  return(obj)
}

#'@export
transform.tsaug_shrink <- function(obj, data, ...) {
  add.tsaug_shrink <- function(obj, data) {
    an <- apply(data, 1, mean)
    x <- data - an
    x <- x * obj$factor
    x[,ncol(data)] <- 0
    data <- data + x
    attr(data, "idx") <- 1:nrow(data)
    return(data)
  }
  result <- add.tsaug_shrink(obj, data)
  if (obj$preserve_data) {
    idx <- c(1:nrow(data), attr(result, "idx"))
    result <- rbind(data, result)
    result <- adjust_ts_data(result)
    attr(result, "idx") <- idx
  }
  return(result)
}

#'@title Time Series Augmentation tsaug_flip
#'@description reverse the order of the data along the time axis.
#'@return a `tsaug_flip` object.
#'@examples trans <- dal_transform()
#'@export
tsaug_flip <- function() {
  obj <- ts_augment()
  class(obj) <- append("tsaug_flip", class(obj))
  return(obj)
}

#'@export
transform.tsaug_flip <- function(obj, data, ...) {
  add.tsaug_flip <- function(obj, data) {
    an <- apply(data, 1, mean)
    x <- data - an
    data <- an - x
    attr(data, "idx") <- 1:nrow(data)
    return(data)
  }
  result <- add.tsaug_flip(obj, data)
  if (obj$preserve_data) {
    idx <- c(1:nrow(data), attr(result, "idx"))
    result <- rbind(data, result)
    result <- adjust_ts_data(result)
    attr(result, "idx") <- idx
  }
  return(result)
}

#'@title Time Series Augmentation tsaug_wormhole
#'@description Inserts or removes segments of the time series data.
#'@return a `tsaug_wormhole` object.
#'@examples trans <- dal_transform()
#'@export
tsaug_wormhole <- function() {
  obj <- ts_augment()
  obj$fold <- 1
  class(obj) <- append("tsaug_wormhole", class(obj))
  return(obj)
}

#'@importFrom utils combn
#'@export
transform.tsaug_wormhole <- function(obj, data, ...) {
  add.tsaug_wormhole <- function(data) {
    n <- ncol(data)
    x <- c(as.vector(data[1,1:(n-1)]), as.vector(data[,n]))
    ts <- ts_data(x, n+1)
    space <- combn(1:n, n-1)
    data <- NULL
    idx <- NULL
    for (i in 1:obj$fold) {
      temp <- adjust_ts_data(ts[,c(space[,ncol(space)-i], ncol(ts))])
      idx <- c(idx, 1:nrow(temp))
      data <- rbind(data, temp)
    }
    attr(data, "idx") <- idx
    return(data)
  }
  result <- add.tsaug_wormhole(data)
  if (obj$preserve_data) {
    idx <- c(1:nrow(data), attr(result, "idx"))
    result <- rbind(data, result)
    result <- adjust_ts_data(result)
    attr(result, "idx") <- idx
  }
  return(result)
}

