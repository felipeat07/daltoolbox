#'@title Time Series Augmentation ts_augjitter
#'@description Augmentation is a technique used to increase the size and
#' diversity of a time series dataset by creating new instances of the original
#' data through transformations or modifications. The goal is to improve the
#' performance of machine learning models trained on time series data by
#' reducing overfitting and improving generalization. ts_augjitter adds random "noise" to each data point in the time series.
#'@return a `ts_augjitter` object
#'@examples trans <- dal_transform()
#'@export
ts_augjitter <- function() {
  obj <- dal_transform()
  obj$preserve_data <- TRUE
  class(obj) <- append("ts_augjitter", class(obj))
  return(obj)
}

#'@importFrom stats sd
#'@export
fit.ts_augjitter <- function(obj, data, ...) {
  an <- apply(data, 1, mean)
  x <- data - an
  obj$sd <- stats::sd(x)
  return(obj)
}

#'@importFrom stats rnorm
#'@export
transform.ts_augjitter <- function(obj, data, ...) {
  add.ts_augjitter <- function(obj, data) {
    x <- stats::rnorm(length(data), mean = 0, sd = obj$sd)
    x <- matrix(x, nrow=nrow(data), ncol=ncol(data))
    x[,ncol(data)] <- 0
    data <- data + x
    attr(data, "idx") <- 1:nrow(data)
    return(data)
  }
  result <- add.ts_augjitter(obj, data)
  if (obj$preserve_data) {
    idx <- c(1:nrow(data), attr(result, "idx"))
    result <- rbind(data, result)
    result <- adjust_ts_data(result)
    attr(result, "idx") <- idx
  }
  return(result)
}

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

#'@title Time Series Augmentation ts_augflip
#'@description Augmentation is a technique used to increase the size and
#' diversity of a time series dataset by creating new instances of the original
#' data through transformations or modifications. The goal is to improve the
#' performance of machine learning models trained on time series data by
#' reducing overfitting and improving generalization.reverse the order of the data along the time axis.
#'@return a `ts_augflip` object.
#'@examples trans <- dal_transform()
#'@export
ts_augflip <- function() {
  obj <- dal_transform()
  obj$preserve_data <- TRUE
  class(obj) <- append("ts_augflip", class(obj))
  return(obj)
}

#'@export
transform.ts_augflip <- function(obj, data, ...) {
  add.ts_augflip <- function(obj, data) {
    an <- apply(data, 1, mean)
    x <- data - an
    data <- an - x
    attr(data, "idx") <- 1:nrow(data)
    return(data)
  }
  result <- add.ts_augflip(obj, data)
  if (obj$preserve_data) {
    idx <- c(1:nrow(data), attr(result, "idx"))
    result <- rbind(data, result)
    result <- adjust_ts_data(result)
    attr(result, "idx") <- idx
  }
  return(result)
}

#'@title Time Series Augmentation ts_augwormhole
#'@description Augmentation is a technique used to increase the size and
#' diversity of a time series dataset by creating new instances of the original
#' data through transformations or modifications. The goal is to improve the
#' performance of machine learning models trained on time series data by
#' reducing overfitting and improving generalization.Inserts or removes segments of the time series data.
#'@return a `ts_augwormhole` object.
#'@examples trans <- dal_transform()
#'@export
ts_augwormhole <- function() {
  obj <- dal_transform()
  obj$preserve_data <- TRUE
  obj$fold <- 1
  class(obj) <- append("ts_augwormhole", class(obj))
  return(obj)
}

#'@importFrom utils combn
#'@export
transform.ts_augwormhole <- function(obj, data, ...) {
  add.ts_augwormhole <- function(data) {
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
  result <- add.ts_augwormhole(data)
  if (obj$preserve_data) {
    idx <- c(1:nrow(data), attr(result, "idx"))
    result <- rbind(data, result)
    result <- adjust_ts_data(result)
    attr(result, "idx") <- idx
  }
  return(result)
}

