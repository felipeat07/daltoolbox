# DAL Library
# version 2.1

# depends dal_transform.R
# depends ts_data.R

### ts_augment
#'@title Time Series Augmentation
#'@description Augmentation is a technique used to increase the size and
#' diversity of a time series dataset by creating new instances of the original
#' data through transformations or modifications. The goal is to improve the
#' performance of machine learning models trained on time series data by
#' reducing overfitting and improving generalization.
#'@details
#'
#'@return a `ts_augment` object
#'@examples
#'@export
ts_augment <- function() {
  obj <- ts_transform()
  obj$preserve_data <- TRUE
  class(obj) <- append("ts_augment", class(obj))
  return(obj)
}

#'@export
fit.ts_augment <- function(obj, data) {
  return(obj)
}



### jitter
#'@title Time Series Augmentation Jitter
#'@description Jitter adds random "noise" to each data point in the time series.
#'@details
#'
#'@return a `jitter` object
#'@examples
#'@export
jitter <- function() {
  obj <- ts_augment()
  class(obj) <- append("jitter", class(obj))
  return(obj)
}

#'@export
fit.jitter <- function(obj, data) {
  an <- apply(data, 1, mean)
  x <- data - an
  obj$sd <- sd(x)
  return(obj)
}

#'@export
transform.jitter <- function(obj, data) {
  add.jitter <- function(obj, data) {
    x <- rnorm(length(data), mean = 0, sd = obj$sd)
    x <- matrix(x, nrow=nrow(data), ncol=ncol(data))
    x[,ncol(data)] <- 0
    data <- data + x
    attr(data, "idx") <- 1:nrow(data)
    return(data)
  }
  result <- add.jitter(obj, data)
  if (obj$preserve_data) {
    idx <- c(1:nrow(data), attr(result, "idx"))
    result <- rbind(data, result)
    result <- adjust.ts_data(result)
    attr(result, "idx") <- idx
  }
  return(result)
}

### stretch
#'@title Time Series Augmentation Stretch
#'@description Apply temporal distortion to the time axis of the data.
#'@details
#'
#'@param factor a real value (default = 1.2) define the degree of distortion applied.
#'@return a `stretch` object.
#'@examples
#'@export
stretch <- function(factor=1.2) {
  obj <- ts_augment()
  obj$factor <- factor
  class(obj) <- append("stretch", class(obj))
  return(obj)
}

#'@export
transform.stretch <- function(obj, data) {
  add.stretch <- function(obj, data) {
    an <- apply(data, 1, mean)
    x <- data - an
    x <- x * obj$factor
    x[,ncol(data)] <- 0
    data <- data + x
    attr(data, "idx") <- 1:nrow(data)
    return(data)
  }
  result <- add.stretch(obj, data)
  if (obj$preserve_data) {
    idx <- c(1:nrow(data), attr(result, "idx"))
    result <- rbind(data, result)
    result <- adjust.ts_data(result)
    attr(result, "idx") <- idx
  }
  return(result)
}

### shrink
#'@title Time Series Augmentation Shrink
#'@description
#'@details
#'
#'@param factor a real value (default = 0.8) define the degree of distortion applied.
#'@return a `shrink` object.
#'@examples
#'@export
shrink <- function(factor = 0.8) {
  obj <- ts_augment()
  obj$factor <- factor
  class(obj) <- append("shrink", class(obj))
  return(obj)
}

#'@export
transform.shrink <- function(obj, data) {
  add.shrink <- function(obj, data) {
    an <- apply(data, 1, mean)
    x <- data - an
    x <- x * obj$factor
    x[,ncol(data)] <- 0
    data <- data + x
    attr(data, "idx") <- 1:nrow(data)
    return(data)
  }
  result <- add.shrink(obj, data)
  if (obj$preserve_data) {
    idx <- c(1:nrow(data), attr(result, "idx"))
    result <- rbind(data, result)
    result <- adjust.ts_data(result)
    attr(result, "idx") <- idx
  }
  return(result)
}

### flip
#'@title Time Series Augmentation Flip
#'@description reverse the order of the data along the time axis.
#'@details
#'
#'@return a `flip` object.
#'@examples
#'@export
flip <- function() {
  obj <- ts_augment()
  class(obj) <- append("flip", class(obj))
  return(obj)
}

#'@export
transform.flip <- function(obj, data) {
  add.flip <- function(obj, data) {
    an <- apply(data, 1, mean)
    x <- data - an
    data <- an - x
    attr(data, "idx") <- 1:nrow(data)
    return(data)
  }
  result <- add.flip(obj, data)
  if (obj$preserve_data) {
    idx <- c(1:nrow(data), attr(result, "idx"))
    result <- rbind(data, result)
    result <- adjust.ts_data(result)
    attr(result, "idx") <- idx
  }
  return(result)
}

### Wormhole
#'@title Time Series Augmentation Wormhole
#'@description Inserts or removes segments of the time series data.
#'@details
#'
#'@return a `wormhole` object.
#'@examples
#'@export
wormhole <- function() {
  obj <- ts_augment()
  obj$fold <- 1
  class(obj) <- append("wormhole", class(obj))
  return(obj)
}

#'@export
transform.wormhole <- function(obj, data) {
  add.wormhole <- function(data) {
    n <- ncol(data)
    x <- c(as.vector(data[1,1:(n-1)]), as.vector(data[,n]))
    ts <- ts_data(x, n+1)
    space <- combn(1:n, n-1)
    data <- NULL
    idx <- NULL
    for (i in 1:obj$fold) {
      temp <- adjust.ts_data(ts[,c(space[,ncol(space)-i], ncol(ts))])
      idx <- c(idx, 1:nrow(temp))
      data <- rbind(data, temp)
    }
    attr(data, "idx") <- idx
    return(data)
  }
  result <- add.wormhole(data)
  if (obj$preserve_data) {
    idx <- c(1:nrow(data), attr(result, "idx"))
    result <- rbind(data, result)
    result <- adjust.ts_data(result)
    attr(result, "idx") <- idx
  }
  return(result)
}

