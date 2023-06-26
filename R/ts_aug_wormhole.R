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

