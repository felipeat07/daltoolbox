#'@title Time Series Adaptive Normalization
#'@description Transform data to a common scale while taking into account the
#' changes in the statistical properties of the data over time.
#'@param remove_outliers logical: if TRUE (default) outliers will be removed.
#'@param nw integer: window size.
#'@return a `ts_an` object.
#'@examples trans <- dal_transform()
#'@export
ts_an <- function(remove_outliers = TRUE, nw = 0) {
  obj <- dal_transform()
  obj$remove_outliers <- remove_outliers

  obj$an_mean <- mean
  obj$nw <- nw
  class(obj) <- append("ts_an", class(obj))
  return(obj)
}

#'@title Removes the first few columns from a data array
#'@description It takes as parameters the variables obj, data, func
#'@param obj object
#'@param data dataset
#'@param func function to aggregate
#'@return normalized sliding windows
#'@examples trans <- dal_transform()
#'@export
ma.ts_an <- function(obj, data, func) {
  if (obj$nw != 0) {
    cols <- ncol(data) - ((obj$nw-1):0)
    data <- data[,cols]

  }
  an <- apply(data, 1, func, na.rm=TRUE)
}

#'@title Fits the "ts_an" normalization model to a training dataset
#'@description It takes two arguments: obj and data. It computes the moving average of the input data using the "ma.ts_an" function, subtracts the moving average from the input data, and scales the data so that the values are in the range between 0 and 1
#'@param obj object
#'@param data dataset
#'@param ... optional arguments
#'@return The adjusted "ts_an" object
#'@examples trans <- dal_transform()
#'@export
fit.ts_an <- function(obj, data, ...) {
  input <- data[,1:(ncol(data)-1)]
  an <- ma.ts_an(obj, input, obj$an_mean)
  data <- data - an #

  if (obj$remove_outliers) {
    out <- outliers()
    out <- fit(out, data)
    data <- transform(out, data)
  }

  obj$gmin <- min(data)
  obj$gmax <- max(data)

  return(obj)
}

#'@title Normalize the input data
#'@description It takes 3 arguments: obj, data and x
#'@param obj object
#'@param data dataset
#'@param x input variable
#'@param ... optional arguments
#'@return Normalized data along with annual moving average value
#'@examples trans <- dal_transform()
#'@export
transform.ts_an <- function(obj, data, x=NULL, ...) {
  if (!is.null(x)) {
    an <- attr(data, "an")
    x <- x - an #
    x <- (x - obj$gmin) / (obj$gmax-obj$gmin)
    return(x)
  }
  else {
    an <- ma.ts_an(obj, data, obj$an_mean)
    data <- data - an #
    data <- (data - obj$gmin) / (obj$gmax-obj$gmin)
    attr(data, "an") <- an
    return (data)
  }
}

#'@title Perform the inversion of the transformation performed by the transform.t_an function
#'@description It takes 3 arguments: obj, data and x
#'@param obj object
#'@param data dataset
#'@param x input variable
#'@param ... optional arguments
#'@return The final result of the inverse transformation
#'@examples trans <- dal_transform()
#'@export
inverse_transform.ts_an <- function(obj, data, x=NULL, ...) {
  an <- attr(data, "an")
  if (!is.null(x)) {
    x <- x * (obj$gmax-obj$gmin) + obj$gmin
    x <- x + an #
    return(x)
  }
  else {
    data <- data * (obj$gmax-obj$gmin) + obj$gmin
    data <- data + an #
    attr(data, "an") <- an
    return (data)
  }
}
