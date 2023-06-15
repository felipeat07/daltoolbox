#ts_diff
#'@title Time Series Diff
#'@description It receives as parameter the variable remove_outliters. This function calculates the difference between the values of a time series
#'@param remove_outliers logical: if TRUE (default) outliers will be removed.
#'@return a `ts_diff` object.
#'@examples trans <- dal_transform()
#'@export
ts_diff <- function(remove_outliers = TRUE) {
  obj <- ts_normalize(remove_outliers)
  class(obj) <- append("ts_diff", class(obj))
  return(obj)
}

#'@title Fit the object created by the ts_diff function to the training data
#'@description It takes two arguments: obj and data
#'@param obj object
#'@param data dataset
#'@param ... optional arguments
#'@return The updated obj object
#'@examples trans <- dal_transform()
#'@export
fit.ts_diff <- function(obj, data, ...) {
  data <- data[,2:ncol(data)]-data[,1:(ncol(data)-1)]
  obj <- fit.ts_gminmax(obj, data)
  return(obj)
}

#'@title Transform the data
#'@description It takes as parameters the variables obj, data and x
#'@param obj object
#'@param data dataset
#'@param x input variable
#'@param ... optional arguments
#'@return Transformed full data matrix
#'@examples trans <- dal_transform()
#'@export
transform.ts_diff <- function(obj, data, x=NULL, ...) {
  if (!is.null(x)) {
    ref <- attr(data, "ref")
    sw <- attr(data, "sw")
    x <- x-ref
    x <- (x-obj$gmin)/(obj$gmax-obj$gmin)
    return(x)
  }
  else {
    ref <- as.vector(data[,ncol(data)])
    cnames <- colnames(data)
    for (i in (ncol(data)-1):1)
      data[,i+1] <- data[, i+1] - data[,i]
    data <- data[,2:ncol(data)]
    data <- (data-obj$gmin)/(obj$gmax-obj$gmin)
    attr(data, "ref") <- ref
    attr(data, "sw") <- ncol(data)
    attr(data, "cnames") <- cnames
    return(data)
  }
}

#'@title Revert the transform applied in the transform.ts_diff function
#'@description It takes as parameters the variables obj, data and x
#'@param obj object
#'@param data dataset
#'@param x predicted values in normalized scale
#'@param ... optional arguments
#'@return If the argument "x" is provided, the function returns the value corresponding to "x" in the original scale. Otherwise, the function returns the complete "data" time series at the original scale
#'@examples trans <- dal_transform()
#'@export
inverse_transform.ts_diff <- function(obj, data, x=NULL, ...) {
  cnames <- attr(data, "cnames")
  ref <- attr(data, "ref")
  sw <- attr(data, "sw")
  if (!is.null(x)) {
    x <- x * (obj$gmax-obj$gmin) + obj$gmin
    x <- x + ref
    return(x)
  }
  else {
    data <- data * (obj$gmax-obj$gmin) + obj$gmin
    data <- cbind(data, ref)
    for (i in (ncol(data)-1):1)
      data[,i] <- data[, i+1] - data[,i]
    colnames(data) <- cnames
    attr(data, "ref") <- ref
    attr(data, "sw") <- ncol(data)
    attr(data, "cnames") <- cnames
    return(data)
  }
}

