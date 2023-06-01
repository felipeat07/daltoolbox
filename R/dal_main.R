# DAL Library
# version 2.1

### basic transformation functions

#'@title dal_base object
#'@description
#'@details
#'
#'@return
#'@examples
#'@export
dal_base <- function() {
  obj <- list()
  obj$log <- FALSE
  obj$debug <- FALSE
  obj$reproduce <- FALSE
  attr(obj, "class") <- "dal_base"
  return(obj)
}

### basic transformation functions

#'@title DAL Transform
#'@description A transformation function can be applied to a time series dataset to alter its properties.
#'@details The dal_transform function returns a dal_transform object, which inherits
#' from the dal_base object. This object can be used to apply various transformations
#' to a time series dataset, such as scaling or differencing, and is used in the context
#' of the Data Analytics Library (DAL).
#'
#'@return a dal_transform object
#'@examples
#'@export
dal_transform <- function() {
  obj <- dal_base()
  class(obj) <- append("dal_transform", class(obj))
  return(obj)
}

#fit
#'@title Fit
#'@description Models a time series dataset by estimating the underlying trend
#' and seasonality components. Used to make predictions and forecast future
#' values of the time series based on the historical data.
#'@details
#'
#'@param obj object: .
#'@param ... further arguments passed to or from other methods.
#'@return
#'@examples
#'@export
fit <- function(obj, ...) {
  UseMethod("fit")
}

#'@export
fit.default <- function(obj, ...) {
  return(obj)
}

#transform
#'@title Transform
#'@description Defines the kind of transformation to be set over a time series.
#'@details This function is used to apply different kinds of transformations to time series data,
#' such as logarithmic, square root, or Box-Cox transformations, which can alter the underlying properties of the series
#' and make it more amenable to modeling and forecasting.
#' The specific kind of transformation is determined by calling the appropriate method for the given transformation type.
#'
#'@param obj object: a dal_transform object to apply the transformation to.
#'@param ... further arguments passed to or from other methods.
#'@return the transformed time series data.
#'@examples
#'@export
transform <- function(obj, ...) {
  UseMethod("transform")
}

#'@export
transform.default <- function(obj, ...) {
  return(NULL)
}

#inverse_transform
#'@title Inverse Transform
#'@description Reverses the transformation applied to a time series dataset using the transform() function.
#'@details This function is used to convert the transformed data back to its original scale, which can be useful for evaluating the performance of a time series model or visualizing the original data alongside the transformed data.
#'
#'@param obj object: The transformed time series dataset.
#'@param ... further arguments passed to or from other methods.
#'@return The time series dataset in its original scale.
#'@examples
#'@export
inverse_transform <- function(obj, ...) {
  UseMethod("inverse_transform")
}

#'@export
inverse_transform.default <- function(obj, ...) {
  return(NULL)
}

#optimize
#'@title Optimize
#'@description In R, the optimize function is a built-in function used for numerical optimization. It is typically used to find the minimum or maximum value of a function within a given interval. The optimize function takes a function and an interval as input, and returns the minimum or maximum value of the function within that interval.
#'@details The optimize function takes two required arguments: f: the function to be optimized; interval: a two-element vector giving the lower and upper bounds of the interval in which to search for the minimum or maximum. In addition, the optimize function has several optional arguments, including: maximum: a logical value indicating whether to find the maximum value of the function instead of the minimum (default is FALSE); tol: a tolerance level for the optimization algorithm (default is sqrt(.Machine$double.eps)). The optimize function returns a list with the following components: minimum: the minimum (or maximum) value of the function within the interval; objective: the value of the function at the minimum (or maximum) point; converged: a logical value indicating whether the optimization algorithm converged; iterations: the number of iterations required by the optimization algorithm.
#'
#'@param obj object: .
#'@param ... further arguments passed to or from other methods.
#'@return
#'@examples
#'@export
optimize <- function(obj, ...) {
  UseMethod("optimize")
}

#'@export
optimize.default <- function(obj) {
  return(obj)
}


#'@title Describe
#'@description Generates a summary of the time series dataset, including statistical
#' measures such as mean, variance, skewness, and kurtosis.
#'@details The describe function is used to generate a statistical summary of the
#' input time series dataset, including measures such as mean, variance, skewness,
#' and kurtosis. The exact measures calculated may vary depending on the specific
#' implementation of the describe function used.
#'
#'@param obj object: the time series dataset to be described.
#'@param ... further arguments passed to or from other methods.
#'@return
#'@examples
#'@export
describe <- function(obj, ...) {
  UseMethod("describe")
}

#'@export
describe.default <- function(obj) {
  if (is.null(obj))
    return("")
  else
    return(as.character(class(obj)[1]))
}

### basic data structures for sliding windows

# general functions
#'@export
adjust_matrix <- function(data) {
  if(!is.matrix(data)) {
    return(as.matrix(data))
  }
  else
    return(data)
}

#'@export
adjust_data.frame <- function(data) {
  if(!is.data.frame(data)) {
    return(as.data.frame(data))
  }
  else
    return(data)
}


### Basic log functions

#start_log
#'@title Start Log
#'@description
#'@details
#'
#'@param obj object: .
#'@return
#'@examples
#'@export
start_log <- function(obj) {
  UseMethod("start_log")
}

#'@export
start_log.default <- function(obj) {
  obj$log_time <- Sys.time()
  return(obj)
}

#register_log
#'@title Register log details
#'@description
#'@details
#'
#'@param obj object: .
#'@param msg string: a message to the log.
#'@param ref .
#'@return
#'@examples
#'@export
register_log <- function(obj, msg, ref) {
  UseMethod("register_log")
}

#'@export
register_log.default <- function(obj, msg = "") {
  obj$log_time <- as.numeric(difftime(Sys.time(), obj$log_time, units = "min"))
  if (msg == "")
    msg <- describe(obj)
  obj$log_msg <- sprintf("%s,%.3f", msg, obj$log_time)
  message(obj$log_msg)
  return(obj)
}
