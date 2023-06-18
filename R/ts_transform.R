#'@title Time Series Transformation
#'@description Manipulate and reshapa a time series to better understand and
#' analyze the underlying patterns, trends, and relationships.
#'@return a `ts_transform` object.
#'@examples trans <- dal_transform()
#'@export
ts_transform <- function() {
  obj <- dal_transform()
  class(obj) <- append("ts_transform", class(obj))
  return(obj)
}

#'@title Transform data to objects of class st_transform
#'@description The function receives as arguments the variables obj and data
#'@param obj object
#'@param data dataset
#'@param ... optional arguments
#'@return The data given as an argument
#'@examples trans <- dal_transform()
#'@export
transform.ts_transform <- function(obj, data, ...) {
  return(data)
}


