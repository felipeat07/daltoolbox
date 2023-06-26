#'@title adjust to matrix
#'@description dataset data is adjusted to a matrix
#'@param data dataset
#'@return an adjusted matrix
#'@examples
#'data(iris)
#'mat <- adjust_matrix(iris)
#'@export
adjust_matrix <- function(data) {
  if(!is.matrix(data)) {
    return(as.matrix(data))
  }
  else
    return(data)
}

#'@title  Adjust to data frame
#'@description dataset data is adjusted to a `data.frame`
#'@param data dataset
#'@return The date argument
#'@examples
#'data(iris)
#'df <- adjust_data.frame(iris)
#'@export
adjust_data.frame <- function(data) {
  if(!is.data.frame(data)) {
    return(as.data.frame(data))
  }
  else
    return(data)
}

#'@title adjust `ts_data`
#'@description dataset data is adjusted to a `ts_data`
#'@param data dataset
#'@return an adjusted `ts_data`
#'@export
adjust_ts_data <- function(data) {
  if (!is.matrix(data))
    data <- as.matrix(data)
  colnames(data) <- paste("t",c((ncol(data)-1):0), sep="")
  class(data) <- append("ts_data", class(data))
  attr(data, "sw") <- ncol(data)
  return(data)
}

#'@title adjust factors
#'@description vector `value` is adjusted to a factor
#'@param value vector to be converted into factor
#'@param ilevels order for categorical values
#'@param slevels labels for categorical values
#'@return an adjusted factor
#'@export
adjust_factor <- function(value, ilevels, slevels) {
  if (!is.factor(value)) {
    if (is.numeric(value))
      value <- factor(value, levels=ilevels)
    levels(value) <- slevels
  }
  return(value)
}

#'@title adjust categorical mapping
#'@description vector `value` is adjusted to a categorical mapping
#'@param x vector to be categorized
#'@param valTrue value to represent true
#'@param valFalse value to represent false
#'@return an adjusted categorical mapping
#'@export
adjust_class_label <- function (x, valTrue = 1, valFalse = 0)
{
  n <- length(x)
  x <- as.factor(x)
  res <- matrix(valFalse, n, length(levels(x)))
  res[(1:n) + n * (unclass(x) - 1)] <- valTrue
  dimnames(res) <- list(names(x), levels(x))
  res
}

