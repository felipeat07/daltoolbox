
#'@title  Adjust input data to matrix
#'@description adjust_matrix function receives
#' a data object and converts it to a matrix.
#'
#' It returns the same input if it is already a matrix.
#'@param data dataset
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

#'@title  Adjust input data to data frame
#'@description adjust_data.frame function receives
#' a data object and converts it to a data frame.
#'
#' It returns the same input if it is already a data frame.
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

#'@title adjust categorical values
#'@description adjust categorical values
#'@param value vector to be categorized
#'@param ilevels - order for categorical values
#'@param slevels - labels for categorical values
#'@return factor
#'@examples trans <- dal_transform()
#'@export
adjust_factor <- function(value, ilevels, slevels) {
  if (!is.factor(value)) {
    if (is.numeric(value))
      value <- factor(value, levels=ilevels)
    levels(value) <- slevels
  }
  return(value)
}

#'@title compute categorical mapping
#'@description compute categorical mapping
#'@param x vector to be categorized
#'@param valTrue - value to represent true
#'@param valFalse - value to represent false
#'@return factor
#'@examples trans <- dal_transform()
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
