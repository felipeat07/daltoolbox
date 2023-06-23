#'@title Normalization
#'@description The normalize() function in the DAL package performs a normalization
#' on the input data so that each feature (column) has zero mean and unit variance.
#' Normalizing the data is a common preprocessing step in machine learning,
#' particularly when using algorithms that are sensitive to the scale of the input features.
#' The resulting normalized object is returned with the "normalize" class added to it.
#'@return obj normalization
#'@examples
#'#see ?minmax() for minmax normalization
#'#see ?zscore() for zscore normalization
#'@export
normalize <- function() {
  obj <- dal_transform()
  class(obj) <- append("normalize", class(obj))
  return(obj)
}

