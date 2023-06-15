#'@title Time Series Normalize
#'@description Transform data to a common scale, facilitating comparisons and
#' analysis.
#'@param remove_outliers logical: if TRUE (default) outliers will be removed.
#'@return a `ts_normalize` object.
#'@examples trans <- dal_transform()
#'@export
ts_normalize <- function(remove_outliers = TRUE) {
  obj <- dal_transform()
  obj$remove_outliers <- remove_outliers
  class(obj) <- append("ts_normalize", class(obj))
  return(obj)
}

