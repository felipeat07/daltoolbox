#'@title Time Series Adaptive Normalization (Exponential Moving Average - EMA)
#'@description It takes 2 parameters: remove_outliers and nw
#'@param remove_outliers logical: if TRUE (default) outliers will be removed.
#'@param nw windows size
#'@return a `ts_norm_ean` object.
#'@examples trans <- dal_transform()
#'@export
ts_norm_ean <- function(remove_outliers = TRUE, nw = 0) {
  emean <- function(data, na.rm = FALSE) {
    n <- length(data)

    y <- rep(0, n)
    alfa <- 1 - 2.0 / (n + 1);
    for (i in 0:(n-1)) {
      y[n-i] <- alfa^i
    }

    m <- sum(y * data, na.rm = na.rm)/sum(y, na.rm = na.rm)
    return(m)
  }
  obj <- ts_norm_an(remove_outliers, nw = nw)
  obj$an_mean <- emean
  class(obj) <- append("ts_norm_ean", class(obj))
  return(obj)
}

