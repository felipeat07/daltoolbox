
#'@title Time Series Moving Average
#'@description Used to smooth out fluctuations and reduce noise in a time series.
#'@param ma moving average size
#'@return a `ts_fil_ma` object.
#'@examples trans <- dal_transform()
#'@export
ts_fil_ma <- function(ma = 3) {
  obj <- dal_transform()
  obj$ma <- ma
  class(obj) <- append("ts_fil_ma", class(obj))
  return(obj)
}

#'@export
transform.ts_fil_ma <- function(obj, data, ...) {
  data <- ts_data(data, obj$ma)
  ma <- apply(data, 1, mean)
  result <- c(rep(NA, obj$ma-1), ma)
  return(result)
}

