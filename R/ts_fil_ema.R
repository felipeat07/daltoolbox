#'@title Time Series Exponential Moving Average
#'@description Used to smooth out fluctuations, while giving more weight to
#' recent observations. Particularly useful when the data has a trend or
#' seasonality component.
#'@param ema exponential moving average size
#'@return a `tsfil_ema` object.
#'@examples trans <- dal_transform()
#'@export
tsfil_ema <- function(ema = 3) {
  obj <- dal_transform()
  obj$ema <- ema
  class(obj) <- append("tsfil_ema", class(obj))
  return(obj)
}

#'@export
transform.tsfil_ema <- function(obj, data, ...) {
  exp_mean <- function(x) {
    n <- length(x)
    y <- rep(0,n)
    alfa <- 1 - 2.0 / (n + 1);
    for (i in 0:(n-1)) {
      y[n-i] <- alfa^i
    }
    m <- sum(y * x)/sum(y)
    return(m)
  }

  data <- ts_data(data, obj$ema)
  ema <- apply(data, 1, exp_mean)
  result <- c(rep(NA, obj$ema-1), ema)
  return(result)
}
