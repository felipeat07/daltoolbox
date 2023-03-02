# DAL Library
# version 2.1

# depends dal_transform.R
# depends ts_data.R

### ts_filter
#'@title Time Series Filter
#'@description Used to extract or remove specific components from a time series.
#'@details
#'
#'@return a `ts_filter` object.
#'@examples
#'@export
ts_filter <- function() {
  obj <- ts_transform()
  class(obj) <- append("ts_filter", class(obj))
  return(obj)
}

#'@export
fit.ts_filter <- function(obj, data) {
  return(obj)
}

### ts_smooth
#'@title Time Series Smooth
#'@description Used to remove or reduce randomness (noise).
#'@details
#'
#'@return a `ts_smooth` object.
#'@examples
#'@export
ts_smooth <- function() {
  obj <- ts_filter()
  class(obj) <- append("ts_smooth", class(obj))
  return(obj)
}

#'@export
transform.ts_smooth <- function(obj, data) {
  progressive_smoothing <- function(serie) {
    serie <- na.omit(serie)
    repeat {
      n <- length(serie)
      diff <- serie[2:n] - serie[1:(n-1)]

      names(diff) <- 1:length(diff)
      bp <- boxplot(diff, plot = FALSE)
      j <- as.integer(names(bp$out))

      rj <- j[(j > 1) & (j < length(serie))]
      serie[rj] <- (serie[rj-1]+serie[rj+1])/2

      diff <- serie[2:n] - serie[1:(n-1)]
      bpn <- boxplot(diff, plot = FALSE)

      if ((length(bpn$out) == 0) || (length(bp$out) == length(bpn$out))) {
        break
      }
    }
    return(serie)
  }

  xd <- progressive_smoothing(data)
  return(xd)
}


### ts_ma
#'@title Time Series Moving Average
#'@description Used to smooth out fluctuations and reduce noise in a time series.
#'@details
#'
#'@param ma
#'@return a `ts_ma` object.
#'@examples
#'@export
ts_ma <- function(ma = 3) {
  obj <- ts_filter()
  obj$ma <- ma
  class(obj) <- append("ts_ma", class(obj))
  return(obj)
}

#'@export
transform.ts_ma <- function(obj, data) {
  data <- ts_data(data, obj$ma)
  ma <- apply(data, 1, mean)
  result <- c(rep(NA, obj$ma-1), ma)
  return(result)
}

### ts_ema
#'@title Time Series Exponential Moving Average
#'@description Used to smooth out fluctuations, while giving more weight to
#' recent observations. Particularly useful when the data has a trend or
#' seasonality component.
#'@details
#'
#'@param ema
#'@return a `ts_ema` object.
#'@examples
#'@export
ts_ema <- function(ema = 3) {
  obj <- ts_filter()
  obj$ema <- ema
  class(obj) <- append("ts_ema", class(obj))
  return(obj)
}

#'@export
transform.ts_ema <- function(obj, data) {
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
