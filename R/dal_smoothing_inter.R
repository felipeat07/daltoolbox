#'@title Smoothing by interval
#'@description The "smoothing by interval" function is used to apply a smoothing technique to a vector or time series data using a moving window approach.
#'@param n number of bins
#'@return obj
#'@examples trans <- dal_transform()
#'@export
smoothing_inter <- function(n) {
  obj <- smoothing(n)
  class(obj) <- append("smoothing_inter", class(obj))
  return(obj)
}

#'@importFrom graphics boxplot
#'@export
fit.smoothing_inter <- function(obj, data, ...) {
  if (length(obj$n) > 1)
    obj <- tune_smoothing(obj, data)
  else {
    v <- data
    n <- obj$n
    bp <- graphics::boxplot(v, range=1.5, plot = FALSE)
    bimax <- bp$stats[5]
    bimin <- bp$stats[1]
    if (bimin == bimax) {
      bimax = max(v)
      bimin = min(v)
    }
    obj$interval <- seq(from = bimin, to = bimax, by = (bimax-bimin)/n)
    obj <- fit.smoothing(obj, data)
  }
  return(obj)
}

