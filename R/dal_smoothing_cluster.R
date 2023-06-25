#'@title Smoothing by cluster
#'@description The function smoothing_cluster() is used to perform smoothing of data by cluster. This function takes as input a numeric vector, which is divided into clusters using the k-means algorithm. The mean of each cluster is then calculated and used as the smoothed value for all observations within that cluster.
#'@param n number of bins
#'@return obj
#'@examples trans <- dal_transform()
#'@export
smoothing_cluster <- function(n) {
  obj <- smoothing(n)
  class(obj) <- append("smoothing_cluster", class(obj))
  return(obj)
}

#'@importFrom stats kmeans
#'@export
fit.smoothing_cluster <- function(obj, data, ...) {
  if (length(obj$n) > 1)
    obj <- tune_smoothing(obj, data)
  else {
    v <- data
    n <- obj$n
    km <- stats::kmeans(x = v, centers = n)
    s <- sort(km$centers)
    s <- (s[1:n-1]+s[2:n])/2
    obj$interval <- c(min(v), s, max(v))
    obj <- fit.smoothing(obj, data)
  }
  return(obj)
}
