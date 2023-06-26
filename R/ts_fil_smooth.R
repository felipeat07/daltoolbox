#'@title Time Series Smooth
#'@description Used to remove or reduce randomness (noise).
#'@return a `tsfil_smooth` object.
#'@examples trans <- dal_transform()
#'@export
tsfil_smooth <- function() {
  obj <- dal_transform()
  class(obj) <- append("tsfil_smooth", class(obj))
  return(obj)
}

#'@export
#'@importFrom stats na.omit
#'@importFrom graphics boxplot
transform.tsfil_smooth <- function(obj, data, ...) {
  progressive_smoothing <- function(serie) {
    serie <- stats::na.omit(serie)
    repeat {
      n <- length(serie)
      diff <- serie[2:n] - serie[1:(n-1)]

      names(diff) <- 1:length(diff)
      bp <- graphics::boxplot(diff, plot = FALSE)
      j <- as.integer(names(bp$out))

      rj <- j[(j > 1) & (j < length(serie))]
      serie[rj] <- (serie[rj-1]+serie[rj+1])/2

      diff <- serie[2:n] - serie[1:(n-1)]
      bpn <- graphics::boxplot(diff, plot = FALSE)

      if ((length(bpn$out) == 0) || (length(bp$out) == length(bpn$out))) {
        break
      }
    }
    return(serie)
  }
  xd <- progressive_smoothing(data)
  return(xd)
}

