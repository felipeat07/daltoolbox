# DAL Library
# version 2.1

# depends dal_transform.R
# depends ts_data.R

### ts_filter
#'@title
#'@description
#'@details
#'
#'@return
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
#'@title
#'@description
#'@details
#'
#'@return
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

#'@export
describe.ts_smooth <- function(obj) {
  return("smooth")
}
