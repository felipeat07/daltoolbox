#'@title ts_data
#'@description Time series data structure used in DAL Toolbox.
#'It receives a vector (representing a time series) or
#'a matrix `y` (representing a sliding windows).
#'Internal ts_data is matrix of sliding windows with size `sw`.
#'If sw equals to zero, it store a time series as a single matrix column.
#'@param y output variable
#'@param sw integer: sliding window size.
#'@return a `ts_data` object.
#'@examples trans <- dal_transform()
#'@export
ts_data <- function(y, sw=1) {
  #https://stackoverflow.com/questions/7532845/matrix-losing-class-attribute-in-r
  ts_sw <- function(x, sw) {
    ts_lag <- function(x, k)
    {
      c(rep(NA, k), x)[1 : length(x)]
    }
    n <- length(x)-sw+1
    window <- NULL
    for(c in (sw-1):0){
      t  <- ts_lag(x,c)
      t <- t[sw:length(t)]
      window <- cbind(window,t,deparse.level = 0)
    }
    col <- paste("t",c((sw-1):0), sep="")
    colnames(window) <- col
    return(window)
  }

  if (sw > 1)
    y <- ts_sw(as.matrix(y), sw)
  else {
    y <- as.matrix(y)
    sw <- 1
  }

  col <- paste("t",(ncol(y)-1):0, sep="")
  colnames(y) <- col

  class(y) <- append("ts_data", class(y))
  attr(y, "sw") <- sw
  return(y)
}

#'@title Extract a subset of a time series stored in an object
#'@description Receives as parameters the variables x, i, j ...
#'@param x input variable
#'@param i row i
#'@param j column j
#'@param ... optional arguments
#'@return A new ts_data object
#'@examples trans <- dal_transform()
#'@export
`[.ts_data` <- function(x, i, j, ...) {
  y <- unclass(x)[i, j, drop = FALSE, ...]
  class(y) <- append("ts_data", class(y))
  attr(y, "sw") <- ncol(y)
  return(y)
}

#'@title Extract the first observations from a time series
#'@description The function takes as arguments the variables x, n (default = 6L), ...
#'@param x input variable
#'@param n integer: size of test data.
#'@param ... optional arguments
#'@return The first n observations of a time series x
#'@examples trans <- dal_transform()
#'@importFrom utils head
#'@export
ts_head <- function(x, n = 6L, ...) {
  utils::head(unclass(x), n)
}

