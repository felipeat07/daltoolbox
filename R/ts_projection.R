#'@title Time Series Projection
#'@description Separates the `ts_data` into input and output.
#'@param ts matrix or data.frame containing the time series.
#'@return a `ts_projection` object.
#'@examples
#'#setting up a ts_data
#'data(sin_data)
#'ts <- ts_data(sin_data$y, 10)
#'
#'io <- ts_projection(ts)
#'
#'#input data
#'ts_head(io$input)
#'
#'#output data
#'ts_head(io$output)
#'@export
ts_projection <- function(ts) {
  input <- ts
  output <- ts

  if (is.matrix(ts) || is.data.frame(ts)) {
    if (nrow(ts) > 1) {
      input <- ts[,1:(ncol(ts)-1)]
      colnames(input) <- colnames(ts)[1:(ncol(ts)-1)]
      output <- ts[,ncol(ts)]
      colnames(output) <- colnames(ts)[ncol(ts)]
    }
    else {
      input <- ts_data(ts[,1:(ncol(ts)-1)], ncol(ts)-1)
      colnames(input) <- colnames(ts)[1:(ncol(ts)-1)]
      output <- ts_data(ts[,ncol(ts)], 1)
      colnames(output) <- colnames(ts)[ncol(ts)]
    }
  }

  proj <- list(input = input, output = output)
  attr(proj, "class") <- "ts_projection"
  return(proj)
}

