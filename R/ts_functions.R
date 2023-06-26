#'@title Time Series Sample
#'@description Has three arguments: ts: the time series to split; test_size: the size of the test sample, in number of observations; offset: the number of observations to be ignored at the end of the time series.
#'@param ts time series data.
#'@param test_size integer: size of test data (default = 1).
#'@param offset integer: starting point (default = 0).
#'@return A list with the two samples
#'@examples trans <- dal_transform()
#'@export
ts_sample <- function(ts, test_size=1, offset=0) {
  offset <- nrow(ts) - test_size - offset
  train <- ts[1:offset, ]
  test <- ts[(offset+1):(offset+test_size),]
  colnames(test) <- colnames(train)
  samp <- list(train = train, test = test)
  attr(samp, "class") <- "ts_sample"
  return(samp)
}


#'@title Transform the date object
#'@description The first check that is done is to see if data is a matrix using the is.matrix() function. If data is not a matrix, the function converts data to a matrix using the as.matrix() function
#'@param data dataset
#'@return The date object changed
#'@export
adjust_ts_data <- function(data) {
  if (!is.matrix(data))
    data <- as.matrix(data)
  colnames(data) <- paste("t",c((ncol(data)-1):0), sep="")
  class(data) <- append("ts_data", class(data))
  attr(data, "sw") <- ncol(data)
  return(data)
}

#'@title Time Series Projection
#'@description It takes a time series as input (an object of type matrix or data.frame)
#'@param ts matrix or data.frame containing the time series.
#'@return a `ts_projection` object.
#'@examples trans <- dal_transform()
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

