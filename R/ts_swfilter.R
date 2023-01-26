# DAL Library
# version 2.1

# depends dal_transform.R
# depends ts_data.R

### ts_swfilter
#'@title
#'@description
#'@details
#'
#'@return
#'@examples
#'@export
ts_swfilter <- function() {
  obj <- ts_transform()
  class(obj) <- append("ts_swfilter", class(obj))
  return(obj)
}

#'@title
#'@description
#'@details
#'
#'@param factor
#'@return
#'@examples
#'@export
ts_awareness <- function(factor = 1) {
  obj <- ts_swfilter()
  obj$factor <- factor
  class(obj) <- append("ts_awareness", class(obj))
  return(obj)
}

#'@export
transform.ts_awareness <- function(obj, data) {
  noise.parameters <- function(obj, data) {
    an <- apply(data, 1, mean)
    x <- data - an
    obj$xsd <- sd(x)
    return(obj)
  }

  add.noise <- function(obj, data) {
    x <- rnorm(length(data), mean = 0, sd = obj$xsd)
    x <- matrix(x, nrow=nrow(data), ncol=ncol(data))
    x[,ncol(data)] <- 0
    data <- data + x
    return(data)
  }
  filter.data <- function(data) {
    n <- nrow(data)
    rate <- 10/n
    i <- ceiling(rexp(10*n, rate))
    i <- i[(i > 0) & (i < n)]
    i <- sample(i, obj$factor*n)
    i <- n - i + 1
    i <- sort(i)
    return(i)
  }
  obj <- noise.parameters(obj, data)
  i <- filter.data(data)
  ndata <- add.noise(obj, data[i,])
  result <- ndata
  attr(result, "idx") <-  i
  idx <- c(1:nrow(data), attr(result, "idx"))
  result <- rbind(data, result)
  result <- adjust.ts_data(result)
  attr(result, "idx") <- idx
  return(result)
}

### ts_aware_smooth
#'@title
#'@description
#'@details
#'
#'@param factor
#'@return
#'@examples
#'@export
ts_aware_smooth <- function(factor = 0) {
  obj <- ts_swfilter()
  obj$factor <- factor
  class(obj) <- append("ts_aware_smooth", class(obj))
  return(obj)
}

#'@export
transform.ts_aware_smooth <- function(obj, data) {
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

  transform_ts_awareness <- function(data, factor) {
    filter_data <- function(data, factor) {
      n <- nrow(data)
      rate <- 10/n
      i <- ceiling(rexp(10*n, rate))
      i <- i[(i > 0) & (i < n)]
      i <- sample(i, factor*n)
      i <- n - i + 1
      i <- sort(i)
      return(i)
    }

    add_noise <- function(input, data) {
      an <- apply(data, 1, mean)
      x <- data - an
      xsd <- sd(x)
      x <- rnorm(length(input), mean = 0, sd = xsd)
      x <- matrix(x, nrow=nrow(input), ncol=ncol(input))
      x[,ncol(input)] <- 0
      input <- input + x
      return(input)
    }

    i <- filter_data(data, factor)
    result <- data[i,]
    result <- add_noise(result, data)
    attr(result, "idx") <-  i
    idx <- c(1:nrow(data), attr(result, "idx"))
    result <- rbind(data, result)
    result <- adjust.ts_data(result)
    attr(result, "idx") <- idx
    return(result)
  }

  n <- ncol(data)
  x <- c(as.vector(data[1,1:(n-1)]), as.vector(data[,n]))
  xd <- progressive_smoothing(x)
  result <- ts_data(xd, n)

  result <- transform_ts_awareness(result, obj$factor)

  idx <- attr(result, "idx")
  return(result)
}

