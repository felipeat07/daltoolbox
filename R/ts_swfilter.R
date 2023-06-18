#'@title Time Series Sliding Window Filter
#'@description Time Series Sliding Window Filter
#'@return a `tsfil_swfilter` object.
#'@examples trans <- dal_transform()
#'@export
tsfil_swfilter <- function() {
  obj <- ts_transform()
  class(obj) <- append("tsfil_swfilter", class(obj))
  return(obj)
}

#'@title Time Series Awareness
#'@description Time Series Awareness filter
#'@param factor factor of awareness
#'@return a `tsaug_awareness` object.
#'@examples trans <- dal_transform()
#'@export
tsaug_awareness <- function(factor = 1) {
  obj <- tsfil_swfilter()
  obj$factor <- factor
  class(obj) <- append("tsaug_awareness", class(obj))
  return(obj)
}

#'@importFrom stats rexp
#'@importFrom stats rnorm
#'@importFrom stats sd
#'@export
transform.tsaug_awareness <- function(obj, data, ...) {
  noise.parameters <- function(obj, data) {
    an <- apply(data, 1, mean)
    x <- data - an
    obj$xsd <- stats::sd(x)
    return(obj)
  }

  add.noise <- function(obj, data) {
    x <- stats::rnorm(length(data), mean = 0, sd = obj$xsd)
    x <- matrix(x, nrow=nrow(data), ncol=ncol(data))
    x[,ncol(data)] <- 0
    data <- data + x
    return(data)
  }
  filter.data <- function(data) {
    n <- nrow(data)
    rate <- 10/n
    i <- ceiling(stats::rexp(10*n, rate))
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
  result <- adjust_ts_data(result)
  attr(result, "idx") <- idx
  return(result)
}

#'@title Time Series Awareness Smooth
#'@description Time Series Awareness Smooth filter
#'@param factor factor of awareness
#'@return a `tsaug_aware_smooth` object.data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==
#'@examples trans <- dal_transform()
#'@export
tsaug_aware_smooth <- function(factor = 0) {
  obj <- tsfil_swfilter()
  obj$factor <- factor
  class(obj) <- append("tsaug_aware_smooth", class(obj))
  return(obj)
}

#'@importFrom stats rexp
#'@importFrom stats rnorm
#'@importFrom stats sd
#'@importFrom graphics boxplot
#'@export
transform.tsaug_aware_smooth <- function(obj, data, ...) {
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

  transform_tsaug_awareness <- function(data, factor) {
    filter_data <- function(data, factor) {
      n <- nrow(data)
      rate <- 10/n
      i <- ceiling(stats::rexp(10*n, rate))
      i <- i[(i > 0) & (i < n)]
      i <- sample(i, factor*n)
      i <- n - i + 1
      i <- sort(i)
      return(i)
    }

    add_noise <- function(input, data) {
      an <- apply(data, 1, mean)
      x <- data - an
      xsd <- stats::sd(x)
      x <- stats::rnorm(length(input), mean = 0, sd = xsd)
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
    result <- adjust_ts_data(result)
    attr(result, "idx") <- idx
    return(result)
  }

  n <- ncol(data)
  x <- c(as.vector(data[1,1:(n-1)]), as.vector(data[,n]))
  xd <- progressive_smoothing(x)
  result <- ts_data(xd, n)

  result <- transform_tsaug_awareness(result, obj$factor)

  idx <- attr(result, "idx")
  return(result)
}

