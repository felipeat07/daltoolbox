#'@title z-score normalization
#'@description Scale data using z-score normalization
#'@param nmean mean
#'@param nsd standard deviation
#'@return z-score transformation object
#'@examples
#'data(iris)
#'trans <- zscore()
#'trans <- fit(trans, iris)
#'tiris <- transform(trans, iris)
#'@export
zscore <- function(nmean=0, nsd=1) {
  obj <- normalize()
  obj$nmean <- nmean
  obj$nsd <- nsd
  class(obj) <- append("zscore", class(obj))
  return(obj)
}


#'@title fit z-score normalization
#'@description Compute z-score normalization parameters
#'@param obj z-score transformation object
#'@param data dataset
#'@param ... optional arguments
#'@return fitted z-score transformation object
#'@examples
#'# see ?zscore
#'@importFrom stats sd
#'@export
fit.zscore <- function(obj, data, ...) {
  nmean <- obj$nmean
  nsd <- obj$nsd
  zscore <- data.frame(t(ifelse(sapply(data, is.numeric), 1, 0)))
  zscore <- rbind(zscore, rep(NA, ncol(zscore)))
  zscore <- rbind(zscore, rep(NA, ncol(zscore)))
  zscore <- rbind(zscore, rep(NA, ncol(zscore)))
  zscore <- rbind(zscore, rep(NA, ncol(zscore)))
  colnames(zscore) <- colnames(data)
  rownames(zscore) <- c("numeric", "mean", "sd","nmean", "nsd")
  for (j in colnames(zscore)[zscore["numeric",]==1]) {
    zscore["mean",j] <- mean(data[,j], na.rm=TRUE)
    zscore["sd",j] <- stats::sd(data[,j], na.rm=TRUE)
    zscore["nmean",j] <- nmean
    zscore["nsd",j] <- nsd
  }
  obj$norm.set <- zscore

  return(obj)
}

#'@title transform z-score normalization
#'@description Scale data using z-score normalization
#'@param obj z-score transformation object
#'@param data dataset
#'@param ... optional arguments
#'@return transformed dataset
#'@examples
#'# see ?zscore
#'@export
transform.zscore <- function(obj, data, ...) {
  zscore <- obj$norm.set
  for (j in colnames(zscore)[zscore["numeric",]==1]) {
    if ((zscore["sd", j]) > 0) {
      data[,j] <- (data[,j] - zscore["mean", j]) / zscore["sd", j] * zscore["nsd", j] + zscore["nmean", j]
    }
    else {
      data[,j] <- obj$nmean
    }
  }
  return (data)
}

#'@title Inverse Transform z-score
#'@description The inverse transform of normalized data to original space
#'@param obj z-score object
#'@param data transformed dataset
#'@param ... optional arguments
#'@return reverse transformed dataset
#'@examples trans <- dal_transform()
#'@export
inverse_transform.zscore <- function(obj, data, ...) {
  zscore <- obj$norm.set
  for (j in colnames(zscore)[zscore["numeric",]==1]) {
    if ((zscore["sd", j]) > 0) {
      data[,j] <- (data[,j] - zscore["nmean", j]) / zscore["nsd", j] * zscore["sd", j] + zscore["mean", j]
    }
    else {
      data[,j] <- zscore["nmean", j]
    }
  }
  return (data)
}
