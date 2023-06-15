#'@title Normalization
#'@description The normalize() function in the DAL package performs a normalization on the input data so that each feature (column) has zero mean and unit variance. Normalizing the data is a common preprocessing step in machine learning, particularly when using algorithms that are sensitive to the scale of the input features. The resulting normalized object is returned with the "normalize" class added to it.
#'@return obj normalization
#'@examples trans <- dal_transform()
#'@export
normalize <- function() {
  obj <- dal_transform()
  class(obj) <- append("normalize", class(obj))
  return(obj)
}


#'@title Min-Max Normalization
#'@description The minmax() function performs Min-Max normalization on the data
#'@return minmax object
#'@examples trans <- dal_transform()
#'@export
minmax <- function() {
  obj <- normalize()
  class(obj) <- append("minmax", class(obj))
  return(obj)
}

#'@title Fit MinMax Normalization
#'@description Compute normalization parameters
#'@param obj minmax object
#'@param data dataset
#'@param ... optional arguments
#'@return fitted obj
#'@examples trans <- dal_transform()
#'@export
fit.minmax <- function(obj, data, ...) {
  minmax = data.frame(t(ifelse(sapply(data, is.numeric), 1, 0)))
  minmax = rbind(minmax, rep(NA, ncol(minmax)))
  minmax = rbind(minmax, rep(NA, ncol(minmax)))
  colnames(minmax) = colnames(data)
  rownames(minmax) = c("numeric", "max", "min")
  for (j in colnames(minmax)[minmax["numeric",]==1]) {
    minmax["min",j] <- min(data[,j], na.rm=TRUE)
    minmax["max",j] <- max(data[,j], na.rm=TRUE)
  }
  obj$norm.set <- minmax
  return(obj)
}


#'@title Transform minmax
#'@description Scale data using minmax normalization
#'@param obj minmax object
#'@param data dataset
#'@param ... optional arguments
#'@return transformed dataset
#'@examples trans <- dal_transform()
#'@export
transform.minmax <- function(obj, data, ...) {
  minmax <- obj$norm.set
  for (j in colnames(minmax)[minmax["numeric",]==1]) {
    if ((minmax["max", j] != minmax["min", j])) {
      data[,j] <- (data[,j] - minmax["min", j]) / (minmax["max", j] - minmax["min", j])
    }
    else {
      data[,j] <- 0
    }
  }
  return (data)
}

#'@title Inverse Transform minmax
#'@description The inverse transform of normalized data to original space
#'@param obj minmax object
#'@param data transformed dataset
#'@param ... optional arguments
#'@return inversed transformed dataset
#'@examples trans <- dal_transform()
#'@export
inverse_transform.minmax <- function(obj, data, ...) {
  minmax <- obj$norm.set
  for (j in colnames(minmax)[minmax["numeric",]==1]) {
    if ((minmax["max", j] != minmax["min", j])) {
      data[,j] <- data[,j] * (minmax["max", j] - minmax["min", j]) + minmax["min", j]
    }
    else {
      data[,j] <- minmax["max", j]
    }
  }
  return (data)
}

#'@title Z-score Normalization
#'@description Scale data using zscore normalization
#'@param nmean mean
#'@param nsd standard deviation
#'@return zscore object
#'@examples trans <- dal_transform()
#'@export
zscore <- function(nmean=0, nsd=1) {
  obj <- normalize()
  obj$nmean <- nmean
  obj$nsd <- nsd
  class(obj) <- append("zscore", class(obj))
  return(obj)
}


#'@title Fit Z-score
#'@description Compute normalization parameters
#'@param obj zscore object
#'@param data dataset
#'@param ... optional arguments
#'@return fitted zscore object
#'@examples trans <- dal_transform()
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

#'@title Transform z-score
#'@description Scale data using miz-score normalization
#'@param obj z-score object
#'@param data dataset
#'@param ... optional arguments
#'@return transformed dataset
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
