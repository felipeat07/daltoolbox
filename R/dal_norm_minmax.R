#'@title min-max normalization
#'@description The minmax performs scales data between \[0,1\]
#'@return min-max transformation object
#'@examples
#'data(iris)
#'trans <- minmax()
#'trans <- fit(trans, iris)
#'tiris <- transform(trans, iris)
#'@export
minmax <- function() {
  obj <- normalize()
  class(obj) <- append("minmax", class(obj))
  return(obj)
}

#'@title fit min-max normalization
#'@description Compute min-max normalization parameters
#'@param obj min-max transformation object
#'@param data dataset
#'@param ... optional arguments
#'@return fitted min-max transformation object
#'@examples
#'# see ?minmax
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


#'@title transform min-max normalization
#'@description Scale data using minmax normalization
#'@param obj min-max transformation object
#'@param data dataset
#'@param ... optional arguments
#'@return transformed dataset
#'@examples
#'# see ?minmax
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

#'@title inverse transform min-max normalization
#'@description The inverse transform of normalized data to original space
#'@param obj min-max transformation object
#'@param data transformed dataset
#'@param ... optional arguments
#'@return inverse transformed dataset
#'@examples
#'# see ?minmax
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

