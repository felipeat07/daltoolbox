#'@title Outliers
#'@description Basic class for outlier removal
#'@param alpha boxplot outlier threshold
#'@return An outlier object
#'@examples trans <- dal_transform()
#'@export
outliers <- function(alpha = 1.5) {
  obj <- dal_transform()
  obj$alpha <- alpha
  class(obj) <- append("outliers", class(obj))
  return(obj)
}

#'@title Fit outliers object
#'@description Calculates upper and lower bounds based on alpha value
#'@param obj object
#'@param data dataset
#'@param ... optional arguments
#'@return An updated 'outliers' object, containing the lower and upper bounds for each numerical variable
#'@examples trans <- dal_transform()
#'@importFrom stats quantile
#'@export
fit.outliers <- function(obj, data, ...) {
  lq1 <- NA
  hq3 <- NA
  if(is.matrix(data) || is.data.frame(data)) {
    lq1 <- rep(NA, ncol(data))
    hq3 <- rep(NA, ncol(data))
    if (nrow(data) >= 30) {
      for (i in 1:ncol(data)) {
        if (is.numeric(data[,i])) {
          q <- stats::quantile(data[,i])
          IQR <- q[4] - q[2]
          lq1[i] <- q[2] - obj$alpha*IQR
          hq3[i] <- q[4] + obj$alpha*IQR
        }
      }
    }
  }
  else {
    if ((length(data) >= 30) && is.numeric(data)) {
      q <- stats::quantile(data)
      IQR <- q[4] - q[2]
      lq1 <- q[2] - obj$alpha*IQR
      hq3 <- q[4] + obj$alpha*IQR
    }
  }
  obj$lq1 <- lq1
  obj$hq3 <- hq3
  return(obj)
}

#'@title Remove outliers from data
#'@description Remove outliers from data
#'@param obj object
#'@param data dataset
#'@param ... optional arguments
#'@return returns the output dataset after applying the transformation and adds an "idx" attribute that contains a boolean vector indicating which rows were removed from the original dataset
#'@examples trans <- dal_transform()
#'@export
transform.outliers <- function(obj, data, ...) {
  idx <- FALSE
  lq1 <- obj$lq1
  hq3 <- obj$hq3
  if (is.matrix(data) || is.data.frame(data)) {
    idx = rep(FALSE, nrow(data))
    for (i in 1:ncol(data))
      if (!is.na(lq1[i]) && !is.na(hq3[i]))
        idx = idx | (!is.na(data[,i]) & (data[,i] < lq1[i] | data[,i] > hq3[i]))
  }
  if(is.matrix(data))
    data <- adjust_matrix(data[!idx,])
  else if (is.data.frame(data))
    data <- adjust_data.frame(data[!idx,])
  else {
    if (!is.na(lq1) && !is.na(hq3)) {
      idx <- data < lq1 | data > hq3
      data <- data[!idx]
    }
    else
      idx <- rep(FALSE, length(data))
  }
  attr(data, "idx") <- idx
  return(data)
}
