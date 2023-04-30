# DAL Library
# version 2.1

# depends dal_transform.R

### outliers
#'@title Outliers
#'@description This R function defines an object of class "outliers" that can be used for outlier detection in a dataset.
#'@details The outliers function has an optional parameter alpha that sets the threshold for identifying outliers. The default value for alpha is 1.5, which is a common value used in many outlier detection methods. The function creates an object using the dal_transform function, which must be defined elsewhere in the code or in an R package. The object created by the dal_transform function can be a base class object "dal_transform", which may include additional properties and methods for data transformation.
#'
#'@param alpha
#'@return
#'@examples out_obj <- outliers()
#'out_obj_custom <- outliers(alpha = 2.0)
#'out_obj_custom
#'
#'This example creates two objects of the "outliers" class, one with the default value of alpha and one with a custom value of alpha. These objects can be used to detect outliers in a dataset using methods specific to the "outliers" class.
#'@export
outliers <- function(alpha = 1.5) {
  obj <- dal_transform()
  obj$alpha <- alpha
  class(obj) <- append("outliers", class(obj))
  return(obj)
}

#'@export
fit.outliers <- function(obj, data) {
  lq1 <- NA
  hq3 <- NA
  if(is.matrix(data) || is.data.frame(data)) {
    lq1 <- rep(NA, ncol(data))
    hq3 <- rep(NA, ncol(data))
    if (nrow(data) >= 30) {
      for (i in 1:ncol(data)) {
        if (is.numeric(data[,i])) {
          q <- quantile(data[,i])
          IQR <- q[4] - q[2]
          lq1[i] <- q[2] - obj$alpha*IQR
          hq3[i] <- q[4] + obj$alpha*IQR
        }
      }
    }
  }
  else {
    if ((length(data) >= 30) && is.numeric(data)) {
      q <- quantile(data)
      IQR <- q[4] - q[2]
      lq1 <- q[2] - obj$alpha*IQR
      hq3 <- q[4] + obj$alpha*IQR
    }
  }
  obj$lq1 <- lq1
  obj$hq3 <- hq3
  return(obj)
}

#'@export
transform.outliers <- function(obj, data) {
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
