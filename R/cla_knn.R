# DAL Library
# version 2.1

# depends dal_transform.R
# depends cla_classification.R

# knn
# loadlibrary("class")

# cla_knn
#'@title K Nearest Neighbor Classification
#'@description Classifies using the K-Nearest Neighbor algorithm.
#'@details This function implements the K-Nearest Neighbor algorithm for classification. It computes the K nearest neighbors of each data point in the training set, and then determines the class label of each point by taking the majority vote of the K neighbors.
#'@param attribute Name of the attribute used as target classification.
#'@param slevels Possible values for the target classification.
#'@param k A vector of integers indicating the number of neighbors to be considered.
#'@return A classification object.
#'@examples
#'@export
cla_knn <- function(attribute, slevels=NULL, k=1:30) {
  obj <- classification(attribute, slevels)
  obj$k <- k
  class(obj) <- append("cla_knn", class(obj))
  return(obj)
}

#'@title Set parameters values for cla_knn
#'@description It receives as input a ts_rf object (obj) and a set of parameters (params)
#'@details If the parameter set contains an entry for nodesize, the corresponding value is assigned to the ts_rf object. Likewise, if the parameter set contains an entry for ntree, the corresponding value is assigned to the ts_rf object
#'@param obj
#'@param params
#'@return The cla_knn object updated with the new parameter values
#'@export
set_params.cla_knn <- function(obj, params) {
  if (!is.null(params$k))
    obj$k <- params$k

  return(obj)
}


#'@import class
#'@export
fit.cla_knn <- function(obj, data) {

  data <- adjust_data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- fit.classification(obj, data)

  x <- data[,obj$x, drop = FALSE]
  y <- data[,obj$attribute]

  obj$model <-list(x=x, y=y, k=obj$k)

  msg <- sprintf("k=%d", obj$k)
  obj <- register_log(obj, msg)
  return(obj)
}

#'@import class
#'@export
predict.cla_knn  <- function(obj, x) {
  x <- adjust_data.frame(x)
  x <- x[,obj$x, drop=FALSE]

  prediction <- class::knn(train=obj$model$x, test=x, cl=obj$model$y, prob=TRUE)
  prediction <- adjustClassLabels(prediction)

  return(prediction)
}


