#'@title Data Sample
#'@description The data_sample function in R is used to randomly sample data from a given data frame. It can be used to obtain a subset of data for further analysis or modeling.
#'@return obj
#'@examples trans <- dal_transform()
#'@export
data_sample <- function() {
  obj <- dal_transform()
  class(obj) <- append("data_sample", class(obj))
  return(obj)
}

#'@title training and test partition object
#'@description training and test partition object
#'@param obj object
#'@param data dataset
#'@param ... optional arguments.
#'@return train and test sets
#'@examples trans <- dal_transform()
#'@export
train_test <- function(obj, data, ...) {
  UseMethod("train_test")
}

#'@export
train_test.default <- function(obj, data, ...) {
  return(list())
}

#'@title k-fold sampling
#'@description k-fold sampling
#'@param obj object
#'@param data dataset
#'@param k number of folds
#'@return k folds
#'@examples trans <- dal_transform()
#'@export
k_fold <- function(obj, data, k) {
  UseMethod("k_fold")
}

#'@export
k_fold.default <- function(obj, data, k) {
  return(list())
}


#'@title Sample Random
#'@description The sample_random function in R is used to generate a random sample of specified size from a given data set.
#'@return obj
#'@examples trans <- dal_transform()
#'@export
sample_random <- function() {
  obj <- data_sample()
  class(obj) <- append("sample_random", class(obj))
  return(obj)
}

#'@export
train_test.sample_random <- function(obj, data, perc=0.8, ...) {
  idx <- base::sample(1:nrow(data),as.integer(perc*nrow(data)))
  train <- data[idx,]
  test <- data[-idx,]
  return (list(train=train, test=test))
}

#'@export
k_fold.sample_random <- function(obj, data, k) {
  folds <- list()
  samp <- list()
  p <- 1.0 / k
  while (k > 1) {
    samp <- train_test.sample_random(obj, data, p)
    data <- samp$test
    folds <- append(folds, list(samp$train))
    k = k - 1
    p = 1.0 / k
  }
  folds <- append(folds, list(samp$test))
  return (folds)
}

#'@title k-fold training and test partition object
#'@description k-fold training and test partition object
#'@param folds data partitioned into folds
#'@param k k-fold for test set, all reminder for training set
#'@return train and test folds
#'@examples trans <- dal_transform()
#'@export
train_test_from_folds <- function(folds, k) {
  test <- folds[[k]]
  train <- NULL
  for (i in 1:length(folds)) {
    if (i != k)
      train <- rbind(train, folds[[i]])
  }
  return (list(train=train, test=test))
}
