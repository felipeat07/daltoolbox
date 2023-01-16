# DAL Library
# version 2.1

# depends dal_sample.R

# sample_stratified
# loadlibrary("caret") for stratified
#'@title
#'@description
#'@details
#'
#'@param attribute
#'@return
#'@examples
#'@export
sample_stratified <- function(attribute) {
  obj <- sample_random()
  obj$attribute <- attribute
  class(obj) <- append("sample_stratified", class(obj))  
  return(obj)
}

#'@export
train_test.sample_stratified <- function(obj, data, perc=0.8) {
  predictors_name <- setdiff(colnames(data), obj$attribute)
  predictand <- data[,obj$attribute] 
  
  idx <- createDataPartition(predictand, p=perc, list=FALSE) 
  train <- data[idx,]
  test <- data[-idx,]
  return (list(train=train, test=test))
}

#'@export
k_fold.sample_stratified <- function(obj, data, k) {
  folds <- list()
  samp <- list()
  p <- 1.0 / k
  while (k > 1) {
    samp <- train_test.sample_stratified(obj, data, p)
    data <- samp$test
    folds <- append(folds, list(samp$train))
    k = k - 1
    p = 1.0 / k
  }
  folds <- append(folds, list(samp$test))
  return (folds)
}
