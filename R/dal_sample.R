#'@title Data Sample
#'@description The data_sample function in R is used to randomly
#' sample data from a given data frame. It can be used to obtain
#' a subset of data for further analysis or modeling.
#'
#' Two basic specializations of data_sample are sample_random and sample_stratified.
#' They provide random sampling and stratified sampling, respectively.
#'
#' Data sample provides both training and testing partitioning (train_test) and
#' k-fold partitioning (k_fold) of data.
#'@return obj
#'@examples
#'#using random sampling
#'sample <- sample_random()
#'tt <- train_test(sample, iris)
#'
#'# distribution of train
#'table(tt$train$Species)
#'
#'# preparing dataset into four folds
#'folds <- k_fold(sample, iris, 4)
#'
#'# distribution of folds
#'tbl <- NULL
#'for (f in folds) {
#'  tbl <- rbind(tbl, table(f$Species))
#'}
#'head(tbl)
#'@export
data_sample <- function() {
  obj <- dal_transform()
  class(obj) <- append("data_sample", class(obj))
  return(obj)
}

#'@title training and test
#'@description training and test partition of a dataset using a sampling method
#'@param obj object
#'@param data dataset
#'@param ... optional arguments.
#'@return train and test sets
#'@examples
#'#using random sampling
#'sample <- sample_random()
#'tt <- train_test(sample, iris)
#'
#'# distribution of train
#'table(tt$train$Species)
#'@export
train_test <- function(obj, data, ...) {
  UseMethod("train_test")
}

#'@export
train_test.default <- function(obj, data, ...) {
  return(list())
}

#'@title k-fold sampling
#'@description k-fold partition of a dataset using a sampling method
#'@param obj object
#'@param data dataset
#'@param k number of folds
#'@return k folds
#'@examples
#'#using random sampling
#'sample <- sample_random()
#'
#'# preparing dataset into four folds
#'folds <- k_fold(sample, iris, 4)
#'
#'# distribution of folds
#'tbl <- NULL
#'for (f in folds) {
#'  tbl <- rbind(tbl, table(f$Species))
#'}
#'head(tbl)
#'@export
k_fold <- function(obj, data, k) {
  UseMethod("k_fold")
}

#'@export
k_fold.default <- function(obj, data, k) {
  return(list())
}


