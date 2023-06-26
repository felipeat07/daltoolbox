#'@title Time Series Support Vector Machine
#'@description Transform data into vectors of features. The algorithm then
#' identifies the support vectors that define the hyperplane that best separates
#' the data into different classes based on temporal proximity. The hyperplane
#' can then be used to make predictions about future values of the time series.
#'@param preprocess normalization
#'@param input_size input size for machine learning model
#'@param kernel SVM kernel (linear, radial, polynomial, sigmoid)
#'@param epsilon error threshold
#'@param cost cost
#'@return a `ts_svm` object.
#'@examples trans <- dal_transform()
#'@export
ts_svm <- function(preprocess=NA, input_size=NA, kernel="radial", epsilon=0, cost=10) {
  obj <- ts_regsw(preprocess, input_size)

  obj$kernel <- kernel #c("radial", "poly", "linear", "sigmoid")
  obj$epsilon <- epsilon #seq(0, 1, 0.1)
  obj$cost <- cost #=seq(10, 100, 10)

  class(obj) <- append("ts_svm", class(obj))
  return(obj)
}

#'@import e1071
#'@export
do_fit.ts_svm <- function(obj, x, y) {
  obj$model <- e1071::svm(x = as.data.frame(x), y = y, epsilon=obj$epsilon, cost=obj$cost, kernel=obj$kernel)
  return(obj)
}

#'@importFrom stats predict
#'@export
do_predict.ts_svm <- function(obj, x) {
  prediction <- stats::predict(obj$model, as.data.frame(x))
  return(prediction)
}
