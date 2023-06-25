#'@title K Nearest Neighbor Classification
#'@description Classifies using the K-Nearest Neighbor algorithm.
#'@param attribute attribute target to model building.
#'@param slevels Possible values for the target classification.
#'@param k A vector of integers indicating the number of neighbors to be considered.
#'@return A knn object.
#'@examples
#'data(iris)
#'slevels <- levels(iris$Species)
#'model <- cla_knn("Species", slevels, k=3)
#'
#'# preparing dataset for random sampling
#'set.seed(1)
#'sr <- sample_random()
#'sr <- train_test(sr, iris)
#'iris_train <- sr$train
#'iris_test <- sr$test
#'
#'model <- fit(model, iris_train)
#'
#'prediction <- predict(model, iris_test)
#'predictand <- adjust_class_label(iris_test[,"Species"])
#'train_eval <- evaluate(model, predictand, prediction)
#'train_eval$metrics
#'@export
cla_knn <- function(attribute, slevels, k=1) {
  obj <- classification(attribute, slevels)
  obj$k <- k
  class(obj) <- append("cla_knn", class(obj))
  return(obj)
}

#'@import class
#'@export
fit.cla_knn <- function(obj, data, ...) {

  data <- adjust_data.frame(data)
  data[,obj$attribute] <- adjust_factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- fit.predictor(obj, data)

  x <- data[,obj$x, drop = FALSE]
  y <- data[,obj$attribute]

  obj$model <-list(x=x, y=y, k=obj$k)

  return(obj)
}

#'@import class
#'@export
predict.cla_knn  <- function(object, x, ...) {
  x <- adjust_data.frame(x)
  x <- x[,object$x, drop=FALSE]

  prediction <- class::knn(train=object$model$x, test=x, cl=object$model$y, prob=TRUE)
  prediction <- adjust_class_label(prediction)

  return(prediction)
}


