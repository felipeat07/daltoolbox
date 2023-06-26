#'@title Classification using Artificial Neural Network (ANN)
#'@description Performs classification using Artificial Neural Network (ANN) algorithm
#'@param attribute attribute target to model building
#'@param slevels possible values for the target classification
#'@param size number of nodes that will be used in the hidden layer
#'@param decay how quickly it decreases in gradient descent
#'@param maxit maximum iterations
#'@return a classification object
#'@examples
#'data(iris)
#'slevels <- levels(iris$Species)
#'model <- cla_mlp("Species", slevels, size=3, decay=0.03)
#'
#'# preparing dataset for random sampling
#'set.seed(1)
#'sr <- sample_random()
#'sr <- train_test(sr, iris)
#'train <- sr$train
#'test <- sr$test
#'
#'model <- fit(model, train)
#'
#'prediction <- predict(model, test)
#'predictand <- adjust_class_label(test[,"Species"])
#'test_eval <- evaluate(model, predictand, prediction)
#'test_eval$metrics
#'@export
cla_mlp <- function(attribute, slevels, size=NULL, decay=0.1, maxit=1000) {
  obj <- classification(attribute, slevels)
  obj$maxit <- maxit
  obj$size <- size
  obj$decay <- decay

  class(obj) <- append("cla_mlp", class(obj))
  return(obj)
}

#'@import nnet
#'@export
fit.cla_mlp <- function(obj, data, ...) {
  data <- adjust_data.frame(data)
  data[,obj$attribute] <- adjust_factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- fit.predictor(obj, data)

  if (is.null(obj$size))
    obj$size <- ceiling(sqrt(ncol(data)))

  x <- data[,obj$x, drop = FALSE]
  y <- data[,obj$attribute]

  obj$model <- nnet::nnet(x = x, y = adjust_class_label(y), size=obj$size, decay=obj$decay, maxit=obj$maxit, trace=FALSE)

  return(obj)
}

#'@export
predict.cla_mlp  <- function(object, x, ...) {
  x <- adjust_data.frame(x)
  x <- x[,object$x, drop = FALSE]

  prediction <- predict(object$model, x, type="raw")

  return(prediction)
}
