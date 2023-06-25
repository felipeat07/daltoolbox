#'@title Majority Classification
#'@description This function creates a classification object that uses the majority vote strategy to predict the target attribute. Given a target attribute, the function counts the number of occurrences of each value in the dataset and selects the one that appears most often.
#'@param attribute attribute target to model building.
#'@param slevels Possible values for the target classification.
#'@return Returns a classification object.
#'@examples
#'data(iris)
#'slevels <- levels(iris$Species)
#'model <- cla_majority("Species", slevels)
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
cla_majority <- function(attribute, slevels) {
  obj <- classification(attribute, slevels)

  class(obj) <- append("cla_majority", class(obj))
  return(obj)
}

#'@export
fit.cla_majority <- function(obj, data, ...) {
  data <- adjust_data.frame(data)
  data[,obj$attribute] <- adjust_factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- fit.predictor(obj, data)

  y <- adjust_class_label(data[,obj$attribute])
  cols <- apply(y, 2, sum)
  col <- match(max(cols),cols)
  obj$model <- list(cols=cols, col=col)

  return(obj)
}

#'@export
predict.cla_majority <- function(object, x, ...) {
  rows <- nrow(x)
  cols <- length(object$model$cols)
  prediction <- matrix(rep.int(0, rows*cols), nrow=rows, ncol=cols)
  prediction[,object$model$col] <- 1
  colnames(prediction) <- names(object$model$cols)
  prediction <- as.matrix(prediction)
  return(prediction)
}


