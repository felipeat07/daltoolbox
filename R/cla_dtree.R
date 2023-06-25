#'@title Majority Classification
#'@description This function creates a classification object that
#'uses the majority vote strategy to predict the target attribute.
#'Given a target attribute, the function counts the number of
#'occurrences of each value in the dataset and selects the one that appears most often.
#'@param attribute attribute target to model building.
#'@param slevels Possible values for the target classification.
#'@return Returns a classification object.
#'@examples
#'data(iris)
#'slevels <- levels(iris$Species)
#'model <- cla_dtree("Species", slevels)
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
cla_dtree <- function(attribute, slevels) {
  obj <- classification(attribute, slevels)

  class(obj) <- append("cla_dtree", class(obj))
  return(obj)
}

#'@import tree
#'@export
fit.cla_dtree <- function(obj, data, ...) {
  data <- adjust_data.frame(data)
  data[,obj$attribute] <- adjust_factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- fit.predictor(obj, data)

  regression <- formula(paste(obj$attribute, "  ~ ."))
  obj$model <- tree::tree(regression, data)


  return(obj)
}

#'@export
predict.cla_dtree <- function(object, x, ...) {
  x <- adjust_data.frame(x)
  x <- x[,object$x, drop=FALSE]

  prediction <- predict(object$model, x, type="vector")

  return(prediction)
}





