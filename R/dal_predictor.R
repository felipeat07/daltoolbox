#'@title DAL Predict
#'@description Ancestor class for regression and classification
#'It provides basis for fit and predict methods.
#'Besides, action method proxies to predict.
#'
#' An example of learner is a decision tree (cla_dtree)
#'@return a predictor object
#'@examples
#'data(iris)
#'slevels <- levels(iris$Species)

#classification learner using decision tree
#'model <- cla_dtree("Species", slevels)
#'model <- fit(model, iris)
#'prediction <- predict(model, iris)

# categorical mapping for predictand
#'predictand <- adjust_class_label(iris[,"Species"])
#'train_eval <- evaluate(model, predictand, prediction)
#'train_eval$metrics
#'@export
predictor <- function() {
  obj <- dal_learner()
  class(obj) <- append("predictor", class(obj))
  return(obj)
}

#'@export
fit.predictor <- function(obj, data, ...) {
  if (obj$reproduce)
    set.seed(1)
  obj$x <- setdiff(colnames(data), obj$attribute)
  return(obj)
}

#'@export
action.predictor <- function(obj, ...) {
  thiscall <- match.call(expand.dots = TRUE)
  thiscall[[1]] <- as.name("predict")
  result <- eval.parent(thiscall)
  return(result)
}

