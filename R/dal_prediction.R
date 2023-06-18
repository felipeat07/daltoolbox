#'@title DAL Predict
#'@description Ancestor class for regression and classification
#'@return a prediction object
#'@examples trans <- prediction()
#'@export
prediction <- function() {
  obj <- dal_learner()
  class(obj) <- append("prediction", class(obj))
  return(obj)
}

#'@title fit predict
#'@description fit abstract method for predict
#'@param obj object
#'@param data dataset
#'@param ... optional arguments
#'@return obj
#'@export
fit.prediction <- function(obj, data, ...) {
  if (obj$reproduce)
    set.seed(1)
  obj$x <- setdiff(colnames(data), obj$attribute)
  return(obj)
}

#'@title Action implementation for prediction
#'@description A default function that defines the action to proxy predict method
#'@param obj object
#'@param ... optional arguments
#'@return It simply returns NULL, which indicates that no transforms are applied
#'@examples trans <- dal_transform()
#'@export
action.prediction <- function(obj, ...) {
  thiscall <- match.call(expand.dots = TRUE)
  thiscall[[1]] <- as.name("predict")
  result <- eval.parent(thiscall)
  return(result)
}

