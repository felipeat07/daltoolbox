#'@title DAL Predict
#'@description Ancestor class for regression and classification
#'@return a predictor object
#'@examples trans <- predictor()
#'@export
predictor <- function() {
  obj <- dal_learner()
  class(obj) <- append("predictor", class(obj))
  return(obj)
}

#'@title fit predict
#'@description fit abstract method for predict
#'@param obj object
#'@param data dataset
#'@param ... optional arguments
#'@return obj
#'@export
fit.predictor <- function(obj, data, ...) {
  if (obj$reproduce)
    set.seed(1)
  obj$x <- setdiff(colnames(data), obj$attribute)
  return(obj)
}

#'@title Action implementation for predictor
#'@description A default function that defines the action to proxy predict method
#'@param obj object
#'@param ... optional arguments
#'@return It simply returns NULL, which indicates that no transforms are applied
#'@examples trans <- dal_transform()
#'@export
action.predictor <- function(obj, ...) {
  thiscall <- match.call(expand.dots = TRUE)
  thiscall[[1]] <- as.name("predict")
  result <- eval.parent(thiscall)
  return(result)
}

