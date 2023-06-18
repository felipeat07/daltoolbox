#'@title DAL Transform
#'@description A transformation function can be applied to a time series dataset to alter its properties.
#'@return a dal_transform object
#'@examples trans <- dal_transform()
#'@export
dal_learner <- function() {
  obj <- dal_base()
  class(obj) <- append("dal_learner", class(obj))
  return(obj)
}

#'@title Default Action implementation
#'@description A default function that defines the default behavior of the transform function for objects of class dal_transform
#'@param obj object
#'@param ... optional arguments
#'@return It simply returns NULL, which indicates that no transforms are applied
#'@examples trans <- dal_transform()
#'@export
action.dal_learner <- function(obj, ...) {
  thiscall <- match.call(expand.dots = TRUE)
  thiscall[[1]] <- as.name("predict")
  result <- eval.parent(thiscall)
  return(result)
}

#'@title evaluate
#'@description evaluate
#'@param obj object
#'@param ... optional arguments
#'@return evaluation
#'@examples trans <- dal_transform()
#'@export
evaluate <- function(obj, ...) {
  UseMethod("evaluate")
}

#'@title evaluate
#'@description evaluate
#'@param obj object
#'@param ... optional arguments
#'@return evaluation
#'@examples trans <- dal_transform()
#'@export
evaluate.default <- function(obj, ...) {
  return(NULL)
}

