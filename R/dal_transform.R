#'@title DAL Transform
#'@description A transformation method applied to a dataset.
#' Fit can be called before transform, if needed.
#'@return a dal_transform object
#'@examples
#'data(iris)
#'#An example of dal_transform is minmax()
#'trans <- minmax()
#'trans <- fit(trans, iris)
#'tiris <- transform(trans, iris)
#'@export
dal_transform <- function() {
  obj <- dal_base()
  class(obj) <- append("dal_transform", class(obj))
  return(obj)
}

#'@title Transform
#'@description Defines a transformation method.
#'@param obj a dal_transform object.
#'@param ... optional arguments.
#'@return transformed data.
#'@examples
#'data(iris)
#'#An example of dal_transform is minmax()
#'trans <- minmax()
#'trans <- fit(trans, iris)
#'tiris <- transform(trans, iris)
#'@export
transform <- function(obj, ...) {
  UseMethod("transform")
}

#'@title Transform
#'@description Executes the default transformation of an object.
#'@param obj a dal_transform object.
#'@param ... optional arguments
#'@return It simply returns NULL, which indicates that no transformation
#'@export
transform.default <- function(obj, ...) {
  return(NULL)
}


#'@title Action implementation for transform
#'@description A default function that defines the action to proxy transform method
#'@param obj object
#'@param ... optional arguments
#'@return Transformed data
#'@examples
#'data(iris)
#'# an example is minmax normalization
#'trans <- minmax()
#'trans <- fit(trans, iris)
#'tiris <- action(trans, iris)
#'@export
action.dal_transform <- function(obj, ...) {
  thiscall <- match.call(expand.dots = TRUE)
  thiscall[[1]] <- as.name("transform")
  result <- eval.parent(thiscall)
  return(result)
}


#'@title Inverse Transform
#'@description Reverses the transformation applied to data.
#'@param obj a dal_transform object.
#'@param ... optional arguments.
#'@return dataset inverse transformed.
#'@examples
#'data(iris)
#'#An example of dal_transform is minmax()
#'trans <- minmax()
#'trans <- fit(trans, iris)
#'tiris <- transform(trans, iris)
#'itiris <- inverse_transform(trans, tiris)
#'@export
inverse_transform <- function(obj, ...) {
  UseMethod("inverse_transform")
}

#'@title Default inverse transform
#'@description Reverses the transformation applied to data.
#'@param obj a dal_transform object.
#'@param ... optional arguments.
#'@return It simply returns NULL, which indicates that no transformation
#'@export
inverse_transform.default <- function(obj, ...) {
  return(NULL)
}
