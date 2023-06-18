#'@title DAL Transform
#'@description A transformation function can be applied to a time series dataset to alter its properties.
#'@return a dal_transform object
#'@examples trans <- dal_transform()
#'@export
dal_transform <- function() {
  obj <- dal_base()
  class(obj) <- append("dal_transform", class(obj))
  return(obj)
}

#'@title Transform
#'@description Defines the kind of transformation to be set over a time series.
#'@param obj object: a dal_transform object to apply the transformation to.
#'@param ... optional arguments.
#'@return the transformed time series data.
#'@examples trans <- dal_transform()
#'@export
transform <- function(obj, ...) {
  UseMethod("transform")
}

#'@title dal_base object
#'@description A default function that defines the default behavior of the transform function for objects of class dal_transform
#'@param obj object
#'@param ... optional arguments
#'@return It simply returns NULL, which indicates that no transforms are applied
#'@examples trans <- dal_base()
#'@export
transform.default <- function(obj, ...) {
  return(NULL)
}

#inverse_transform
#'@title Inverse Transform
#'@description Reverses the transformation applied to a time series dataset using the transform() function.
#'@param obj object: The transformed time series dataset.
#'@param ... optional arguments.
#'@return The time series dataset in its original scale.
#'@examples trans <- dal_transform()
#'@export
inverse_transform <- function(obj, ...) {
  UseMethod("inverse_transform")
}

#'@title dal_base object
#'@description It receives as parameter the object obj, ...
#'@param obj object
#'@param ... optional arguments
#'@return Simply returns NULL
#'@examples trans <- dal_transform()
#'@export
inverse_transform.default <- function(obj, ...) {
  return(NULL)
}

