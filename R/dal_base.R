#'@title dal_base object
#'@description Creates a base object that will be used by other library functions
#'@return An empty object
#'@examples trans <- dal_base()
#'@export
dal_base <- function() {
  obj <- list()
  obj$reproduce <- FALSE
  attr(obj, "class") <- "dal_base"
  return(obj)
}

#'@title Action
#'@description Defines the kind of transformation to be set over a time series.
#'@param obj object: a dal_transform object to apply the transformation to.
#'@param ... optional arguments.
#'@return the transformed time series data.
#'@examples trans <- dal_transform()
#'@export
action <- function(obj, ...) {
  UseMethod("action")
}

#'@title Default Action implementation
#'@description A default function that defines the default behavior of the transform function for objects of class dal_transform
#'@param obj object
#'@param ... optional arguments
#'@return It simply returns NULL, which indicates that no transforms are applied
#'@examples trans <- dal_transform()
#'@export
action.default <- function(obj, ...) {
  return(NULL)
}

#'@title Fit
#'@description Models a time series dataset by estimating the underlying trend
#' and seasonality components. Used to make predictions and forecast future
#' values of the time series based on the historical data.
#'@param obj object
#'@param ... optional arguments.
#'@return obj
#'@examples trans <- dal_transform()
#'@export
fit <- function(obj, ...) {
  UseMethod("fit")
}

#'@title fit default implementation
#'@description fit efault implementation
#'@param obj object
#'@param ... optional arguments
#'@return obj
#'@examples trans <- dal_transform()
#'@export
fit.default <- function(obj, ...) {
  return(obj)
}

#'@title Update an object's parameters
#'@description The function receives the obj and params variables as parameters
#'@param obj object
#'@param params parameters
#'@examples trans <- dal_transform()
#'@export
set_params <- function(obj, params) {
  UseMethod("set_params")
}

#'@title Defines a default method for the set_params function
#'@description This function receives the obj and params variables as parameters.
#'@param obj object
#'@param params parameters
#'
#'@return The obj object
#'@export
set_params.default <- function(obj, params) {
  return(obj)
}

#'@title Set parameters values for dal_base
#'@description It receives as input a dal_base object (obj) and a set of parameters (params)
#'@param obj object
#'@param params parameters
#'@return The dal_base object updated with the new parameter values
#'@export
set_params.dal_base <- function(obj, params) {
  if (!is.null(params)) {
    params <- as.list(params)
    nobj <- names(obj)
    nobj <- nobj[nobj != ""]
    nparams <- names(params)
    nparams <- nparams[nparams != ""]

    for (i in 1:length(nparams)) {
      j <- which(nparams[i] == nobj)
      if (length(j)> 0)
        obj[[nobj[j]]] <- params[[nparams[i]]]
    }
  }
  return(obj)
}



# general functions
#'@title dal_base object
#'@description  It takes a data object as an argument
#'@param data dataset
#'@return An array corresponding to the object passed as a parameter, if it is not an array. If the object is already an array, the function simply returns it
#'@examples trans <- dal_transform()
#'@export
adjust_matrix <- function(data) {
  if(!is.matrix(data)) {
    return(as.matrix(data))
  }
  else
    return(data)
}

#'@title dal_base object
#'@description It takes as parameter an obj object
#'@param data dataset
#'@return The date argument
#'@examples trans <- dal_transform()
#'@export
adjust_data.frame <- function(data) {
  if(!is.data.frame(data)) {
    return(as.data.frame(data))
  }
  else
    return(data)
}


