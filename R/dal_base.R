#'@title Class dal_base
#'@description The dal_base class is an abstract class for all dal descendants classes. It provides both fit() and action() functions
#'@return A dal_base object
#'@examples trans <- dal_base()
#'@export
dal_base <- function() {
  obj <- list()
  obj$reproduce <- FALSE
  attr(obj, "class") <- "dal_base"
  return(obj)
}

#'@title Fit
#'@description Fits a model.
#'@param obj object
#'@param ... optional arguments.
#'@return obj
#'@examples
#'data(iris)
#'# an example is minmax normalization
#'trans <- minmax()
#'trans <- fit(trans, iris)
#'tiris <- action(trans, iris)
#'@export
fit <- function(obj, ...) {
  UseMethod("fit")
}

#'@title Fit
#'@description Fits a model from input data
#'@param obj object
#'@param ... optional arguments
#'@return The default behavior is return the object as it is
#'@export
fit.default <- function(obj, ...) {
  return(obj)
}

#'@title Action
#'@description Executes the action of model applied in provided data
#'@param obj object: a dal_base object to apply the transformation on the input dataset.
#'@param ... optional arguments.
#'@return The result of an action of the model applied in provided data
#'@examples
#'data(iris)
#'# an example is minmax normalization
#'trans <- minmax()
#'trans <- fit(trans, iris)
#'tiris <- action(trans, iris)
#'@export
action <- function(obj, ...) {
  UseMethod("action")
}

#'@title Action
#'@description Executes the action of an object.
#'@param obj object
#'@param ... optional arguments
#'@return The default behavior is return the input data
#'@export
action.default <- function(obj, ...) {
  par <- c(as.list(environment()), list(...))
  data <- NULL
  if (length(par) > 1)
    data <- par[[2]]
  return (data)
}

#'@title Assign parameters
#'@description set_params function assigns all parameters to the attributes presented in the object.
#'It returns the object with the parameters set.
#'@param obj object of class dal_base
#'@param params parameters to set obj
#'@return obj with parameters set
#'@examples
#'obj <- set_params(dal_base(), list(reproduce = TRUE))
#'@export
set_params <- function(obj, params) {
  UseMethod("set_params")
}

#'@title Assign parameters
#'@description This function receives the obj and params variables as parameters.
#' It returns the obj as it is.
#'@param obj object
#'@param params parameters
#'@return obj
#'@export
set_params.default <- function(obj, params) {
  return(obj)
}

#'@title Assign parameters
#'@description set_params function assigns all parameters to the attributes presented in the object.
#'It returns the object with the parameters set.
#'@param obj object of class dal_base
#'@param params parameters to set obj
#'@return obj with parameters set
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
      if (length(j)> 0) {
        val <- params[[nparams[i]]]
        if (is.factor(val))
          val <- as.character(val)
        obj[[nobj[j]]] <- val
      }
    }
  }
  return(obj)
}

#'@title  Adjust input data to matrix
#'@description adjust_matrix function receives
#' a data object and converts it to a matrix.
#'
#' It returns the same input if it is already a matrix.
#'@param data dataset
#'@examples
#'data(iris)
#'mat <- adjust_matrix(iris)
#'@export
adjust_matrix <- function(data) {
  if(!is.matrix(data)) {
    return(as.matrix(data))
  }
  else
    return(data)
}

#'@title  Adjust input data to data frame
#'@description adjust_data.frame function receives
#' a data object and converts it to a data frame.
#'
#' It returns the same input if it is already a data frame.
#'@param data dataset
#'@return The date argument
#'@examples
#'data(iris)
#'df <- adjust_data.frame(iris)
#'@export
adjust_data.frame <- function(data) {
  if(!is.data.frame(data)) {
    return(as.data.frame(data))
  }
  else
    return(data)
}



