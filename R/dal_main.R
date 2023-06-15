#'@title dal_base object
#'@description Creates a base object that will be used by other library functions
#'@return An empty object
#'@examples trans <- dal_transform()
#'@export
dal_base <- function() {
  obj <- list()
  obj$log <- FALSE
  obj$debug <- FALSE
  obj$reproduce <- FALSE
  attr(obj, "class") <- "dal_base"
  return(obj)
}

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
#'@examples trans <- dal_transform()
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



#'@title select hyper parameters
#'@description select hyper parameters
#'@param obj object
#'@param hyperparameters data set with hyper parameters.
#'@return index of hyperparameter
#'@examples trans <- dal_transform()
#'@export
select_hyper <- function(obj, hyperparameters) {
  UseMethod("select_hyper")
}

select_hyper.default <- function(obj, hyperparameters) {
  return(1)
}

#'@title Describe object status
#'@description Generates a summary object status
#'@param obj object
#'@return description of object
#'@examples trans <- dal_transform()
#'@export
describe <- function(obj) {
  UseMethod("describe")
}

#'@title dal_base object
#'@description Generates a summary object status
#'@param obj object
#'@return description of object
#'@examples trans <- dal_transform()
#'@export
describe.default <- function(obj) {
  if (is.null(obj))
    return("")
  else
    return(as.character(class(obj)[1]))
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


### Basic log functions

#start_log
#'@title Start logging on an obj object
#'@description It takes as parameter an obj object
#'@param obj object
#'@return obj
#'@examples trans <- dal_transform()
#'@export
start_log <- function(obj) {
  UseMethod("start_log")
}

#'@title dal_base object
#'@description It takes as parameter the obj object
#'@param obj object
#'@return The object with a new attribute "log_time",
#'@examples trans <- dal_transform()
#'@export
start_log.default <- function(obj) {
  obj$log_time <- Sys.time()
  return(obj)
}

#register_log
#'@title Register log details
#'@description It takes as parameters the variables obj, msg and ref. It serves to direct the appropriate method call, depending on the type of object passed as the obj argument.
#'@param obj object
#'@param msg string: a message to the log.
#'@param ref .
#'@return obj
#'@examples trans <- dal_transform()
#'@export
register_log <- function(obj, msg, ref) {
  UseMethod("register_log")
}

#'@title dal_base object
#'@description It takes as parameters the obj object and the variable msg. This function logs a log message to an object and returns the updated object itself
#'@param obj object
#'@param msg message to debug
#'@param ref reference
#'@return The obj object updated with log record information
#'@examples trans <- dal_transform()
#'@export
register_log.default <- function(obj, msg = "", ref = "") {
  obj$log_time <- as.numeric(difftime(Sys.time(), obj$log_time, units = "min"))
  if (msg == "")
    msg <- describe(obj)
  obj$log_msg <- sprintf("%s,%.3f", msg, obj$log_time)
  message(obj$log_msg)
  return(obj)
}
