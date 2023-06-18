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

