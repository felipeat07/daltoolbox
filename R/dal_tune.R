#'@title DAL Tune
#'@description Ancestor class for hyper parameter optimization
#'@param base_model base model for tuning
#'@param folds number of folds for cross-validation
#'@return a `dal_tune` object.
#'@examples
#'#See ?cla_tune for classification tuning
#'#See ?reg_tune for regression tuning
#'#See ?ts_tune for time series tuning
#'@export
dal_tune <- function(base_model, folds=10) {
  obj <- dal_base()
  obj$base_model <- base_model
  obj$folds <- folds
  class(obj) <- append("dal_tune", class(obj))
  return(obj)
}

#'@title Selection hyper parameters
#'@description Selection hyper parameters from a k-fold cross-validation execution
#'@param obj object
#'@param hyperparameters data set with hyper parameters and quality measure from execution
#'@return index of selected hyper parameter
#'@examples trans <- dal_transform()
#'@export
select_hyper <- function(obj, hyperparameters) {
  UseMethod("select_hyper")
}

#'@export
select_hyper.default <- function(obj, hyperparameters) {
  return(length(hyperparameters))
}

