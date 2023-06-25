#'@export
select_hyper <- function(obj, hyperparameters) {
  UseMethod("select_hyper")
}

select_hyper.default <- function(obj, hyperparameters) {
  return(1)
}

