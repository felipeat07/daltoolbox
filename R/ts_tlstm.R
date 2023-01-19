# DAL Library
# version 2.1

# depends dal_transform.R
# depends ts_data.R
# depends ts_regression.R
# depends ts_preprocessing.R

# class ts_tlstm
# loadlibrary("reticulate")
# source_python('ts_tlstm.py')

#'@title
#'@description
#'@details
#'
#'@param preprocess
#'@param input_size
#'@param epochs
#'@return
#'@examples
#'@export
ts_tlstm <- function(preprocess = NA, input_size = NA, epochs = 10000L) {
  obj <- tsreg_sw(preprocess, input_size)
  obj$deep_debug <- FALSE
  obj$epochs <- epochs
  class(obj) <- append("ts_tlstm", class(obj))
  return(obj)
}

#'@export
describe.ts_tlstm <- function(obj) {
  if (!is.na(obj$input_size))
    return(sprintf("lstm(input_size=%d,epochs=%d)-%s", obj$input_size, obj$epochs, describe(obj$preprocess)))
  else
    return("lstm")
}

#'@export
set_params.ts_tlstm <- function(obj, params) {
  if (!is.null(params$epochs))
    obj$epochs <- as.integer(params$epochs)
  return(obj)
}

## TODO: verificar se é melhor abordagem
#'@export
do_fit.ts_tlstm <- function(obj, x, y) {
  if (is.null(obj$model))
    obj$model <- python_env$create_torch_lstm(obj$input_size, obj$input_size)  ## TODO: verificar uso python_env

  df_train <- as.data.frame(x)
  df_train$t0 <- as.vector(y)

  ## TODO: verificar uso python_env
  obj$model <- python_env$train_torch_lstm(obj$model, df_train, obj$epochs, 0.001, obj$deep_debug, obj$reproduce)

  return(obj)
}

#'@export
do_predict.ts_tlstm <- function(obj, x) {
  X_values <- as.data.frame(x)
  X_values$t0 <- 0
  prediction <- python_env$predict_torch_lstm(obj$model, X_values)
  prediction <- as.vector(prediction)
  return(prediction)
}
