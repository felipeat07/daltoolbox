#'@title Create an object of class "ts_regsw", which is an extension of class "ts_reg"
#'@description Preprocessing parameters and input size are user specified
#'@param preprocess normalization
#'@param input_size input size for machine learning model
#'@return A ts_reg object
#'@examples trans <- dal_transform()
#'@export
ts_regsw <- function(preprocess=NA, input_size=NA) {
  obj <- ts_reg()

  obj$preprocess <- preprocess
  obj$input_size <- input_size

  class(obj) <- append("ts_regsw", class(obj))
  return(obj)
}

#'@title Prepare the data to feed a machine learning model
#'@description Takes a time series dataset data and an input size input_size
#'@param data dataset
#'@param input_size input size for machine learning model
#'@return An array of the latest input_size observations of each time series at date
#'@examples trans <- dal_transform()
#'@export
ts_as_matrix <- function(data, input_size) {
  result <- data[,(ncol(data)-input_size+1):ncol(data)]
  return(result)
}

#'@title Performs the adjustment (training) of a regression model in time series
#'@description It takes as input an obj object of the ts_regsw class, which contains the model settings as well as the x and y data to fit the model
#'@param obj object
#'@param x input variable
#'@param y output variable
#'@param ... optional arguments
#'@return The updated obj object
#'@examples trans <- dal_transform()
#'@export
fit.ts_regsw <- function(obj, x, y, ...) {
  if (obj$reproduce)
    set.seed(1)

  obj$preprocess <- fit(obj$preprocess, x)

  x <- transform(obj$preprocess, x)

  y <- transform(obj$preprocess, x, y)

  obj <- do_fit(obj, ts_as_matrix(x, obj$input_size), y)

  return(obj)
}

#'@title Performs prediction of values for a time series
#'@description The function receives three variables as a parameter, which are obj, x and steps_ahead
#'@param object object
#'@param x input variable
#'@param steps_ahead number of step ahead for prediction
#'@param ... optional arguments
#'@return A vector with forecasts for each time period
#'@examples trans <- dal_transform()
#'@export
predict.ts_regsw <- function(object, x, steps_ahead=1, ...) {
  if (steps_ahead == 1) {
    x <- transform(object$preprocess, x)
    data <- ts_as_matrix(x, object$input_size)
    y <- do_predict(object, data)
    y <- inverse_transform(object$preprocess, x, y)
    return(as.vector(y))
  }
  else {
    if (nrow(x) > 1)
      stop("In steps ahead, x should have a single row")
    prediction <- NULL
    cnames <- colnames(x)
    x <- x[1,]
    for (i in 1:steps_ahead) {
      colnames(x) <- cnames
      x <- transform(object$preprocess, x)
      y <- do_predict(object, ts_as_matrix(x, object$input_size))
      x <- adjust_ts_data(inverse_transform(object$preprocess, x))
      y <- inverse_transform(object$preprocess, x, y)
      for (j in 1:(ncol(x)-1)) {
        x[1, j] <- x[1, j+1]
      }
      x[1, ncol(x)] <- y
      prediction <- c(prediction, y)
    }
    return(as.vector(prediction))
  }
  return(prediction)
}

#'@title Performs the prediction step of the temporal regression model
#'@description It receives as input the obj object that contains the trained model and the input matrix x
#'@param obj object
#'@param x input variable
#'@return The forecast matrix
#'@export
#'@importFrom stats predict
do_predict.ts_regsw <- function(obj, x) {
  prediction <- stats::predict(obj$model, x)
  return(prediction)
}

