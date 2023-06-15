#'@title Create an object of class "tsreg" which is a subclass of object "dal_transform
#'@description The function sets some object attributes, such as log, debug and reproduce, to TRUE or FALSE. Then it adds the "tsreg" class to the object and returns the created object
#'@return An object of the class "tsreg"
#'@examples trans <- dal_transform()
#'@export
tsreg <- function() {
  obj <- dal_transform()
  class(obj) <- append("tsreg", class(obj))
  return(obj)
}

#'@title predict data from input
#'@description predict data from input
#'@param object object
#'@param x input variable
#'@param ... optional arguments
#'@return predicted values
#'@examples trans <- dal_transform()
#'@export
predict.tsreg <- function(object, x, ...) {
  return(x[,ncol(x)])
}

#'@title Delegate the task of adjusting the model to the specific methods of each class that implements it.
#'@description This function receives the obj, x and y variables as parameters
#'@param obj object
#'@param x input variable
#'@param y output variable
#'@return fitted object
#'@examples trans <- dal_transform()
#'@export
do_fit <- function(obj, x, y = NULL) {
  UseMethod("do_fit")
}

#'@title Define a generic method and delegate the implementation
#'@description This function defines the "do_predict" method for a generic object "obj" and a dataset "x"
#'@param obj object
#'@param x input variable
#'@return predicted values
#'@examples trans <- dal_transform()
#'@export
do_predict <- function(obj, x) {
  UseMethod("do_predict")
}

#'@title Create an object of class "tsreg_sw", which is an extension of class "tsreg"
#'@description Preprocessing parameters and input size are user specified
#'@param preprocess normalization
#'@param input_size input size for machine learning model
#'@return A tsreg object
#'@examples trans <- dal_transform()
#'@export
tsreg_sw <- function(preprocess=NA, input_size=NA) {
  obj <- tsreg()

  obj$preprocess <- preprocess
  obj$input_size <- input_size

  class(obj) <- append("tsreg_sw", class(obj))
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
#'@description It takes as input an obj object of the tsreg_sw class, which contains the model settings as well as the x and y data to fit the model
#'@param obj object
#'@param x input variable
#'@param y output variable
#'@param ... optional arguments
#'@return The updated obj object
#'@examples trans <- dal_transform()
#'@export
fit.tsreg_sw <- function(obj, x, y, ...) {
  obj <- start_log(obj)
  if (obj$reproduce)
    set.seed(1)

  obj$preprocess <- fit(obj$preprocess, x)

  x <- transform(obj$preprocess, x)

  y <- transform(obj$preprocess, x, y)

  obj <- do_fit(obj, ts_as_matrix(x, obj$input_size), y)

  if (obj$log)
    obj <- register_log(obj)
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
predict.tsreg_sw <- function(object, x, steps_ahead=1, ...) {
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
      x <- adjust.ts_data(inverse_transform(object$preprocess, x))
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
do_predict.tsreg_sw <- function(obj, x) {
  prediction <- stats::predict(obj$model, x)
  return(prediction)
}

#'@title Calculate the mean squared error (MSE) between actual values and forecasts of a time series
#'@description The function receives two variables as a parameter, which are actual and prediction
#'@param actual real observations
#'@param prediction predicted observations
#'@return A number, which is the calculated MSE
#'@export
MSE.tsreg <- function (actual, prediction) {
  if (length(actual) != length(prediction))
    stop("actual and prediction have different lengths")
  n <- length(actual)
  res <- mean((actual - prediction)^2)
  res
}

#'@title Calculate the symmetric mean absolute percent error (sMAPE)
#'@description The function receives two variables as a parameter, which are actual and prediction
#'@param actual real observations
#'@param prediction predicted observations
#'@return The sMAPE between the actual and prediction vectors
#'@export
sMAPE.tsreg <- function (actual, prediction) {
  if (length(actual) != length(prediction))
    stop("actual and prediction have different lengths")
  n <- length(actual)
  res <- (1/n) * sum(abs(actual - prediction)/((abs(actual) +
                                                  abs(prediction))/2))
  res
}

#'@title Calculate the Mean Squared Error (MSE) error metric and the Symmetric Mean Absolute Percentage Error (sMAPE) error metric
#'@description The function receives two variables as a parameter, which are values and prediction
#'@param obj object
#'@param values real observations
#'@param prediction predicted observations
#'@param ... optional arguments.
#'@return An object that contains these metrics and their values, stored in a data frame
#'@export
evaluate.tsreg <- function(obj, values, prediction, ...) {
  result <- list(values=values, prediction=prediction)

  result$smape <- sMAPE.tsreg(values, prediction)
  result$mse <- MSE.tsreg(values, prediction)

  result$metrics <- data.frame(mse=result$mse, smape=result$smape)

  return(result)
}

#'@title Plot a time series chart
#'@description The function receives six variables as a parameter, which are obj and y, yadj, main and xlabels. The graph is plotted with 3 lines: the original series (in black), the adjusted series (in blue) and the predicted series (in green)
#'@param x input variable
#'@param y output variable
#'@param color color for time series
#'@param label_x x-axis label
#'@param label_y y-axis label
#'@export
#'@import ggplot2
ts_plot <- function(x = NULL, y, color="black", label_x = "", label_y = "")  {
  y <- as.vector(y)
  if (is.null(x))
    x <- 1:length(y)
  grf <- ggplot() + geom_point(aes(x = x, y = y)) + geom_line(aes(x = x, y = y))
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
  grf <- grf + theme(legend.title = element_blank()) + theme(legend.position = "bottom") + theme(legend.key = element_blank())
  return(grf)
}

#'@title Plot a time series chart
#'@description The function receives six variables as a parameter, which are obj and y, yadj, main and xlabels. The graph is plotted with 3 lines: the original series (in black), the adjusted series (in blue) and the predicted series (in green)
#'@param x time index
#'@param y time series
#'@param yadj  adjustment of time series
#'@param ypred prediction of the time series
#'@param color color for the time series
#'@param label_x x-axis title
#'@param label_y y-axis title
#'@export
#'@import ggplot2
ts_plot_pred <- function(x = NULL, y, yadj, ypred = NULL, color="black", label_x = "", label_y = "") {
  y <- as.vector(y)
  if (is.null(x))
    x <- 1:length(y)
  y <- as.vector(y)
  yadj <- as.vector(yadj)
  ntrain <- length(yadj)
  yhat <- yadj
  ntest <- 0
  if (!is.null(ypred)) {
    ypred <- as.vector(ypred)
    yhat <- c(yhat, ypred)
    ntest <- length(ypred)
  }

  grf <- ggplot() + geom_point(aes(x = x, y = y)) + geom_line(aes(x = x, y = y))
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
  grf <- grf + theme(legend.title = element_blank()) + theme(legend.position = "bottom") + theme(legend.key = element_blank())

  smape_train <- sMAPE.tsreg(y[1:ntrain], yadj)*100
  if (ntest > 0)
    smape_test <- sMAPE.tsreg(y[(ntrain+1):(ntrain+ntest)], ypred)*100

  grf <- grf + geom_line(aes(x = x[1:ntrain], y = yhat[1:ntrain]),
                         color = "blue", linetype = "dashed")
  if (!is.null(ypred))
    grf <- grf +geom_line(aes(x = x[ntrain:(ntrain+ntest)], y = yhat[ntrain:(ntrain+ntest)]),
                          color = "green", linetype = "dashed")
  return(grf)
}
