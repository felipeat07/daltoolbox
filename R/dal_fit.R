# DAL Library
# version 2.1

# depends dal_transform.R

### Fit
#'@title Fit
#'@description In R, the fit function is a generic function used to fit a model to a data set. It is a fundamental function in many machine learning algorithms and statistical models. The fit function takes a data set and a model specification as input and returns an object containing the fitted model parameters and other relevant information
#'@details The exact arguments and behavior of the fit function will depend on the specific model being fit. However, the fit function typically takes a data set as its first argument, and one or more additional arguments specifying the model to be fit. The function then performs an optimization algorithm to find the model parameters that best fit the data.
#'
#'@return
#'@examples
#'@export
fit_curvature <- function() {
  obj <- dal_transform()
  obj$df <- 2
  obj$deriv <- 2
  class(obj) <- append("fit_curvature", class(obj))
  return(obj)
}

#' @title transform.fit_curvature
#' @description This function applies curvature smoothing to the input data to transform it.
#' @details The function fits a spline to the input data and computes the curvature of the spline, and then applies the transformation function defined in the obj argument to the curvature values. The function returns a data frame with the transformed data.
#'
#' @param obj An object of class "fit_curvature", which contains the transformation parameters.
#' @param y A numeric vector of the input data.
#'
#' @return A data frame with the transformed data.
#'
#' @examples
#'@export
transform.fit_curvature <- function(obj, y) {
  x <- 1:length(y)
  smodel = smooth.spline(x, y, df = obj$df)
  curvature = predict(smodel, x = x, deriv = obj$deriv)
  yfit = obj$func(curvature$y)
  xfit = match(yfit, curvature$y)
  y <- y[xfit]
  res <- data.frame(x=xfit, y=y, yfit = yfit)
  return(res)
}

#' @title plot.fit_curvature
#'
#' @description A function to plot a fit_curvature object
#'
#' @details This function takes a fit_curvature object as input and plots the corresponding data.
#' The points where the curvature is maximum or minimum are highlighted in red.
#'
#' @param obj A fit_curvature object
#' @param y A numeric vector of data points to be plotted
#' @param res A list containing the results of fitting the data using the fit_curvature object
#'
#' @return None
#'
#' @examples
#' @export
plot.fit_curvature <- function(obj, y, res) {
  x <- 1:length(y)
  plot(x, y, col=ifelse(x==res$x, "red", "black"))
}

#' @title fit_curvature_min
#' @description The function fit_curvature_min() fits a curvature function to a dataset, and returns a list object with the results. This function is similar to fit_curvature(), but it selects the minimum value of the curvature function as the optimal cut point.
#' @details The fit_curvature_min() function uses the fit_curvature() function to fit a curvature function to the dataset. It then selects the minimum value of the curvature function as the optimal cut point, and returns a list object with the following elements:
#'
#' - cutoff: the optimal cut point.
#' - curvature: the values of the curvature function for each possible cut point.
#' - data: the input dataset.
#' - func: the function used to select the optimal cut point (minimum value).
#'
#' @return A list object with the optimal cut point, the values of the curvature function, the input dataset, and the function used to select the optimal cut point (minimum value).
#'
#' @examples
#'
#'@export
fit_curvature_min <- function() {
  obj <- fit_curvature()
  obj$func <- min
  class(obj) <- append("fit_curvature_min", class(obj))
  return(obj)
}

#' @title fit_curvature_max
#' @description This function creates an object for fitting a curvature function that uses the maximum curvature value.
#' The resulting object can be used to transform a data set using the max curvature value.
#' @details The fit_curvature_max function is an extension of the fit_curvature function, which creates an object for fitting a curvature function based on a specified input function.
#' The fit_curvature_max function sets the input function as max, resulting in a curvature function that calculates the maximum curvature value.
#' The resulting object can be used to transform a data set by scaling the data according to the maximum curvature value, using the transform method.
#'
#' @return Returns an object of class fit_curvature_max, which inherits from the fit_curvature and dal_transform classes.
#' The object contains a list with the following elements:
#' \itemize{
#' \item func: The function used to calculate the curvature, which is set to max.
#' \item curvature: The maximum curvature value calculated during the fitting process.
#' }
#'
#' @examples
#' @export
fit_curvature_max <- function() {
  obj <- fit_curvature()
  obj$func <- max
  class(obj) <- append("fit_curvature_max", class(obj))
  return(obj)
}

