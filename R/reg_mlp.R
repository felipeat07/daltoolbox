#'@title Multi-Layer Perceptron (MLP) Regression
#'@description Multi-Layer Perceptron (MLP) Regression
#'@param attribute attribute target to model building
#'@param size number of neurons in hidden layers
#'@param decay decay learning rate
#'@param maxit number of maximum iterations for training
#'@return obj
#'@examples trans <- dal_transform()
#'@export
reg_mlp <- function(attribute, size=NULL, decay=0.05, maxit=1000) {
  obj <- regression(attribute)
  obj$maxit <- maxit
  obj$size <- size
  obj$decay <- decay
  class(obj) <- append("reg_mlp", class(obj))
  return(obj)
}

#'@export
fit.reg_mlp <- function(obj, data, ...) {
  data <- adjust_data.frame(data)
  obj <- fit.prediction(obj, data)

  if (is.null(obj$size))
    obj$size <- ceiling(ncol(data)/3)

  x <- data[,obj$x]
  y <- data[,obj$attribute]

  obj$model <- nnet(x = x, y = y, size = obj$size, decay = obj$decay, maxit=obj$maxit, linout=TRUE, trace = FALSE)

  return(obj)
}

#'@title predict data from input
#'@description predict data from input
#'@param object object
#'@param x input variable
#'@param ... optional arguments
#'@return predicted values
#'@export
predict.reg_mlp  <- function(object, x, ...) {
  x <- adjust_data.frame(x)
  x <- x[,object$x]
  prediction <- predict(object$model, x)
  return(prediction)
}
