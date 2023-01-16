# DAL Library
# version 2.1

# depends dal_transform.R
# depends cla_classification.R

# loadlibrary("RSNNS")
# loadlibrary("Matrix")  

# majority
#'@title
#'@description
#'@details
#'
#'@param attribute
#'@param slevels
#'@return
#'@examples
#'@export
cla_majority <- function(attribute, slevels=NULL) {
  obj <- classification(attribute, slevels)
  
  class(obj) <- append("cla_majority", class(obj))    
  return(obj)
}

#'@export
fit.cla_majority <- function(obj, data) {
  data <- adjust.data.frame(data)
  data[,obj$attribute] <- adjust.factor(data[,obj$attribute], obj$ilevels, obj$slevels)
  obj <- fit.classification(obj, data)
  
  y <- decodeClassLabels(data[,obj$attribute])
  cols <- apply(y, 2, sum)
  col <- match(max(cols),cols)
  obj$model <- list(cols=cols, col=col)
  
  obj <- register_log(obj)  
  return(obj)
}

#'@export
predict.cla_majority <- function(obj, x) {
  rows <- nrow(x)
  cols <- length(obj$model$cols)
  prediction <- Matrix(rep.int(0, rows*cols), nrow=rows, ncol=cols)
  prediction[,obj$model$col] <- 1
  colnames(prediction) <- names(obj$model$cols)
  prediction <- as.matrix(prediction)
  return(prediction)
}
