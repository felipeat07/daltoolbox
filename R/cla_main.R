#'@title classification class
#'@description Ancestor class for classification problems
#'@param attribute attribute target to model building
#'@param slevels - possible values for the target classification
#'@return classification object
#'@examples trans <- dal_transform()
#'data(iris)
#'template_model <- classification("Species", levels(iris$Species))
#'print(template_model)
#'@export
classification <- function(attribute, slevels) {
  obj <- dal_transform()
  class(obj) <- append("classification", class(obj))
  obj$attribute <- attribute
  obj$slevels <- slevels
  obj$ilevels <- 1:length(slevels)
  return(obj)
}

#'@title adjust categorical values
#'@description adjust categorical values
#'@param value vector to be categorized
#'@param ilevels - order for categorical values
#'@param slevels - labels for categorical values
#'@return factor
#'@examples trans <- dal_transform()
#'@export
adjust.factor <- function(value, ilevels, slevels) {
  if (!is.factor(value)) {
    if (is.numeric(value))
      value <- factor(value, levels=ilevels)
    levels(value) <- slevels
  }
  return(value)
}

#'@title fit classification
#'@description fit abstract method for classification
#'@param obj object
#'@param data dataset
#'@param ... optional arguments
#'@return obj
#'@export
fit.classification <- function(obj, data, ...) {
  obj <- start_log(obj)
  if (obj$reproduce)
    set.seed(1)
  obj$x <- setdiff(colnames(data), obj$attribute)
  return(obj)
}

#'@title compute categorical mapping
#'@description compute categorical mapping
#'@param x vector to be categorized
#'@param valTrue - value to represent true
#'@param valFalse - value to represent false
#'@return factor
#'@examples trans <- dal_transform()
#'@export
adjustClassLabels <- function (x, valTrue = 1, valFalse = 0)
{
  n <- length(x)
  x <- as.factor(x)
  res <- matrix(valFalse, n, length(levels(x)))
  res[(1:n) + n * (unclass(x) - 1)] <- valTrue
  dimnames(res) <- list(names(x), levels(x))
  res
}

#'@title Classification evaluation
#'@description Evaluate major classification metrics for trained model
#'@param obj object
#'@param data real observations
#'@param prediction predicted observations
#'@param ... optional arguments.
#'@return Computed metrics
#'@examples trans <- dal_transform()
#'@import MLmetrics nnet
#'@export
evaluate.classification <- function(obj, data, prediction, ...) {
  result <- list(data=data, prediction=prediction)

  adjust_predictions <- function(predictions) {
    predictions_i <- matrix(rep.int(0, nrow(predictions)*ncol(predictions)), nrow=nrow(predictions), ncol=ncol(predictions))
    y <- apply(predictions, 1, nnet::which.is.max)
    for(i in unique(y)) {
      predictions_i[y==i,i] <- 1
    }
    return(predictions_i)
  }
  predictions <- adjust_predictions(result$prediction)
  result$conf_mat <- MLmetrics::ConfusionMatrix(data, predictions)
  result$accuracy <- MLmetrics::Accuracy(y_pred = predictions, y_true = data)
  result$f1 <- MLmetrics::F1_Score(y_pred = predictions, y_true = data, positive = 1)
  result$sensitivity <- MLmetrics::Sensitivity(y_pred = predictions, y_true = data, positive = 1)
  result$specificity <- MLmetrics::Specificity(y_pred = predictions, y_true = data, positive = 1)
  result$precision <- MLmetrics::Precision(y_pred = predictions, y_true = data, positive = 1)
  result$recall <- MLmetrics::Recall(y_pred = predictions, y_true = data, positive = 1)
  result$metrics <- data.frame(accuracy=result$accuracy, f1=result$f1,
    sensitivity=result$sensitivity, specificity=result$specificity,
    precision=result$precision, recall=result$recall)

  return(result)
}

