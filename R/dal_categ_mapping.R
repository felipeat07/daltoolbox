#'@title Categorical Mapping
#'@description The CategoricalMapping class in R provides a way to map the levels of a categorical variable to new values. It is often used to recode or reclassify categorical variables in data preprocessing and data analysis tasks.
#'@param attribute attribute target to model building.
#'@return An instance of the CategoricalMapping class.
#'@examples trans <- dal_transform()
#'@export
categ_mapping <- function(attribute) {
  obj <- dal_transform()
  obj$attribute <- attribute
  class(obj) <- append("categ_mapping", class(obj))
  return(obj)
}

#'@export
#'@importFrom stats formula
#'@importFrom stats model.matrix
transform.categ_mapping <- function(obj, data, ...) {
  mdlattribute <- stats::formula(paste("~", paste(obj$attribute, "-1")))
  data <- as.data.frame(stats::model.matrix(mdlattribute, data=data))
  data[,obj$attribute] <- NULL
  return(data)
}

