#'@title Time Series Global Min-Max
#'@description Rescales data, so the minimum value is mapped to 0 and the maximum value is mapped to 1.
#'@param remove_outliers logical: if TRUE (default) outliers will be removed.
#'@return a `ts_gminmax` object.
#'@examples trans <- dal_transform()
#'@export
ts_gminmax <- function(remove_outliers = TRUE) {
  obj <- dal_transform()
  obj$remove_outliers <- remove_outliers
  class(obj) <- append("ts_gminmax", class(obj))
  return(obj)
}

#'@export
fit.ts_gminmax <- function(obj, data, ...) {
  if (obj$remove_outliers) {
    out <- outliers()
    out <- fit(out, data)
    data <- transform(out, data)
  }

  obj$gmin <- min(data)
  obj$gmax <- max(data)

  return(obj)
}

#'@export
transform.ts_gminmax <- function(obj, data, x=NULL, ...) {
  if (!is.null(x)) {
    x <- (x-obj$gmin)/(obj$gmax-obj$gmin)
    return(x)
  }
  else {
    data <- (data-obj$gmin)/(obj$gmax-obj$gmin)
    return(data)
  }
}

#'@export
inverse_transform.ts_gminmax <- function(obj, data, x=NULL, ...) {
  if (!is.null(x)) {
    x <- x * (obj$gmax-obj$gmin) + obj$gmin
    return(x)
  }
  else {
    data <- data * (obj$gmax-obj$gmin) + obj$gmin
    return (data)
  }
}
