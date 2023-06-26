#'@title Time Series AutoRegressive Integrated Moving Average (ARIMA)
#'@description ARIMA is a regression model for forecasting time series data
#'@return obj
#'@examples trans <- dal_transform()
#'@export
tsreg_arima <- function() {
  obj <- ts_reg()

  class(obj) <- append("tsreg_arima", class(obj))
  return(obj)
}

#'@import forecast
#'@export
fit.tsreg_arima <- function(obj, x, y = NULL, ...) {
  if (obj$reproduce)
    set.seed(1)

  obj$model <- forecast::auto.arima(x, allowdrift = TRUE, allowmean = TRUE)
  order <- obj$model$arma[c(1, 6, 2, 3, 7, 4, 5)]
  obj$p <- order[1]
  obj$d <- order[2]
  obj$q <- order[3]
  obj$drift <- (NCOL(obj$model$xreg) == 1) && is.element("drift", names(obj$model$coef))
  params <- list(p = obj$p, d = obj$d, q = obj$q, drift = obj$drift)
  attr(obj, "params") <- params

  return(obj)
}

#'@import forecast
#'@export
predict.tsreg_arima <- function(object, x, y = NULL, steps_ahead=NULL, ...) {
  if (!is.null(x) && (length(object$model$x) == length(x)) && (sum(object$model$x-x) == 0)){
    #get adjusted data
    pred <- object$model$x - object$model$residuals
  }
  else {
    if (is.null(steps_ahead))
      steps_ahead <- length(x)
    if ((steps_ahead == 1) && (length(x) != 1)) {
      pred <- NULL
      model <- object$model
      i <- 1
      y <- model$x
      while (i <= length(x)) {
        pred <- c(pred, forecast(model, h = 1)$mean)
        y <- c(y, x[i])

        model <- tryCatch(
          {
            forecast::Arima(y, order=c(object$p, object$d, object$q), include.drift = object$drift)
          },
          error = function(cond) {
            forecast::auto.arima(y, allowdrift = TRUE, allowmean = TRUE)
          }
        )
        i <- i + 1
      }
    }
    else {
      pred <- forecast::forecast(object$model, h = steps_ahead)$mean
    }
  }
  return(pred)
}
