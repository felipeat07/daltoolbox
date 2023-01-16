# DAL Library
# version 2.1

# depends dal_transform.R
# depends ts_data.R

### ts_augment
#'@title
#'@description
#'@details
#'
#'@return
#'@examples
#'@export
ts_augment <- function() {
  obj <- ts_transform()
  obj$preserve_data <- TRUE
  class(obj) <- append("ts_augment", class(obj))    
  return(obj)
}

#'@export
fit.ts_augment <- function(obj, data) {
  return(obj)
}

### jitter
#'@title
#'@description
#'@details
#'
#'@return
#'@examples
#'@export
jitter <- function() {
  obj <- ts_augment()
  class(obj) <- append("jitter", class(obj))    
  return(obj)
}

#'@export
fit.jitter <- function(obj, data) {
  an <- apply(data, 1, mean) 
  x <- data - an 
  obj$sd <- sd(x)
  return(obj)
}

#'@export
transform.jitter <- function(obj, data) {
  add.jitter <- function(obj, data) {
    x <- rnorm(length(data), mean = 0, sd = obj$sd)
    x <- matrix(x, nrow=nrow(data), ncol=ncol(data))
    x[,ncol(data)] <- 0
    data <- data + x
    attr(data, "idx") <- 1:nrow(data) 
    return(data)
  }
  result <- add.jitter(obj, data)
  if (obj$preserve_data) {
    idx <- c(1:nrow(data), attr(result, "idx"))
    result <- rbind(data, result)
    result <- adjust.ts_data(result)
    attr(result, "idx") <- idx 
  }
  return(result)
}

#'@export
describe.jitter <- function(obj) {
  return("jitter")
}

### stretch
#'@title
#'@description
#'@details
#'
#'@param factor
#'@return
#'@examples
#'@export
stretch <- function(factor=1.2) {
  obj <- ts_augment()
  obj$factor <- factor
  class(obj) <- append("stretch", class(obj))    
  return(obj)
}

#'@export
transform.stretch <- function(obj, data) {
  add.stretch <- function(obj, data) {
    an <- apply(data, 1, mean) 
    x <- data - an 
    x <- x * obj$factor
    x[,ncol(data)] <- 0
    data <- data + x
    attr(data, "idx") <- 1:nrow(data) 
    return(data)
  }
  result <- add.stretch(obj, data)
  if (obj$preserve_data) {
    idx <- c(1:nrow(data), attr(result, "idx"))
    result <- rbind(data, result)
    result <- adjust.ts_data(result)
    attr(result, "idx") <- idx 
  }
  return(result)
}

#'@export
describe.stretch <- function(obj) {
  return(sprintf("stretch%.1f", obj$factor))
}

### shrink
#'@title
#'@description
#'@details
#'
#'@param factor
#'@return
#'@examples
#'@export
shrink <- function(factor = 0.8) {
  obj <- ts_augment()
  obj$factor <- factor
  class(obj) <- append("shrink", class(obj))    
  return(obj)
}

#'@export
transform.shrink <- function(obj, data) {
  add.shrink <- function(obj, data) {
    an <- apply(data, 1, mean) 
    x <- data - an 
    x <- x * obj$factor
    x[,ncol(data)] <- 0
    data <- data + x
    attr(data, "idx") <- 1:nrow(data) 
    return(data)
  }
  result <- add.shrink(obj, data)
  if (obj$preserve_data) {
    idx <- c(1:nrow(data), attr(result, "idx"))
    result <- rbind(data, result)
    result <- adjust.ts_data(result)
    attr(result, "idx") <- idx 
  }
  return(result)
}

#'@export
describe.shrink <- function(obj) {
  return(sprintf("shrink%.1f", obj$factor))
}

### flip
#'@title
#'@description
#'@details
#'
#'@return
#'@examples
#'@export
flip <- function() {
  obj <- ts_augment()
  class(obj) <- append("flip", class(obj))    
  return(obj)
}

#'@export
transform.flip <- function(obj, data) {
  add.flip <- function(obj, data) {
    an <- apply(data, 1, mean) 
    x <- data - an
    data <- an - x
    attr(data, "idx") <- 1:nrow(data) 
    return(data)
  }
  result <- add.flip(obj, data)
  if (obj$preserve_data) {
    idx <- c(1:nrow(data), attr(result, "idx"))
    result <- rbind(data, result)
    result <- adjust.ts_data(result)
    attr(result, "idx") <- idx 
  }
  return(result)
}

#'@export
describe.flip <- function(obj) {
  return("flip")
}

### Wormhole
#'@title
#'@description
#'@details
#'
#'@return
#'@examples
#'@export
wormhole <- function() {
  obj <- ts_augment()
  obj$fold <- 1
  class(obj) <- append("wormhole", class(obj))    
  return(obj)
}

#'@export
transform.wormhole <- function(obj, data) {
  add.wormhole <- function(data) {
    n <- ncol(data)
    x <- c(as.vector(data[1,1:(n-1)]), as.vector(data[,n]))
    ts <- ts_data(x, n+1)
    space <- combn(1:n, n-1)
    data <- NULL
    idx <- NULL
    for (i in 1:obj$fold) {
      temp <- adjust.ts_data(ts[,c(space[,ncol(space)-i], ncol(ts))])
      idx <- c(idx, 1:nrow(temp))
      data <- rbind(data, temp)
    }
    attr(data, "idx") <- idx
    return(data)
  }
  result <- add.wormhole(data)
  if (obj$preserve_data) {
    idx <- c(1:nrow(data), attr(result, "idx"))
    result <- rbind(data, result)
    result <- adjust.ts_data(result)
    attr(result, "idx") <- idx 
  }
  return(result)
}

#'@export
describe.wormhole <- function(obj) {
  return("warmhole")
}
