# DAL Library
# version 2.1

# depends dal_transform.R

#cluster
#'@title cluster
#'@description Models a time series dataset by estimating the underlying trend
#' and seasonality components. Used to make predictions and forecast future
#' values of the time series based on the historical data.
#'@details
#'
#'@param obj object: .
#'@param ... further arguments passed to or from other methods.
#'@return
#'@examples
#'@export
cluster <- function(obj, ...) {
  UseMethod("cluster")
}

#'@title dal_base object
#'@description Receives the obj object as a parameter, ...
#'@details
#'
#'@return The input object "obj"
#'@examples
#'@export
cluster.default <- function(obj, ...) {
  return(obj)
}

### clustering
#'@title Clustering Class
#'@description Ancestor class for clustering problems
#'@details basic wrapper for clustering problems
#'
#'@return clustering object
#'@examples
#'@export
clustering <- function() {
  obj <- dal_base()
  attr(obj, "class") <- "clustering"
  return(obj)
}

#cluster_evaluation
#'@title Cluster Evaluation
#'@description Evaluate the quality of a clustering model using entropy metric
#'@details This function computes the entropy metric of a clustering model. The entropy metric measures how well the clustering labels match the true labels. A lower entropy means that the clustering model is doing a better job at grouping similar data together.
#'
#'@param cluster A vector of integers indicating the clustering labels of the data
#'@param attribute A vector of integers indicating the true labels of the data
#'@return A cluster_evaluation object containing the entropy metric of the clustering model
#'@examples
#'@import dplyr
#'@export
evaluate.clustering <- function(obj, cluster, attribute) {
  result <- list(data=as.factor(cluster), attribute=as.factor(attribute))

  compute_entropy <- function(obj) {
    value <- getOption("dplyr.summarise.inform")
    options(dplyr.summarise.inform = FALSE)

    dataset <- data.frame(x = obj$data, y = obj$attribute)
    tbl <- dataset |> dplyr::group_by(x, y) |> dplyr::summarise(qtd=n())
    tbs <- dataset |> dplyr::group_by(x) |> dplyr::summarise(t=n())
    tbl <- base::merge(x=tbl, y=tbs, by.x="x", by.y="x")
    tbl$e <- -(tbl$qtd/tbl$t)*log(tbl$qtd/tbl$t,2)
    tbl <- tbl |> dplyr::group_by(x) |> dplyr::summarise(ce=sum(e), qtd=sum(qtd))
    tbl$ceg <- tbl$ce*tbl$qtd/length(obj$data)
    obj$entropy_clusters <- tbl
    obj$entropy <- sum(obj$entropy$ceg)

    options(dplyr.summarise.inform = value)
    return(obj)
  }

  result <- compute_entropy(result)
  return(result)
}
