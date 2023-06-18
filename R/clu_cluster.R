#'@title cluster dataset basic method
#'@description cluster dataset basic method
#'@param obj object
#'@param ... optional arguments.
#'@return empty data frame
#'@examples trans <- dal_transform()
#'@export
cluster.default <- function(obj, ...) {
  return(data.frame())
}

#'@title Clustering Class
#'@description Ancestor class for clustering problems
#'@return clustering object
#'@examples trans <- dal_transform()
#'@export
clustering <- function() {
  obj <- dal_learner()
  attr(obj, "class") <- "clustering"
  return(obj)
}

#'@title Action implementation for clustering
#'@description A default function that defines the action to proxy cluster method
#'@param obj object
#'@param ... optional arguments
#'@return It simply returns NULL, which indicates that no transforms are applied
#'@examples trans <- dal_transform()
#'@export
action.cluster <- function(obj, ...) {
  thiscall <- match.call(expand.dots = TRUE)
  thiscall[[1]] <- as.name("cluster")
  result <- eval.parent(thiscall)
  return(result)
}

#'@title cluster dataset abstract method
#'@description cluster dataset abstract method
#'@param obj object
#'@param ... optional arguments.
#'@return obj
#'@examples trans <- dal_transform()
#'@export
cluster <- function(obj, ...) {
  UseMethod("cluster")
}

#'@title Cluster Evaluation
#'@description Evaluate the quality of a clustering model using entropy metric
#'@param obj object
#'@param cluster A vector of integers indicating the clustering labels of the data
#'@param attribute attribute target to model building
#'@param ... optional arguments.
#'@return Computed metrics
#'@examples trans <- dal_transform()
#'@importFrom dplyr filter summarise group_by n
#'@export
evaluate.clustering <- function(obj, cluster, attribute, ...) {
  x <- y <- e <- qtd <- n <- 0
  result <- list(data=as.factor(cluster), attribute=as.factor(attribute))

  compute_entropy <- function(obj) {
    value <- getOption("dplyr.summarise.inform")
    options(dplyr.summarise.inform = FALSE)

    dataset <- data.frame(x = obj$data, y = obj$attribute)
    tbl <- dataset |> dplyr::group_by(x, y) |> dplyr::summarise(qtd=dplyr::n())
    tbs <- dataset |> dplyr::group_by(x) |> dplyr::summarise(t=dplyr::n())
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
