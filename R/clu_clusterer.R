#'@export
cluster.default <- function(obj, ...) {
  return(data.frame())
}

#'@title clusterer Class
#'@description Ancestor class for clustering problems
#'@return clusterer object
#'@examples trans <- dal_transform()
#'@export
clusterer <- function() {
  obj <- dal_learner()
  class(obj) <- append("clusterer", class(obj))
  return(obj)
}

#'@title Action implementation for clusterer
#'@description A default function that defines the action to proxy cluster method
#'@param obj object
#'@param ... optional arguments
#'@return It simply returns NULL, which indicates that no transforms are applied
#'@examples trans <- dal_transform()
#'@export
action.clusterer <- function(obj, ...) {
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

#'@importFrom dplyr filter summarise group_by n
#'@export
evaluate.clusterer <- function(obj, cluster, attribute, ...) {
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
