# DAL Library
# version 2.1

# depends dal_transform.R

### clustering
# loadlibrary("dplyr")

#'@title
#'@description
#'@details
#'
#'@return
#'@examples
#'@export
clustering <- function() {
  obj <- list()
  attr(obj, "class") <- "clustering"
  return(obj)
}

#cluster_evaluation
#'@title
#'@description
#'@details
#'
#'@param cluster
#'@param attribute
#'@return
#'@examples
#'@import dplyr
#'@export
cluster_evaluation <- function(cluster, attribute) {
  obj <- list(data=as.factor(cluster), attribute=as.factor(attribute))
  attr(obj, "class") <- "cluster_evaluation"

  compute_entropy <- function(obj) {
    value <- getOption("dplyr.summarise.inform")
    options(dplyr.summarise.inform = FALSE)

    base <- data.frame(x = obj$data, y = obj$attribute)
    tbl <- base %>% group_by(x, y) %>% summarise(qtd=n())
    tbs <- base %>% group_by(x) %>% summarise(t=n())
    tbl <- merge(x=tbl, y=tbs, by.x="x", by.y="x")
    tbl$e <- -(tbl$qtd/tbl$t)*log(tbl$qtd/tbl$t,2)
    tbl <- tbl %>% group_by(x) %>% summarise(ce=sum(e), qtd=sum(qtd))
    tbl$ceg <- tbl$ce*tbl$qtd/length(obj$data)
    obj$entropy_clusters <- tbl
    obj$entropy <- sum(obj$entropy$ceg)

    options(dplyr.summarise.inform = value)
    return(obj)
  }
  obj <- compute_entropy(obj)
  return(obj)
}
