#'@title PAM
#'@description Creates a clusterer object that
#' uses the Partition Around Medoids (PAM) method
#' It wraps the cluster library.
#'@param k The number of clusters to generate.
#'@return A PAM object.
#'@examples
#'# setup clustering
#'model <- cluster_pam(k = 3)
#'
#'#load dataset
#'data(iris)
#'
#'# build model
#'model <- fit(model, iris[,1:4])
#'clu <- cluster(model, iris[,1:4])
#'table(clu)
#'
#'# evaluate model using external metric
#'eval <- evaluate(model, clu, iris$Species)
#'eval
#'@export
cluster_pam <- function(k) {
  obj <- clusterer()
  obj$k <- k

  class(obj) <- append("cluster_pam", class(obj))
  return(obj)
}

#'@import cluster
#'@export
cluster.cluster_pam <- function(obj, data, ...) {
  cluster <- cluster::pam(data, obj$k)
  dist <- 0
  for (i in 1:obj$k) {
    idx <- i==cluster$clustering
    center <- cluster$medoids[i,]
    dist <- dist + sum(rowSums((data[idx,] - center)^2))
  }

  cluster <- cluster$cluster
  attr(cluster, "metric") <- dist
  return(cluster)
}

