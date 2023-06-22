#'@title Partition Around Medoids (PAM) Clustering
#'@description Implements the Partition Around Medoids (PAM) clustering algorithm.
#' PAM is a partitioning clustering method that seeks to minimize the sum of dissimilarities between data points and the medoid of its assigned cluster. Unlike k-means, PAM can handle categorical data as well as distance measures other than Euclidean distance.
#'@param k The number of clusters to generate.
#'@return A clusterer object.
#'@examples trans <- dal_transform()
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

