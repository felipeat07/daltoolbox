#'@title Partition Around Medoids (PAM) Clustering
#'@description Implements the Partition Around Medoids (PAM) clustering algorithm.
#' PAM is a partitioning clustering method that seeks to minimize the sum of dissimilarities between data points and the medoid of its assigned cluster. Unlike k-means, PAM can handle categorical data as well as distance measures other than Euclidean distance.
#'@param k The number of clusters to generate.
#'@return A clustering object.
#'@examples trans <- dal_transform()
#'@export
cluster_pam <- function(k) {
  obj <- clustering()
  obj$k <- k

  class(obj) <- append("cluster_pam", class(obj))
  return(obj)
}

#'@title Updates the parameters of cluster_pam
#'@description It takes as input the obj model object and a set of params parameters to update.
#'@param obj object
#'@param params parameters
#'@return The obj template object updated with the new parameters
#'@export
set_params.cluster_pam <- function(obj, params) {
  if (!is.null(params$k))
    obj$k <- params$k
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

