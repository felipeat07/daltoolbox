# DAL Library
# version 2.1

# depends dal_transform.R

# depends clu_clustering.R

# cluster_pam
# loadlibrary("factoextra")
# loadlibrary("cluster")

#'@title Partition Around Medoids (PAM) Clustering
#'@description Implements the Partition Around Medoids (PAM) clustering algorithm.
#' PAM is a partitioning clustering method that seeks to minimize the sum of dissimilarities between data points and the medoid of its assigned cluster. Unlike k-means, PAM can handle categorical data as well as distance measures other than Euclidean distance.
#'@details
#' The algorithm selects k medoids (representative points) from the data set, and assigns each data point to the closest medoid. It then tries to improve the solution by swapping a medoid with a non-medoid point and computing the total dissimilarity for the new configuration. If the total dissimilarity is reduced by the swap, the solution is updated; otherwise, the swap is rejected.
#'
#'@param k The number of clusters to generate.
#'@return A clustering object.
#'@examples
#'@export
cluster_pam <- function(k) {
  obj <- clustering()
  obj$k <- k

  class(obj) <- append("cluster_pam", class(obj))
  return(obj)
}

#'@import cluster
#'@export
fit.cluster_pam <- function(obj, data) {
  cluster <- cluster::pam(data, obj$k)
  dist <- 0
  for (i in 1:obj$k) {
    idx <- i==cluster$clustering
    center <- cluster$medoids[i,]
    dist <- dist + sum(rowSums((data[idx,] - center)^2))
  }

  cluster <- cluster$cluster
  attr(cluster, "dist") <- dist
  return(cluster)
}

#optimize.cluster_pam <- function(obj, data, kmax=20, do_plot=FALSE) {
#  t <- factoextra::fviz_nbclust(data, pam, k.max = kmax, method = "wss")

#  y <- t$data$y
#  myfit <- fit_curvature_max()
#  res <- transform(myfit, y)
#  if (do_plot)
#    plot(myfit, y, res)
#  obj$k <- res$x
#
#  return(obj)
#}

