# DAL Library
# version 2.1

# depends dal_transform.R
# depends clu_clustering.R

# cluster_dbscan
# loadlibrary("fpc")

#'@title Clustering using DBSCAN
#'@description
#'@details
#'
#'@param eps
#'@param MinPts
#'@return
#'@examples
#'@export
cluster_dbscan <- function(eps, MinPts) {
  obj <- clustering()
  obj$eps <- eps
  obj$MinPts <- MinPts

  class(obj) <- append("cluster_dbscan", class(obj))
  return(obj)
}

#'@import dbscan
#'@export
cluster.cluster_dbscan <- function(obj, data) {
  t <- sort(dbscan::kNNdist(data, k = obj$MinPts))
  y <- t
  myfit <- fit_curvature_max()
  res <- transform(myfit, y)
  obj$eps <- res$y
  return(obj)
}


#'@import dbscan
#'@export
cluster.cluster_dbscan <- function(obj, data) {
  cluster <- dbscan::dbscan(data, eps = obj$eps, MinPts = obj$MinPts)
  cluster <- cluster$cluster
  attr(cluster, "metric") <- 0
  return(cluster)
}

