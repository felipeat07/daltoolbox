#'@title Clustering using DBSCAN
#'@description Clustering using DBSCAN
#'@param eps distance value
#'@param minPts minimum number of points
#'@return obj
#'@examples trans <- dal_transform()
#'@export
cluster_dbscan <- function(eps, minPts) {
  obj <- clusterer()
  obj$eps <- eps
  obj$minPts <- minPts

  class(obj) <- append("cluster_dbscan", class(obj))
  return(obj)
}

#'@title fit dbscan model
#'@description fit dbscan model
#'@param obj object
#'@param data dataset
#'@param ... optional arguments
#'@return fitted obj
#'@import dbscan
#'@export
fit.cluster_dbscan <- function(obj, data, ...) {
  t <- sort(dbscan::kNNdist(data, k = obj$minPts))
  y <- t
  myfit <- fit_curvature_max()
  res <- transform(myfit, y)
  obj$eps <- res$y
  return(obj)
}


#'@import dbscan
#'@export
cluster.cluster_dbscan <- function(obj, data, ...) {
  cluster <- dbscan::dbscan(data, eps = obj$eps, minPts = obj$minPts)
  cluster <- cluster$cluster
  attr(cluster, "metric") <- 0
  return(cluster)
}

