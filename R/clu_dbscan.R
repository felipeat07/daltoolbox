# DAL Library
# version 2.1

# depends dal_transform.R
# depends clu_clustering.R

# cluster_dbscan
# loadlibrary("fpc")
# loadlibrary("Rcpp")
# loadlibrary("dbscan")

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

#'@export
fit.cluster_dbscan <- function(obj, data) {

  cluster <- fpc::dbscan(data, eps = obj$eps, MinPts = obj$MinPts)

  cluster <- cluster$cluster
  attr(cluster, "dist") <- 0
  return(cluster)
}

#'@export
optimize.cluster_dbscan <- function(obj, data, do_plot=FALSE) {
  t <- sort(kNNdist(data, k = obj$MinPts))

  y <- t
  myfit <- fit_curvature_max()
  res <- transform(myfit, y)
  if (do_plot)
    plot(myfit, y, res)
  obj$eps <- res$y

  return(obj)
}
