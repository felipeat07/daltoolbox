#'@title DBSCAN
#'@description Creates a clusterer object that
#' uses the DBSCAN method
#' It wraps the dbscan library.
#'@param eps distance value
#'@param minPts minimum number of points
#'@return A dbscan object.
#'@examples
#'# setup clustering
#'model <- cluster_dbscan(minPts = 3)
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
cluster_dbscan <- function(minPts, eps = NULL) {
  obj <- clusterer()
  obj$minPts <- minPts
  obj$eps <- eps

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
  if (is.null(obj$eps)) {
    t <- sort(dbscan::kNNdist(data, k = obj$minPts))
    y <- t
    myfit <- fit_curvature_max()
    res <- transform(myfit, y)
    obj$eps <- res$y
  }
  return(obj)
}


#'@import dbscan
#'@export
cluster.cluster_dbscan <- function(obj, data, ...) {
  db_cluster <- dbscan::dbscan(data, eps = obj$eps, minPts = obj$minPts)
  cluster <- db_cluster$cluster

  #intrinsic quality metric
  null_cluster <- length(cluster[cluster==0])
  attr(cluster, "metric") <- null_cluster

  return(cluster)
}

