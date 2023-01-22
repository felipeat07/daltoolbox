# DAL Library
# version 2.1

# depends dal_transform.R
# depends clu_clustering.R

# cluster_pam
# loadlibrary("factoextra")
# loadlibrary("cluster")

#'@title Partition Around Medoids (PAM) Clustering
#'@description
#'@details
#'
#'@param k - number of clusters
#'@return clustering object
#'@examples
#'@export
cluster_pam <- function(k) {
  obj <- clustering()
  obj$k <- k

  class(obj) <- append("cluster_pam", class(obj))
  return(obj)
}

#'@import factoextra
#'@export
optimize.cluster_pam <- function(obj, data, kmax=20, do_plot=FALSE) {
  t <- factoextra::fviz_nbclust(data, pam, k.max = kmax, method = "wss")

  y <- t$data$y
  myfit <- fit_curvature_max()
  res <- transform(myfit, y)
  if (do_plot)
    plot(myfit, y, res)
  obj$k <- res$x

  return(obj)
}

#'@export
fit.cluster_pam <- function(obj, data) {
  cluster <- pam(data, obj$k)
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
