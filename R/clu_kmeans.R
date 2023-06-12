# DAL Library
# version 2.1

# depends dal_transform.R
# depends clu_clustering.R

# kmeans
# loadlibrary("cluster")
# loadlibrary("factoextra")

#'@title Clustering using k-means
#'@description This function applies the k-means clustering algorithm to a given data set and returns a clustering object.
#'@details The k-means algorithm partitions a set of observations into k clusters, such that the observations within each cluster have low variance and are similar to each other, while observations in different clusters are dissimilar. The algorithm works by iteratively assigning each observation to the nearest cluster center and then re-calculating the cluster centers based on the newly assigned observations.
#'
#'@param k The number of clusters to form.
#'@return A clustering object.
#'@examples
#'@export
cluster_kmeans <- function(k) {
  obj <- clustering()
  obj$k <- k

  class(obj) <- append("cluster_kmeans", class(obj))
  return(obj)
}

#'@importFrom stats kmeans
#'@export
fit.cluster_kmeans <- function(obj, data) {
  k <- obj$k
  cluster <- stats::kmeans(x = data, centers = k)
  dist <- 0
  for (i in 1:k) {
    idx <- i == cluster$cluster
    center <- cluster$centers[i,]
    dist <- dist + sum(rowSums((data[idx,] - center)^2))
  }

  cluster <- cluster$cluster
  attr(cluster, "dist") <- dist
  return(cluster)
}

#optimize.cluster_kmeans <- function(obj, data, kmax=20, do_plot=FALSE) {
#  t <- factoextra::fviz_nbclust(data, kmeans, k.max = kmax, method = "wss")

#  y <- t$data$y
#  myfit <- fit_curvature_max()
#  res <- transform(myfit, y)
#  if (do_plot)
#    plot(myfit, y, res)
#  obj$k <- res$x

#  return(obj)
#}

