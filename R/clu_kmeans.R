#'@title Clustering using k-means
#'@description This function applies the k-means clustering algorithm to a given data set and returns a clustering object.
#'@param k The number of clusters to form.
#'@return A clustering object.
#'@examples trans <- dal_transform()
#'@export
cluster_kmeans <- function(k) {
  obj <- clustering()
  obj$k <- k
  class(obj) <- append("cluster_kmeans", class(obj))
  return(obj)
}

#'@importFrom stats kmeans
#'@export
cluster.cluster_kmeans <- function(obj, data, ...) {
  k <- obj$k
  cluster <- stats::kmeans(x = data, centers = k)
  dist <- 0
  for (i in 1:k) {
    idx <- i == cluster$cluster
    center <- cluster$centers[i,]
    dist <- dist + sum(rowSums((data[idx,] - center)^2))
  }

  cluster <- cluster$cluster
  attr(cluster, "metric") <- dist
  return(cluster)
}

