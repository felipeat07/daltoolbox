# DAL Library
# version 2.1

# depends dal_transform.R
# depends clu_clustering.R

# kmeans
# loadlibrary("cluster")
# loadlibrary("factoextra")  

#'@title
#'@description
#'@details
#'
#'@param k
#'@return
#'@examples
#'@export
cluster_kmeans <- function(k) {
  obj <- clustering()
  obj$k <- k
  
  class(obj) <- append("cluster_kmeans", class(obj))
  return(obj)
}

#'@export
optimize.cluster_kmeans <- function(obj, data, kmax=20, do_plot=FALSE) {
  t <- fviz_nbclust(data, kmeans, k.max = kmax, method = "wss")
  
  y <- t$data$y
  myfit <- fit_curvature_max()
  res <- transform(myfit, y)
  if (do_plot)
    plot(myfit, y, res)
  obj$k <- res$x

  return(obj)
}

#'@export
fit.cluster_kmeans <- function(obj, data) {
  k <- obj$k
  cluster <- kmeans(x = data, centers = k)
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
