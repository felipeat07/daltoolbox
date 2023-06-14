iris <- datasets::iris

model <- clu_tune(cluster_dbscan(k=0))
ranges <- list(k = 1:20)
model <- fit(model, iris[,1:4], ranges)
clu <- cluster(model, iris[,1:4])

