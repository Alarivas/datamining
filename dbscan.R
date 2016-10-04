library('fpc')
library('factoextra')

A <- read.table("mining.data", sep=",",
                col.names=c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n"))


rango <- 1:13
cluster <- dbscan(A[,rango], 10, method="hybrid",showplot=2)
fviz_cluster(cluster, A[,rango], stand= FALSE, frame= FALSE, geom= "point")
print(cluster)