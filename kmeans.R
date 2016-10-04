
A <- read.table("mining.data", sep=",",
                col.names=c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n"))



rango <- 1:13

cluster <- kmeans(A[,rango], 5, nstart = 20)
print(cluster)
plot(cluster)