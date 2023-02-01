# Partial Mantel test of log SPAD - log leaf N relation
library(vegan)

data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
LeafN.dist <- vegdist(log(data.Set4.1$LeafN), method = "euclidean")
SPAD.dist <- vegdist(log(data.Set4.1$SPAD), method = "euclidean")
dist.mat <- with(data.Set4.1,
   dist(cbind(Easting, Northing), upper = FALSE))
invdist.mat <- 1 / dist.mat
mantel.partial(SPAD.dist, LeafN.dist, dist.mat)
mantel.partial(SPAD.dist, LeafN.dist, invdist.mat)


