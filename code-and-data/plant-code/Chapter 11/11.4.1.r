# Partial Mantel test of log SPAD - log leaf N relation
# Two by two example
library(vegan)

data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
data.2by2 <- with(data.Set4.1,data.Set4.1[which((Row <= 2) &
   (Column <= 2)),])
print(log.LeafN <- log(data.2by2$LeafN), digits = 3)
print(A <- vegdist(log.LeafN, method = "euclidean"), digits  = 3)

log.SPAD <- log(data.2by2$SPAD)
B <- vegdist(log.SPAD, method = "euclidean")

mantel(A, B)

