library(maptools)
library(sf)
library(lattice)

Yield96 <- read.csv("created\\set4.1yld96ptsidw.csv", header = TRUE)
Yield97 <- read.csv("created\\set4.1yld97ptsidw.csv", header = TRUE)

Yield96.norm <- 100 * Yield96$Yield / max(Yield96$Yield)
Yield97.norm <- 100 * Yield97$Yield / max(Yield97$Yield)

# Demo method with two years of Yield
Yield.2Yr <- data.frame(Yield96 = Yield96.norm,
   Yield97 = Yield97.norm)


set.seed(123)
cl <- kmeans(Yield.2Yr, 4)
par(mai = c(1,1,1,1))
plot(Yield.2Yr$Yield96, Yield.2Yr$Yield97, pch = cl$cluster,
   axes = TRUE, xlab = "1996", ylab = "1997", cex.lab = 1.5,
   main = "Four Clusters", cex.main = 2)
legend(70, 55, c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"),
  pch = 1:4, title = "Cluster Number")  # Fig. 15.9

data.Set4.1 <- read.csv("Set4\\Set4.196sample.csv", header = TRUE)
clusters <- data.frame(Cluster = cl$cluster, ID = 1:nrow(Yield96),
   Easting = data.Set4.1$Easting, Northing = data.Set4.1$Northing)
coordinates(clusters) <- c("Easting", "Northing")
clusters$Color <- "red"
clusters$Color[which(clusters$Cluster == 2)] <- "blue"
clusters$Color[which(clusters$Cluster == 3)] <- "green"
clusters$Color[which(clusters$Cluster == 4)] <- "dark green"

# Color version
plot(Yield.2Yr$Yield96, Yield.2Yr$Yield97, pch = 16, col = clusters$Color,
   axes = TRUE, xlab = "1996", ylab = "1997", cex.lab = 1.5,
   main = "Four Clusters", cex.main = 2)
legend(70, 55, c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"),
  pch = 16, col = c("red", "blue", "green", "dark green",
  title = "Cluster Number"))  

bdry.sf <- st_read("created\\Set419697bdry.shp")
bdry.sp <- as(bdry.sf,"Spatial")
plot(bdry.sp, axes = TRUE)
plot(clusters, axes = TRUE, pch = clusters$Cluster,
   add = TRUE)  # Fig. 15.10
title(main = "Field 4.1 Two Year Clusters", cex.main = 2,
    xlab = "Easting (m)", ylab = "Northing (m)", cex.lab = 1.5)
legend(592440, 4270600, c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"),
  pch = 1:4, title = "Cluster Number") 

# Color version
plot(bdry.sp, axes = TRUE)
plot(clusters, axes = TRUE, pch = 16, col = clusters$Color,
   add = TRUE)  # Fig. 15.10
title(main = "Field 4.1 Two Year Clusters", cex.main = 2,
    xlab = "Easting (m)", ylab = "Northing (m)", cex.lab = 1.5)
legend(592440, 4270600, c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"),
  pch = 16, col = c("red", "blue", "green", "dark green"), title = "Cluster Number") 

#######################################
# 4 years of Yield

Yield96 <-
   read.csv("created\\set4.1yld96ptsidw.csv", header = TRUE)[1:74,]
Yield97 <-
   read.csv("created\\set4.1yld97ptsidw.csv", header = TRUE)[1:74,]
Yield98 <-
   read.csv("created\\set4.1yld98ptsidw.csv", header = TRUE)[1:74,]
Yield99 <-
   read.csv("created\\set4.1yld99ptsidw.csv", header = TRUE)[1:74,]
data.Set4.1 <-
   read.csv("Set4\\Set4.196sample.csv", header = TRUE)[1:74,]

Yield96.norm <- 100 * Yield96$Yield / max(Yield96$Yield)
Yield97.norm <- 100 * Yield97$Yield / max(Yield97$Yield)
Yield98.norm <- 100 * Yield98$Yield / max(Yield98$Yield)
Yield99.norm <- 100 * Yield99$Yield / max(Yield99$Yield)
Yield.4Yr <- data.frame(Yield96 = Yield96.norm,
   Yield97 = Yield97.norm, Yield98 = Yield98.norm,
   Yield99 = Yield99.norm)

# Do an example with 3 clusters and 9 reps
cluster.k <- 3
n.reps <- 9
n.sites <- nrow(Yield.4Yr)
CM <- matrix(nrow = n.sites, ncol = n.reps)
set.seed(123)
for (i in 1:n.reps) CM[,i] <-
    kmeans(Yield.4Yr, cluster.k)$cluster
CM

library(e1071)
print(perms <- t(permutations(cluster.k)))

# Example of the use of functions test and diff.sum
test <- numeric(nrow(CM))
for (m in 1:nrow(perms)) test[which(CM[,2] == m)] <- perms[m,3]
CM[,2]
test

diff.sum <- numeric(ncol(perms))
for (n in 1:length(diff.sum)){
  for (m in 1:nrow(perms)) test[which(CM[,2] == m)] <- perms[m,n]
  diff.sum[n] <- sum((test != CM[,4]))
}
diff.sum

min.sum <- function(i, j, perms, CM){
   test <- numeric(nrow(CM))
   diff.sum <- numeric(ncol(perms))
   for (n in 1:length(diff.sum)){
# test replaces the IDs in rep i with those in column n of perms
      for (m in 1:nrow(perms)) test[which(CM[,j] == m)] <- perms[m,n]
      diff.sum[n] <- sum((test != CM[,i]))
      }
# which.min automatically selects the first in a tie
   n.min <- which.min(diff.sum)
   opt.id <- diff.sum[n.min]
   return(c(n.min, opt.id))
}

dist.mat <- matrix(nrow = n.reps, ncol = n.reps)
min.id <- matrix(nrow = n.reps, ncol = n.reps)
for (i in 1:n.reps){
   for (j in 1:n.reps){
      x <- min.sum(i,j,perms,CM)
      min.id[i,j] <- x[1]
      dist.mat[i,j] <- x[2]
   }
}
dist.mat
min.id

# Realign the clusters
ref.rep <- 1
best <- min.id[ref.rep,]
CM.align <- matrix(nrow = n.sites, ncol = n.reps)
for (i in 1:n.reps){
   for (j in 1:cluster.k){
     CM.align[which(CM[,i] == j),i] <- perms[j,best[i]]
     }
}
CM.align

CMf <- apply(CM.align, 2, factor)
clusters <- data.frame(CMf)
library(maptools)
clusters$Easting <- data.Set4.1$Easting
clusters$Northing <- data.Set4.1$Northing
coordinates(clusters) <- c("Easting", "Northing")

greys <- grey(c(25,150,225) / 255)
trellis.device(color = FALSE)

# 9 reps
spplot(clusters, zcol = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9"),
    names.attr = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
    col.regions = greys, layout = c(3,3), key.space = "right")  # Fig. 15.11

# Now carry out the full analysis

c.clus <- function(x) as.numeric(sum(abs(x[2:length(x)] - x[1])) == 0)
gamma.clus <- function(CM, set, k){
   sum.c <- sum(apply(CM[,set], 1, c.clus))
   m <- length(set)
   n <- nrow(CM)
   return((sum.c - n/m^k) / (n - n/m^k))
}

# The following code is executed repeatedly for increasing
# values of cluster.k
cluster.k <- 2
cluster.k <- 3
cluster.k <- 4

# Beginnig of repeateadly executed code
n.reps <- 25
n.sites <- nrow(Yield.4Yr)
CM <- matrix(nrow = n.sites, ncol = n.reps)
set.seed(123)
for (i in 1:n.reps) CM[,i] <- kmeans(Yield.4Yr, cluster.k)$cluster
# Realign the clusters
dist.mat <- matrix(nrow = n.reps, ncol = n.reps)
min.id <- matrix(nrow = n.reps, ncol = n.reps)
perms <- t(permutations(cluster.k))
for (i in 1:n.reps){
   for (j in 1:n.reps){
      x <- min.sum(i,j,perms,CM)
      min.id[i,j] <- x[1]
      dist.mat[i,j] <- x[2]
   }
}
# Display the distance matrix to determine cluster sets
dist.mat

ref.rep <- 1
best <- min.id[ref.rep,]
CM.align <- matrix(nrow = n.sites, ncol = n.reps)
for (i in 1:n.reps){
   for (j in 1:cluster.k){
     CM.align[which(CM[,i] == j),i] <- perms[j,best[i]]
     }
}
CM.align
# End of repeatedly executed code

# Beginning of analysis for cluster.k = 2
gamma.clus(CM.align, 1:ncol(CM.align), 2)

clusters2 <- data.frame(CM.align)
clusters2$Easting <- data.Set4.1$Easting
clusters2$Northing <- data.Set4.1$Northing
coordinates(clusters2) <- c("Easting", "Northing")
plot(clusters2, pch = clusters2$X1) #Fig. 15.12a
title(main = "k = 2", cex.main = 2)


par(mai = c(1,1,1,1))
clusters.2mean <- matrix(nrow = 4, ncol = 2)  
for (i in 1:4) clusters.2mean[i,] <-
   tapply(Yield.4Yr[,i], clusters2$X1, mean)
matplot(1:4, clusters.2mean, type = "o",   # Fig. 15.13a
  pch = c(1,2), col = "black", lty = 1, cex = 1,
  main = "k = 2", cex.main = 2, cex.lab = 1.5,
  xaxt = "n", xlab = "Year", ylab = "Normalized Yield",
  ylim = c(0,100))
axis(side = 1, at = c(1,2,3,4),
   labels = c("1996","1997","1998","1999"))
legend(3, 40, c("Cluster 1", "Cluster 2"), pch = c(1,2))

write.csv(clusters2$X1, "created\\clusters2.csv")
# End of cluster.k = 2

# Beginning of analysis for cluster.k = 3
gamma.clus(CM.align, 1:ncol(CM.align), 3)

# Display the clusters
clusters3 <- data.frame(CM.align)
clusters3$Easting <- data.Set4.1$Easting
clusters3$Northing <- data.Set4.1$Northing
coordinates(clusters3) <- c("Easting", "Northing")
plot(clusters3, pch = clusters3$X4) #Fig. 15.12b
title(main = "k = 3", cex.main = 2)

clusters.3mean <- matrix(nrow = 4, ncol = 3)
for (i in 1:4) clusters.3mean[i,] <-
   tapply(Yield.4Yr[,i], clusters3$X1, mean)
   
par(mai = c(1,1,1,1))
matplot(1:4, clusters.3mean, type = "o",  # Fig. 15.13b
  pch = c(1,2,3), col = "black", lty = 3:1,
  main = "k = 3", cex.main = 2,
  cex.lab = 1.5, xaxt = "n", xlab = "Year", ylab = "Nrmalized Yield",
  ylim = c(0,100))
axis(side = 1, at = c(1,2,3,4),
   labels = c("1996","1997","1998","1999"))
legend(40,-1, c("Cluster 1", "Cluster 2", "Cluster 3"), pch = c(1,2,3))

write.csv(clusters3$X1,"created\\clusters3.csv")
# End of cluster.k = 3

# Beginning of cluster.k = 4
gamma.clus(CM.align, 1:ncol(CM.align), 4)
gamma.clus(CM.align, c(1,5,19), 4)
gamma.clus(CM.align, c(2,4,6,8,9,15,17,18,20,21,22,23), 4)
gamma.clus(CM.align, c(3,10,16,24), 4)
gamma.clus(CM.align, c(5,13,14), 4)

clusters4 <- subset(data.frame(CM.align), select = c(X2 = X2, X3 = X3))
clusters4$Easting <- data.Set4.1$Easting
clusters4$Northing <- data.Set4.1$Northing
coordinates(clusters4) <- c("Easting", "Northing")
plot(clusters4, pch = clusters4$X2) #Fig. 15.12c
title(main = "k = 4, Cluster Set 1", cex.main = 2)
plot(clusters4, pch = clusters4$X3) # Almost the same
title(main = "k = 4, Cluster Set 2", cex.main = 2)

cluster.set <- clusters4$X2
clusters.4mean <- matrix(nrow = 4, ncol = 4)
for (i in 1:4) clusters.4mean[i,] <-
   tapply(Yield.4Yr[,i], cluster.set, mean)
par(mai = c(1,1,1,1))
matplot(1:4, clusters.4mean, type = "o",  # Fig. 15.13c
  pch = c(1,2,3,4), col = "black", lty = 4:1,
  main = "k = 4", cex.main = 2, cex.lab = 1.5,
  xaxt = "n", xlab = "Year", ylab = "Normalized Yield",
  ylim = c(0,100))
axis(side = 1, at = c(1,2,3,4),
   labels = c("1996","1997","1998","1999"))
legend(40,-1, c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"),
   pch = c(1,2,3))

write.csv(clusters4$X2,"created\\clusters4.csv")
