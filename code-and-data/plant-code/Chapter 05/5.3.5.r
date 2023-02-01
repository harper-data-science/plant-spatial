# Simulation of cluster sampling the artificial population
library(sf)
pop.data.sf <- st_read("Created\\Set42pop.shp")
sampbdry.sf <- st_read("created\\Set42sampbdry.shp")

pop.mean <- mean(pop.data.sf$Yield)
pop.sd <- sd(pop.data.sf$Yield)

# Determine the member of the population closest to a sample point
closest.point <- function(sample.pt, grid.data){
  dist.sq <- (st_coordinates(grid.data)[,1] - sample.pt[1])^2 +
    (st_coordinates(grid.data)[,2] - sample.pt[2])^2
  return(which.min(dist.sq))
}

# From Sec. 5.2.1
W <- st_bbox(pop.data.sf)[1]
E <- st_bbox(pop.data.sf)[3]
S <- st_bbox(pop.data.sf)[2]
N <- st_bbox(pop.data.sf)[4]
N <- N + 2.5
S <- S - 2.5
E <- E + 2.5
W <- W - 2.5


# Cluster-based sampling
nrows <- 4
ncols <- 8
grid.size <- (E - W) / ncols
grid.offset <- 0.5 * grid.size
cluster.size <- 10
n <- 0
for (i in seq(W + grid.offset,E,grid.size))
  for (j in seq(N - grid.offset,S,-grid.size)){
     spsamp.clst <- expand.grid(x = seq(i-cluster.size, i+cluster.size, cluster.size),
        y = seq(j-cluster.size, j+cluster.size, cluster.size))
     if (n == 0)
       spsamp.pts <- spsamp.clst
     else
       spsamp.pts <- rbind(spsamp.pts, spsamp.clst)
     n <- n + 1
}
samp.pts <- apply(spsamp.pts, 1, closest.point,
   grid.data = pop.data.sf)
data.samp <- pop.data.sf[samp.pts,]
abs(mean(data.samp$Yield) - pop.mean) / pop.mean

plot(sampbdry.sf["z"], axes = TRUE, main = "",
    col = "white", reset = FALSE) # Fig. 5.10
plot(data.samp["Yield"], add = TRUE, pch = 19,
   cex = 0.8, col = "black")
title(main = "Cluster Grid Sampling", cex.lab = 1)


















