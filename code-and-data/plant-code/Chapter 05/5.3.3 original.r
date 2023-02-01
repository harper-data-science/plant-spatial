# Simulation of sampling the artificial population
library(maptools)
library(sf)
pop.sf <- st_read("Created\\Set42pop.shp")
pop.data <- as(pop.sf, "Spatial")
sampbdry.sf <- st_read("created\\Set42sampbdry.shp")
sampbdry.sp <- as(sampbdry.sf, "Spatial")

pop.mean <- mean(pop.data$Yield)
pop.sd <- sd(pop.data$Yield)

# Determine the member of the population closest to a sample point
closest.point <- function(sample.pt, grid.data){
  dist.sq <- (coordinates(grid.data)[,1] - sample.pt[1])^2 +
    (coordinates(grid.data)[,2] - sample.pt[2])^2
  return(which.min(dist.sq))
}

# From Sec. 5.2.1
W <- bbox(pop.data)[1,1]
E <- bbox(pop.data)[1,2]
S <- bbox(pop.data)[2,1]
N <- bbox(pop.data)[2,2]
N <- N + 2.5
S <- S - 2.5
E <- E + 2.5
W <- W - 2.5

# Grid-based sampling, 32 points
nrows <- 4
ncols <- 8
grid.size <- (E - W) / ncols
grid.offset <- 0.5 * grid.size
spsamp.pts <- expand.grid(x = seq(W+grid.offset,E,grid.size),
  y = seq(N - grid.offset,S,-grid.size))
samp.pts <- apply(spsamp.pts, 1, closest.point, grid.data = pop.data)
data.samp <- pop.data[samp.pts,]
print(abs(mean(data.samp$Yield) - pop.mean) / pop.mean, digits = 4)
coordinates(spsamp.pts) <- c("x", "y")
plot(sampbdry.sp, axes = TRUE)  # Fig. 5.5
plot(spsamp.pts, pch = 19, add = TRUE)
title(main = "Grid Sampling", xlab = "Easting",
  ylab = "Northing", cex.main = 2, cex.lab = 1.5)

# 162 points
nrows <- 9
ncols <- 18
grid.size <- (E - W) / ncols
grid.offset <- 0.5 * grid.size
spsamp.pts <- expand.grid(x = seq(W+grid.offset,E,grid.size),
   y = seq(N - grid.offset,S,-grid.size))
samp.pts <- apply(spsamp.pts, 1, closest.point, grid.data = pop.data)
data.samp <- pop.data[samp.pts,]
print(abs(mean(data.samp$Yield) - pop.mean) / pop.mean, digits = 4)

# 288 points
nrows <- 9
ncols <- 18
grid.size <- (E - W) / ncols
grid.offset <- 0.5 * grid.size
spsamp.pts <- expand.grid(x = seq(W+grid.offset,E,grid.size),
   y = seq(N - grid.offset,S,-grid.size))
samp.pts <- apply(spsamp.pts, 1, closest.point, grid.data = pop.data)
data.samp <- pop.data[samp.pts,]
print(abs(mean(data.samp$Yield) - pop.mean) / pop.mean, digits = 4)
