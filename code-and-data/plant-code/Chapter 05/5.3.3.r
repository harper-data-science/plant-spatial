# Simulation of sampling the artificial population
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

# Grid-based sampling, 32 points
nrows <- 4
ncols <- 8
grid.size <- (E - W) / ncols
grid.offset <- 0.5 * grid.size
spsamp.pts <- expand.grid(x = seq(W+grid.offset,E,grid.size),
  y = seq(N - grid.offset,S,-grid.size))
spsamp.pts.sf <- st_as_sf(spsamp.pts,
   coords = c("x","y"))
samp.pts <- apply(spsamp.pts, 1, closest.point,
   grid.data = pop.data.sf)
data.samp <- pop.data.sf[samp.pts,]
print(abs(mean(data.samp$Yield) - pop.mean) / pop.mean, digits = 4)
plot(sampbdry.sf["z"], axes = TRUE, main = "",
    col = "white", reset = FALSE) # Fig. 5.6
plot(spsamp.pts.sf, pch = 19, add = TRUE)
title(main = "Grid Sampling", cex.main = 1)

# 162 points
nrows <- 9
ncols <- 18
grid.size <- (E - W) / ncols
grid.offset <- 0.5 * grid.size
spsamp.pts <- expand.grid(x = seq(W+grid.offset,E,grid.size),
   y = seq(N - grid.offset,S,-grid.size))
samp.pts <- apply(spsamp.pts, 1, closest.point,
   grid.data = pop.data.sf)
data.samp <- pop.data.sf[samp.pts,]
print(abs(mean(data.samp$Yield) - pop.mean) / pop.mean, digits = 4)

# 288 points
nrows <- 9
ncols <- 18
grid.size <- (E - W) / ncols
grid.offset <- 0.5 * grid.size
spsamp.pts <- expand.grid(x = seq(W+grid.offset,E,grid.size),
   y = seq(N - grid.offset,S,-grid.size))
samp.pts <- apply(spsamp.pts, 1, closest.point,
   grid.data = pop.data.sf)
data.samp <- pop.data.sf[samp.pts,]
print(abs(mean(data.samp$Yield) - pop.mean) / pop.mean, digits = 4)
