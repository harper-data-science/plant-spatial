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
print(x.size <- (E - W) / 8)
print(y.size <- (N - S) / 4)
x.offset <- W + 0.5 * x.size
y.offset <- S + 0.5 * y.size
samp.gt <- GridTopology(c(x.offset,y.offset),
   c(x.size,y.size), c(8,4))
samp.sp <- as(samp.gt,"SpatialPolygons")

# Stratified sample of locations
set.seed(123)
samp.size <- 32
plot(samp.sp, axes = TRUE)  # Fig. 5.5
title(main = "Stratified Sampling", xlab = "Easting",
  ylab = "Northing", cex.main = 2, font.lab = 2, cex.lab = 1.5)
spsamp.pts <- spsample(sampbdry.sp, samp.size, type = "stratified")
plot(spsamp.pts, pch = 19, add = TRUE )

# Monte Carlo simulation of stratified sampling
strat.samp <- function (samp.size){
# Create the locations of the random sample
   spsamp.pts <- spsample(sampbdry.sp, samp.size, type = "stratified")
# Extract a two column array of the x and y coords
   sample.coords <- coordinates(spsamp.pts)
# Apply the function closest.point() to each row of the
# array sample.coords (i.e., each sample location)
   samp.pts <- apply(sample.coords, 1, closest.point,
   grid.data = pop.data)
# Each element of samp.pts is the index of the population value
# closest to the corresponding location in sample.coords
   data.samp <- pop.data[samp.pts,]
   samp.mean <- mean(data.samp$Yield)
   prct.err <- abs(samp.mean - pop.mean) / pop.mean
   return(c(samp.mean,prct.err))
}
samp.size <- 32
set.seed(123)
U <- replicate(1000,strat.samp(samp.size))
mean(U[2,]) # Equation (5.1)
sd(U[1,])


