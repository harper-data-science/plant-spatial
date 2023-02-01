# Simulation of sampling the artificial population
library(sf)
pop.data.sf <- st_read("Created\\Set42pop.shp")
sampbdry.sf <- st_read("created\\Set42sampbdry.shp")

pop.mean <- mean(pop.data.sf$Yield)
pop.sd <- sd(pop.data.sf$Yield)

# Random sample
set.seed(123)
spsamp.pts <- st_sample(sampbdry.sf, 32, type = "random")
# Plot the sample points
plot(sampbdry.sf["z"], axes = TRUE, main = "",
    col = "white", reset = FALSE) # Fig. 5.4
plot(spsamp.pts, add = TRUE, pch = 19, cex = 0.8)
title(main = "Random Sampling", cex.lab = 1.5)
  
  
# Determine the member of the population closest to a sample point
closest.point <- function(sample.pt, grid.data){
   dist.sq <- (st_coordinates(grid.data)[,1] - sample.pt[1])^2 +
      (st_coordinates(grid.data)[,2] - sample.pt[2])^2
   return(which.min(dist.sq))
}

# Monte Carlo simulation of random sampling
rand.samp <- function (samp.size){
# Create the locations of the random sample
   spsamp.pts <- st_sample(sampbdry.sf, samp.size, type = "random")
# Extract a two column array of the x and y coords
   sample.coords <- st_coordinates(spsamp.pts)
# Apply the function closest.point() to each row of the
# array sample.coords (i.e., each sample location)
   samp.pts <- apply(sample.coords, 1, closest.point,
      grid.data = pop.data.sf)
# Each element of samp.pts is the index of the population value
# closest to the corresponding location in sample.coords
   data.samp <- pop.data.sf[samp.pts,]
   samp.mean <- mean(data.samp$Yield)
   prct.err <- abs(samp.mean - pop.mean) / pop.mean
   return(c(samp.mean,prct.err))
}

samp.size <- 32
set.seed(123)
U <- replicate(1000,rand.samp(samp.size))
print(mean(U[2,]), 3) # Equation (5.1)
print(sd(U[1,]), 4)

hist(U[1,])

















