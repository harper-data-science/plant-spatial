# Demonstration of resampling of grid data
library(raster) 
library(sp)

data.Set4.2.May <- raster("set4\\Set4.20596.tif")
proj4string(data.Set4.2.May) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
cell.size <- 1.9

E <- 591967
W <- 593139
S <- 4267140.95
N <- 4268320.95
new.cell.size <- 3

ncol.new <- (W - E) / new.cell.size
nrow.new <- (N - S) / new.cell.size
new.ras <- raster(ncol = ncol.new, nrow = nrow.new, xmn = E, 
  xmx = W, ymn = S, ymx = N) 
proj4string(new.ras) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
data.Set4.2.new <- resample(data.Set4.2.May, new.ras, method = "ngb")  

# Use sp objects for plotting   
new.grid <- expand.grid(Easting = seq(E, W, new.cell.size),
  Northing = seq(N, S, -new.cell.size))
coordinates(new.grid) <- c("Easting","Northing")
proj4string(new.grid) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
point.set <- as(data.Set4.2.May, "SpatialPointsDataFrame")
data.plot <- as(data.Set4.2.May, "SpatialGridDataFrame")
greys.num <- c(rep(0,100),seq(0,255,5))
greys.num <- c(greys.num, rep(255, 255-length(greys.num)))
greys.num
greys <- grey(greys.num/255)
par(mai = c(1,1,1,1))
image(data.plot, col = greys,
  xlim = c(592285,592290), ylim = c(4267715,4267725),
  axes = TRUE)   # Fig. 6.13a
points(point.set, pch = 3)
points(new.grid, pch = 19)
title(main = "Nearest Neighbor Resampling",
  xlab = "Easting", ylab = "Northing", cex.main = 1.5, cex.lab = 1.5)

grid.resample <- as(data.Set4.2.new, "SpatialGridDataFrame")
image(grid.resample, col = greys,
  xlim = c(592285,592290), ylim = c(4267715,4267725),
  axes = TRUE)  # Fig. 6.13b
title(main = "Resampling Result",
  xlab = "Easting", ylab = "Northing", cex.main = 1.5, cex.lab = 1.5)


