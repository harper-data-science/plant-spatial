# Demonstration of resampling of grid data
library(terra) 

data.Set4.2.May.ter <- rast("set4\\Set4.20596.tif")
crs(data.Set4.2.May.ter) <- "EPSG:32610" # UTM Zone 10N
cell.size <- 1.9

E <- 591967
W <- 593139
S <- 4267140.95
N <- 4268320.95
new.cell.size <- 3

ncol.new <- (W - E) / new.cell.size
nrow.new <- (N - S) / new.cell.size
new.ter <- rast(ncol = ncol.new, nrow = nrow.new, xmin = E, 
  xmax = W, ymin = S, ymax = N) 
crs(new.ter) <- "EPSG:32610" # UTM Zone 10N
data.Set4.2.new <- terra::resample(data.Set4.2.May.ter, new.ter,
  method = "near")  

# Use sf objects for plotting   
new.grid <- expand.grid(Easting = seq(E, W, new.cell.size),
  Northing = seq(N, S, -new.cell.size))
new.grid.vec <- vect(new.grid, geom = c("Easting",
   "Northing"))
crs(new.grid.vec) <- "EPSG:32610" # UTM Zone 10N
# Create a terra point vector object
point.set.vec <- as.points(data.Set4.2.May.ter)
greys.num <- c(rep(0,100),seq(0,255,5))
greys.num <- c(greys.num, rep(255, 255-length(greys.num)))
greys.num
greys <- grey(greys.num/255)
par(mai = c(1,1,1,1))
image(data.Set4.2.May.ter, col = greys,
  xlim = c(592285,592290), ylim = c(4267715,4267725),
  axes = TRUE)   # Fig. 6.13a
points(point.set.vec, pch = 3)
points(new.grid.vec, pch = 19)
title(main = "Nearest Neighbor Resampling",
  xlab = "Easting", ylab = "Northing", cex.main = 1.5, cex.lab = 1.5)

image(data.Set4.2.new, col = greys,
  xlim = c(592285,592290), ylim = c(4267715,4267725),
  axes = TRUE)  # Fig. 6.13b
title(main = "Resampling Result",
  xlab = "Easting", ylab = "Northing", cex.main = 1.5, cex.lab = 1.5)


