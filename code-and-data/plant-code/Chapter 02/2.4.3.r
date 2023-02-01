# Don't forget to run setwd() first!
library(gstat)
library(sf)

data.Set1.obs <- read.csv("set1\\obspts.csv", header = TRUE)
class(data.Set1.obs)
str(data.Set1.obs)

# Read the data for Data Set 1 and create an sf point object
data.Set1.sf <- st_as_sf(data.Set1.obs, coords = c("Easting", "Northing"))

class(data.Set1.sf)
str(data.Set1.sf)

str(data.Set1.sf$geometry)

st_crs(data.Set1.sf) <- "EPSG:32610" # UTM Zone 10N
st_crs(data.Set1.sf)

# If you execute the step below directly you will get a warning message saying 
# crs already established.
# Instead, start over and skip the st_crs assignment step above
st_crs(data.Set1.sf) <- 32610
st_crs(data.Set1.sf)

############################

#Create a boundary for Field 4.2
N <- 4267873
S <- 4267483
E <- 592860
W <- 592080

N - S
E - W

print(coords.mat <- matrix(c(W,E,E,W,W,N,N,S,S,N), ncol = 2))
# Convert the coordinate matrix to a list object 
coords.lst <- list(coords.mat)
# Create the sf object by specifying the coordinates
coords.pol = st_sfc(st_polygon(coords.lst))
# Assign the value z = 1 to the cell of the polygon
Set42bdry.sf = st_sf(z = 1, coords.pol)
st_crs(Set42bdry.sf) <- 32601
plot(Set42bdry.sf)

st_write(Set42bdry, "created\\Set42bdry.shp")
data.Set1.landcover.sf <- st_read("Set1\\landcover.shp")
str(data.Set1.landcover.sf)
plot(data.Set1.landcover.sf)
plot(st_geometry(data.Set1.landcover.sf))


###########################

# The section on dealing with sp objects has been removed

###############################
# Raster objects

# Terra objects
# Read the three bands of a tiff image as a terra object
library(terra)
data.4.2.May <- rast("set4\\Set4.20596.tif")
data.4.2.May

crs(data.4.2.May) <- "EPSG:32610" # UTM Zone 10N
data.4.2.May

plot(data.4.2.May)
# Only the first layger (IR)
plot(data.4.2.May, y = 1)
# Plot in grayscale
greys <- grey(0:255 / 255)
plot(data.4.2.May, y = 1, col = greys)

# Run this code to create an interpolated EC grid
data.Set4.2EC <- read.csv("Set4\\Set4.2EC.csv", header = TRUE)
data.Set4.2EC$ID <- 1:nrow(data.Set4.2EC)
# Establish the coordinates of the rectangular boundary
N <- 4267868
S <- 4267513
E <- 592867
W <- 592082
cell.size <- 10
# Create the interpolation grid
library(stars)
library(starsExtra)
# Based on https://rdrr.io/cran/starsExtra/man/make_grid.html
SW <- st_point(c(W, S))
NE <- st_point(c(E, N))
xy.sfc <- st_sfc(SW, NE)
class(xy.sfc)
grid.xy <- make_grid(xy.sfc, res = cell.size, buffer = 0)
st_crs(grid.xy) <- "EPSG:32610" # UTM Zone 10N
# Select every (cell.size)th data value
data.vgm <- data.Set4.2EC[-which(data.Set4.2EC$ID %% cell.size != 0),]
data.vgm.sf <- st_as_sf(data.vgm, 
  coords = c("Easting", "Northing"), crs = "EPSG:32610")
EC.vgm <- variogram(ECto30 ~ 1, data.vgm.sf)
EC.fit <- fit.variogram(EC.vgm, model = vgm(100000, "Sph", 700,10000))
plot(EC.vgm, EC.fit, col = "black") # Fig.1.2a
EC.krig <- krige(ECto30 ~ 1, data.vgm.sf, grid.xy, model = EC.fit)
library(ggplot2)
ggplot() + 
    geom_stars(data = EC.krig, aes(fill = var1.pred,
       x = x, y = y)) +
    scale_fill_gradient(low = "white", high = "black") + 
    geom_sf(data = xy.sfc, cex = 0.5)

