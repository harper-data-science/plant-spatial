# Don't forget to run setwd() first!

data.Set1.obs <- read.csv("set1\\obspts.csv", header = TRUE)
class(data.Set1.obs)
str(data.Set1.obs)

# Read the data for Data Set 1 and create an sf point object
library(sf)
data.Set1.sf <- st_as_sf(data.Set1.obs, coords = c("Easting", "Northing"))

class(data.Set1.sf)
str(data.Set1.sf)

str(data.Set1.sf$geometry)

st_crs(data.Set1.sf) <- "+proj=utm +zone=10 +ellps=WGS84"
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
Set42bdry = st_sf(z = 1, coords.pol)
st_crs(Set42bdry) <- 32601
plot(Set42bdry)

st_write(Set42bdry, "created\\Set42bdry.shp")

?plot

data.Set1.landcover <- st_read("Set1\\landcover.shp")
str(data.Set1.landcover)
plot(data.Set1.landcover)

###########################

# Dealing with sp objects
library(maptools)

data.Set1.landcover.sp <- as(data.Set1.landcover, "Spatial")
str(data.Set1.landcover.sp)
class(slot(data.Set1.landcover.sp, "data")) 
slot(data.Set1.landcover.sp, "data")$VegType[1:10]
data.Set1.landcover.sp@data$VegType[1:10]
data.Set1.landcover.sp@polygons[[1]]

lapply(data.Set1.landcover.sp@polygons, slot, "ID")
y <- lapply(data.Set1.landcover.sp@polygons, slot, "labpt")
data.Set1.landcover.sp.loc <- matrix(0, length(y), 2)
for (i in 1:length(y)) data.Set1.landcover.sp.loc[i,] <- unlist(y[[i]])
data.Set1.landcover.sp.loc

data.Set1.sp <- data.Set1.obs
coordinates(data.Set1.sp) <- c("Easting", "Northing") 
proj4string(data.Set1.sp) <- CRS("+proj=utm +zone=10 +ellps=WGS84")

###############################

# Raster objects
# Read the three bands of a tiff image as a raster stack
library(raster)
data.4.2.May <- brick("set4\\Set4.20596.tif")
data.4.2.May

projection(data.4.2.May) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
data.4.2.May

plot(data.4.2.May)
# Only the first layger of the brick (IR)
plot(data.4.2.May[[1]])

# Run this code to create an interpolated EC grid
library(gstat)
library(maptools)
data.Set4.2EC <- read.csv("Set4\\Set4.2EC.csv", header = TRUE)
data.Set4.2EC$ID <- 1:nrow(data.Set4.2EC)
# Establish the coordinates of the rectangular boundary
N <- 4267868
S <- 4267513
E <- 592867
W <- 592082
cell.size <- 10
# Create the interpolation grid
grid.xy <- expand.grid(x = seq(W, E, cell.size),  y = seq(N, S, -cell.size))
coordinates(grid.xy) <- ~x + y
gridded(grid.xy) = TRUE
proj4string(grid.xy) = CRS("+proj=utm +zone=10 +ellps=WGS84")
grid.ras <- raster(grid.xy)
# Select every (cell.size)th data value
data.vgm <- data.Set4.2EC[-which(data.Set4.2EC$ID %% cell.size != 0),]
coordinates(data.vgm) <- c("Easting", "Northing")
projection(data.vgm) <- projection(grid.ras)
EC.vgm <- variogram(ECto30 ~ 1, data.vgm)
EC.fit <- fit.variogram(EC.vgm, model = vgm(100000, "Sph", 700,10000))
plot(EC.vgm, EC.fit, col = "black") # Fig.1.2a
EC.krig <- gstat(formula = ECto30 ~ 1, locations = grid.xy, data = data.vgm,
           model = EC.fit)
EC.ras <- interpolate(grid.ras, EC.krig)
extent(EC.ras)
extent(data.4.2.May)
IR.4.2.May <- resample(data.4.2.May[[1]], EC.ras)
data.4.2.May.stack <- stack(IR.4.2.May, EC.ras)
plot(data.4.2.May.stack)
