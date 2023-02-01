# Figure 1.1
# Display of 1/10 of points in Data Set 2
library(rgdal)
library(maptools)
# Hillshade map downloaded from UCSB Biogeograph Lab
# http://www.biogeog.ucsb.edu/projects/gap/gap_data_state.html
# and gepregistered in ArcGIS
ca.elev <- readGDAL("Auxiliary\\caelev.tif")
proj4string(ca.elev) <- CRS("+proj=longlat +datum=WGS84")
greys <- grey(0:255 / 255)
par(mai = c(1,1,1,1))
image(ca.elev, col = greys, axes = TRUE) # Fig. 1.1
# From Appendix B/Set2 Input
data.Set2 <- read.csv("set2\\set2data.csv", header = TRUE)
# Assign ID values and use the modulo function to select 1 in 10
data.Set2$ID <- 1:nrow(data.Set2)
Set2.plot <- data.Set2[-which(data.Set2$ID %% 10 != 0),]
coordinates(Set2.plot) <- c("Longitude", "Latitude")
proj4string(Set2.plot) <- CRS("+proj=longlat +datum=WGS84")
plot(Set2.plot, pch = 16, cex = 0.5, add = TRUE, col = "white") # Fig. 1.1
title(main = "Wieslander Survey Locations", cex.main = 2, xlab = "Longitude",
   ylab = "Latitude", cex.lab = 1.5)

# Color version of Fig.1.1
library(RColorBrewer)
col.terrain <- terrain.colors(10)[10:1]
col.terrain[10] <- "white"
image(ca.elev, col = col.terrain, axes = TRUE)
plot(Set2.plot, pch = 16, cex = 0.5, add = TRUE, col = "red") 
title(main = "Wieslander Survey Locations", cex.main = 2, xlab = "Longitude",
   ylab = "Latitude", cex.lab = 1.5)
   
# -----------------------------------------------------------

# Kriging of Field 4-2 ECa to create Fig. 1.2a
library(gstat)
library(maptools)
data.Set4.2EC <- read.csv("Set4\\Set4.2EC.csv", header = TRUE)
data.Set4.2EC$ID <- 1:nrow(data.Set4.2EC)
# Establish the coordinates of the rectangular boundary
N <- 4267868
S <- 4267513
E <- 592867
W <- 592082
# If this cell size chokes the computer, set cell.size to 10
cell.size <- 5
# Create the interpolation grid
grid.xy <- expand.grid(x = seq(W, E, cell.size),  y = seq(N, S, -cell.size))
coordinates(grid.xy) <- ~x + y
gridded(grid.xy) = TRUE
# Select every (cell.size)th data value
data.vgm <- data.Set4.2EC[-which(data.Set4.2EC$ID %% cell.size != 0),]
coordinates(data.vgm) <- c("Easting", "Northing")
EC.vgm <- variogram(ECto30 ~ 1, data.vgm)
EC.fit <- fit.variogram(EC.vgm, model = vgm(100000, "Sph", 700,10000))
plot(EC.vgm, EC.fit, col = "black") # Fig.1.2a
EC.krig <- krige(ECto30 ~ 1, data.vgm, grid.xy, model = EC.fit)
greys <- grey(0:255 / 255)
spplot(EC.krig["var1.pred"], col.regions = greys, # Fig. 1.2a
  xlim = c(W,E), ylim = c(S,N), scales = list(draw = TRUE),
  xlab = "Easting", ylab = "Northing",
  main = "Soil Apparent Electrical Conductivity (mS/m)",
  font = "serif")
  
# --------------------------------------------------

# Plot of Field 4-2 NDVI to create Fig. 1.2b
library(gstat)
library(rgdal)
library(sp)
data.4.2.May <- readGDAL("set4\\Set4.20596.tif")
N <- 4267868
S <- 4267513
E <- 592867
W <- 592082
# Create a data frame of image values
img.df <- data.frame(data.4.2.May@data$band1)
# Determine the inage coordinates
img.coords <- coordinates(data.4.2.May)
# Assign these coordinantes to the data frame
img.df$x <- img.coords[,1]
img.df$y <- img.coords[,2]
# Convert to a SpatialPointsDataFrame
coordinates(img.df) <- c("x", "y")
proj4string(img.df) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
# Create a boundary polygon
coords.mat <- matrix(c(W,E,E,W,W,N,N,S,S,N), ncol = 2)
# Create a list to hold the corner coords
bdry.vec <- vector(mode="list", length=1)
# Build the objects leading to a SpatialPolygonsDataFrame
bdry.vec[[1]] <- Polygons(list(Polygon(coords.mat)), ID="1")
bdry.poly <- SpatialPolygons(bdry.vec,
   proj4string = CRS("+proj=utm +zone=10 +ellps=WGS84"))
# Exclude points outside the boundary
in.f <- over(img.df,bdry.poly)
f.img <- img.df[which(!is.na(in.f)),]
names(f.img@data) <- "IR"
gridded(f.img) <- TRUE
greys <- grey(0:255 / 255)
spplot(f.img, zcol = "IR", col.regions = greys, # Fig. 1.2b
  xlim = c(W,E),  ylim = c(S,N), scales = list(draw = TRUE),
  xlab = "Easting", ylab = "Northing", main = "Infrared Band Digital Number")

# --------------------------------------------------
  
# Construction of Fig. 1.2c
# Interpolate yield to a 10 meter grid
library(gstat)
library(maptools)
# This file is created in Section 6.2.3
data.Yield4.2 <- read.csv("created\\Set4.2yld96cleaned.csv")
data.Yield4.2$ID <- 1:nrow(data.Yield4.2)
cell.size <- 10
N <- 4267868
S <- 4267513
E <- 592867
W <- 592082
grid.xy <- expand.grid(x = seq(W, E, cell.size), y = seq(N, S, -cell.size))
coordinates(grid.xy) <- ~x + y
gridded(grid.xy) = TRUE
proj4string(grid.xy) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
# Remove yield trend prior to kriging
data.Yield4.2$x <- with(data.Yield4.2, Easting - min(Easting))
data.Yield4.2$y <- with(data.Yield4.2, Northing - min(Northing))
trend.lm <- lm(Yield ~ x + y + I(x^2) + I(y^2) +
   I(x*y), data = data.Yield4.2)
data.Yield4.2$trend <- fitted(trend.lm)
data.Yield4.2$YieldDT <- data.Yield4.2$Yield - fitted(trend.lm)
# Select every nth value
krig.dat <- data.Yield4.2[-which(data.Yield4.2$ID %% cell.size != 0),]
coordinates(krig.dat) <- c("Easting", "Northing")
proj4string(krig.dat) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
# Compute kriged detrended yield
yield.vgm <- variogram(YieldDT ~ 1, krig.dat)
yield.fit <- fit.variogram(yield.vgm, model = vgm(100000, "Sph", 200,10000))
# Check the fit visually and compute kriged interploation
plot(yield.vgm, yield.fit, col = "black")
yield.krig <- krige(YieldDT ~ 1, krig.dat, grid.xy, model = yield.fit)
# Add trend back
x <- coordinates(yield.krig)[,1] - W
y <- coordinates(yield.krig)[,2] - S
b <- coef(trend.lm)
yield.trend <- b[1] + b[2]*x + b[3]*y + b[4]*x^2 + b[5]*y^2 + b[6]*x*y
yield.krig@data$var1.pred <-
   yield.krig@data$var1.pred + yield.trend
greys <- grey(255:0 / 255)
spplot(yield.krig["var1.pred"], col.regions = greys,
  xlim = c(W,E), ylim = c(S,N), scales = list(draw = TRUE),
  xlab = "Easting", ylab = "Northing",
  main = "Grain Yield (kg/ha)")  # Fig. 1.2c





  







