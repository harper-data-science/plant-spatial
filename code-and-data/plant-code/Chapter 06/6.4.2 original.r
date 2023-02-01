# Create a plot showing the different kinds of data in Field 4.2
library(rgdal)
library(maptools)
library(spatstat)
library(sf)
library(gstat)

data.4.2.May <- readGDAL("set4\\Set4.20596.tif")
data.Set4.2Yield96raw <- read.csv("Set4\\Set4.296wheatyield.csv", header = TRUE)
data.Set4.2 <- read.csv("set4\\set4.296sample.csv",header = TRUE)
data.Set4.2EC <- read.csv("set4\\Set4.2EC.csv",header = TRUE)
coordinates(data.Set4.2Yield96raw) <- c("Easting","Northing")
coordinates(data.Set4.2) <- c("Easting","Northing")
coordinates(data.Set4.2EC) <- c("Easting","Northing")
proj4string(data.4.2.May) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
proj4string(data.Set4.2Yield96raw) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
proj4string(data.Set4.2) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
proj4string(data.Set4.2EC) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
greys <- grey(0:255 / 255)
par(mai = c(1,1,1,1))
image(data.4.2.May, "band1", col = greys,
  xlim = c(592285,592295), ylim = c(4267710,4267730),
  axes = TRUE)  # Fig. 6.11
title(main = expression(Field~4.2~May~IR~Yield*","~Sample~and~EC[a]),
  xlab = "Easting", ylab = "Northing", cex.main = 1.5, cex.lab = 1.5)
plot(data.Set4.2, add = TRUE, pch = 8, cex = 1.5)
plot(data.Set4.2Yield96raw, add = TRUE, pch = 1, cex = 1.5)
plot(data.Set4.2EC, add = TRUE, pch = 3, cex = 1.5)
legend(592296, 4267715, c("Yield", "ECa", "Sample"),
  pt.cex = 1.5, pch = c(1,3,8))

# Color version
greens <- rgb((50:255)/255, red = 0, blue = 0)
image(data.4.2.May, "band1", col = greens,
  xlim = c(592285,592295), ylim = c(4267710,4267730),
  axes = TRUE)  
title(main = expression(Field~4.2~May~IR~Yield*","~Sample~and~EC[a]),
  xlab = "Easting", ylab = "Northing", cex.main = 1.5, cex.lab = 1.5)
plot(data.Set4.2, add = TRUE, pch = 8, cex = 2, col = "red")
plot(data.Set4.2Yield96raw, add = TRUE, pch = 1, cex = 1.5, col = "blue")
plot(data.Set4.2EC, add = TRUE, pch = 3, cex = 1.5, col = "yellow")
legend(592296, 4267715, c("Yield", "ECa", "Sample"),
  pt.cex = 1.5, pch = c(1,3,8), col = c("blue", "yellow", "red"))

# Create 10m buffers around sample points
xy <- coordinates(data.Set4.2)[1,]
samp.rad <- 10
# Buffers are created 1 at a time and added w/ spRbind
d <- disc(radius = samp.rad, centre = xy)
buffer.10m <- as(d, "SpatialPolygons")
for (i in 2:nrow(data.Set4.2@data)){
  xy <- coordinates(data.Set4.2)[i,]
  d <- disc(radius = samp.rad, centre = xy)
  sp.d <- as(d, "SpatialPolygons")
  sp.d@polygons[[1]]@ID <- as.character(i)
  buffer.10m <- spRbind(buffer.10m, sp.d)
}

par(mai = c(1,1,1,1))
plot(buffer.10m, xlim = c(592270,592370), ylim = c(4267700,4267800),
  axes = TRUE, lwd = 2)
title(main = "Field 4.2 Buffered Sample Locations",
  xlab = "Easting", ylab = "Northing",
  cex.main = 1.5, cex.lab = 1.5)  #Fig 6.12a
plot(data.Set4.2, add = TRUE, pch = 19)
plot(data.Set4.2Yield96raw, add = TRUE, pch = 1)

#Check that the IDs match using sf objects
data.Set4.2.sf <- as(data.Set4.2, "sf")
buffer.10m.sf <- as(buffer.10m, "sf")
head(buffer.10m.sf)
st_crs(buffer.10m.sf) <- "+proj=utm +zone=10 +ellps=WGS84"
y <- st_contains(buffer.10m.sf, data.Set4.2.sf)
unlist(y)
all.equal(data.Set4.2.sf$ID, unlist(y))


# Check that IDs match by plotting both
plot(data.Set4.2, pch = 1, cex = 0.1)
text(coordinates(data.Set4.2)[,1] + 25,
     coordinates(data.Set4.2)[,2],
     labels=as.character(data.Set4.2$ID), cex = 1)
y <- lapply(buffer.10m@polygons, slot, "labpt")
buffer.10m.loc <- matrix(0, length(y), 2)
for (i in 1:length(y)) buffer.10m.loc[i,] <- unlist(y[[i]])
text(buffer.10m.loc,labels=lapply(buffer.10m@polygons, slot, "ID"))

# Compare buffered yield means with interpolated data
proj4string(buffer.10m) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
yield.bufmean <- over(buffer.10m, data.Set4.2Yield96raw, fn = mean)

# Interpolation
grid.xy <- data.frame(slot(data.Set4.2, "coords"))
coordinates(grid.xy) <- c("Easting", "Northing")
proj4string(grid.xy) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
# Make sure we use gstat::idw and not spatstat::idw
yield.idw <- gstat::idw(Yield ~ 1, data.Set4.2Yield96raw, grid.xy, idp = 2, nmax = 12)
r <- cor(yield.bufmean, yield.idw$var1.pred)
plot(yield.bufmean$Yield, yield.idw$var1.pred,
   xlab = "10 m Buffer Mean Yield", ylab = "IDW Yield",
   main = "Comparison of Yield Estimates (kg/ha), Field 4.2",
   cex.main = 1.5, cex.lab = 1.5)  #Fig 6.12b
lines(c(2000,6000),c(2000,6000))
text(5000, 3000, paste("r = ",as.character(round(r,2))[2]), cex = 1.5)

# Save the interpolated yield
moist.idw <- gstat::idw(Moisture ~ 1,
   data.Set4.2Yield96raw, grid.xy, idp = 2, nmax = 12)
yield.idw.df <- data.frame(Yield = yield.idw$var1.pred,
   GrnMoist = moist.idw$var1.pred,
   Easting = coordinates(yield.idw)[,1],
   Northing = coordinates(yield.idw)[,2])
write.csv(yield.idw.df, "created\\set4.2yld96ptsidw.csv")

