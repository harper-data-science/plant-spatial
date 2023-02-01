# Create a plot showing the different kinds of data in Field 4.2
library(spatstat)
library(sf)
library(gstat)
library(terra)

data.4.2.May.ter <- rast("set4\\Set4.20596.tif")
crs(data.4.2.May.ter) <- "EPSG:32610" # UTM Zone 10N
data.Set4.2Yield96raw <- read.csv("Set4\\Set4.296wheatyield.csv", header = TRUE)
data.Set4.2 <- read.csv("set4\\set4.296sample.csv",header = TRUE)
data.Set4.2EC <- read.csv("set4\\Set4.2EC.csv",header = TRUE)
data.Set4.2Yield96raw.sf <- st_as_sf(data.Set4.2Yield96raw, 
   coords = c("Easting","Northing"))
data.Set4.2.sf <- st_as_sf(data.Set4.2,
 coords = c("Easting","Northing"))
data.Set4.2EC.sf <- st_as_sf(data.Set4.2EC,
 coords = c("Easting","Northing"))
crs(data.4.2.May.ter) <- "EPSG:32610" # UTM Zone 10N
st_crs(data.Set4.2Yield96raw.sf) <- "EPSG:32610" # UTM Zone 10N
st_crs(data.Set4.2.sf) <- "EPSG:32610" # UTM Zone 10N
st_crs(data.Set4.2EC.sf) <- "EPSG:32610" # UTM Zone 10N

greys <- grey(0:255 / 255)
par(mai = c(1,1,1,1))
image(data.4.2.May.ter, y = 1, col = greys,
  xlim = c(592285,592295), ylim = c(4267710,4267730),
  axes = TRUE)  # Fig. 6.11
title(main = expression(Field~4.2~May~IR~Yield*","~Sample~and~EC[a]),
  xlab = "Easting", ylab = "Northing", cex.main = 1.5, cex.lab = 1.5)
plot(data.Set4.2.sf["ID"], add = TRUE, pch = 8, cex = 1.5,
   col = "black")
plot(data.Set4.2Yield96raw.sf["Yield"], add = TRUE, pch = 1,
   cex = 1.5, col = "black")
plot(data.Set4.2EC.sf["ECto30"], add = TRUE, pch = 3, cex = 1.5,
  col = "black")
legend(592296, 4267715, c("Yield", "ECa", "Sample"),
  pt.cex = 1.5, pch = c(1,3,8))

# Color version
greens <- rgb((50:255)/255, red = 0, blue = 0)
image(data.4.2.May.ter, y = 1, col = greens,
  xlim = c(592285,592295), ylim = c(4267710,4267730),
  axes = TRUE)  
title(main = expression(Field~4.2~May~IR~Yield*","~Sample~and~EC[a]),
  xlab = "Easting", ylab = "Northing", cex.main = 1.5, cex.lab = 1.5)
plot(data.Set4.2.sf["ID"], add = TRUE, pch = 8, cex = 1.5,
   col = "red")
plot(data.Set4.2Yield96raw.sf["Yield"], add = TRUE, pch = 1,
   cex = 1.5, col = "blue")
plot(data.Set4.2EC.sf["ECto30"], add = TRUE, pch = 3, cex = 1.5,
  col = "yellow")
legend(592296, 4267715, c("Yield", "ECa", "Sample"),
  pt.cex = 1.5, pch = c(1,3,8))
legend(592296, 4267715, c("Yield", "ECa", "Sample"),
  pt.cex = 1.5, pch = c(1,3,8), col = c("blue", "yellow", "red"))

# Create 10m buffers around sample points
pt.list <- 1 # This is for the check below
xy <- st_coordinates(data.Set4.2.sf)[1,]
samp.rad <- 10
d <- disc(radius = samp.rad, centre = xy)
buf <- cbind(d[[4]][[1]]$x, d[[4]][[1]]$y)
# Close the polygon
buf <- rbind(buf, buf[1,])
buflist <- list(buf)
for (i in 2:nrow(data.frame(data.Set4.2.sf))){
  xy <- st_coordinates(data.Set4.2.sf)[i,]
  d <- disc(radius = samp.rad, centre = xy)
  buf <- cbind(d[[4]][[1]]$x, d[[4]][[1]]$y)
  buf <- rbind(buf, buf[1,])
  buflist <- c(buflist, list(buf))
  pt.list <- c(pt.list, i)
  }
buffer.10m.sf <- st_multipolygon(list(buflist))  
 
plot(buffer.10m.sf, xlim = c(592270,592370),
  ylim = c(4267700,4267800), axes = TRUE, lwd = 2,
   main = "Field 4.2 Buffered Sample Locations",    #Fig 6.12a
  xlab = "Easting", ylab = "Northing",
  cex.main = 1.5, cex.lab = 1.5)
plot(data.Set4.2.sf["ID"], add = TRUE, pch = 19, col = "black")
plot(data.Set4.2Yield96raw.sf["Yield"], add = TRUE, pch = 1,
  col = "black")

#Check that the IDs match using sf objects
all.equal(data.Set4.2.sf$ID, pt.list)

# Compare buffered yield means with interpolated data
yield.bufmean <- numeric(nrow(data.frame(data.Set4.2.sf)))
for (i in 1:length(yield.bufmean)){
   xy <- st_coordinates(data.Set4.2.sf)[i,]
   samp.rad <- 10
   d <- disc(radius = samp.rad, centre = xy)
   buf <- cbind(d[[4]][[1]]$x, d[[4]][[1]]$y)
   buf <- rbind(buf, buf[1,])
   buf.sf <- st_polygon(list(buf))
   in.buf <- st_intersects(buf.sf, data.Set4.2Yield96raw.sf)
   yield.bufmean[i] <- 
      mean(data.Set4.2Yield96raw$Yield[in.buf[[1]]])
   }
   
# Interpolation
# Make sure we use gstat::idw and not spatstat::idw
yield.idw <- gstat::idw(Yield ~ 1, data.Set4.2Yield96raw.sf,
  data.Set4.2.sf, idp = 2, nmax = 12)
r <- cor(yield.bufmean, yield.idw$var1.pred)
plot(yield.bufmean, yield.idw$var1.pred,
   xlab = "10 m Buffer Mean Yield", ylab = "IDW Yield",
   main = "Comparison of Yield Estimates (kg/ha), Field 4.2",
   cex.main = 1.5, cex.lab = 1.5)  #Fig 6.12b
lines(c(2000,6000),c(2000,6000))
text(5000, 3000, paste("r = ",as.character(round(r,2))), cex = 1.5)

# Save the interpolated yield
moist.idw <- gstat::idw(Moisture ~ 1,
   data.Set4.2Yield96raw.sf, data.Set4.2.sf, idp = 2, nmax = 12)
yield.idw.df <- data.frame(Yield = yield.idw$var1.pred,
   GrnMoist = moist.idw$var1.pred,
   Easting = st_coordinates(yield.idw)[,1],
   Northing = st_coordinates(yield.idw)[,2])
write.csv(yield.idw.df, "created\\set4.2yld96ptsidw.csv")

