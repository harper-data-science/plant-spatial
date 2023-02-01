# 6.1
library(maptools)
library(rgdal)
data.Set2 <- read.csv("set2\\set2data.csv", header = TRUE)
coordinates(data.Set2) <- c("Longitude", "Latitude")
proj4string(data.Set2) <- CRS("+proj=longlat +datum=WGS84")
plot(data.Set2, axes = TRUE)
data.Set2K <- data.Set2[which((data.Set2@coords[,2] > 40) &
   (data.Set2@coords[,2] < 41)),]
data.Set2K.UTM <- spTransform(data.Set2K,
   CRS("+proj=utm +zone=10 +ellps=WGS84"))
par(mai = c(1,1,1,1))
plot(data.Set2K, pch = 1, axes = TRUE, cex = 0.5)
title(main = "Klamath Range in WGS 84 Coordinates", xlab = "Longitude",
  ylab = "Latitude", cex.main = 2, cex.lab = 1.5)   
plot(data.Set2K.UTM, pch = 1, axes = TRUE, cex = 0.5)
title(main = "Klamath Range in UTM Coordinates", xlab = "Easting (m)",
  ylab = "Northing (m)", cex.main = 2, cex.lab = 1.5)
  
# 6.2
# Apply moran.plot() to detect discordant values in Set 2
library(spdep)
data.Set2 <- read.csv("Set2\\Set2data.csv", header = TRUE)
coordinates(data.Set2) <- c("Longitude","Latitude")
nlist <- knn2nb(knearneigh(data.Set2, k = 4, longlat = TRUE))
W <- nb2listw(nlist, style = "W")
# apply the function moran.plot()
mp <- moran.plot(data.Set2$QUDO_BA, W)
mp.mat <- matrix(as.numeric(mp$is.inf), ncol = 6)
# Identify as discordant any data record with any positive
# diagnostic value
discordant.data <-
     which(rowSums(matrix(as.numeric(mp$is.inf), ncol = 6)) > 0)
length(discordant.data)
library(maps)
data(stateMapEnv)
cal.map <- map("state", "california",
   fill=TRUE, col="transparent",  plot = FALSE)
cal.poly <- map2SpatialPolygons(cal.map, "California")
proj4string(cal.poly) <- CRS("+proj=longlat +datum=WGS84")
# Plot a map of the locations of discordant points
par(mai = c(1,1,1,1))
plot(cal.poly, axes = FALSE) # Fig 6.1
title(main = "Data Set 2 Discordant Data",cex.main = 2)
plot(data.Set2[discordant.data,], add = TRUE, pch = 11, cex = 0.1)
deg.per.km <- 360 / 40075
lat <- 34
text(-124, lat+0.35, "0")
text(-122, lat+0.35, "200 km")
deg.per.km <- deg.per.km * cos(pi*lat / 180)
SpatialPolygonsRescale(layout.scale.bar(), offset = c(-124, lat),
  scale = 200 * deg.per.km, fill=c("transparent","black"),
  plot.grid = FALSE)
SpatialPolygonsRescale(layout.north.arrow(), offset = c(-124, 36),
  scale = 1, plot.grid = FALSE)

   
# 6.3
library(maptools)
grid.xy <- expand.grid(x = seq(0,30,10), y = seq(20,0,-10))
class(grid.xy)
grid.xy$z <- 1:nrow(grid.xy)
coordinates(grid.xy) <- c("x", "y")
class(grid.xy)
plot(grid.xy, pch = grid.xy$z)
spplot(grid.xy, zcol = "z")
gridded(grid.xy) <- TRUE
class(grid.xy)   
plot(grid.xy, pch = grid.xy$z)
spplot(grid.xy, zcol = "z")

# 6.4
library(maptools)
library(spatstat)
data.Set4.2 <- read.csv("set4\\set4.296sample.csv", header = TRUE)
bdry.spdf <- readShapePoly("created\\Set4.2bdry")
cell.ppp <- ppp(data.Set4.2$Easting, data.Set4.2$Northing,
     window = owin(bbox(bdry.spdf)[1,], bbox(bdry.spdf)[2,]))
pop.data <- readShapePoints("Created\\Set4.2pop")
sample.coords <- cbind(data.Set4.2$Easting, data.Set4.2$Northing)
closest.point <- function(sample.pt, grid.data){
   dist.sq <- (slot(grid.data, "coords")[,1]-sample.pt[1])^2 +
      (slot(grid.data, "coords")[,2]-sample.pt[2])^2
   return(which.min(dist.sq))
}
samp.pts <- apply(sample.coords, 1, closest.point, grid.data = pop.data)
data.Set4.2$Yield <- pop.data$Yield[samp.pts]
#coordinates(data.Set4.2) <- c("Easting", "Northing")
#proj4string(data.Set4.2) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
thsn.pp <- dirichlet(cell.ppp)
thsn.sp <- as(thsn.pp, "SpatialPolygons")
thsn.spdf <- SpatialPolygonsDataFrame(thsn.sp, data.Set4.2, FALSE)
# Check that IDs match
coordinates(data.Set4.2) <- c("Easting", "Northing")
plot(data.Set4.2, pch = 1, cex = 0.1)
text(slot(data.Set4.2,"coords")[,1] + 25,
     slot(data.Set4.2,"coords")[,2],
     labels=as.character(data.Set4.2$ID), cex = 1)
plot(thsn.spdf)
y <- lapply(thsn.spdf@polygons, slot, "labpt")
y.loc <- matrix(0, length(y), 2)
for (i in 1:length(y)) y.loc[i,] <- unlist(y[[i]])
text(y.loc, labels=lapply(thsn.spdf@polygons, slot, "ID"))
greys <- grey(0:255 / 255)
trellis.device(color = FALSE)
spplot(thsn.spdf, zcol = "Yield",
    col.regions = greys, scales = list(draw = TRUE))

# 6.5
library(sf)
library(gstat)
library(maptools)
data.Set4.1Yield96raw <- read.csv("Set4\\Set4.196wheatyield.csv", header = TRUE)
data.Set4.1 <- read.csv("set4\\set4.196sample.csv",header = TRUE)
coordinates(data.Set4.1Yield96raw) <- c("Easting","Northing")
coordinates(data.Set4.1) <- c("Easting","Northing")
proj4string(data.Set4.1Yield96raw) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
proj4string(data.Set4.1) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
grid.xy <- data.frame(slot(data.Set4.1, "coords"))
coordinates(grid.xy) <- c("Easting", "Northing")
proj4string(grid.xy) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
yield.idw <- idw(Yield ~ 1, data.Set4.1Yield96raw, grid.xy, idp = 2, nmax = 12)
moist.idw <- idw(Moisture ~ 1,
   data.Set4.1Yield96raw, grid.xy, idp = 2, nmax = 12)
yield.idw.df <- data.frame(Yield = yield.idw$var1.pred,
   Easting = coordinates(yield.idw)[,1],
   Northing = coordinates(yield.idw)[,2])
write.csv(yield.idw.df, "created\\set4.1yld96ptsidw.csv")
 
# 6.6
library(maptools) 
library(gstat)
library(sf)
data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
coordinates(data.Set4.1) <- c("Easting", "Northing")
proj4string(data.Set4.1) <- CRS("+proj=longlat +datum=WGS84")
bdry.sf <- st_read("created\\Set419697bdry.shp")
bdry.spdf <- as(bdry.sf, "Spatial")
proj4string(bdry.spdf) <- CRS("+proj=longlat +datum=WGS84")
# Interpolate to a 5 meter grid
print(Left <- bbox(bdry.spdf)[1,1])
print(Right <- bbox(bdry.spdf)[1,2])
print(Top <- bbox(bdry.spdf)[2,2])
print(Bottom <- bbox(bdry.spdf)[2,1])
cell.size <- 5
grid.xy <- expand.grid(Easting = seq(Left,Right,cell.size),
  Northing = seq(Top,Bottom,-cell.size))
coordinates(grid.xy) <- c("Easting", "Northing")
clay.idw <- idw(Clay ~ 1, data.Set4.1, grid.xy)
proj4string(clay.idw) <- proj4string(bdry.spdf)
# Plot the grid before clip overlay
plot(clay.idw)
grid.4.1ovr <- over(clay.idw, bdry.spdf)
class(grid.4.1ovr)
names(grid.4.1ovr)
clay.ovr <- clay.idw[!is.na(grid.4.1ovr$z),]
# You can also write clay.ovr <- clay.idw[bdry.spdf,]
plot(bdry.spdf)
points(clay.ovr)

# 6.7
library(maptools)
data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
# Compute coordinates for detrending
min.E <- min(data.Set4.1$Easting)
min.N <- min(data.Set4.1$Northing)
data.Set4.1$x <- data.Set4.1$Easting - min.E
data.Set4.1$y <- data.Set4.1$Northing - min.N
coordinates(data.Set4.1) <- c("Easting", "Northing")
bdry.spdf <- readShapePoly("created\\Set4.19697bdry")
# Interpolate to a 5 meter grid
library(gstat)
print(Left <- bbox(bdry.spdf)[1,1])
print(Right <- bbox(bdry.spdf)[1,2])
print(Top <- bbox(bdry.spdf)[2,2])
print(Bottom <- bbox(bdry.spdf)[2,1])
cell.size <- 5
grid.xy <- expand.grid(Easting = seq(Left,Right,cell.size),
  Northing = seq(Top,Bottom,-cell.size))
coordinates(grid.xy) <- c("Easting", "Northing")
# Without detrending
data.var <- variogram(Clay ~ 1, data.Set4.1, cutoff = 600)
data.fit <- fit.variogram(data.var, model = vgm(1, "Gau", 700, 1))
par(mai = c(1,1,1,1))
plot(data.var, data.fit, col = "black",
   cex.main = 2, cex.lab = 1.5,
   main = "Yield Variogram",
   xlab = expression(bold("lag h")), xlim = c(0,400),
   ylab = (expression(bold(hat(gamma)*(h)))))
clay.krig1 <- krige(Clay ~ 1, data.Set4.1, grid.xy,
   model = data.fit)
spplot(clay.krig1, zcol = "var1.pred")
# Now do detrending
trend.lm <- lm(Clay ~ x + y + I(x^2) +
  I(y^2) + I(x*y), data = data.Set4.1)
data.Set4.1$ClayDet <- data.Set4.1$Clay - predict(trend.lm)
data.var <- variogram(ClayDet ~ 1, data.Set4.1, cutoff = 600)
data.fit <- fit.variogram(data.var, model = vgm(1, "Sph", 700, 1))
par(mai = c(1,1,1,1))
plot(data.var, data.fit, col = "black",
   cex.main = 2, cex.lab = 1.5,
   main = "Yield Variogram",
   xlab = expression(bold("lag h")), xlim = c(0,400),
   ylab = (expression(bold(hat(gamma)*(h)))))
clay.krig2 <- krige(ClayDet ~ 1, data.Set4.1, grid.xy,
   model = data.fit)
grid.xy.df <- as(grid.xy, "data.frame")
xy.df <- data.frame(x = grid.xy.df$Easting - min.E,
  y = grid.xy.df$Northing - min.N)
clay.trend <- clay.krig2$var1.pred + predict(trend.lm, xy.df)
clay.krig2@data$ClayKrig <- clay.trend
spplot(clay.krig2, zcol = "ClayKrig")
clay.dif <- clay.trend - clay.krig1$var1.pred
clay.krig2@data$dif <- clay.dif
proj4string(grid.xy) <- proj4string(bdry.spdf)
grid.4.1 <- over(grid.xy, bdry.spdf)
krig.dif <- clay.krig2[!is.na(grid.4.1$z),]
spplot(clay.krig2, zcol = "dif")

#6.8
closest.point <- function(sample.pt, grid.data){
   dist.sq <- (slot(grid.data, "coords")[,1]-sample.pt[1])^2 +
      (slot(grid.data, "coords")[,2]-sample.pt[2])^2
   return(which.min(dist.sq))
}

library(maptools)
data.Set4.2 <- read.csv("set4\\set4.296sample.csv", header = TRUE)
pop.data <- readShapePoints("Created\\Set4.2pop")
bdry.spdf <- readShapePoly("created\\Set4.2sampbdry")
sample.coords <- cbind(data.Set4.2$Easting, data.Set4.2$Northing)
# Find the yield at the closest sample points
samp.pts <- apply(sample.coords, 1, closest.point, grid.data = pop.data)
data.Set4.2$Yield <- pop.data$Yield[samp.pts]
coordinates(data.Set4.2) <- c("Easting", "Northing")
proj4string(data.Set4.2) <- CRS("+proj=utm +zone=10 +ellps=WGS84")


# Find the yield at the closest sample points
samp.pts <- apply(sample.coords, 1, closest.point, grid.data = pop.data)
data.Set4.2$Yield <- pop.data$Yield[samp.pts]
coordinates(data.Set4.2) <- c("Easting", "Northing")
proj4string(data.Set4.2) <- CRS("+proj=utm +zone=10 +ellps=WGS84")

gridded(grid.xy) = TRUE
proj4string(grid.xy) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
greys <- grey(0:200 / 255)

# Kriging
data.var <- variogram(Yield ~ 1, data.Set4.2, cutoff = 600)
data.fit <- fit.variogram(data.var, model = vgm(1, "Sph", 700, 1))
par(mai = c(1,1,1,1))
plot(data.var, data.fit, col = "black",
   cex.main = 2, cex.lab = 1.5,
   main = "Yield Variogram",   # Fig. 6.6
   xlab = expression(bold("lag h")), xlim = c(0,400),
   ylab = (expression(bold(hat(gamma)*(h)))))

r1 <- seq(0,250,25)
a <- data.fit$range[2]
g.sph <- 1.5 * r1 / a -0.5 * (r1 / a)^3
b <- data.fit$psill[1]
cc <- data.fit$psill[2]
g.hat <- b + cc * g.sph
c.hat <- cc - g.hat

par(mai = c(1,1,1,1))
plot(r1, c.hat, type = "l",
   ylim = c(0,2000000), xlim = c(0,400),
   xlab = expression(bold("lag h")),
   ylab = expression(bold(paste(tilde(C), "(h)"))),
   main = expression(Correlogram~tilde(C)*(h)))
lines(c(250,400),c(0,0))

yield.krig <- krige(Yield ~ 1, data.Set4.2, grid.xy,
   model = data.fit)
spplot(yield.krig["var1.pred"], col.regions = greys,
  xlim = c(Left,Right), scales = list(draw = TRUE),
  xlab = "Easting", ylab = "Northing",
  main = "Field 4.2 Kriged Yield")



# Compare IDW and Kriged values of yield
library(maptools)
data.Set4.2 <- read.csv("set4\\set4.296sample.csv", header = TRUE)
pop.data <- readShapePoints("Created\\Set4.2pop")
sample.coords <- cbind(data.Set4.2$Easting, data.Set4.2$Northing)
library(gstat)
Left <- 592102.5
Right <- 592817.5
Top <- 4267857.5
Bottom <- 4267502.5
cell.size <- 5
grid.xy <- expand.grid(x = seq(Left,Right,cell.size),
  y = seq(Top,Bottom,-cell.size))
coordinates(grid.xy) <- ~x + y

closest.point <- function(sample.pt, grid.data){
   dist.sq <- (slot(grid.data, "coords")[,1]-sample.pt[1])^2 +
      (slot(grid.data, "coords")[,2]-sample.pt[2])^2
   return(which.min(dist.sq))
}

# Find the yield at the closest sample points
samp.pts <- apply(sample.coords, 1, closest.point,
   grid.data = pop.data)
data.Set4.2$Yield <- pop.data$Yield[samp.pts]
coordinates(data.Set4.2) <- c("Easting", "Northing")

# Interpolate to to a 5 meter grid
yield.idw <- idw(Yield ~ 1, data.Set4.2, grid.xy)
idw.pred <- yield.idw@data$var1.pred
error.idw <- pop.data@data$Yield - idw.pred
hist(error.idw)

data.var <- variogram(Yield ~ 1, data.Set4.2, cutoff = 600)
data.fit <- fit.variogram(data.var, model = vgm(1, "Sph", 700, 1))
par(mai = c(1,1,1,1))
plot(data.var, data.fit, col = "black",
   cex.main = 2, cex.lab = 1.5,
   main = "Yield Variogram",   
   xlab = expression(bold("lag h")), xlim = c(0,400),
   ylab = (expression(bold(hat(gamma)*(h)))))
yield.krige <- krige(Yield ~ 1, data.Set4.2, grid.xy,
   model = data.fit)
krige.pred <- yield.krige@data$var1.pred
error.krige <- pop.data@data$Yield - krige.pred
hist(error.krige)
hist(error.idw - error.krige)
sum(error.idw^2)
sum(error.krige^2)

pop.data@data$err.idw <- error.idw
pop.data@data$err.krige <- error.krige
spplot(pop.data, zcol = c("err.idw", "err.krige"))

# 6.9
library(maptools)
data.Set4.2 <- read.csv("set4\\set4.296sample.csv", header = TRUE)
sample.coords <- cbind(data.Set4.2$Easting, data.Set4.2$Northing)
bdry.spdf <- readShapePoly("created\\Set4.2bdry")
coordinates(data.Set4.2) <- c("Easting", "Northing")

# Interpolate to a 5 meter grid
library(gstat)
print(Left <- bbox(bdry.spdf)[1,1])
print(Right <- bbox(bdry.spdf)[1,2])
print(Top <- bbox(bdry.spdf)[2,2])
print(Bottom <- bbox(bdry.spdf)[2,1])
cell.size <- 5
grid.xy <- expand.grid(Easting = seq(Left,Right,cell.size),
  Northing = seq(Top,Bottom,-cell.size))
coordinates(grid.xy) <- c("Easting", "Northing")
gridded(grid.xy) = TRUE
greys <- grey(0:200 / 255)

# First do kriging
clay.var <- variogram(Clay ~ 1, data.Set4.2, cutoff = 600)
clay.fit <- fit.variogram(clay.var, model = vgm(1, "Sph", 700, 1))
par(mai = c(1,1,1,1))
plot(clay.var, clay.fit, col = "black",
   cex.main = 2, cex.lab = 1.5,
   main = "Clay Variogram", 
   xlab = expression(bold("lag h")), xlim = c(0,400),
   ylab = (expression(bold(hat(gamma)*(h)))))
# Range = about 400
clay.krige <- krige(Clay ~ 1, data.Set4.2, grid.xy,
   model = clay.fit)

# Now do cokriging
# Create the gstat object
# First do a variogram of EC to match the ranges
cor(data.Set4.2$Clay, data.Set4.2$EM38F)
cor(data.Set4.2$Clay, data.Set4.2$EM38B)
EC.var <- variogram(EM38B ~ 1, data.Set4.2, cutoff = 600)
EC.fit <- fit.variogram(EC.var, model = vgm(1, "Sph", 700, 1))
plot(EC.var, EC.fit, col = "black",
   cex.main = 2, cex.lab = 1.5,
   main = "EC Variogram", 
   xlab = expression(bold("lag h")), xlim = c(0,400),
   ylab = (expression(bold(hat(gamma)*(h)))))
# Range = about 200, so use 300 from the combined range
   
g.cok <- gstat(NULL, "Clay", Clay ~ 1, data.Set4.2)
g.cok <- gstat(g.cok, "EM38B", EM38B ~ 1, data.Set4.2)
g.var <- variogram(g.cok)
g.fit <- fit.lmc(g.var, g.cok, vgm(1, "Sph", 300, 1))
g.cokrige <- predict(g.fit, grid.xy)
# Didn't work. Try reducting the size of the data set
names(g.cokrige)
spplot.vcov(g.cokrige)
spplot(g.cokrige["Clay.pred"], col.regions = greys,
  xlim = c(Left,Right), scales = list(draw = TRUE),
  xlab = "Easting", ylab = "Northing", 
  main = "Field 4.2 Cokriged Clay")

spplot(g.cokrige["Clay.pred"], col.regions = greys,
  xlim = c(Left,Right), scales = list(draw = TRUE),
  xlab = "Easting", ylab = "Northing", 
  main = "Field 4.2 Cokriged Clay") 
  
spplot(clay.krige["var1.pred"], col.regions = greys,
  xlim = c(Left,Right), scales = list(draw = TRUE),
  xlab = "Easting", ylab = "Northing", 
  main = "Field 4.2 Kriged Clay")   # Fig. 6.10b


# 6.10
# Create a plot showing the different kinds of data in Field 4.2
library(maptools)
data.Yield4.2 <- read.csv("Set4\\Set4.296wheatyield.csv", header = TRUE)
data.Set4.2 <- read.csv("set4\\set4.296sample.csv",header = TRUE)
coordinates(data.Yield4.2) <- c("Easting","Northing")
coordinates(data.Set4.2) <- c("Easting","Northing")
library(spatstat)
xy <- data.Set4.2@coords[1,]
samp.rad <- 10
d <- disc(radius = samp.rad, centre = xy)
buffer.10m <- as(d, "SpatialPolygons")
for (i in 2:nrow(data.Set4.2@data)){
   xy <- data.Set4.2@coords[i,]
   d <- disc(radius = samp.rad, centre = xy)
   sp.d <- as(d, "SpatialPolygons")
   sp.d@polygons[[1]]@ID <- as.character(i)
   buffer.10m <- spRbind(buffer.10m, sp.d)
}
print(yield.bufmean <- over(buffer.10m, data.Yield4.2, fn = mean))
yield.buf <- over(data.Yield4.2,buffer.10m)
data.Yield4.2$bufID <- yield.buf
yield.inbuf <- slot(data.Yield4.2, "data")[!is.na(yield.buf),]
print(yield.bufmean2 <- tapply(data.Yield4.2$Yield, data.Yield4.2$bufID, mean))

# 6.11
library(rgdal)
library(maptools)
library(gstat)
data.Set4.1Yield96raw <- read.csv("Set4\\Set4.196wheatyield.csv", header = TRUE)
data.Set4.1 <- read.csv("set4\\set4.196sample.csv",header = TRUE)
coordinates(data.Set4.1Yield96raw) <- c("Easting","Northing")
coordinates(data.Set4.1) <- c("Easting","Northing")
proj4string(data.Set4.1Yield96raw) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
proj4string(data.Set4.1) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
grid.xy <- data.frame(slot(data.Set4.1, "coords"))
coordinates(grid.xy) <- c("Easting", "Northing")
proj4string(grid.xy) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
yield.idw <- gstat::idw(Yield ~ 1, data.Set4.1Yield96raw, grid.xy, idp = 2, nmax = 12)
moist.idw <- gstat::idw(Moisture ~ 1,
   data.Set4.1Yield96raw, grid.xy, idp = 2, nmax = 12)
yield.idw.df <- data.frame(Yield = yield.idw$var1.pred,
   GrnMoist = moist.idw$var1.pred,
   Easting = coordinates(yield.idw)[,1],
   Northing = coordinates(yield.idw)[,2])
write.csv(yield.idw.df, "created\\set4.1yld96ptsidw.csv")
