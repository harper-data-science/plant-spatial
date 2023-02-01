#Fig. 1.8
library(maps)
library(maptools)
uy.map <- map("world", "uruguay",
   fill=TRUE, col="transparent",  plot = FALSE)
uy.poly <- map2SpatialPolygons(uy.map, "Uruguay")
proj4string(uy.poly) <- CRS("+proj=longlat +datum=WGS84")
data.Set3 <- read.csv("Set3\\Set3data.csv", header = TRUE)
data.disp <- data.frame(Field = 1:16)
data.disp$Longitude <- tapply(data.Set3$Longitude, data.Set3$Field, mean)
data.disp$Latitude <- tapply(data.Set3$Latitude, data.Set3$Field, mean)
data.dispN <- data.disp[which(data.disp$Latitude > -32.8316),]
data.dispS <- data.disp[which(data.disp$Latitude < -33.7008),]
data.dispC <- data.disp[which(data.disp$Latitude < -32.8314 &
  data.disp$Latitude > -33.7007),]
data.dispN$Longitude <- data.dispN$Longitude + 4 +
  30  * (data.dispN$Longitude + 53.7548)
data.dispN$Latitude <- data.dispN$Latitude + 3 +
  30  * (data.dispN$Latitude + 32.8316)
data.dispC$Longitude <- data.dispC$Longitude + 7 +
  45  * (data.dispC$Longitude + 53.90967)
data.dispC$Latitude <- data.dispC$Latitude +
  45  * (data.dispC$Latitude + 33.21518)
data.dispS$Longitude <- data.dispS$Longitude + 3 +
  30  * (data.dispS$Longitude + 54.51)
data.dispS$Latitude <- data.dispS$Latitude - 2 +
  15  * (data.dispS$Latitude + 33.72)
coordinates(data.disp) <- c("Longitude", "Latitude")
proj4string(data.disp) <- CRS("+proj=longlat +datum=WGS84")
coordinates(data.dispN) <- c("Longitude", "Latitude")
proj4string(data.dispN) <- CRS("+proj=longlat +datum=WGS84")
coordinates(data.dispC) <- c("Longitude", "Latitude")
proj4string(data.dispC) <- CRS("+proj=longlat +datum=WGS84")
coordinates(data.dispS) <- c("Longitude", "Latitude")
proj4string(data.dispS) <- CRS("+proj=longlat +datum=WGS84")
plot(uy.poly, xlim = c(-59, -46), axes = FALSE)
title(main = "Data Set 3 Field Locations", cex.main = 2)
plot(data.disp, add = TRUE, pch = 1, cex = 0.5)
plot(data.dispN, add = TRUE, pch = 1)
plot(data.dispC, add = TRUE, pch = 1)
plot(data.dispS, add = TRUE, pch = 1)
text(coordinates(data.dispN)[,1]+0.25, coordinates(data.dispN)[,2]+0.25,
   as.character(data.dispN$Field))
text(coordinates(data.dispC)[,1]+0.25, coordinates(data.dispC)[,2]+0.25,
   as.character(data.dispC$Field))
text(coordinates(data.dispS)[,1]+0.25, coordinates(data.dispS)[,2]+0.25,
   as.character(data.dispS$Field))
lines(c(-51.8,-49), c(-30,-30))
lines(c(-51.8,-49), c(-28,-28))
lines(c(-51.8,-51.8), c(-30,-28))
lines(c(-49,-49), c(-30,-28))
arrows(-51.8,-29.7, -53.65,-32.65, length = 0.1)
lines(c(-51.1,-46.3), c(-34.2,-34.2))
lines(c(-51.1,-46.3), c(-31.9,-31.9))
lines(c(-51.1,-51.1), c(-34.2,-31.9))
lines(c(-46.3,-46.3), c(-31.9,-34.2))
arrows(-51.1,-33, -53.8,-33.2, length = 0.1)
lines(c(-52.1,-50.5), c(-36.45,-36.45))
lines(c(-52.1,-50.5), c(-35,-35))
lines(c(-52.1,-52.1), c(-36.45,-35))
lines(c(-50.5,-50.5), c(-36.45,-35))
arrows(-52.1,-35.5, -54.4,-33.8, length = 0.1)

deg.per.km <- 360 / 40075
lat <- -36
text(-57, lat+0.35, "0")
text(-55, lat+0.35, "200 km")
deg.per.km <- deg.per.km * cos(pi*lat / 180)
SpatialPolygonsRescale(layout.scale.bar(), offset = c(-57, lat),
  scale = 200 * deg.per.km, fill=c("transparent","black"),
  plot.grid = FALSE)
SpatialPolygonsRescale(layout.north.arrow(), offset = c(-55, -30),
  scale = 1, plot.grid = FALSE)
