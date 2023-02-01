#Fig. 1.8
library(maps)
library(sf)
uy.map <- map("world", "uruguay",  # Similar to Fig. 1.4
   fill=TRUE, col="transparent",  plot = FALSE)
uy.poly.sf <- st_as_sf(uy.map, "Uruguay")
st_crs(uy.poly.sf) <- "EPSG:4326" # WGS 84
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
data.disp.sf <- st_as_sf(data.disp,  # Same as Fig. 1.3a
  coords = c("Longitude", "Latitude"), crs = "EPSG:4326")
data.dispN.sf <- st_as_sf(data.dispN,  # Same as Fig. 1.3a
  coords = c("Longitude", "Latitude"), crs = "EPSG:4326")
data.dispC.sf <- st_as_sf(data.dispC,  # Same as Fig. 1.3a
  coords = c("Longitude", "Latitude"), crs = "EPSG:4326")
data.dispS.sf <- st_as_sf(data.dispS,  # Same as Fig. 1.3a
  coords = c("Longitude", "Latitude"), crs = "EPSG:4326")
plot(uy.poly.sf, xlim = c(-59, -46), axes = FALSE, reset = FALSE,
   main = "",  col = "white")  # Same as Fig. 1.4
title(main = "Data Set 3 Field Locations", cex.main = 2)
plot(data.disp.sf, add = TRUE, pch = 1, cex = 0.5, col = "black")
plot(data.dispN.sf, add = TRUE, pch = 1, col = "black")
plot(data.dispC.sf, add = TRUE, pch = 1, col = "black")
plot(data.dispS.sf, add = TRUE, pch = 1, col = "black")
text(st_coordinates(data.dispN.sf)[,1]+0.25,
   st_coordinates(data.dispN.sf)[,2]+0.25,
      as.character(data.dispN.sf$Field))
text(st_coordinates(data.dispC.sf)[,1]+0.25,
   st_coordinates(data.dispC.sf)[,2]+0.25,
      as.character(data.dispC.sf$Field))
text(st_coordinates(data.dispS.sf)[,1]+0.25,
    st_coordinates(data.dispS.sf)[,2]+0.25,
      as.character(data.dispS.sf$Field))
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
# Scale bar
deg.per.km <- 360 / 40075
lat <- -36
text(-57, lat+0.35, "0")
text(-55, lat+0.35, "200 km")
deg.per.km <- deg.per.km * cos(pi*lat / 180)
lines(c(-57,-57 + 200 * deg.per.km), c(lat,lat), lwd = 3)
# North arrow
arrows(-54, lat + 5.5, -54, lat + 7, lwd = 3, length = 0.1)
text(-54, lat +6.25, "N", cex = 2)
