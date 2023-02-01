# Creation of Fig. 1.4: Map of the yellow-billed cuckoo habitat
library(maptools)
library(rgdal)
library(maps)
library(sf)
data(stateMapEnv)
cal.map <- map("state", "california",
   fill=TRUE, col="transparent",  plot = FALSE)
cal.poly <- map2SpatialPolygons(cal.map, "California")
proj4string(cal.poly) <- CRS("+proj=longlat +datum=WGS84")
data.Set1.sf <- st_read("set1\\set1data.shp")
st_crs(data.Set1.sf) <- "+proj=utm +zone=10 +ellps=WGS84"
data.Set1.sp <- as(data.Set1.sf, "Spatial")
data.Set1.wgs <- spTransform(data.Set1.sp, CRS("+proj=longlat
  +datum=WGS84"))
par(mai = c(1,1,1,1))
plot(cal.poly, axes = FALSE) # Fig. 1.4
title(main = "Data Set 1 Site Location",cex.main = 2)
plot(data.Set1.wgs, add = TRUE)
lines(c(-122.15,-121.9), c(39.7,39.7))
lines(c(-122.15,-122.15), c(39.7,39.95))
lines(c(-122.15,-121.9), c(39.95,39.95))
lines(c(-121.9,-121.9), c(39.7,39.95))
lines(c(-118,-113), c(36.3,36.3), lwd = 3)
lines(c(-118,-113), c(41.9,41.9), lwd = 3)
lines(c(-118,-118), c(36.3,41.9), lwd = 3)
lines(c(-113,-113), c(36.3,41.9), lwd = 3)
arrows(-118, 39.82, -121.85, 39.82, lwd = 3, length = 0.1)
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

#This is from Fig. 1.5 of Murrel (2006) to draw the small map
par(fig = c(0.5, 0.94, 0.3, 0.95), new = TRUE)
plot.new()
xrange <- bbox(data.Set1.wgs)[1,]
yrange <- bbox(data.Set1.wgs)[2,]
plot.window(xlim = xrange, ylim = yrange)
plot(data.Set1.wgs, add = TRUE, axes = FALSE)

# --------------------------------------------

# Creation of Fig. 1.5
# Land classes in the site of Data Set 1

library(maptools)
library(rgdal)
library(sf)
data.Set1.sf <- st_read("set1\\landcover.shp")
data.Set1.cover <- as(data.Set1.sf, "Spatial")
proj4string(data.Set1.cover) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
levels(data.Set1.cover$VegType)
data.Set1.cover@data$VegType <-
  factor(as.character(slot(data.Set1.cover, "data")$VegType),
  labels = c("Grassland", "Cropland","Developed",
  "Freshwater Wetland", "Gravel Bar", "Lacustrine", "Orchard",
  "Riverine", "Riparian Forest", "Oak Woodland"))
north <- list("SpatialPolygonsRescale", layout.north.arrow(),
   offset = c(579800,4420000), scale = 600)
scale <- list("SpatialPolygonsRescale", layout.scale.bar(),
   offset = c(579400,4419000), scale = 1000,
   fill = c("transparent", "black"))
text1 <- list("sp.text", c(579400, 4419200), "0")
text2 <- list("sp.text", c(580300, 4419200), "1 km")
map.layout = list(north, text1, text2, scale)
greys <- grey(c(180, 190, 140, 240, 150, 250, 130, 250, 30, 170) / 255)
spplot(data.Set1.cover, "VegType", col.regions = greys,
   main = "Data Set 1 Land Cover, Northern End, 1997",
   xlim = c(576000,580500), ylim = c(4417400,4421000),
   sp.layout = map.layout)

# Rainbow color version
spplot(data.Set1.cover, "VegType", col.regions = rainbow(10),
   main = "Land Use, Northern End, 1997",
   xlim = c(576000,580500), ylim = c(4417400,4421000),
   sp.layout = map.layout)

# Color version using RColorBrewer
library(RColorBrewer)
brown.bg <- brewer.pal(11,"BrBG")
purple.green <- brewer.pal(11,"PiYG")
color.array <- c(
   purple.green[8], #Grassland
   purple.green[4], #Cropland
   purple.green[3], #Developed
   brown.bg[6], #Freshwater Wetland
   brown.bg[4], #Gravel Bar
   brown.bg[8], #Lacustrine
   purple.green[2], #Orchard
   brown.bg[7], #Riverine
   "green", #Riparian Forest
   purple.green[11]) #Oak Woodland

spplot(data.Set1.cover, "VegType", col.regions = color.array,
   main = "Data Set 1 Land Cover, Northern End, 1997",
   xlim = c(576000,580500), ylim = c(4417400,4421000),
   sp.layout = map.layout)


