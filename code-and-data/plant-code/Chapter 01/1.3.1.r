# Creation of Fig. 1.4: Map of the yellow-billed cuckoo habitat
library(maps)
library(sf)
data(stateMapEnv)
cal.map <- map("state", "california",
   fill=TRUE, col="transparent",  plot = FALSE)
cal.poly.sf <- st_as_sf(cal.map, "California")
st_crs(cal.poly.sf) <- "EPSG:4326" # WGS 84
data.Set1.sf <- st_read("set1\\set1data.shp")
st_crs(data.Set1.sf) <- "EPSG:32610" # UTM Zone 10N
data.Set1.wgs <- st_transform(data.Set1.sf, crs = st_crs("EPSG:4326"))
par(mai = c(1,1,1,1))
plot(cal.poly.sf, axes = FALSE, main = "", col = "white",
   reset = FALSE) # Fig. 1.4
title(main = "Data Set 1 Site Location",cex.main = 2)
lines(c(-122.15,-121.9), c(39.7,39.7))
lines(c(-122.15,-122.15), c(39.7,39.95))
lines(c(-122.15,-121.9), c(39.95,39.95))
lines(c(-121.9,-121.9), c(39.7,39.95))
lines(c(-119.0,-113), c(35.3,35.3), lwd = 3)
lines(c(-119.0,-113), c(41.9,41.9), lwd = 3)
lines(c(-119.0,-119.0), c(35.3,41.9), lwd = 3)
lines(c(-113,-113), c(35.3,41.9), lwd = 3)
arrows(-119.0, 39.82, -121.85, 39.82, lwd = 3, length = 0.1)
lat <- 34
text(-124, lat+0.35, "0")
text(-122, lat+0.35, "200 km")
deg.per.km <- 360 / 40075
deg.per.km <- deg.per.km * cos(pi*lat / 180)
lines(c(-124,-124 + 200 * deg.per.km), c(lat,lat), lwd = 3)
arrows(-124 + 100 * deg.per.km, 35,
   -124 + 100 * deg.per.km, 35.8, lwd = 3, length = 0.1)
text(-124 + 100 * deg.per.km, 35.4, "N", cex = 1.5)
#This is from Fig. 1.5 of Murrel (2006) to draw the small map
par(fig = c(0.5, 0.94, 0.3, 0.95), new = TRUE)
plot.new()
xrange <- c(st_bbox(data.Set1.wgs)[1], st_bbox(data.Set1.wgs)[3])
yrange <- c(st_bbox(data.Set1.wgs)[2], st_bbox(data.Set1.wgs)[4])
plot.window(xlim = xrange, ylim = yrange)
# Ignore the warning
plot(data.Set1.wgs, add = TRUE, axes = FALSE)

# --------------------------------------------

# Creation of Fig. 1.5
# Land classes in the site of Data Set 1

library(sf)
data.Set1.sf <- st_read("set1\\landcover.shp")
st_crs(data.Set1.sf) <- "EPSG:32610" # UTM Zone 10N
unique(data.Set1.sf$VegType)
data.Set1.sf$VegClass <-
  factor(as.character(data.Set1.sf$VegType),
  labels = c("Grassland", "Cropland","Developed",
  "Freshwater Wetland", "Gravel Bar", "Lacustrine", "Orchard",
  "Riverine", "Riparian Forest", "Oak Woodland"))
greys <- grey(c(180, 190, 140, 240, 150, 250,
   130, 250, 30, 170) / 255)
library(ggplot2)
arrow.df <- data.frame(Longitude = 580300, Latitude = 4419000,
   x2 = 580300, y2 = 4420000)
scalebar.df <- data.frame(x1 = 579500, y1 = 4418500,
   x2 = 580500, y2 = 4418500)
ggplot() + 
    geom_sf(data = data.Set1.sf, aes(fill = VegClass)) +
        scale_fill_manual(values = greys) +
       coord_sf(xlim = c(576000, 580500), 
           ylim = c(4417400, 4421000)) + 
       ggtitle("Data Set 1 Land Cover, Northern End, 1997") +
       geom_segment(aes(x = Longitude, y = Latitude, xend = x2,
          yend = y2),
          data = arrow.df, lwd = 1, 
          arrow = arrow(length = unit(0.30, "cm"), ends="last",
             type = "closed")) +
       geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
          data = scalebar.df, lwd = 1, color = "black")+ 
       geom_text() + annotate("text", label = "N",
         x = arrow.df$Longitude, 
         y = (arrow.df$Latitude + arrow.df$y2) / 2,
         size = 7, color = "black") +
       geom_text() + annotate("text", label = "0",
         x = scalebar.df$x1, y = scalebar.df$y1 + 200 ,
         size = 4, color = "black") +
       geom_text() + annotate("text", label = "1 km",
         x = scalebar.df$x2 - 300, y = scalebar.df$y1 + 200 ,
         size = 4, color = "black") +
       theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) 

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


ggplot() + 
    geom_sf(data = data.Set1.sf, aes(fill = VegClass)) +
        scale_fill_manual(values = color.array) +
       coord_sf(xlim = c(576000, 580500), 
           ylim = c(4417400, 4421000)) + 
       ggtitle("Data Set 1 Land Cover, Northern End, 1997") +
       geom_segment(aes(x = Longitude, y = Latitude, xend = x2,
          yend = y2),
          data = arrow.df, lwd = 1, 
          arrow = arrow(length = unit(0.30, "cm"), ends="last",
             type = "closed")) +
       geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
          data = scalebar.df, lwd = 1, color = "black")+ 
       geom_text() + annotate("text", label = "N",
         x = arrow.df$Longitude,
         y = (arrow.df$Latitude + arrow.df$y2) / 2,
         size = 7, color = "black") +
       geom_text() + annotate("text", label = "0",
         x = scalebar.df$x1, y = scalebar.df$y1 + 200 ,
         size = 4, color = "black") +
       geom_text() + annotate("text", label = "1 km",
         x = scalebar.df$x2 - 300, y = scalebar.df$y1 + 200 ,
         size = 4, color = "black") +
       theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) 
