# Figures of weed level and detrended sand content in Field 4.1
library(sf)
data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
data.Set4.1$x <- data.Set4.1$Easting - min(data.Set4.1$Easting)
data.Set4.1$y <- data.Set4.1$Northing - min(data.Set4.1$Northing)
trend.lm <- lm(Sand ~ x + y + I(x^2) +
  I(y^2) + I(x*y), data = data.Set4.1)
data.Set4.1$SandDT <- data.Set4.1$Sand - predict(trend.lm)
data.Set4.1.sf <- st_as_sf(data.Set4.1, 
  coords = c("Easting", "Northing"), crs = "EPSG:32610")
bdry.sf <- st_read("created\\set419697bdry.shp")
st_crs(bdry.sf) <- "EPSG:32610"
#par(mar = c(10,10,10,10))  par() doesn't work for me 
plot(bdry.sf["ID"],  main = "", col = "white", axes = TRUE,
   reset = FALSE) # Fig. 4.1a
plot(st_geometry(data.Set4.1.sf), pch = 1, add = TRUE, col = "black") # Fig. 4.1a
text(st_coordinates(data.Set4.1.sf)[,1] + 25,
     st_coordinates(data.Set4.1.sf)[,2],
     labels=as.character(data.Set4.1.sf$Weeds), cex = 1)
title(main = "Field 4.1 Weed Levels", cex.main = 1)

plot(bdry.sf["ID"],  main = "", col = "white", axes = TRUE,
   reset = FALSE) # Fig. 4.1b
plot(st_geometry(data.Set4.1.sf), pch = 1, add = TRUE, col = "black") # Fig. 4.1b
text(st_coordinates(data.Set4.1.sf)[,1] + 25,
     st_coordinates(data.Set4.1.sf)[,2],
     labels=as.character(round(data.Set4.1.sf$SandDT, 1)), cex = 0.8)
title(main = "Field 4.1 Detrended Sand Content", cex.main = 1)




