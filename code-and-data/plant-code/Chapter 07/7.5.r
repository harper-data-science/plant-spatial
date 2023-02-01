# Exploration of Field 4.1
library(sf)
library(stars)
library(starsExtra)
library(ggplot2)
library(gstat)
library(terra)
library(lattice) # For splom()

data.Yield4.1 <- read.csv("created\\set4.1yld96cleaned.csv", header = TRUE)
data.Yield4.2 <- read.csv("created\\set4.2yld96cleaned.csv", header = TRUE)
yield.Set4.1 <- data.frame(Yield = data.Yield4.1$Yield)
yield.Set4.2 <- data.frame(Yield = data.Yield4.2$Yield)

yield.Set4.1$Field <- 1
yield.Set4.2$Field <- 2
yield.Set4 <- rbind(yield.Set4.1, yield.Set4.2)
par(mai = c(1,1,1,1))
boxplot(Yield ~ Field, data = yield.Set4, xlab = "Field", ylab = "Yield (kg/ha)",
  main = "1996 Wheat Yields, Data Set 4", outline = FALSE,
  cex.main = 2, cex.lab = 1.5) # Fig. 7.22

# Do the same thing with ggplot()
# Here we need the x axis to be categorical
yield.Set4.1$FFac <- "1"
yield.Set4.2$FFac <- "2"
yield.Set4 <- rbind(yield.Set4.1, yield.Set4.2)
ggplot(data = yield.Set4) +
  geom_boxplot(mapping = aes(x = FFac, y = Yield))

# This file was created in Exercise 2.11
bdry.vec <- vect("created\\set419697bdry.shp")
data.4.1.Dec.ter <- rast("set4\\set4.11295.tif")
NDVI <- function(img) (img[[2]] - img[[1]]) / (img[[2]] + img[[1]]) 
Dec.NDVI.ter <- crop(NDVI(data.4.1.Dec.ter), bdry.vec)
plot(Dec.NDVI.ter, main = "December 1995 IR")

data.4.1.Mar.ter <- rast("set4\\set4.10396.tif")
Mar.NDVI.ter <- crop(NDVI(data.4.1.Mar.ter), bdry.vec)
plot(Mar.NDVI.ter, main = "March 1996 IR")

data.4.1.Mar.ter <- rast("set4\\set4.10596.tif")
May.NDVI.ter <- crop(NDVI(data.4.1.May.ter), bdry.vec)
plot(May.NDVI.ter, main = "May 1996 IR")

# From Sec. 1.1
cell.size <- 5
SW <- st_point(c(W, S))
NE <- st_point(c(E, N))
xy.sfc <- st_sfc(SW, NE)
grid.xy <- make_grid(xy.sfc, res = cell.size, buffer = 0)
st_crs(grid.xy) <- "EPSG:32610" # UTM Zone 10N
grid.xy.sf <- st_as_sf(grid.xy)
data.Yield4.1.sf <- st_as_sf(data.Yield4.1, coords = c("Easting",
   "Northing"))
st_crs(data.Yield4.1.sf) <- "EPSG:32610" # UTM Zone 10N
yield.idw <- idw(Yield ~ 1, data.Yield4.1.sf, grid.xy.sf, idp = 2, nmax = 12)
plot(yield.idw)

# This file was downloaded from the SSURGO site and
# created in ArcGIS. 
soiltypes.sf <- st_read("auxiliary\\f4.1soiltypes.shp")
par(mai = c(1,1,1,1))
plot(st_geometry(soiltypes.sf), axes = TRUE)
soillabels <- as.character(soiltypes.sf$musym)
soilpts <- st_centroid(soiltypes.sf)                                                 
soilpts.mat <- matrix(soilpts, nrow = 3, ncol = 2, byrow = TRUE)
text(soilpts, labels = soillabels)
invisible(text(soilpts.mat, labels = soillabels, cex = 1.5))
title(main = "Field 4.1 Soil Types",  # Fig. 7.24
  ylab = "Northing (m)", xlab = "Easting (m)", cex.main = 2,
  cex.lab = 1.5)

#----------------- Exploration of sample data

data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
data.Yield4.1idw <- read.csv("created\\set4.1yld96ptsidw.csv", header = TRUE)
# Check that the points line up
all.equal(data.Set4.1$Easting, data.Yield4.1idw$Easting)
all.equal(data.Set4.1$Northing, data.Yield4.1idw$Northing)
data.Set4.1$Yield <- data.Yield4.1idw$Yield

with(data.Set4.1, cor(cbind(Sand, Silt, Clay)))

agron.data <- subset(data.Set4.1,
   select = c(Clay, Silt, SoilP, SoilK, SoilpH, SoilTOC, SoilTN,
   LeafN, FLN, GrainProt, Yield))
splom(agron.data, par.settings = list(fontsize=list(text=9),
  plot.symbol = list(col = "black")), pscales = 0,
  main = "Agronomic Relationships, Field 4.1") #Fig. 7.25
  
with(data.Set4.1, cor(SoilK, SoilTN))
  
apply(with(data.Set4.1, cbind(FLN, SoilP, SoilK)), 2, stem)
apply(with(data.Set4.1, cbind(Clay, GrainProt, Yield)), 2, stem)

f.spdf <- data.Set4.1
coordinates(f.spdf) <- c("Easting", "Northing")
x <- over(f.spdf, data.4.1.Dec)
DecIR <- x$band1
x <- over(f.spdf, data.4.1.Mar)
MarNDVI <- (x$band1 - x$band2) / (x$band1 + x$band2)
x <- over(f.spdf, data.4.1.May)
MayNDVI <- (x$band1 - x$band2) / (x$band1 + x$band2)
MayIR <- x$band1
gauge.data <- with(data.Set4.1, data.frame(Clay, DecIR, EM38 = EM38F520,
   SPAD, LeafN, CropDens, MarNDVI, MayNDVI, MayIR, Yield = Yield))
splom(gauge.data, par.settings = list(fontsize=list(text=9),
  plot.symbol = list(col = "black")), pscales = 0,
  main = "Gauge Data Relationships") # Not shown in text
  
#  Display soil values above and below 30%
# This file can be created in ArcGIS or GeoDa
thsn.sf <- st_read("auxiliary\\set4.1thiessen.shp")
thsn.spdf <- as(thsn.sf, "Spatial")
# Check that IDs are the same
all.equal(thsn.spdf$ThPolyID, data.Set4.1$ID)
slot(thsn.spdf, "data")$Clay30 <- factor(data.Set4.1$Clay <= 30,
  labels = c("Greater than 30%", "Less than 30%"))
greys <- grey(c(100, 200) / 255)
spplot(thsn.spdf, zcol = "Clay30", col.regions = greys,
   main = "Field 4.1 Clay Content Categories") # Fig. 7.26

# Star plot of mineral nutrients and clay
star.data <- with(data.Set4.1, data.frame(SoilP, Clay,
   SoilK, SoilTN))
star.loc <- 2* cbind(data.Set4.1$Column, 13 - data.Set4.1$Row)
stars(star.data, locations = star.loc,
  labels = NULL, key.loc = c(18,2),
  main = "Nutrient Star Plot", cex.main = 2) # Fig. 7.27
  
# Star plot of response and process variable
star.data <- with(data.Set4.1, data.frame(GrainProt,
   FLN, Yield = data.Set4.1$Yield))
star.loc <- 2* cbind(data.Set4.1$Column, 13 - data.Set4.1$Row)
stars(star.data, locations = star.loc,
  labels = NULL, key.loc = c(18,2),
  main = "Response Variable Star Plot", cex.main = 2) # Not shown in text
  
# Plot the thematic maps of important variables
slot(thsn.spdf, "data")$LoP <- factor(data.Set4.1$SoilP <=6,
  labels = c("Greater than 6 ppm", "Less than 6 ppm"))
greys <- grey(c(100, 200) / 255)
spplot(thsn.spdf, zcol = "LoP", col.regions = greys,
   main = "Field 4.1 Low Soil P Regions") # Fig. 7.28a

slot(thsn.spdf, "data")$LoGP <- factor(data.Set4.1$GrainProt <=13,
  labels = c("Greater than 13%", "Less than 13%"))
greys <- grey(c(100, 200) / 255)
spplot(thsn.spdf, zcol = "LoGP", col.regions = greys,
   main = "Field 4.1 Low Grain Protein Regions") # Fig. 7.28b


   
