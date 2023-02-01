# Computation of local I
data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
data.Set4.1$x <- data.Set4.1$Easting - min(data.Set4.1$Easting)
data.Set4.1$y <- data.Set4.1$Northing - min(data.Set4.1$Northing)
trend.lm <- lm(Sand ~ x + y + I(x^2) +
  I(y^2) + I(x*y), data = data.Set4.1)
data.Set4.1$SandDT <- data.Set4.1$Sand - predict(trend.lm)
library(sf)
data.Set4.1.sf <- st_as_sf(data.Set4.1, 
  coords = c("Easting", "Northing"))
SandDT <- data.Set4.1.sf$SandDT
library(spdep)
data.Set4.1.sp <- as(data.Set4.1.sf, "Spatial")
nlist <- dnearneigh(data.Set4.1.sp, d1 = 0, d2 = 61)
W <- nb2listw(nlist, style = "W")
SandIi <- localmoran(SandDT, W)[,1]

# This file is created in Exercise 2.11
bdry.sf <- st_read("created\\set419697bdry.shp")
# par(mai = c(1,1,1,1)) Doesn't seem to have any effect
# Similar to 4.2.1.r
plot(bdry.sf["ID"],  main = "", col = "white", axes = TRUE,
   reset = FALSE) 
plot(st_geometry(data.Set4.1.sf), pch = 1, add = TRUE, col = "black",
   cex = (1 + data.Set4.1.sf$SandDT / 10)) # Fig. 4.5a
text(st_coordinates(data.Set4.1.sf)[,1] + 25,
   st_coordinates(data.Set4.1.sf)[,2],
   labels = as.character(data.Set4.1.sf$ID), cex = 0.8)
title(main = "Linear Detrending", cex.main = 1)
legend(592450, 4270600, c("-10","0", "10"),
  pt.cex = c(0.1,1,2), pch = 1, y.intersp = 1,
  title = "Percent Sand")

cex.max <- 3
cex.min <- 0.5
I.max <- max(SandIi)
I.min <- min(SandIi)
# Implement equation (4.20)
SandIi.scaled <- cex.min + (cex.max - cex.min) *
  (SandIi - I.min) / (I.max - I.min)
plot(bdry.sf["ID"],  main = "", col = "white", axes = TRUE,
   reset = FALSE) # Fig. 4.1a
plot(st_geometry(data.Set4.1.sf), pch = 1, add = TRUE, col = "black",
   cex = SandIi.scaled) # Fig. 4.5b
title(main = "Local Moran,s I", cex.main = 1)
legend(592500, 4270600, c(as.character(round(I.min,2)),
  as.character(round(I.max,2))),
  pt.cex = c(cex.min,cex.max), pch = 1,
  y.intersp = 1, title = "Local I")
  
