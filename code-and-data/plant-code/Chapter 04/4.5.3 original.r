# Computation of local I
library(spdep)
data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
data.Set4.1$x <- data.Set4.1$Easting - min(data.Set4.1$Easting)
data.Set4.1$y <- data.Set4.1$Northing - min(data.Set4.1$Northing)
trend.lm <- lm(Sand ~ x + y + I(x^2) +
  I(y^2) + I(x*y), data = data.Set4.1)
data.Set4.1$SandDT <- data.Set4.1$Sand - predict(trend.lm)
coordinates(data.Set4.1) <- c("Easting", "Northing")
SandDT <- data.Set4.1@data$SandDT
nlist <- dnearneigh(data.Set4.1, d1 = 0, d2 = 61)
W <- nb2listw(nlist, style = "W")
SandIi <- localmoran(SandDT, W)[,1]

# This file is created in Exercise 2.11
library(sf)
bdry.sf <- st_read("created\\set419697bdry.shp")
bdry.spdf <- as(bdry.sf, "Spatial")
par(mai = c(1,1,1,1))
plot(bdry.spdf, axes = TRUE) #Fig. 4.5a
plot(data.Set4.1, add = TRUE, pch = 1,
  cex = (1 + data.Set4.1$SandDT / 10))
text(coordinates(data.Set4.1)[,1] + 25, coordinates(data.Set4.1)[,2],
     labels=as.character(data.Set4.1$ID), cex = 0.8)
title(main = "Linear Detrending", cex.main = 2, xlab = "Easting",
   ylab = "Northing", cex.lab = 1.5)
legend(592450, 4270600, c("-10","0", "10"), pt.cex = c(0.1,1,2), pch = 1,
  y.intersp = 1, title = "Percent Sand")
  
cex.max <- 3
cex.min <- 0.5
I.max <- max(SandIi)
I.min <- min(SandIi)
# Implement equation (4.20)
SandIi.scaled <- cex.min + (cex.max - cex.min) *
  (SandIi - I.min) / (I.max - I.min)
plot(bdry.spdf, axes = TRUE) #Fig. 4.5b
plot(data.Set4.1, add = TRUE, pch = 1,
  cex = SandIi.scaled)
title(main = "Local Moran's I", cex.main = 2, xlab = "Easting",
   ylab = "Northing", cex.lab = 1.5)
legend(592500, 4270600, c(as.character(round(I.min,2)),
  as.character(round(I.max,2))),
  pt.cex = c(cex.min,cex.max), pch = 1,
  y.intersp = 1, title = "Local I")

# Remove the influential point 50
moran(data.Set4.1@data$SandDT,W,86,Szero(W))
data.Set4.1a <- data.Set4.1[-50,]
nlist <- dnearneigh(data.Set4.1a, d1 = 0, d2 = 61)
W <- nb2listw(nlist, style = "W")
moran(data.Set4.1a@data$SandDT,W,85,Szero(W))
