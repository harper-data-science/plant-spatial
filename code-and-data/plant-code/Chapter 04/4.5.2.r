# Construction of Moran scatterplot of detrended sand content
data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
data.Set4.1$x <- data.Set4.1$Easting - min(data.Set4.1$Easting)
data.Set4.1$y <- data.Set4.1$Northing - min(data.Set4.1$Northing)
trend.lm <- lm(Sand ~ x + y + I(x^2) +
  I(y^2) + I(x*y), data = data.Set4.1)
data.Set4.1$SandDT <- data.Set4.1$Sand - predict(trend.lm)
library(sf)
data.Set4.1.sf <- st_as_sf(data.Set4.1, coords = c("x", "y"))
library(spdep)
data.Set4.1.sp <- as(data.Set4.1.sf, "Spatial")
SandDT <- data.Set4.1.sp@data$SandDT
nlist <- dnearneigh(data.Set4.1.sp, d1 = 0, d2 = 61)
W <- nb2listw(nlist, style = "W")
SandDT.mp <- moran.plot(SandDT,W)  # Fig. 4.4
title(main = "Moran Scatterplot for Detrended Sand, Field 4.1")
inf.rows <- which(SandDT.mp$is_inf)
if (length(inf.rows > 0)){
  print("Infinite rows")
  inf.rows
  }
