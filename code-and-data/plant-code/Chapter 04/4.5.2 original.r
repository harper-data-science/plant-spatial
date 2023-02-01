# Construction of Moran scatterplot of detrended sand content
library(spdep)
data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
data.Set4.1$x <- data.Set4.1$Easting - min(data.Set4.1$Easting)
data.Set4.1$y <- data.Set4.1$Northing - min(data.Set4.1$Northing)
trend.lm <- lm(Sand ~ x + y + I(x^2) +
  I(y^2) + I(x*y), data = data.Set4.1)
data.Set4.1$SandDT <- data.Set4.1$Sand - predict(trend.lm)
coordinates(data.Set4.1) <- c("x", "y")
SandDT <- data.Set4.1@data$SandDT

nlist <- dnearneigh(data.Set4.1, d1 = 0, d2 = 61)
W <- nb2listw(nlist, style = "W")
SandDT.mp <- moran.plot(SandDT,W)  # Fig. 4.4
title(main = "Moran Scatterplot for Detrended Sand, Field 4.1")
print (inf.rows <- which(rowSums(SandDT.mp$is.inf) > 0))
SandDT.mp$is.inf[inf.rows,]

