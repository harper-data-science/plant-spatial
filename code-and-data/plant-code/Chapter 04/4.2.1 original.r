# Figures of weed level and detrended sand content in Field 4.1
library(spdep)
library(sf)
data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
data.Set4.1$x <- data.Set4.1$Easting - min(data.Set4.1$Easting)
data.Set4.1$y <- data.Set4.1$Northing - min(data.Set4.1$Northing)
trend.lm <- lm(Sand ~ x + y + I(x^2) +
  I(y^2) + I(x*y), data = data.Set4.1)
data.Set4.1$SandDT <- data.Set4.1$Sand - predict(trend.lm)
coordinates(data.Set4.1) <- c("Easting", "Northing")
bdry.sf <- st_read("created\\set419697bdry.shp")
bdry.sp <- as(bdry.sf, "Spatial")
par(mai = c(1,1,1,1))
plot(bdry.sp, axes = TRUE )
plot(data.Set4.1, pch = 1, add = TRUE) # Fig. 4.1a
text(coordinates(data.Set4.1)[,1] + 25,
     coordinates(data.Set4.1)[,2],
     labels=as.character(data.Set4.1$Weeds), cex = 1)
title(main = "Field 4.1 Weed Levels", cex.main = 2,
   cex.lab = 1.5, xlab = "Easting", ylab = "Northing")

plot(bdry.sp, axes = TRUE )
plot(data.Set4.1, pch = 1, add = TRUE) # Fig. 4.1b
text(coordinates(data.Set4.1)[,1] + 25,
     coordinates(data.Set4.1)[,2],
     labels=as.character(round(data.Set4.1$SandDT, 1)), cex = 0.8)
title(main = "Field 4.1 Detrended Sand Content", cex.main = 2,
   cex.lab = 1.5, xlab = "Easting", ylab = "Northing")




