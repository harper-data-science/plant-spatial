data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
yield.pts <- read.csv("created\\set4.1yld96ptsidw.csv")
data.Set4.1$Yield <- yield.pts$Yield

library(spdep)
library(sf)
# This file can be created in ArcGIS or GeoDa
thsn.sf <- st_read("auxiliary\\set4.1thiessen.shp")
thsn.spdf <- as(thsn.sf, "Spatial")
greys <- grey(250:0 / 255)
slot(thsn.spdf, "data")$Clay <- data.Set4.1$Clay
spplot(thsn.spdf, zcol = "Clay", col.regions = greys,
   main = "Field 4.1 Clay Content") # Fig. 17.8
slot(thsn.spdf, "data")$SoilpH <- data.Set4.1$SoilpH
spplot(thsn.spdf, zcol = "SoilpH", col.regions = greys,
   main = "Field 4.1 Soil pH") # Fig. 17.#
slot(thsn.spdf, "data")$SoilP <- data.Set4.1$SoilP
spplot(thsn.spdf, zcol = "SoilP", col.regions = greys,
   main = "Field 4.1 Soil P Content") # Fig. 17.#
slot(thsn.spdf, "data")$SoilK <- data.Set4.1$SoilK
spplot(thsn.spdf, zcol = "SoilK", col.regions = greys,
   main = "Field 4.1 Soil K") # Not shown
slot(thsn.spdf, "data")$Disease <- data.Set4.1$Disease
spplot(thsn.spdf, zcol = "Disease", col.regions = greys,
   main = "Field 4.1 Disease Level") # Not shown
slot(thsn.spdf, "data")$Yield <- data.Set4.1$Yield
spplot(thsn.spdf, zcol = "Yield", col.regions = greys,
   main = "Field 4.1 Yield")  #Not shown 

data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
yield.pts <- read.csv("created\\set4.1yld96ptsidw.csv")
data.Set4.1$Yield <- yield.pts$Yield

library(spdep)
library(sf)
# This file can be created in ArcGIS or GeoDa
thsn.sf <- st_read("auxiliary\\set4.1thiessen.shp")
thsn.spdf <- as(thsn.sf, "Spatial")
greys <- grey(250:0 / 255)
slot(thsn.spdf, "data")$Clay <- data.Set4.1$Clay
spplot(thsn.spdf, zcol = "Clay", col.regions = greys,
   main = "Field 4.1 Clay Content") # Fig. 17.8
slot(thsn.spdf, "data")$SoilpH <- data.Set4.1$SoilpH
spplot(thsn.spdf, zcol = "SoilpH", col.regions = greys,
   main = "Field 4.1 Soil pH") # Not in book
slot(thsn.spdf, "data")$SoilP <- data.Set4.1$SoilP
spplot(thsn.spdf, zcol = "SoilP", col.regions = greys,
   main = "Field 4.1 Soil P Content") # Not in book

model.1 = lm(Yield ~ Clay + Silt + SoilpH + SoilTN + SoilK +
     SoilP + Disease + Weeds + I(Clay*SoilP) + I(Clay*SoilK) + 
     I(Clay*SoilpH) + I(Clay*SoilTN), data = data.Set4.1)
model.2 <- lm(Yield ~ Clay + Silt + SoilTN +
     Weeds + I(Clay*SoilTN), data = data.Set4.1)
model.3 <- lm(Yield ~ Clay + SoilpH + SoilP + Weeds +
     I(Clay*SoilP) + I(Clay*SoilpH), data = data.Set4.1)
model.4 <- lm(Yield ~ Clay + SoilpH + SoilP +
     Weeds + I(Clay*SoilP) + Weeds, data = data.Set4.1)
model.5 <- lm(Yield ~ Clay + SoilP + I(Clay*SoilP) +
    Weeds, data = data.Set4.1)


data.Set4.2 <- read.csv("set4\\set4.296sample.csv", header = TRUE)
yield.pts <- read.csv("created\\set4.2yld96ptsidw.csv")
data.Set4.2$Yield <- yield.pts$Yield

range(data.Set4.1$Clay)
range(data.Set4.2$Clay)
stem(data.Set4.1$Clay)
stem(data.Set4.2$Clay)
range(data.Set4.1$Yield[which(data.Set4.1$Clay >= 44)])
range(data.Set4.2$Yield[which(data.Set4.2$Clay >= 44)])

range(data.Set4.1$SoilP)
range(data.Set4.2$SoilP)

stem(data.Set4.1$SoilpH)
stem(data.Set4.2$SoilpH)



