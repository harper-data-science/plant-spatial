data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
yield.pts <- read.csv("created\\set4.1yld96ptsidw.csv")
data.Set4.1$Yield <- yield.pts$Yield

library(terra)
library(sf)
# This file can be created in ArcGIS or GeoDa
thsn.sf <- st_read("auxiliary\\set4.1thiessen.shp")
thsn.sf <- st_read("auxiliary\\set4.1thiessen.shp")
thsn.sf["Clay"] <- data.Set4.1$Clay
library(terra)
thsn.ter <- vect(thsn.sf)
greys <- grey(250:0 / 255)
plot(thsn.ter, y = 4, col = greys, main = "Field 4.1 Clay Content")  # Fig. 17.8

data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
yield.pts <- read.csv("created\\set4.1yld96ptsidw.csv")
data.Set4.1$Yield <- yield.pts$Yield

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



