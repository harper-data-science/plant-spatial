data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
data.Yield4.1idw <- read.csv("created\\set4.1yld96ptsidw.csv", header = TRUE)
data.Set4.1$Yield <- data.Yield4.1idw$Yield
model.5 <- lm(Yield ~ Clay + SoilP + I(Clay*SoilP) + Weeds,
   data = data.Set4.1)
library(spdep)
coordinates(data.Set4.1) <- c("Easting", "Northing")
nlist <- dnearneigh(data.Set4.1, d1 = 0, d2 = 61)
W <- nb2listw(nlist, style = "W")
lm.morantest(model.5, W)

