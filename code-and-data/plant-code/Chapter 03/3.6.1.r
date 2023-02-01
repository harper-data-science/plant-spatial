library(sf)
library(spdep) # needed for dnearneigh()
library(spatialreg) 
# Computation of lambda of detrended sand content in Field 4.1
data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
# Reduce the size of the x and y vars. to avoid numerical problems
data.Set4.1$x <- data.Set4.1$Easting - min(data.Set4.1$Easting)
data.Set4.1$y <- data.Set4.1$Northing - min(data.Set4.1$Northing)
trend.lm <- lm(Sand ~ x + y + I(x^2) +
  I(y^2) + I(x*y), data = data.Set4.1)
# Subtract the trend
data.Set4.1$SandDT <- data.Set4.1$Sand - predict(trend.lm)
data.Set4.1.sf <- st_as_sf(data.Set4.1,
  coords = c("Easting", "Northing"), crs = "EPSG:32610")
nlist <- dnearneigh(data.Set4.1.sf, d1 = 0, d2 = 61)
W <- nb2listw(nlist, style = "W")
Y.mod <- errorsarlm(SandDT ~ 1, data = data.Set4.1, listw = W)
print(Y.mod$lambda, digits = 4)

W <- nb2listw(nlist, style = "B")
Y.mod <- errorsarlm(SandDT ~ 1, data = data.Set4.1, listw = W)
print(Y.mod$lambda, digits = 4)


nlist <- knn2nb(knearneigh(data.Set4.1.sf, k = 4))
W <- nb2listw(nlist)
Y.mod <- errorsarlm(SandDT ~ 1, data = data.Set4.1, listw = W)
print(Y.mod$lambda, digits = 4)

nlist <- knn2nb(knearneigh(data.Set4.1.sf, k = 4))
W <- nb2listw(nlist, style = "B")
Y.mod <- errorsarlm(SandDT ~ 1, data = data.Set4.1, listw = W)
print(Y.mod$lambda, digits = 4)

# Thiesen polygon data
library(spatstat)
W <- 592000
E <- 592500
S <- 4270300
N <- 4271200
cell.ppp <- ppp(data.Set4.1$Easting, data.Set4.1$Northing,
     window = owin(c(W, E), c(S, N)))
thsn.tess <- dirichlet(cell.ppp)
library(sf)
thsn.sfc <- st_as_sfc(thsn.tess)
thsn.geom.sf <- st_sf(thsn.sfc)

