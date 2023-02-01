# Computation of lambda of detrended sand content in Field 4.1
library(spdep)
# Read data based on code in Appendix B.4
data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
# Reduce the size of the x and y vars. to avoid numerical problems
data.Set4.1$x <- data.Set4.1$Easting - min(data.Set4.1$Easting)
data.Set4.1$y <- data.Set4.1$Northing - min(data.Set4.1$Northing)
trend.lm <- lm(Sand ~ x + y + I(x^2) +
  I(y^2) + I(x*y), data = data.Set4.1)
# Subtract the trend
data.Set4.1$SandDT <- data.Set4.1$Sand - predict(trend.lm)
coordinates(data.Set4.1) <- c("x", "y")
nlist <- dnearneigh(data.Set4.1, d1 = 0, d2 = 61)
W <- nb2listw(nlist, style = "W")
library(spatialreg)
Y.mod <- errorsarlm(SandDT ~ 1, data = data.Set4.1, listw = W)
print(Y.mod$lambda, digits = 4)

W <- nb2listw(nlist, style = "B")
Y.mod <- errorsarlm(SandDT ~ 1, data = data.Set4.1, listw = W)
print(Y.mod$lambda, digits = 4)


nlist <- knn2nb(knearneigh(data.Set4.1, k = 4))
W <- nb2listw(nlist)
Y.mod <- errorsarlm(SandDT ~ 1, data = data.Set4.1, listw = W)
print(Y.mod$lambda, digits = 4)

nlist <- knn2nb(knearneigh(data.Set4.1, k = 4))
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
library(maptools)
thsn.sp <- as(thsn.tess, "SpatialPolygons")
thsn.spdf <- SpatialPolygonsDataFrame(thsn.sp,
  slot(data.Set4.1, "data"), FALSE)


