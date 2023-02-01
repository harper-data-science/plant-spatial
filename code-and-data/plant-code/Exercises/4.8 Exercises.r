# 4.1
library(spdep)
data.1 <- read.csv("set2\\set2data.csv", header = TRUE)
data.1a <- data.1[which((data.1$Longitude >= -121.6)
  & (data.1$Longitude <= -121.55)),]
data.1b <- data.1a[which((data.1a$Latitude >= 36.32)
  & (data.1a$Latitude <= 36.45)),]
coordinates(data.1b) <- c("Longitude", "Latitude")
plot(data.1b, pch = data.1a$QUDO, axes = TRUE)
nlist.wk <- knn2nb(knearneigh(data.1b, k = 4))
W <- nb2listw(nlist.wk, style = "B")
joincount.test(factor(data.1b$QUDO), W)

# --------------------------------------

# 4.2
library(spdep)
data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
coordinates(data.Set4.1) <- c("Easting", "Northing")
nlist.wk <- knn2nb(knearneigh(data.Set4.1, k = 4))
W.kW <- nb2listw(nlist.wk, style = "W")
joincount.test(factor(data.Set4.1$Weeds), W.kW)
moran.test(data.Set4.1$Weeds,W.kW,
   randomisation = FALSE,alternative = "greater")

# 4.3   
library(spdep)
data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
data.Set4.1$x <- data.Set4.1$Easting - min(data.Set4.1$Easting)
data.Set4.1$y <- data.Set4.1$Northing - min(data.Set4.1$Northing)
trend.lm <- lm(Sand ~ x + y + I(x^2) +
  I(y^2) + I(x*y), data = data.Set4.1)
data.Set4.1$SandDT <- data.Set4.1$Sand - predict(trend.lm)
coordinates(data.Set4.1) <- c("x", "y")
nlistd <- dnearneigh(data.Set4.1, d1 = 0, d2 = 61)
W.dW <- nb2listw(nlistd, style = "W")
moran.test(data.Set4.1@data$Sand,W.dW)
moran.test(data.Set4.1@data$SandDT,W.dW)

   
# 4.4
library(spdep)
data.Set2 <- read.csv("set2\\set2data.csv", header = TRUE)
data.Set2a <- data.Set2[which((data.Set2$Longitude >= -121)
  & (data.Set2$Longitude <= -119)),]
data.Set2b <- data.Set2a[which((data.Set2a$Latitude >= 37)
  & (data.Set2a$Latitude <= 39)),]
coordinates(data.Set2b) <- c("Longitude", "Latitude")
nlist.wk <- knn2nb(knearneigh(data.Set2b, k = 4))
W <- nb2listw(nlist.wk, style = "B")
mp <- moran.plot(data.Set2b$Precip,W)
plot(data.Set2b[which(rowSums(mp$is.inf)>0),])
plot(data.Set2b, pch = 1, cex = data.Set2b$Elevation/max(data.Set2b$Elevation),
   add = TRUE)

# 4.5
library(spdep)
library(maptools)
N <- 4267868
S <- 4267513
E <- 592867
W <- 592082
cell.size <- 50  # Try different values here
grid.xy <- expand.grid(x = seq(W, E, cell.size),  y = seq(N, S, -cell.size))
library(spatstat)
cell.ppp <- ppp(grid.xy[,1], grid.xy[,2],
     window = owin(c(W, E), c(S, N)))
thsn.pp <- dirichlet(cell.ppp)
thsn.sp <- as(thsn.pp, "SpatialPolygons")
data.Yield4.2 <- read.csv("set4\\set4.296wheatyield.csv", header = TRUE)
coordinates(data.Yield4.2) <- c("Easting", "Northing")
yield.mean <- over(thsn.sp, data.Yield4.2, fn = mean)
thsn.spdf <- SpatialPolygonsDataFrame(thsn.sp, yield.mean)
nlist <- poly2nb(thsn.spdf)
W <- nb2listw(nlist)
moran.test(thsn.spdf@data$Yield, W)

# 4.6
library(maptools)
data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
data.Set4.1$x <- data.Set4.1$Easting - min(data.Set4.1$Easting)
data.Set4.1$y <- data.Set4.1$Northing - min(data.Set4.1$Northing)
trend.lm <- lm(Sand ~ x + y + I(x^2) +
  I(y^2) + I(x*y), data = data.Set4.1)
data.Set4.1$SandDT <- data.Set4.1$Sand - predict(trend.lm)
coordinates(data.Set4.1) <- c("Easting", "Northing")
W <- 592000
E <- 592500
S <- 4270300
N <- 4271200
grid.xy <- expand.grid(x = seq(W, E, 5), y = seq(N, S, -5))
coordinates(grid.xy) <- ~x + y
gridded(grid.xy) = TRUE
library(gstat)
SandDT.vgm <- variogram(SandDT ~ 1, data.Set4.1, cutoff = 600)
plot(SandDT.vgm, col = "black", main = "Detrended Sand Content Variogram",
   xlab = expression(bold("lag h")),
   ylab = expression(bold(hat(gamma)*"(h)")))

SandDT.vgm <- variogram(Sand ~ x+y+I(x*y)+I(x^2)+I(y^2),
  data.Set4.1, cutoff = 600)
plot(SandDT.vgm, col = "black", main = "Detrended Sand Content Variogram",
   xlab = expression(bold("lag h")),
   ylab = expression(bold(hat(gamma)*"(h)")))

