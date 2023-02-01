library(spdep)
data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
data.Set4.1$x <- data.Set4.1$Easting - min(data.Set4.1$Easting)
data.Set4.1$y <- data.Set4.1$Northing - min(data.Set4.1$Northing)
coordinates(data.Set4.1) <- c("x", "y")
nlistd <- dnearneigh(data.Set4.1, d1 = 0, d2 = 61)
W.dW <- nb2listw(nlistd, style = "W")
W.dB <- nb2listw(nlistd, style = "B")
nlistk <- knn2nb(knearneigh(data.Set4.1, k = 4))
W.kW <- nb2listw(nlistk, style = "W")
W.kB <- nb2listw(nlistk, style = "B")
Silt <- data.Set4.1@data$Silt
moran(Silt,W.dW,86,Szero(W.dW))
moran.test(Silt,W.dW,randomisation = FALSE,alternative = "greater")
moran.test(Silt,W.dW,randomisation = TRUE,alternative = "greater")
moran.test(Silt,W.dB,randomisation = FALSE,alternative = "greater")
moran.test(Silt,W.dB,randomisation = TRUE,alternative = "greater")
moran.test(Silt,W.kW,randomisation = FALSE,alternative = "greater")
moran.test(Silt,W.kW,randomisation = TRUE,alternative = "greater")
moran.test(Silt,W.kB,randomisation = FALSE,alternative = "greater")
moran.test(Silt,W.kB,randomisation = TRUE,alternative = "greater")
set.seed(123)
moran.mc(Silt,W.dW,nsim = 1000,alternative = "greater")

geary.test(Silt,W.dW,randomisation = FALSE,alternative = "less")
geary.test(Silt,W.dW,randomisation = TRUE,alternative = "less")
geary.test(Silt,W.dB,randomisation = FALSE,alternative = "less")
geary.test(Silt,W.dB,randomisation = TRUE,alternative = "less")
geary.test(Silt,W.kW,randomisation = FALSE,alternative = "less")
geary.test(Silt,W.kW,randomisation = TRUE,alternative = "less")
geary.test(Silt,W.kB,randomisation = FALSE,alternative = "less")
geary.test(Silt,W.kB,randomisation = TRUE,alternative = "less")
set.seed(123)
geary.mc(Silt,W.dW,nsim = 1000,alternative = "greater")

# Thiesen polygon data
library(spatstat)
library(maptools)
W <- 592000
E <- 592500
S <- 4270300
N <- 4271200
cell.ppp <- ppp(data.Set4.1$Easting, data.Set4.1$Northing,
     window = owin(c(W, E), c(S, N)))
thsn.tess <- dirichlet(cell.ppp)
thsn.sp <- as(thsn.tess, "SpatialPolygons")
thsn.spdf <- SpatialPolygonsDataFrame(thsn.sp,
  slot(data.Set4.1, "data"), FALSE)

# Check that IDs match 
library(sf)
thsn.sf <- st_as_sf(thsn.spdf)
all.equal(1:length(thsn.sf$ID), thsn.sf$ID)

  
# Rook's case
nlist <- poly2nb(thsn.spdf,
  row.names = as.character(thsn.spdf$ID), queen = FALSE)
W.rB <- nb2listw(nlist, style = "B")
W.rW <- nb2listw(nlist, style = "W")
moran.test(Silt,W.rB,randomisation = FALSE,alternative = "greater")
moran.test(Silt,W.rB,randomisation = TRUE,alternative = "greater")
moran.test(Silt,W.rW,randomisation = FALSE,alternative = "greater")
moran.test(Silt,W.rW,randomisation = TRUE,alternative = "greater")
geary.test(Silt,W.rB,randomisation = FALSE,alternative = "greater")
geary.test(Silt,W.rB,randomisation = TRUE,alternative = "greater")
geary.test(Silt,W.rW,randomisation = FALSE,alternative = "greater")
geary.test(Silt,W.rW,randomisation = TRUE,alternative = "greater")

# Queen's case
nlist <- poly2nb(thsn.spdf,
  row.names = as.character(thsn.spdf$ID), queen = TRUE)
W.qB <- nb2listw(nlist, style = "B")
W.qW <- nb2listw(nlist, style = "W")
moran.test(Silt,W.qB,randomisation = FALSE,alternative = "greater")
moran.test(Silt,W.qB,randomisation = TRUE,alternative = "greater")
moran.test(Silt,W.qW,randomisation = FALSE,alternative = "greater")
moran.test(Silt,W.qW,randomisation = TRUE,alternative = "greater")
geary.test(Silt,W.qB,randomisation = FALSE,alternative = "greater")
geary.test(Silt,W.qB,randomisation = TRUE,alternative = "greater")
geary.test(Silt,W.qW,randomisation = FALSE,alternative = "greater")
geary.test(Silt,W.qW,randomisation = TRUE,alternative = "greater")


