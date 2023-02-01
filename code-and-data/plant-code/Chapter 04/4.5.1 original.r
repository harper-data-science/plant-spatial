# Construction of Moran correlograms
library(maptools)
library(spatstat)
library(spdep)

data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
data.Set4.1$x <- data.Set4.1$Easting - min(data.Set4.1$Easting)
data.Set4.1$y <- data.Set4.1$Northing - min(data.Set4.1$Northing)
coordinates(data.Set4.1) <- c("x", "y")
Silt <- data.Set4.1@data$Silt

nlist <- dnearneigh(data.Set4.1, d1 = 0, d2 = 61)
I.d <- sp.correlogram(nlist, Silt, order = 5,
  method = "I", style = "W", randomisation = TRUE)
  
nlist.1 <- dnearneigh(data.Set4.1, d1 = 0, d2 = 61)
nlist.2 <- dnearneigh(data.Set4.1, d1 = 61.1, d2 = 122)
# Thiesen polygon data
W <- 592000
E <- 592500
S <- 4270300
N <- 4271200
cell.ppp <- ppp(data.Set4.1$Easting, data.Set4.1$Northing,
     window = owin(c(W, E), c(S, N)))
thsn.tess <- dirichlet(cell.ppp)
thsn.sp <- as(thsn.tess, "SpatialPolygons")
thsn.spdf <- SpatialPolygonsDataFrame(thsn.sp,
  data.Set4.1@data, FALSE)

# Check that IDs match 
library(sf)
thsn.sf <- st_as_sf(thsn.spdf)
all.equal(1:length(thsn.sf$ID), thsn.sf$ID)

  
# Rook's case
nlist <- poly2nb(thsn.spdf,
  row.names = as.character(thsn.spdf$ID), queen = FALSE)
I.r <- sp.correlogram(nlist, Silt, order = 5,
  method = "I", style = "W", randomisation = TRUE)

# Queen's case
nlist <- poly2nb(thsn.spdf,
  row.names = as.character(thsn.spdf$ID), queen = TRUE)
I.q <- sp.correlogram(nlist, Silt, order = 5,
  method = "I", style = "W", randomisation = TRUE)

moran.cor <- c(I.d$res[,1], I.r$res[,1], I.q$res[,1])
cor.type <- sort(rep(1:3, 5))
cor.type
cor.index <- factor(cor.type,
  labels = c("Distance Based", "Rook's Case", "Queen's Case"))
mor.frame <- data.frame(moran.cor)
mor.frame$index <- cor.index
mor.frame$lag <- rep(1:5, 3)
head(mor.frame, 2)
tail(mor.frame, 2)
library(lattice)
trellis.device(color = FALSE)
xyplot(moran.cor ~ lag | index, data = mor.frame, type = "o",
   layout = c(3,1), col = "black", aspect = c(1.0),
   xlab = "Lag", ylab = "Moran's I",
   main = "Moran Correlograms") # Fig. 4.3

