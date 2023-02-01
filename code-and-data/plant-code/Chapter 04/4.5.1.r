# Construction of Moran correlograms
library(spatstat)
library(sf)

data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
data.Set4.1$x <- data.Set4.1$Easting - min(data.Set4.1$Easting)
data.Set4.1$y <- data.Set4.1$Northing - min(data.Set4.1$Northing)
data.Set4.1.sf <- st_as_sf(data.Set4.1, coords = c("x", "y"))

Silt <- data.Set4.1.sf$Silt
library(spdep)
nlist <- dnearneigh(data.Set4.1.sf, d1 = 0, d2 = 61)
I.d <- sp.correlogram(nlist, Silt, order = 5,
  method = "I", style = "W", randomisation = TRUE)
  
nlist.1 <- dnearneigh(data.Set4.1.sf, d1 = 0, d2 = 61)
nlist.2 <- dnearneigh(data.Set4.1.sf, d1 = 61.1, d2 = 122)
# Thiesen polygon data
W <- 592000
E <- 592500
S <- 4270300
N <- 4271200

library(spatstat)
cell.ppp <- ppp(data.Set4.1$Easting, data.Set4.1$Northing,
     window = owin(c(W, E), c(S, N)))
thsn.tess <- dirichlet(cell.ppp)

# Chack that each location is within the correspondng cell
all.equal <- TRUE
for (i in 1:length(data.Set4.1$ID)){
   if (data.Set4.1$Easting[i] > 
      max(thsn.tess$tiles[[i]]$bdry[[1]]$x)) all.equal <- FALSE
   if (data.Set4.1$Easting[i] < 
      min(thsn.tess$tiles[[i]]$bdry[[1]]$x)) all.equal <- FALSE
   if (data.Set4.1$Northing[i] > 
      max(thsn.tess$tiles[[i]]$bdry[[1]]$y)) all.equal <- FALSE
   if (data.Set4.1$Northing[i] < 
      min(thsn.tess$tiles[[i]]$bdry[[1]]$y)) all.equal <- FALSE
  }
all.equal  
  
# Rook's case
thsn.sfc <- st_as_sfc(thsn.tess)
thsn.sf <- st_sf(thsn.sfc)
thsn.sp <- as(thsn.sf, "Spatial")
data.Set4.1.sp <- as(data.Set4.1.sf, "Spatial")
thsn.spdf <- SpatialPolygonsDataFrame(thsn.sp,
  data.Set4.1.sp@data, FALSE)

nlist <- poly2nb(thsn.sp,
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

