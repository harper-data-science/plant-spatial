not.spplot <- function(xyz, plot.main, plot.xlab, plot.ylab, 
  plot.col = FALSE){
   grid.xy <- expand.grid(
      x = seq(xmin(xyz) + 0.5 * res(xyz)[1],
         xmax(xyz) - 0.5 * res(xyz)[1], res(xyz)[1]),
      y = seq(ymin(xyz)+ 0.5 * res(xyz)[2],
         ymax(xyz) - 0.5 * res(xyz)[2], res(xyz)[2]))
   M1 <- matrix(values(xyz), nrow = nrow(xyz),
     ncol = ncol(xyz), byrow = TRUE)
   M2 <- matrix(0, nrow = nrow(xyz), 
      ncol = ncol(xyz))
   for (i in 1:nrow(M1))
      for(j in 1:ncol(M1))
         M2[i,j] <- M1[nrow(M1)-i+1,j]
   grid.xy$z <- as.vector(t(M2))
   p <- levelplot(z ~ x + y, grid.xy, aspect = nrow(xyz) / ncol(xyz),
   main = plot.main, xlab = plot.xlab, ylab = plot.ylab,
   col.regions = plot.col)
return(p)
}
        
# Fig. 1.3a: Kriged clay content in Field 4.2
# First in sf
library(sf)
data.Set4.2 <- read.csv("set4\\set4.296sample.csv", header = TRUE)
head(data.Set4.2)
data.Set4.2.sf <- st_as_sf(data.Set4.2, 
  coords = c("Easting", "Northing"), crs = "EPSG:32610")
# Establish the coordinates of the rectangular boundary
N <- 4267868
S <- 4267513
E <- 592867
W <- 592082
cell.size <- 5
library(stars)
library(starsExtra)
# Create the interpolation grid
# Based on https://rdrr.io/cran/starsExtra/man/make_grid.html
SW <- st_point(c(W, S))
NE <- st_point(c(E, N))
xy <- st_sfc(SW, NE)
xy.sf <- st_sf(xy)
grid.xy <- make_grid(xy.sf, res = cell.size, buffer = 0)
st_crs(grid.xy) <- "EPSG:32610" # UTM Zone 10N
library(gstat)
Clay.vgm <- variogram(Clay ~ 1, data.Set4.2.sf)
Clay.fit <- fit.variogram(Clay.vgm,
  model = vgm(100000, "Sph", 700,10000))
Clay.krig <- krige(Clay ~ 1, data.Set4.2.sf, grid.xy,
   model = Clay.fit)
library(ggplot2)
ggplot() + 
    geom_stars(data = Clay.krig, aes(fill = var1.pred,
       x = x, y = y)) +
    scale_fill_gradient(low = "white", high = "black") + 
    geom_sf(data = xy.sf, cex = 0.5)

# Next to raster and terra
# First convert to a raster object
library(raster)
Clay.krig.ras <- as(Clay.krig["var1.pred"], "Raster")
greys <- grey(255:50 / 255)
# Next convert to a terra object
library(terra)
library(lattice)
Clay.krig.ter <- rast(Clay.krig.ras)
library("latticeExtra")

my.sppanel <- 	function(obj, pch = 3, ...)
		panel.points(st_coordinates(obj), pch = pch, ...)
my.points <- function(obj, pch=3, ...) my.sppanel(obj, pch = pch, ...)


not.spplot(Clay.krig.ter, 
   plot.main = "Field 4.2 Clay Content (Percent)",   
   plot.xlab = "Easting", plot.ylab = "Northing",
   plot.col = greys)  +
  layer(my.points(data.Set4.2.sf, pch = 19, col = 1,
     magicdots = FALSE, cex = 0.5))


   
# Fig. 1.3b: Thiessen polygons for sample points in Field 4.2
library(spatstat)
library(sf)
data.Set4.2 <- read.csv("set4\\set4.296sample.csv", header = TRUE)
data.Set4.2 <- read.csv("set4\\set4.296sample.csv", header = TRUE)
head(data.Set4.2)
data.Set4.2.sf <- st_as_sf(data.Set4.2, 
  coords = c("Easting", "Northing"), crs = "EPSG:32610")
# Establish the coordinates of the rectangular boundary
N <- 4267868
S <- 4267513
E <- 592867
W <- 592082
# Create the Thiessen polygons
# From https://github.com/r-spatial/sf/issues/1233 
cell.ppp <- ppp(data.Set4.2$Easting, data.Set4.2$Northing,
     window = owin(c(W, E), c(S, N)))
cell.quad <- quadratcount(cell.ppp, nx = 13, ny = 6)
thsn.tess <- as.tess(cell.quad)
thsn.sfc <- st_as_sfc(thsn.tess)
plot(thsn.sfc) # Thiessen polygons
thsn.sf <- st_sf(thsn.sfc)
thsn.sf$Clay <- data.Set4.2$Clay
st_crs(thsn.sf) <- "EPSG:32610" # UTM Zone 10N
plot(thsn.sf) # Plots Clay content of sf polygon object
# Next convert to a terra object
library(terra)
thsn.ter <- vect(thsn.sf)
plot(thsn.ter, y = 1) # Still the problem with y-axis
data.Set4.2.sf <- st_as_sf(data.Set4.2, 
  coords = c("Easting", "Northing"), crs = "EPSG:32610")
library(ggplot2)     
ggplot() + 
    geom_sf(data = thsn.sf, aes(fill = Clay)) +
       scale_fill_gradient(low = "white", high = "black") +
       xlab(expression(paste("Easting"))) +
       ylab(expression(paste("Northing"))) +
       ggtitle("                  Field 4.2 Clay Content (Percent)") +
       geom_sf(data = data.Set4.2.sf, cex = 1) +
       theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
       coord_sf(datum = st_crs(32610))    

