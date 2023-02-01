library(sf)
library(gstat)

# Create Fig. 6.2
plot(0:1,0:1,type="n", axes = FALSE,  # Fig. 6.2
   xlab = "", ylab = "")
points(0.5, 0.5, pch = 16)
points(0.75, 0.75, pch = 16)
arrows(0.5, 0.5, 0.75, 0.75, length = 0.1, code = 3, lwd = 2)
text(0.47, 0.52, expression(bold(x)), cex = 1.5)
text (0.73, 0.78, expression(bold(x[1])), cex = 1.5)
text(0.6, 0.67, expression(bold(d(x,x[1]))))
points(0.7, 0.4, pch = 16)
arrows(0.5, 0.5, 0.7, 0.4, length = 0.1, code = 3, lwd = 2)
text (0.73, 0.42, expression(bold(x[2])), cex = 1.5)
text(0.65, 0.49, expression(bold(d(x,x[2]))))
points(0.4, 0.4, pch = 16)
text (0.4, 0.35, expression(bold(x[3])), cex = 1.5)
arrows(0.5, 0.5, 0.4, 0.4, length = 0.1, code = 3, lwd = 2)
text(0.38, 0.46, expression(bold(d(x,x[3]))))

# Create Fig. 6.3
x <- seq(0.25,10,0.25)
d1 <- 1/x
d2 <- 1/x^2
d4 <- 1/x^4
par(mai = c(1,1,1,1))
plot(x,d1, type = "l", main = expression(d^"-p"~"for various p"),
   cex.main = 2, ylab = expression(d^"-p"), cex.lab = 1.5)  #Fig 6.3
lines(x,d2, lty = 2)
lines(x,d4, lty = 3)
legend(6, 3, c("p = 1", "p = 2", "p = 4"),
  lty = c(1,2,3))

# Color version
plot(x,d1, type = "l", main = expression(d^"-p"~"for various p"),
   cex.main = 2, ylab = expression(d^"-p"), cex.lab = 1.5, lwd = 2, col = "red")  
lines(x,d2, lwd = 2, col = "blue")
lines(x,d4, lwd = 2, col = "green")
legend(6, 3, c("p = 1", "p = 2", "p = 4"), lwd = c(2,2,2),
  col = c("red", "blue", "green"))


# IDW Interpolation
# Determine the member of the population closest to a sample point
closest.point <- function(sample.pt, grid.data){
   dist.sq <- (st_coordinates(grid.data)[,1]-sample.pt[1])^2 +
      (st_coordinates(grid.data)[,2]-sample.pt[2])^2
   return(which.min(dist.sq))
}

data.Set4.2 <- read.csv("set4\\set4.296sample.csv", header = TRUE)
data.Set4.2.sf <- st_as_sf(data.Set4.2,
  coords = c("Easting", "Northing"))
st_crs(data.Set4.2.sf) <- "EPSG:32610" # UTM Zone 10N
pop.data.sf <- st_read("Created\\Set42pop.shp")
st_crs(pop.data.sf) <- "EPSG:32610" # UTM Zone 10N


# From Sec. 5.3
W <- st_bbox(pop.data.sf)[1]
E <- st_bbox(pop.data.sf)[3]
S <- st_bbox(pop.data.sf)[2]
N <- st_bbox(pop.data.sf)[4]
N <- N + 2.5
S <- S - 2.5
E <- E + 2.5
W <- W - 2.5
coords.mat <- matrix(c(W,E,E,W,W,N,N,S,S,N), ncol = 2)
coords.lst <- list(coords.mat)
coords.pol = st_sfc(st_polygon(coords.lst))
sampbdry.sf = st_sf(z = 1, coords.pol)
st_crs(sampbdry.sf) <- "EPSG:32610" # UTM Zone 10N

# Find the yield at the closest sample points
data.Set4.2.coords <- st_coordinates(data.Set4.2.sf)
samp.pts.sf <- apply(data.Set4.2.coords, 1, closest.point,
   grid.data = pop.data.sf)
data.Set4.2.sf$Yield <- pop.data.sf$Yield[samp.pts.sf]

# Interpolate to a 5 meter grid
print(Left <- st_bbox(sampbdry.sf)[1])
print(Right <- st_bbox(sampbdry.sf)[3])
print(Top <- st_bbox(sampbdry.sf)[4])
print(Bottom <- st_bbox(sampbdry.sf)[2])

# From Sec. 5.2.1
SW <- st_point(c(Left, Bottom))
NE <- st_point(c(Right, Top))
xy <- st_sfc(SW, NE)
xy.sf <- st_sf(xy)
cell.size <- 5
library(stars)
library(starsExtra)
grid.xy <- make_grid(xy.sf, res = cell.size, buffer = 0)
st_crs(grid.xy) <- "EPSG:32610" # UTM Zone 10N
yield.idw.stars <- krige(Yield ~ 1, data.Set4.2.sf, grid.xy)
library(raster)
yield.idw.ras <- as(yield.idw.stars["var1.pred"], "Raster")
library(terra)
library(lattice)
yield.idw.ter <- rast(yield.idw.ras)
library("latticeExtra")

greys <- grey(0:250 / 255)

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

not.spplot(yield.idw.ter, 
   plot.main = "Field 4.2 Interpolated Yield, n = 12, p = 2",   
   plot.xlab = "Easting", plot.ylab = "Northing",
   plot.col = greys)  

yield.idw.stars <- gstat::idw(Yield ~ 1, data.Set4.2.sf, grid.xy,
   idp = 2, nmax = 4)
yield.idw.ter <- rast(yield.idw.ras)
not.spplot(yield.idw.ter, 
   plot.main = "Field 4.2 Interpolated Yield, n = 4, p = 2",   
   plot.xlab = "Easting", plot.ylab = "Northing",
   plot.col = greys)  

yield.idw.stars <- gstat::idw(Yield ~ 1, data.Set4.2.sf, grid.xy,
   idp = 4, nmax = 12)
yield.idw.ter <- rast(yield.idw.ras)
not.spplot(yield.idw.ter, 
   plot.main = "Field 4.2 Interpolated Yield, n = 12, p = 4",   
   plot.xlab = "Easting", plot.ylab = "Northing",
   plot.col = greys) 

yield.idw.stars <- gstat::idw(Yield ~ 1, data.Set4.2.sf, grid.xy,
   idp = 1, nmax = 12)
yield.idw.ter <- rast(yield.idw.ras)
not.spplot(yield.idw.ter, 
   plot.main = "Field 4.2 Interpolated Yield, n = 12, p = 1",   
   plot.xlab = "Easting", plot.ylab = "Northing",
   plot.col = greys) 
    
