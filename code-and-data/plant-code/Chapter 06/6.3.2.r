library(sf)
library(gstat)

# Kriging interpolation
# From Section 6.3.1

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
greys <- grey(0:200 / 255)

# Kriging
data.var <- variogram(Yield ~ 1, data.Set4.2.sf, cutoff = 600)
data.fit <- fit.variogram(data.var, model = vgm(150000, "Sph", 250, 1))
par(mai = c(1,1,1,1))
plot(data.var, data.fit, col = "black",
   cex.main = 2, cex.lab = 1.5,
   main = "Yield Variogram",   # Fig. 6.5a
   xlab = expression(bold("lag h")), xlim = c(0,400),
   ylab = (expression(bold(hat(gamma)*(h)))))

r1 <- seq(0,250,25)
a <- data.fit$range[2]
g.sph <- 1.5 * r1 / a -0.5 * (r1 / a)^3
b <- data.fit$psill[1]
cc <- data.fit$psill[2]
g.hat <- b + cc * g.sph
c.hat <- cc - g.hat

par(mai = c(1,1,1,1))
plot(r1, c.hat, type = "l", # Fig. 6.5b
   ylim = c(0,2000000), xlim = c(0,400),
   xlab = expression(bold("lag h")),
   ylab = expression(bold(tilde(C)~"(h)")),
   main = expression(Covariogram~tilde(C)*(h)))
lines(c(250,400),c(0,0))

yield.krig.stars <- krige(Yield ~ 1, data.Set4.2.sf, grid.xy,
   model = data.fit)
library(raster)
yield.krig.ras <- as(yield.krig.stars["var1.pred"], "Raster")
library(terra)
library(lattice)
yield.krig.ter <- rast(yield.krig.ras)
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

not.spplot(yield.krig.ter,       # Fig. 6.6
   plot.main = "Field 4.2 Kriged Yield",   
   plot.xlab = "Easting", plot.ylab = "Northing",
   plot.col = greys)  

