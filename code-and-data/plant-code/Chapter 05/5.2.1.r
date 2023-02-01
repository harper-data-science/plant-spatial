# Create an artificial population for sampling.
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

library(gstat)
library(sf)

data.Set4.2Yield96raw <- 
  read.csv("Set4\\Set4.296wheatyield.csv", header = TRUE)
data.Set4.2Yield96raw.sf <- st_as_sf(data.Set4.2Yield96raw, 
  coords = c("Easting", "Northing"), crs = "EPSG:32610")

# Interpolate to a 5 meter grid
Left <- 592102.5
Right <- 592817.5
Top <- 4267857.5
Bottom <- 4267502.5
cell.size <- 5
library(stars)
library(starsExtra)
# Create the interpolation grid
# Based on https://rdrr.io/cran/starsExtra/man/make_grid.html
SW <- st_point(c(Left, Bottom))
NE <- st_point(c(Right, Top))
xy <- st_sfc(SW, NE)
xy.sf <- st_sf(xy)
grid.xy <- make_grid(xy.sf, res = cell.size, buffer = 0)
st_crs(grid.xy) <- "EPSG:32610" # UTM Zone 10N
yield.idw.stars <- krige(Yield ~ 1, data.Set4.2Yield96raw.sf, grid.xy)
#pop.data.stars <- data.frame(yield.idw@data$var1.pred)
# sf object for Fig 5.1b
#grid.xy <- expand.grid(Easting = seq(Left,Right,cell.size),
#  Northing = seq(Top,Bottom,-cell.size))
#pop.data.sf
library(raster)
yield.idw.ras <- as(yield.idw.stars["var1.pred"], "Raster")
greys <- grey(255:50 / 255)
library(terra)
library(lattice)
yield.idw.ter <- rast(yield.idw.ras)
library("latticeExtra")
greys <- grey(0:200 / 255)

not.spplot(yield.idw.ter, 
   plot.main = "Field 4.2 1996 Artificial Yield Population",   
   plot.xlab = "Easting", plot.ylab = "Northing",
   plot.col = greys)  

yield.idw.sf <- st_as_sf(yield.idw.stars, as_points = TRUE)
# BW version using plot()
plot(data.Set4.2Yield96raw.sf["Yield"], axes = TRUE, col = "grey",
   pch = 16, reset = FALSE,
   xlim = c(592500,592550), ylim = c(4267750,4267800),
   main = "Actual Yield (Gray) and Artificial Population (Black)")
plot(yield.idw.sf["var1.pred"], add = TRUE, pch = 16, col = "black")

# Color version using plot()
reds <- rgb((50:255)/255, green = 0, blue = 0)
par(mai = c(1,1,1,1))
plot(data.Set4.2Yield96raw.sf["Yield"], axes = TRUE, col = reds,
   pch = 16, reset = FALSE,
   xlim = c(592500,592550), ylim = c(4267750,4267800),
   main = "Actual Yield (Red) and Artificial Population (Blue)")
blues <- rgb((50:255)/255, green = 0, red = 0)
plot(yield.idw.sf["var1.pred"], add = TRUE, pch = 16, col = "blue")

print(pop.mean <- mean(yield.idw.sf$var1.pred), digits = 5)
print(pop.sd <- sd(yield.idw.sf$var1.pred), digits = 5)

par(mai = c(1,1,1,1))
hist(yield.idw.sf$var1.pred, 100, plot = TRUE,
   main = "Histogram of Artificial Population",
   xlab = "Yield, kg/ha", font.lab = 2, cex.main = 2,
   cex.lab = 1.5)  # Fig. 5.2

# Save to disk
st_write(yield.idw.sf, "created\\Set42pop.shp")

# Create the sample boundary
W <- st_bbox(yield.idw.sf)[1]
E <- st_bbox(yield.idw.sf)[3]
S <- st_bbox(yield.idw.sf)[2]
N <- st_bbox(yield.idw.sf)[4]
N - S
E - W
N <- N + 2.5
S <- S - 2.5
E <- E + 2.5
W <- W - 2.5
coords.mat <- matrix(c(W,E,E,W,W,N,N,S,S,N), ncol = 2)
coords.lst <- list(coords.mat)
coords.pol = st_sfc(st_polygon(coords.lst))
sampbdry.sf = st_sf(z = 1, coords.pol)
st_crs(sampbdry.sf) <- "EPSG:32610" # UTM Zone 10N
st_write(sampbdry.sf, "created\\Set42sampbdry.shp")

