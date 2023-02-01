# Cokriging interpolation
# Determine the member of the population closest to a sample point
library(sf)
library(gstat)

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

sample.coords <- cbind(data.Set4.2$Easting, data.Set4.2$Northing)
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

data.Set4.2EC <- read.csv("Set4\\Set4.2EC.csv", header = TRUE)
data.Set4.2EC.sf <- st_as_sf(data.Set4.2EC,
   coords = c("Easting", "Northing"))
st_crs(data.Set4.2EC.sf) <- "EPSG:32610" # UTM Zone 10N

plot(data.Set4.2EC.sf["ECto30"],
   axes = TRUE, cex = 0.4, pch = 1, col = "black",
   main = expression(Soil~EC[a]~and~Clay~Sample~Locations),
      cex.main = 1, reset = FALSE)   # Fig. 6.7
plot(st_geometry(data.Set4.2.sf), add = TRUE, pch = 16, col = "black")

# Color version
plot(data.Set4.2EC.sf["ECto30"],
   axes = TRUE, cex = 0.4, pch = 1, col = "blue",
   main = expression(Soil~EC[a]~and~Clay~Sample~Locations),
      cex.main = 1, reset = FALSE)   # Fig. 6.7
plot(st_geometry(data.Set4.2.sf), add = TRUE, pch = 16, col = "red")

names(data.Set4.2EC.sf)
EC.pts <-
   apply(sample.coords, 1, closest.point,
    grid.data = data.Set4.2EC.sf)
data.Set4.2.sf$ECto30 <- data.Set4.2EC$ECto30[EC.pts]
data.Set4.2.sf$ECto100 <- data.Set4.2EC$ECto100[EC.pts]
with(data.Set4.2.sf, cor(ECto30, Clay))
with(data.Set4.2.sf, cor(ECto100, Clay))
with(data.Set4.2.sf, plot(ECto30, Clay, cex.main = 2,
   main = expression(Soil~EC[a]~vs.~Clay~Content),
   xlab = expression(EC[a]~"("*mS/m*")"), cex.lab = 1.5,
   ylab = "Percent Clay")) # Fig. 6.8
identify(data.Set4.2.sf$ECto30, data.Set4.2.sf$Clay,
    data.Set4.2.sf$ID)
data.Set4.2.cleaned.sf <- data.Set4.2.sf[-c(18,31),]
with(data.Set4.2.cleaned.sf, cor(ECto30, Clay))
with(data.Set4.2.cleaned.sf, cor(ECto100, Clay))
with(data.Set4.2.cleaned.sf, plot(ECto30, Clay, cex.main = 2,
   main = expression(Soil~EC[a]~vs.~Clay~Content),
   xlab = expression(EC[a]~"("*mS/m*")"), cex.lab = 1.5,
   ylab = "Percent Clay")) 

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

# First do kriging
clay.var <- variogram(Clay ~ 1, data.Set4.2.cleaned.sf, cutoff = 600)
clay.fit <- fit.variogram(clay.var, model = vgm(1, "Sph", 700, 1))
par(mai = c(1,1,1,1))
plot(clay.var, clay.fit, col = "black",
   cex.main = 2, cex.lab = 1.5,
   main = "Clay Variogram", 
   xlab = expression(bold("lag h")), xlim = c(0,400),
   ylab = (expression(bold(hat(gamma)*(h)))))
# Range = about 400
clay.krige.stars <- krige(Clay ~ 1, data.Set4.2.cleaned.sf, grid.xy,
   model = clay.fit)

# Now do cokriging
# Create the gstat object
# First do a variogram of EC to match the ranges
EC.var <- variogram(ECto30 ~ 1, data.Set4.2EC.sf, cutoff = 600)
EC.fit <- fit.variogram(EC.var, model = vgm(1, "Sph", 700, 1))
plot(EC.var, EC.fit, col = "black",
   cex.main = 2, cex.lab = 1.5,
   main = "EC Variogram", 
   xlab = expression(bold("lag h")), xlim = c(0,400),
   ylab = (expression(bold(hat(gamma)*(h)))))
# Range = about 300, so use 350 from the combined range
   
g.cok <- gstat(NULL, "Clay", Clay ~ 1, data.Set4.2.cleaned.sf)
g.cok <- gstat(g.cok, "ECto30", ECto30 ~ 1, data.Set4.2EC.sf)
g.var <- variogram(g.cok)

# If that didn't work, try reducing the size of the data set
nrow(data.Set4.2EC)
ID <- 1:nrow(data.Set4.2EC.sf)
mod.fac <- 100
data.Set4.2ECmod.sf <- data.Set4.2EC.sf[which(ID %% mod.fac == 0),]
g.cok <- gstat(NULL, "Clay", Clay ~ 1, data.Set4.2.sf)
g.cok <- gstat(g.cok, "ECto30", ECto30 ~ 1, data.Set4.2ECmod.sf)
g.var <- variogram(g.cok)

g.fit <- fit.lmc(g.var, g.cok, vgm(1, "Sph", 350, 1))
g.cokrige.stars <- predict(g.fit, grid.xy) # This works
names(g.cokrige.stars)

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

library(raster)
g.cokrige.ras <- as(g.cokrige.stars, "Raster")
library(terra)
library(lattice)
g.cokrige.ter <- rast(g.cokrige.ras)
library("latticeExtra")
greys <- grey(0:200 / 255)

not.spplot(g.cokrige.ter, 
   plot.main = "Field 4.2 Cokriged Clay",   
   plot.xlab = "Easting", plot.ylab = "Northing",
   plot.col = greys)      # Fig. 6.9a

clay.krige.ras <- as(clay.krige.stars, "Raster")
clay.krige.ter <- rast(clay.krige.ras)

not.spplot(clay.krige.ter, 
   plot.main = "Field 4.2 Kriged Clay",   
   plot.xlab = "Easting", plot.ylab = "Northing",
   plot.col = greys)      # Fig. 6.9b



# Cross validation
claykrige.cv <- krige.cv(Clay ~ 1, data.Set4.2, grid.xy,
   model = clay.fit, nfold = nrow(data.Set4.2))
res.krige <- claykrige.cv$residual
mean(res.krige)   # Mean error (bias)
sqrt(mean(res.krige^2)) # RMSE (variance)

claycok.cv <- gstat.cv(g.fit, nfold = nrow(data.Set4.2))
res.cok <- claycok.cv$residual
mean(res.cok)   # Mean error (bias)
sqrt(mean(res.cok^2)) # RMSE (variance)
