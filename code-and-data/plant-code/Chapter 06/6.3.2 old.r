library(maptools)
library(sf)
library(gstat)

# Kriging interpolation
# From Section 5.4
# Determine the member of the population closest to a sample point
closest.point <- function(sample.pt, grid.data){
   dist.sq <- (coordinates(grid.data)[,1]-sample.pt[1])^2 +
      (coordinates(grid.data)[,2]-sample.pt[2])^2
   return(which.min(dist.sq))
}

data.Set4.2 <- read.csv("set4\\set4.296sample.csv", header = TRUE)
pop.sf <- st_read("Created\\Set42pop.shp")
pop.data <- as(pop.sf, "Spatial")

# From Sec. 5.3
W <- bbox(pop.data)[1,1]
E <- bbox(pop.data)[1,2]
S <- bbox(pop.data)[2,1]
N <- bbox(pop.data)[2,2]
N <- N + 2.5
S <- S - 2.5
E <- E + 2.5
W <- W - 2.5

coords.mat <- matrix(c(W,E,E,W,W,N,N,S,S,N), ncol = 2)
coords.lst <- list(coords.mat)
coords.pol = st_sfc(st_polygon(coords.lst))
sampbdry.sf = st_sf(z = 1, coords.pol)
st_crs(sampbdry.sf) <- "+proj=utm +zone=10 +ellps=WGS84"
sampbdry.sp <- as(sampbdry.sf, "Spatial")
sample.coords <- cbind(data.Set4.2$Easting, data.Set4.2$Northing)

# Find the yield at the closest sample points
samp.pts <- apply(sample.coords, 1, closest.point, grid.data = pop.data)
data.Set4.2$Yield <- pop.data$Yield[samp.pts]
coordinates(data.Set4.2) <- c("Easting", "Northing")
proj4string(data.Set4.2) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
# Interpolate to a 5 meter grid
print(Left <- bbox(sampbdry.sp)[1,1])
print(Right <- bbox(sampbdry.sp)[1,2])
print(Top <- bbox(sampbdry.sp)[2,2])
print(Bottom <- bbox(sampbdry.sp)[2,1])
cell.size <- 5
grid.xy <- expand.grid(Easting = seq(Left,Right,cell.size),
  Northing = seq(Top,Bottom,-cell.size))
coordinates(grid.xy) <- c("Easting", "Northing")
gridded(grid.xy) = TRUE
proj4string(grid.xy) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
greys <- grey(0:200 / 255)

# Kriging
data.var <- variogram(Yield ~ 1, data.Set4.2, cutoff = 600)
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

yield.krig <- krige(Yield ~ 1, data.Set4.2, grid.xy,
   model = data.fit)
spplot(yield.krig["var1.pred"], col.regions = greys,
  xlim = c(Left,Right), scales = list(draw = TRUE),
  xlab = "Easting", ylab = "Northing",  # Fig. 6.6
  main = "Field 4.2 Kriged Yield")
