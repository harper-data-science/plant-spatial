library(sf)
library(maptools)
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
samp.pts <- apply(sample.coords, 1, closest.point,
   grid.data = pop.data)
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
greys <- grey(0:250 / 255)

yield.idw <- idw(Yield ~ 1, data.Set4.2, grid.xy, idp = 2, nmax = 12)
spplot(yield.idw["var1.pred"], col.regions = greys, # Fig. 6.5a
  xlim = c(Left,Right), scales = list(draw = TRUE),
  xlab = "Easting", ylab = "Northing",
  main = "Field 4.2 Interpolated Yield, n = 12, p = 2")

yield.idw <- idw(Yield ~ 1, data.Set4.2, grid.xy, idp = 2, nmax = 4)
spplot(yield.idw["var1.pred"], col.regions = greys,
  xlim = c(Left,Right), scales = list(draw = TRUE),
  xlab = "Easting", ylab = "Northing", # Fig. 6.5b
  main = "Field 4.2 Interpolated Yield, n = 4, p = 2")
  
yield.idw <- idw(Yield ~ 1, data.Set4.2, grid.xy, idp = 4, nmax = 12)
spplot(yield.idw["var1.pred"], col.regions = greys,
  xlim = c(Left,Right), scales = list(draw = TRUE),
  xlab = "Easting", ylab = "Northing", # Fig. 6.5c
  main = "Field 4.2 Interpolated Yield, n = 12, p = 4")
  
yield.idw <- idw(Yield ~ 1, data.Set4.2, grid.xy, idp = 1, nmax = 12)
spplot(yield.idw["var1.pred"], col.regions = greys,
  xlim = c(Left,Right), scales = list(draw = TRUE),
  xlab = "Easting", ylab = "Northing", # Fig. 6.5d
  main = "Field 4.2 Interpolated Yield, n = 12, p = 1")


