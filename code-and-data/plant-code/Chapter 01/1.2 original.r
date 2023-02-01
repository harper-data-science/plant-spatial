# Fig. 1.3a: Kriged clay content in Field 4.2
library(gstat)
# - library(maptools)
data.Set4.2 <- read.csv("set4\\set4.296sample.csv", header = TRUE)
# Establish the coordinates of the rectangular boundary
N <- 4267868
S <- 4267513
E <- 592867
W <- 592082
cell.size <- 5
# Create the interpolation grid
# expand.grid() is a base package function
grid.xy <- expand.grid(x = seq(W, E, cell.size),  y = seq(N, S, -cell.size))
coordinates(grid.xy) <- ~x + y
gridded(grid.xy) = TRUE
coordinates(data.Set4.2) <- c("Easting", "Northing")
Clay.vgm <- variogram(Clay ~ 1, data.Set4.2)
Clay.fit <- fit.variogram(Clay.vgm, model = vgm(100000, "Sph", 700,10000))
plot(Clay.vgm, Clay.fit, col = "black")
Clay.krig <- krige(Clay ~ 1, data.Set4.2, grid.xy, model = Clay.fit)
pts.list = list("sp.points", data.Set4.2, pch = 19, col = "black",
   cex = 0.5)
greys <- grey(255:50 / 255)
spplot(Clay.krig["var1.pred"], col.regions = greys, # Fig. 1.3a
  xlim = c(W,E), ylim = c(S,N), scales = list(draw = TRUE),
  xlab = "Easting", ylab = "Northing",  sp.layout = list(pts.list),
  main = "Field 4.2 Clay Content (Percent)")

# Fig. 1.3b: Thiessen polygons for sample points in Field 4.2
library(spatstat)
# Create the Thiessen polygons
cell.ppp <- ppp(coordinates(data.Set4.2)[,1], coordinates(data.Set4.2)[,2],
     window = owin(c(W, E), c(S, N)))
thsn.pp <- dirichlet(cell.ppp)
thsn.sp <- as(thsn.pp, "SpatialPolygons")
row.names(thsn.sp) <- row.names(data.Set4.2@data)
# Transfer the data to create a SP Data Frame
thsn.shp <- SpatialPolygonsDataFrame(thsn.sp, data.Set4.2@data)
pts.list = list("sp.points", data.Set4.2, pch = 19, col = "black",
   cex = 0.5)
greys <- grey(255:50 / 255)
spplot(thsn.shp, "Clay", col.regions = greys,  scales = list(draw = TRUE),
  xlab = "Easting", ylab = "Northing", sp.layout = list(pts.list),
  main = "Field 4.2 Clay Content (Percent)")
