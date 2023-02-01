# Create an artificial population for sampling.
library(rgdal)
library(maptools)
library(gstat)
library(sf)

data.Set4.2Yield96raw <- read.csv("Set4\\Set4.296wheatyield.csv", header = TRUE)

# Rather than using sf objects, it is simpler to work directly with sp objects
coordinates(data.Set4.2Yield96raw) <- c("Easting", "Northing")
proj4string(data.Set4.2Yield96raw) <- CRS("+proj=utm +zone=10 +ellps=WGS84")

# Interpolate to a 5 meter grid
Left <- 592102.5
Right <- 592817.5
Top <- 4267857.5
Bottom <- 4267502.5
cell.size <- 5
grid.xy <- expand.grid(x = seq(Left,Right,cell.size),
  y = seq(Top,Bottom,-cell.size))
coordinates(grid.xy) <- ~x + y
gridded(grid.xy) = TRUE
proj4string(grid.xy) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
yield.idw <- krige(Yield ~ 1, data.Set4.2Yield96raw, grid.xy)
pop.data <- data.frame(yield.idw@data$var1.pred)
grid.xy <- expand.grid(Easting = seq(Left,Right,cell.size),
  Northing = seq(Top,Bottom,-cell.size))
coordinates(pop.data) <- grid.xy
names(pop.data) <- "Yield"
proj4string(pop.data) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
greys <- grey(0:200 / 255)
spplot(pop.data, zcol = "Yield", col.regions = greys,
  xlim = c(Left,Right), scales = list(draw = TRUE),
  xlab = "Easting", ylab = "Northing",
  main = "Field 4.2 1996 Artificial Yield Population") # Fig. 5.1a

# Create a figure of original and interpolated data points
data.Pop <- spRbind(pop.data,data.Set4.2Yield96raw[,2])
spplot(data.Pop, zcol = "Yield", col.regions = greys,
  xlim = c(592500,592550), ylim = c(4267750,4267800),
  scales = list(draw = TRUE),  # Fig. 5.1b
  xlab = "Easting", ylab = "Northing",
  main = "Actual Yield and Artificial Population")

# Color version using plot()
reds <- rgb((50:255)/255, green = 0, blue = 0)
par(mai = c(1,1,1,1))
plot(data.Set4.2Yield96raw[,2], axes = TRUE, col = reds, pch = 16,
   xlim = c(592500,592550), ylim = c(4267750,4267800),
   main = "Actual Yield (Red) and Artificial Population (Blue)")
blues <- rgb((50:255)/255, green = 0, red = 0)
plot(pop.data, add = TRUE, pch = 16, col = blues)

print(pop.mean <- mean(pop.data$Yield), digits = 5)
print(pop.sd <- sd(pop.data$Yield), digits = 5)

par(mai = c(1,1,1,1))
hist(pop.data$Yield, 100, plot = TRUE,
   main = "Histogram of Artificial Population",
   xlab = "Yield, kg/ha", font.lab = 2, cex.main = 2,
   cex.lab = 1.5)  # Fig. 5.2

# Convert to sf and save to disk
pop.sf <- st_as_sf(pop.data)
st_crs(pop.sf) <- "+proj=utm +zone=10 +ellps=WGS84"
st_write(pop.sf, "created\\Set42pop.shp")

# Create the sample boundary
W <- bbox(pop.data)[1,1]
E <- bbox(pop.data)[1,2]
S <- bbox(pop.data)[2,1]
N <- bbox(pop.data)[2,2]
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
st_crs(sampbdry.sf) <- "+proj=utm +zone=10 +ellps=WGS84"
st_write(sampbdry.sf, "created\\Set42sampbdry.shp")

