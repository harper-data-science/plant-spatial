# Cokriging interpolation
# Determine the member of the population closest to a sample point
library(sf)
library(maptools)
library(gstat)

closest.point <- function(sample.pt, grid.data){
   dist.sq <- (coordinates(grid.data)[,1]-sample.pt[1])^2 +
      (coordinates(grid.data)[,2]-sample.pt[2])^2
   return(which.min(dist.sq))
}

data.Set4.2 <- read.csv("set4\\set4.296sample.csv", header = TRUE)
pop.sf <- st_read("Created\\Set42pop.shp")
pop.data <- as(pop.sf, "Spatial")

sample.coords <- cbind(data.Set4.2$Easting, data.Set4.2$Northing)
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

data.Set4.2EC <- read.csv("Set4\\Set4.2EC.csv", header = TRUE)
coordinates(data.Set4.2EC) <- c("Easting", "Northing")
coordinates(data.Set4.2) <- c("Easting", "Northing")


par(mai = c(1,1,1,1))
plot(data.Set4.2EC, axes = TRUE, cex = 0.4, pch = 1)   # Fig. 6.7
plot(data.Set4.2, add = TRUE, pch = 16)
title(main = expression(Soil~EC[a]~and~Clay~Sample~Locations),
   cex.main = 2, xlab = "Easting", ylab = "Northing", cex.lab = 1.5)

# Color version
par(mai = c(1,1,1,1))
plot(data.Set4.2EC, axes = TRUE, cex = 0.4, pch = 1, col = "blue")
plot(data.Set4.2, add = TRUE, pch = 16, col = "red")
title(main = expression(Soil~EC[a]~and~Clay~Sample~Locations),
   cex.main = 2, xlab = "Easting", ylab = "Northing", cex.lab = 1.5)

names(data.Set4.2EC@data)
EC.pts <-
   apply(sample.coords, 1, closest.point, grid.data = data.Set4.2EC)
data.Set4.2@data$ECto30 <- data.Set4.2EC@data$ECto30[EC.pts]
data.Set4.2@data$ECto100 <- data.Set4.2EC@data$ECto100[EC.pts]
with(data.Set4.2@data, cor(ECto30, Clay))
with(data.Set4.2@data, cor(ECto100, Clay))
with(data.Set4.2@data, plot(ECto30, Clay, cex.main = 2,
   main = expression(Soil~EC[a]~vs.~Clay~Content),
   xlab = expression(EC[a]~"("*mS/m*")"), cex.lab = 1.5,
   ylab = "Percent Clay")) # Fig. 6.8

identify(data.Set4.2@data$ECto30, data.Set4.2@data$Clay,
    data.Set4.2@data$ID)
data.Set4.2.cleaned <- data.Set4.2[-c(18,31),]
with(data.Set4.2.cleaned@data, cor(ECto30, Clay))
with(data.Set4.2.cleaned@data, cor(ECto100, Clay))
with(data.Set4.2.cleaned@data, plot(ECto30, Clay, cex.main = 2,
   main = expression(Soil~EC[a]~vs.~Clay~Content),
   xlab = expression(EC[a]~"("*mS/m*")"), cex.lab = 1.5,
   ylab = "Percent Clay")) 

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
greys <- grey(0:200 / 255)

# First do kriging
clay.var <- variogram(Clay ~ 1, data.Set4.2.cleaned, cutoff = 600)
clay.fit <- fit.variogram(clay.var, model = vgm(1, "Sph", 700, 1))
par(mai = c(1,1,1,1))
plot(clay.var, clay.fit, col = "black",
   cex.main = 2, cex.lab = 1.5,
   main = "Clay Variogram", 
   xlab = expression(bold("lag h")), xlim = c(0,400),
   ylab = (expression(bold(hat(gamma)*(h)))))
# Range = about 400
clay.krige <- krige(Clay ~ 1, data.Set4.2.cleaned, grid.xy,
   model = clay.fit)

# Now do cokriging
# Create the gstat object
# First do a variogram of EC to match the ranges
EC.var <- variogram(ECto30 ~ 1, data.Set4.2EC, cutoff = 600)
EC.fit <- fit.variogram(EC.var, model = vgm(1, "Sph", 700, 1))
plot(EC.var, EC.fit, col = "black",
   cex.main = 2, cex.lab = 1.5,
   main = "EC Variogram", 
   xlab = expression(bold("lag h")), xlim = c(0,400),
   ylab = (expression(bold(hat(gamma)*(h)))))
# Range = about 300, so use 350 from the combined range
   
g.cok <- gstat(NULL, "Clay", Clay ~ 1, data.Set4.2.cleaned)
g.cok <- gstat(g.cok, "ECto30", ECto30 ~ 1, data.Set4.2EC)
g.var <- variogram(g.cok)
# Didn't work. Try reducting the size of the data set
nrow(data.Set4.2EC)
data.Set4.2EC <- read.csv("Set4\\Set4.2EC.csv")
ID <- 1:nrow(data.Set4.2EC)
mod.fac <- 10
data.Set4.2ECmod <- data.Set4.2EC[which(ID %% mod.fac == 0),]
coordinates(data.Set4.2ECmod) <- c("Easting", "Northing")
g.cok <- gstat(NULL, "Clay", Clay ~ 1, data.Set4.2)
g.cok <- gstat(g.cok, "ECto30", ECto30 ~ 1, data.Set4.2ECmod)
g.var <- variogram(g.cok)
g.fit <- fit.lmc(g.var, g.cok, vgm(1, "Sph", 350, 1))
g.cokrige <- predict(g.fit, grid.xy) # This works
names(g.cokrige)

spplot(g.cokrige["Clay.pred"], col.regions = greys,
  xlim = c(Left,Right), scales = list(draw = TRUE),
  xlab = "Easting", ylab = "Northing", 
  main = "Field 4.2 Cokriged Clay")  # Fig. 6.9a
  
spplot(clay.krige["var1.pred"], col.regions = greys,
  xlim = c(Left,Right), scales = list(draw = TRUE),
  xlab = "Easting", ylab = "Northing", 
  main = "Field 4.2 Kriged Clay")   # Fig. 6.9b


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
