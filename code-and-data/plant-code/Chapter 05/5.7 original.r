# Model based sampling
library(maptools)
library(sf)
library(raster)
library(rgdal)

pop.sf <- st_read("Created\\Set42pop.shp")
pop.data <- as(pop.sf, "Spatial")

# Determine the member of the population closest to a sample point
closest.point <- function(sample.pt, grid.data){
   dist.sq <- (coordinates(grid.data)[,1]-sample.pt[1])^2 +
      (coordinates(grid.data)[,2]-sample.pt[2])^2
   return(which.min(dist.sq))
}   
pop.mean <- mean(pop.data$Yield)
pop.sd <- sd(pop.data$Yield)

W <- bbox(pop.data)[1,1]
E <- bbox(pop.data)[1,2]
S <- bbox(pop.data)[2,1]
N <- bbox(pop.data)[2,2]
N <- N + 2.5
S <- S - 2.5
E <- E + 2.5
W <- W - 2.5
nrows <- 4
ncols <- 8
grid.size <- (E - W) / ncols
grid.offset <- 0.5 * grid.size
spsamp.pts <- expand.grid(x = seq(W+grid.offset,E,grid.size),
  y = seq(N - grid.offset,S,-grid.size))
samp.pts <- apply(spsamp.pts, 1, closest.point, grid.data = pop.data)
data.samp <- pop.data[samp.pts,]

data.May.ras <- raster("set4\\set4.20596.tif")
class(data.May.ras)
data.samp$IRvalue <- extract(data.May.ras, spsamp.pts)
Yield.band1 <- lm(Yield ~ IRvalue, data = data.samp)
summary(Yield.band1)

print(Y.bar <- mean(predict(Yield.band1)), digits  = 5)
print(abs(Y.bar - pop.mean) / pop.mean, digits = 3)


par(mai = c(1,1,1,1))
plot(data.samp$IRvalue, data.samp$Yield,  # Fig. 5.12
   xlab = "Infrared Digital Number", ylab = "Yield (kg/ha)",
   main = "Yield vs. May Infrared Digital Number", pch = 16,
   cex.main = 2, cex.lab = 1.5)
abline(reg = Yield.band1)


data.4.2.May <- readGDAL("set4\\Set4.20596.tif")
proj4string(data.4.2.May) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
greys <- grey(0:255 / 255)
image(data.4.2.May, "band1", col = greys,  # Fig. 5.13
  xlim = c(592100,592900), ylim = c(4267500,4267860),
  axes = TRUE) 
title(main = "Sample Transects", xlab = "Easting",
  ylab = "Northing", cex.main = 2, cex.lab = 1.5,
  font.lab = 2)
spsamp1.pts <- data.frame(Northing = seq(4267500, 4267800, 50))
spsamp1.pts$Easting <- 592400
coordinates(spsamp1.pts) <- c("Easting", "Northing")
points(spsamp1.pts, pch = 1)
spsamp2.pts <- data.frame(Northing = seq(4267500, 4267800, 50))
spsamp2.pts$Easting <- 592300
coordinates(spsamp2.pts) <- c("Easting", "Northing")
points(spsamp2.pts, pch = 3)
spsamp3.pts <- data.frame(Easting = seq(592200, 592800, 100))
spsamp3.pts$Northing <- 4267700
coordinates(spsamp3.pts) <- c("Easting", "Northing")
points(spsamp3.pts, pch = 12)
legend(592600, 4267675, c("Sample 1", "Sample 2", "Sample 3"),
  pt.cex = 1, pch = c(1,3,12), y.intersp = 1,
  title = "Transect")

sample.coords <- coordinates(spsamp1.pts)
samp1.pts <- apply(sample.coords, 1,
  closest.point, grid.data = pop.data)
data.samp1 <- data.frame(Yield = pop.data$Yield[samp1.pts])
data.samp1$IRvalue <- extract(data.May.ras, sample.coords)
Yield.IR1 <- lm(Yield ~ IRvalue, data = data.samp1)
summary(Yield.IR1)
print(Y1.bar <- mean(predict(Yield.IR1)))
abs(Y1.bar - pop.mean) / pop.mean

sample.coords <- coordinates(spsamp2.pts)
samp2.pts <- apply(sample.coords, 1,
  closest.point, grid.data = pop.data)
data.samp2 <- data.frame(Yield = pop.data$Yield[samp2.pts])
data.samp2$IRvalue <- extract(data.May.ras, sample.coords)
Yield.IR2 <- lm(Yield ~ IRvalue, data = data.samp2)
summary(Yield.IR2)
print(Y2.bar <- mean(predict(Yield.IR2)))
abs(Y2.bar - pop.mean) / pop.mean

sample.coords <- coordinates(spsamp3.pts)
samp3.pts <- apply(sample.coords, 1,
  closest.point, grid.data = pop.data)
data.samp3 <- data.frame(Yield = pop.data$Yield[samp3.pts])
data.samp3$IRvalue <- extract(data.May.ras, sample.coords)
Yield.IR3 <- lm(Yield ~ IRvalue, data = data.samp3)
summary(Yield.IR3)
print(Y3.bar <- mean(predict(Yield.IR3)))
abs(Y3.bar - pop.mean) / pop.mean

plot(data.samp$IRvalue, data.samp$Yield, # Fig. 5.14a
   xlab = "Infrared Digital Number", ylab = "Yield (kg/ha)",
   xlim = c(110, 170), ylim = c(2000, 7000),
   main = "Transect 1", pch = 16,
   cex.main = 2, cex.lab = 1.5, font.lab = 2)
abline(reg = Yield.band1)
with(data.samp1, points(IRvalue, Yield, pch = 1))
abline(reg = Yield.IR1, lty = 2)
points(mean(data.samp$IRvalue), Y.bar,
   pch = 19, cex = 2)
text(144, 4600, expression(bold(bar(Y))))
points(mean(data.samp1$IRvalue), Y1.bar,
   pch = 1, cex = 2)
text(161, 3200, expression(bold(bar(Y)[1])))
legend(150, 6000, c("All Data", "Transect 1"),
  pt.cex = 1, pch = c(19,1), y.intersp = 1,
  title = "Sample")
legend(122, 3700, c("All Data", "Transect 1"),
  lty = c(1,2), y.intersp = 1,  title = "Linear Fit")

# Color version
plot(data.samp$IRvalue, data.samp$Yield, 
   xlab = "Infrared Digital Number", ylab = "Yield (kg/ha)",
   xlim = c(110, 170), ylim = c(2000, 7000),
   main = "Transect 1", pch = 16, col = "blue",
   cex.main = 2, cex.lab = 1.5, font.lab = 2)
abline(reg = Yield.band1, col = "blue")
with(data.samp1, points(IRvalue, Yield, pch = 16, col = "red"))
abline(reg = Yield.IR1, lty = 2, col = "red")
points(mean(data.samp$IRvalue), Y.bar,
   pch = 19, cex = 2, col = "green")
text(144, 4600, expression(bold(bar(Y))), col = "green")
points(mean(data.samp1$IRvalue), Y1.bar,
   pch = 1, cex = 2)
text(161, 3200, expression(bold(bar(Y)[1])))
legend(150, 6000, c("All Data", "Transect 1"),
  pt.cex = 1, pch = c(19,19), y.intersp = 1, col = c("blue","red"),
  title = "Sample")
legend(122, 3700, c("All Data", "Transect 1"),
  lty = c(1,2), y.intersp = 1,  title = "Linear Fit", col = c("blue", "red"))


plot(data.samp$IRvalue, data.samp$Yield, # Fig. 5.14b
   xlab = "Infrared Digital Number", ylab = "Yield (kg/ha)",
   xlim = c(110, 170), ylim = c(2000, 7000),
   main = "Transect 2", pch = 16,
   cex.main = 2, cex.lab = 1.5, font.lab = 2)
abline(reg = Yield.band1)
with(data.samp2, points(IRvalue, Yield, pch = 3))
abline(reg = Yield.IR2, lty = 3)
points(mean(data.samp$IRvalue), Y.bar,
   pch = 19, cex = 2)
text(144, 4600, expression(bold(bar(Y))))
points(mean(data.samp2$IRvalue), Y2.bar,
   pch = 3, cex = 2)
text(139, 5500, expression(bold(bar(Y)[2])))
legend(150, 6000, c("All Data","Transect 2"),
  pt.cex = 1, pch = c(19,3), y.intersp = 1,
  title = "Sample")
legend(122, 3700, c("All Data","Transect 2"),
  lty = c(1,3), y.intersp = 1,  title = "Linear Fit")

plot(data.samp$IRvalue, data.samp$Yield, # Fig. 5.14c
   xlab = "Infrared Digital Number", ylab = "Yield (kg/ha)",
   xlim = c(110, 170), ylim = c(2000, 7000),
   main = "Transect 3", pch = 16,
   cex.main = 2, cex.lab = 1.5, font.lab = 2)
abline(reg = Yield.band1)
with(data.samp3, points(IRvalue, Yield, pch = 12))
abline(reg = Yield.IR3, lty = 4)
points(mean(data.samp3$IRvalue), Y3.bar,
   pch = 12, cex = 2)
text(137, 4650, expression(bold(bar(Y)[3])))
legend(150, 6000, c("All Data","Transect 3"),
  pt.cex = 1, pch = c(19,12), y.intersp = 1,
  title = "Sample")
legend(122, 3700, c("All Data","Transect 3"),
  lty = c(1,4), y.intersp = 1,  title = "Linear Fit")
   
   