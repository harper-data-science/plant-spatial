# Exercise 5.1
closest.point <- function(sample.pt, grid.data) which.min(spDistN1(grid.data, sample.pt))

# Exercise 5.2
library(spdep)
library(sf)
bdry.sp <- st_read("created\\Set419697bdry.shp")
Set.bdry <- as(bdry.sp, "Spatial")
spsamp.pts <- spsample(Set.bdry, 100, type = "regular")
par(mai = c(1,1,1,1))
plot(Set.bdry, axes = TRUE)
title(main = "Field 4.1 Boundary", xlab = "Easting",
   ylab = "Northing", cex.lab = 1.5, cex.main = 2)
plot(spsamp.pts, add = TRUE)
class(spsamp.pts)
str(spsamp.pts)
coordinates(spsamp.pts)[1:10,]

# Exercise 5.3
library(maptools)
print(x.dist <- spsamp.pts@coords[2,1] - spsamp.pts@coords[1,1])
print(y.dist <- spsamp.pts@coords[9,2] - spsamp.pts@coords[1,2])
print(x.min <- min(spsamp.pts@coords[,1]))
print(x.max <- max(spsamp.pts@coords[,1]))
print(y.min <- min(spsamp.pts@coords[,2]))
print(y.max <- max(spsamp.pts@coords[,2]))
samp.xy <- expand.grid(x = seq(x.min,x.max,x.dist), y = seq(y.min,y.max,y.dist))
coordinates(samp.xy) <- c("x", "y")
class(samp.xy)
plot(Set.bdry, axes = TRUE)
title(main = "Field 4.1 Boundary", xlab = "Easting",
   ylab = "Northing", cex.lab = 1.5, cex.main = 2)
plot(spsamp.pts, add = TRUE, cex = 2)
plot(samp.xy, add = TRUE, pch = 16)

# Exercise 5.4
proj4string(samp.xy) <- proj4string(Set.bdry)
samp.in <- over(samp.xy, Set.bdry) 
samp.xy.in <- samp.xy[which(!is.na(samp.in)),]
plot(Set.bdry, axes = TRUE)
title(main = "Field 4.1 Boundary", xlab = "Easting",
   ylab = "Northing", cex.lab = 1.5, cex.main = 2)
plot(samp.xy.in, add = TRUE, pch = 16)

# Exercise 5.5a
data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
clay.mean <- mean(data.Set4.1$Clay)
set.seed(123)
samp.r <- sample(data.Set4.1$Clay, size = 7, replace = FALSE)
abs(mean(samp.r) - clay.mean) / clay.mean

# Exercise 5.5b
ID.order <- data.Set4.1$ID[order(data.Set4.1$EM38B425)]
ID.low <- ID.order[1:36]
ID.high <- ID.order[37:86]
set.seed(123)
samp.low <- sample(data.Set4.1$Clay[ID.low], size = 3, replace = FALSE)
samp.high <- sample(data.Set4.1$Clay[ID.high], size = 4, replace = FALSE)
abs((3*mean(samp.low)+4*mean(samp.high))/7 - clay.mean) / clay.mean

# Exercise 5.5c
samp.trans <- data.Set4.1$Clay[c(4,18,32,46,60,72,84)]
abs(mean(samp.trans) - clay.mean) / clay.mean
EM.trans <- data.Set4.1$EM38B425[c(4,18,32,46,60,72,84)]
EM.lm <- lm(samp.trans ~ EM.trans)
abs(mean(predict(EM.lm)) - clay.mean) / clay.mean