library(spatstat)
library(sf)
library(terra)

data.Yield4.1 <- read.csv("created\\set4.1yld96cleaned.csv", header = TRUE)
data.Yield4.1.sf <- st_as_sf(data.Yield4.1, coords = c("Easting","Northing"))
img.terra <- rast("set4\\set4.10596.tif")
NDVI.terra <- (img.terra[[1]] - img.terra[[2]]) /
    (img.terra[[1]] + img.terra[[2]])
img.df <- data.frame(crds(NDVI.terra))
img.df$NDVI <- values(NDVI.terra)
img.sf <- st_as_sf(img.df, coords = c("x", "y"))

E <- 592400
W <- 592050
S <- 4270400
N <- 4271100

#img.df$x <- img.coords[,1]
#img.df$y <- img.coords[,2]
img.rect <- with(img.df, img.df[which((x >= W) & (x <= E) &
   (y >= S) & (y <= N)),])
img.rect.sf <- st_as_sf(img.rect, coords = c("x", "y"))

cell.size <- 350 / 5
xmin <- W + cell.size / 2
xmax <- E - cell.size / 2
ymin <- S + cell.size / 2
ymax <- N - cell.size / 2
cell.ctrs <- expand.grid(seq(xmin, xmax, cell.size),
      seq(ymin, ymax, cell.size))
cell.ppp <- ppp(cell.ctrs[,1], cell.ctrs[,2],
     window = owin(c(W, E), c(S, N)))
thsn.pp <- dirichlet(cell.ppp)
thsn.sfc <- st_as_sfc(thsn.pp)
thsn.sf <- st_sf(thsn.sfc)

bdry.sf <- st_read("created\\set419697bdry.shp")
par(mai = c(1,1,1,1))
plot(st_geometry(bdry.sf), axes = TRUE, xlim = c(592000,592500)) # Fig. 11.5a
title(main = "Regular Configuration", xlab = "Easting",
   ylab = "Northing", cex.lab = 1.5, cex.main = 2)
plot(thsn.sf, add = TRUE)
plot(st_geometry(data.Yield4.1.sf[which(data.Yield4.1.sf$ID %% 50 == 0),]),
   pch = 1, add = TRUE, col = "black")

data.int <- st_intersects(thsn.sf, data.Yield4.1.sf)
image.int <- st_intersects(thsn.sf, img.sf)
Yield.means <- numeric(length(data.int))
NDVI.means <- numeric(length(image.int))
for(i in 1:length(Yield.means)){
   Yield.means[i] <- mean(data.Yield4.1.sf$Yield[data.int[[i]]])
   NDVI.means[i] <-  mean(img.sf$NDVI[image.int[[i]]])
   }
plot(NDVI.means, Yield.means)
print(cor(Yield.means, NDVI.means), digits = 3)

yield.vs.NDVI <- function(cell.size){
   xmin <- W + cell.size / 2
   xmax <- E - cell.size / 2
   ymin <- S + cell.size / 2
   ymax <- N - cell.size / 2
   c(xmin, xmax, ymin, ymax)
   cell.ctrs <- expand.grid(seq(xmin, xmax, cell.size),
      seq(ymin, ymax, cell.size))
   cell.ppp <- ppp(cell.ctrs[,1], cell.ctrs[,2],
     window = owin(c(W, E), c(S, N)))
   thsn.pp <- dirichlet(cell.ppp)
   thsn.sfc <- st_as_sfc(thsn.pp)
   thsn.sf <- st_sf(thsn.sfc)
   data.int <- st_intersects(thsn.sf, data.Yield4.1.sf)
   image.int <- st_intersects(thsn.sf, img.sf)
   Yield.means <- numeric(length(data.int))
   NDVI.means <- numeric(length(image.int))
   for(i in 1:length(Yield.means)){
      Yield.means[i] <- mean(data.Yield4.1.sf$Yield[data.int[[i]]])
      NDVI.means[i] <-  mean(img.sf$NDVI[image.int[[i]]])
   }
   cor(Yield.means, NDVI.means)
}

print(yield.vs.NDVI(350/20), digits = 2)
print(yield.vs.NDVI(350/10), digits = 2)
print(yield.vs.NDVI(350/5), digits = 2)
print(yield.vs.NDVI(350/2), digits = 2)


# Uniform random distribution
set.seed(123)
ran1.ppp <- runifpoint(nrow(cell.ctrs), win = owin(c(W, E), c(S, N)))
thsn.pp <- dirichlet(ran1.ppp)
thsn.sfc <- st_as_sfc(thsn.pp)
thsn.sf <- st_sf(thsn.sfc)
plot(st_geometry(bdry.sf), axes = TRUE, xlim = c(592000,592500))
title(main = "Random Configuration 1", xlab = "Easting",
   ylab = "Northing", cex.lab = 1.5, cex.main = 2)
plot(thsn.sf, add = TRUE)  # Fig. 11.5b
data.int <- st_intersects(thsn.sf, data.Yield4.1.sf)
image.int <- st_intersects(thsn.sf, img.sf)
Yield.means <- numeric(length(data.int))
NDVI.means <- numeric(length(image.int))
for(i in 1:length(Yield.means)){
   Yield.means[i] <- mean(data.Yield4.1.sf$Yield[data.int[[i]]])
   NDVI.means[i] <-  mean(img.sf$NDVI[image.int[[i]]])
  }
print(cor(Yield.means, NDVI.means), digits = 2)


ran2.ppp <- runifpoint(nrow(cell.ctrs), win = owin(c(W, E), c(S, N)))
thsn.pp <- dirichlet(ran2.ppp)
thsn.sfc <- st_as_sfc(thsn.pp)
thsn.sf <- st_sf(thsn.sfc)
plot(st_geometry(bdry.sf), axes = TRUE, xlim = c(592000,592500))
title(main = "Random Configuration 2", xlab = "Easting",
   ylab = "Northing", cex.lab = 1.5, cex.main = 2)
plot(thsn.sf, add = TRUE)  # Fig. 11.5c
data.int <- st_intersects(thsn.sf, data.Yield4.1.sf)
image.int <- st_intersects(thsn.sf, img.sf)
Yield.means <- numeric(length(data.int))
NDVI.means <- numeric(length(image.int))
for(i in 1:length(Yield.means)){
   Yield.means[i] <- mean(data.Yield4.1.sf$Yield[data.int[[i]]])
   NDVI.means[i] <-  mean(img.sf$NDVI[image.int[[i]]])
  }
print(cor(Yield.means, NDVI.means), digits = 2)


# Series of 100 uniform random distributions
set.seed(123)
u <- numeric(100)
for (i in 1:100){
   ran.ppp <- runifpoint(nrow(cell.ctrs), win = owin(c(W, E), c(S, N)))
   thsn.pp <- dirichlet(ran.ppp)
   thsn.sfc <- st_as_sfc(thsn.pp)
   thsn.sf <- st_sf(thsn.sfc)
   data.int <- st_intersects(thsn.sf, data.Yield4.1.sf)
   image.int <- st_intersects(thsn.sf, img.sf)
   Yield.means <- numeric(length(data.int))
   NDVI.means <- numeric(length(image.int))
   for(j in 1:length(Yield.means)){
      Yield.means[j] <- mean(data.Yield4.1.sf$Yield[data.int[[j]]])
      NDVI.means[j] <-  mean(img.sf$NDVI[image.int[[j]]])
  }
   u[i] <- cor(Yield.means, NDVI.means)
}
par(mai = c(1,1,1,1))
hist(u, main = "Histogram of r Values, 50 Sample Points",
   cex.main = 2, cex.lab = 1.5)
mean(u)
sd(u)
