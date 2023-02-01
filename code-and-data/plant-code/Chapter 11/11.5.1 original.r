library(spatstat)
library(maptools)
library(rgdal)
library(sf)

data.Yield4.1 <- read.csv("created\\set4.1yld96cleaned.csv", header = TRUE)
coordinates(data.Yield4.1) <- c("Easting","Northing")
data.4.1.May <- readGDAL("set4\\set4.10596.tif")
img.df <- data.frame(IR = slot(data.4.1.May, "data")$band1)
img.df$R <- slot(data.4.1.May, "data")$band2
img.df$NDVI <- with(img.df, (IR - R) / (IR + R))
img.coords <- coordinates(data.4.1.May)

E <- 592400
W <- 592050
S <- 4270400
N <- 4271100

img.df$x <- img.coords[,1]
img.df$y <- img.coords[,2]
img.rect <- with(img.df, img.df[which((x >= W) & (x <= E) &
   (y >= S) & (y <= N)),])
coordinates(img.rect) <- c("x", "y")

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
thsn.sp <- as(thsn.pp, "SpatialPolygons")
bdry.sf <- st_read("created\\set419697bdry.shp")
bdry.sp <- as(bdry.sf, "Spatial")
par(mai = c(1,1,1,1))
plot(bdry.sp, axes = TRUE, xlim = c(592000,592500)) # Fig. 11.5a
title(main = "Regular Configuration", xlab = "Easting",
   ylab = "Northing", cex.lab = 1.5, cex.main = 2)
plot(thsn.sp, add = TRUE)
plot(data.Yield4.1[which(data.Yield4.1$ID %% 50 == 0),], pch = 1,
   add = TRUE)

data.ovrl <- over(thsn.sp, data.Yield4.1, fn = mean)
image.ovrl <- over(thsn.sp, img.rect, fn = mean)
print(cor(data.ovrl$Yield, image.ovrl$NDVI), digits = 3)

yield.vs.NDVI <- function(cell.size){
   xmin <- W + cell.size / 2
   xmax <- E - cell.size / 2
   ymin <- S + cell.size / 2
   ymax <- N - cell.size / 2
   cell.ctrs <- expand.grid(seq(xmin, xmax, cell.size),
      seq(ymin, ymax, cell.size))
   cell.ppp <- ppp(cell.ctrs[,1], cell.ctrs[,2],
     window = owin(c(W, E), c(S, N)))
   thsn.pp <- dirichlet(cell.ppp)
   thsn.sp <- as(thsn.pp, "SpatialPolygons")
   data.ovrl <- over(thsn.sp, data.Yield4.1, fn = mean)
   image.ovrl <- over(thsn.sp, img.rect, fn = mean)
   c(cor(data.ovrl$Yield, image.ovrl$NDVI),
      cov(data.ovrl$Yield, image.ovrl$NDVI),
      sd(data.ovrl$Yield) * sd(image.ovrl$NDVI))
}

print(yield.vs.NDVI(350/20), digits = 2)
print(yield.vs.NDVI(350/10), digits = 2)
print(yield.vs.NDVI(350/5), digits = 2)
print(yield.vs.NDVI(350/2), digits = 2)


# Uniform random distribution
set.seed(123)
ran1.ppp <- runifpoint(nrow(cell.ctrs), win = owin(c(W, E), c(S, N)))
thsn.pp <- dirichlet(ran1.ppp)
thsn.sp <- as(thsn.pp, "SpatialPolygons")
plot(bdry.sp, axes = TRUE, xlim = c(592000,592500))
title(main = "Random Configuration 1", xlab = "Easting",
   ylab = "Northing", cex.lab = 1.5, cex.main = 2)
plot(thsn.sp, add = TRUE)  # Fig. 11.5b
data.ovrl <- over(thsn.sp, data.Yield4.1, fn = mean)
image.ovrl <- over(thsn.sp, img.rect, fn = mean)
print(c(cor(data.ovrl$Yield, image.ovrl$NDVI),
   cov(data.ovrl$Yield, image.ovrl$NDVI),
   sd(data.ovrl$Yield) * sd(image.ovrl$NDVI)), digits = 2)


ran2.ppp <- runifpoint(nrow(cell.ctrs), win = owin(c(W, E), c(S, N)))
thsn.pp <- dirichlet(ran2.ppp)
thsn.sp <- as(thsn.pp, "SpatialPolygons")
plot(bdry.sp, axes = TRUE, xlim = c(592000,592500))
title(main = "Random Configuration 2", xlab = "Easting",
   ylab = "Northing", cex.lab = 1.5, cex.main = 2)
plot(thsn.sp, add = TRUE)  # Fig. 11.5c
data.ovrl <- over(thsn.sp, data.Yield4.1, fn = mean)
image.ovrl <- over(thsn.sp, img.rect, fn = mean)
print(c(cor(data.ovrl$Yield, image.ovrl$NDVI),
   cov(data.ovrl$Yield, image.ovrl$NDVI),
   sd(data.ovrl$Yield) * sd(image.ovrl$NDVI)), digits = 2)


# Series of 100 uniform random distributions
set.seed(123)
u <- numeric(100)
for (i in 1:100){
   ran.ppp <- runifpoint(nrow(cell.ctrs), win = owin(c(W, E), c(S, N)))
   thsn.pp <- dirichlet(ran.ppp)
   thsn.sp <- as(thsn.pp, "SpatialPolygons")
   data.ovrl <- over(thsn.sp, data.Yield4.1, fn = mean)
   image.ovrl <- over(thsn.sp, img.rect, fn = mean)
   u[i] <- cor(data.ovrl$Yield, image.ovrl$NDVI)
}
par(mai = c(1,1,1,1))
hist(u, main = "Histogram of r Values, 50 Sample Points",
   cex.main = 2, cex.lab = 1.5)
mean(u)
sd(u)
