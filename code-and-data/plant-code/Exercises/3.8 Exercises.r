# 3.1 Monte Carlo simulation of t-test with normal and lognormal data

#Normal data
set.seed(123)
ttest <- function(){
  Y <- rnorm(10)
  t.ttest <- t.test(Y,alternative = "two.sided")
  TypeI <- t.ttest$p.value < 0.05
}
U <- replicate(10000, ttest())
mean(U)

#Lognormal data
set.seed(123)
ttest <- function(){
  Y <- rlnorm(10)
  t.ttest <- t.test(Y, mu = exp(1/2), alternative = "two.sided")
  TypeI <- t.ttest$p.value < 0.05
  Ybar = mean(Y)
  return(c(TypeI, Ybar))
}
U <- replicate(10000, ttest())
mean(U[1,])
mean(U[2,])
exp(1/2)

# 3.2 Monte Carlo coin toss
set.seed(123)
n.tosses <- 50
coin.toss <- function (n.tosses){
  z <- rbinom(1,n.tosses,0.5)
  n.heads <- sum(z)
  b <- binom.test(n.heads,n.tosses,0.5,"two.sided",0.95)
  TypeI <- b$p.value < 0.05
  c(TypeI, b$p.value)
}
U <- replicate(10000, coin.toss(n.tosses))
mean(U[1,])
sort(unique(U[2,]))

# 3.3 4x4 W matrix
library(spdep)
nb4x4 <- cell2nb(4,4) #Rook's case by default
W.list <- nb2listw(nb4x4, style = "W")
print(W <- listw2mat(W.list))
apply(W, 1, sum)


# 3.4 Thiessen polygons
library(spatstat)
library(spdep)
Y <- expand.grid(x = seq(0.5,2.5), y = seq(2.5, 0.5, by = -1))
cell.ppp <- ppp(Y[,1], Y[,2],
     window = owin(c(0,3), c(0,3)))
thsn.pp <- dirichlet(cell.ppp)
str(thsn.pp)

# 3.5
data.Set2 <- read.csv("set2\\set2data.csv", header = TRUE)
data.Set2a <- data.Set2[which((data.Set2$Longitude >= -123.2463)
  & (data.Set2$Longitude <= -123.0124)),]
print(data.Set2b <- data.Set2a[which((data.Set2a$Latitude >= 39.61633)
  & (data.Set2a$Latitude <= 39.95469)),])
coordinates(data.Set2b) <- c("Longitude", "Latitude")
proj4string(data.Set2b) <- CRS("+proj=longlat +datum=WGS84")
plot(data.Set2b, axes = TRUE)
title(main = "Exercise 3.5", xlab = "Longitude",
   ylab = "Latitude")
   
#3.6 (run 3.5 first)
nlist.w <- knn2nb(knearneigh(data.Set2b, k = 4))
str(nlist.w)
W <- nb2listw(nlist.w)
Y.mod <- errorsarlm(Precip ~ 1, data = data.Set2b, listw = W)
Y.mod$lambda

# 3.7
library(spdep)
set.seed(123)
lambda <- 0.6
sidelength <- 10
sideplus <- sidelength + 4
nlist <- cell2nb(sideplus, sideplus)
IrWinv <- invIrM(nlist, lambda)
ttest <- function(IrWinv,sideplus){
  Y <- IrWinv %*% rnorm(sideplus^2)
  Ytrim <- matrix(Y, nrow = sideplus, byrow = TRUE)[3:(sideplus-2),3:(sideplus-2)]
  Ybar <- mean(Ytrim)
  t.ttest <- t.test(Ytrim,alternative = "two.sided")
  TypeI <- t.ttest$p.value < 0.05
  return(c(TypeI, Ybar))
}
U <- replicate(10000, ttest(IrWinv, sideplus))
mean(U[1,])
mean(U[2,])
sqrt(var(U[2,]))

# 3.8
library(spdep)
# Read data based on code in Appendix B.4
data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
data.Set4.1$x <- data.Set4.1$Easting - min(data.Set4.1$Easting)
data.Set4.1$y <- data.Set4.1$Northing - min(data.Set4.1$Northing)
coordinates(data.Set4.1) <- c("x", "y")
data.wrong <- data.Set4.1
data.wrong$ID <- seq(86,1)
data.wrong
library(spatstat)
W <- 592000
E <- 592500
S <- 4270300
N <- 4271200
cell.ppp <- ppp(data.Set4.1$Easting, data.Set4.1$Northing,
                window = owin(c(W, E), c(S, N)))
thsn.tess <- dirichlet(cell.ppp)
library(maptools)
thsn.sp <- as(thsn.tess, "SpatialPolygons")
thsn.spdf <- SpatialPolygonsDataFrame(thsn.sp,
  slot(data.wrong, "data"), FALSE)
# Check that IDs match by plotting both
plot(thsn.spdf, pch = 1, cex = 0.1)
text(coordinates(thsn.spdf),
     labels=as.character(thsn.spdf@data$ID))
plot(thsn.spdf, pch = 1, cex = 0.1)
text(coordinates(thsn.spdf),
     labels=lapply(thsn.spdf@polygons, slot, "ID"))

# Using sf objects
library(sf)
thsn.sf <- st_as_sf(thsn.spdf)
thsn.sf$ID
all.equal(1:length(thsn.sf$ID), thsn.sf$ID)
