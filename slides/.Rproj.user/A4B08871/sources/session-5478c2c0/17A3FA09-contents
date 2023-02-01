# 2.1
i <- 1:20
while(i < 5) i <- 2
# 2.2a
3 * 4 ^ 2 + 2
-1:2
-(1:2)
#2.2b
1:3 - 1
#2.2c
w <- 1:10
w > 5
w == 5
w != 5

# 2.3a
A <- matrix(c(1,1,1,1,2,2,2,2,3,3,3,3), nrow = 3, byrow = TRUE)
A
A[2,]
A[-2,]
#2.3b
B <- cbind(A[,1],5*A[,1])
#2.3c
C <- data.frame(B)
names(C) <- c("C1","C2")
#2,3d
L <- list(L1 = w, L2 = B, L3 = C)
L$L3$C2
L[[3]][,2]

#2.4a
w <- c(6, 2.4, 3)
w
z <- c("a", "b", "c")
z
#2.4b
z <- c(w, z)
z
class(z)
#2.4c
w <- as.numeric(z)
w

# 2.5
# These do not give the same results
w <- 1:10
for (i in 1:10){
   {if (w[i] > 5)
      w[i] <- 3
   else
      w[i] <- 0}
}
w           
w <- 1:10
w > 5
w[w > 5]
w[w > 5] <- 3
w[w <= 5] <- 0
w
# This does give the same result
w <- 1:10
i <- w[w > 5]
j <- w[w <= 5]
w[i] <- 3
w[j] <- 0
w

#2.6
#a)
z <- numeric(20)
i <- 0:19
#b
z[i %% 5 == 0] <- 1
#c)
z <- cumsum(z)
z

#2.7a
print(pi, digits = 20)
#2.7b
w <- seq(0, 6, 0.25)
z1 <- sin(0.5 * pi * w)
#2.7c
sin.npi <- function(w) sin(n * pi * w)
n <- 0.5
z2 <- sin.npi(w)
z <- cbind(z1,z2)
z
#2.7d
all.equal(z1, z2)
z1 <- exp(-z2)
#2.7e
exp.m.sin <- function(w) exp(-sin.npi(w))
z2 <- exp.m.sin(w)
z <- cbind(z1,z2)
z

#2.8a
data(UKgas)
#2.8b
UKg <- matrix(UKgas, ncol = 4, byrow = TRUE)
#2.8c
UKgm <- apply(UKg, 1, mean)
mean(UKg[1,])
#2.8d
UKg.df <- data.frame(UKg)
names(UKg.df) <- c( "Winter", "Spring", "Summer", "Fall")
UKg.df$Mean <- UKgm
UKg.df$Year <- 1960:1986
#2.8e
apply(UKg, 2, mean)

#2.9a
plot(UKg.df$Year, UKg.df$Mean)
# 2.9b
plot(UKg.df$Year, UKg.df$Mean, type = "l",
 main = "Mean UK Gas Consumption", xlab = "Year", ylab = "Consumption")
#2.9c
points(UKg.df$Year,UKg.df$Winter)
max(max(UKg.df$Spring), max(UKg.df$Summer), max(UKg.df$Fall),
 max(UKg.df$Winter))
#2.9d
plot(UKg.df$Year, UKg.df$Mean, type = "l", ylim = c(0,1200),
 main = "Mean UK Gas Consumption", xlab = "Year", ylab = "Consumption")
points(UKg.df$Year, UKg.df$Winter, pch = 1)
points(UKg.df$Year, UKg.df$Spring, pch = 2)
points(UKg.df$Year, UKg.df$Summer, pch = 3)
points(UKg.df$Year, UKg.df$Fall, pch = 4)
points(1965,1000, pch = 1)
#2.9e
text(1965, 1000, "Winter", pos = 4)
points(1965,925, pch = 2)
text(1965, 925, "Spring", pos = 4)
points(1965,850, pch = 3)
text(1965, 850, "Summer", pos = 4)
points(1965,775, pch = 4)
text(1965, 775, "Fall", pos = 4)
# 2.9f
plot(UKg.df$Year, UKg.df$Mean, type = "l", ylim = c(0,1200),
 main = "Mean UK Gas Consumption", xlab = "Year", ylab = "Consumption")
points(UKg.df$Year, UKg.df$Winter, pch = 1)
points(UKg.df$Year, UKg.df$Spring, pch = 2)
points(UKg.df$Year, UKg.df$Summer, pch = 3)
points(UKg.df$Year, UKg.df$Fall, pch = 4)
legend(1965, 1000, c("Winter", "Spring", "Summer",
  "Fall"), pt.cex = 1, pch = 1:4, y.intersp = 1,
  title = "Season")

#2.10
library(raster)
getAnywhere("plot")
raster::plot
graphics::plot
library(sf)
?plot
?plot.sf
?plot.raster
#2.11

#Set 4-1, 1996 and 1997
N <- 4271132
W <- 592025
NE <- 592470
SE <- 592404
S <- 4270352
coords.mat <- matrix(c(W,NE,SE,W,W,N,N,S,S,N),
  ncol = 2)
library(sf)
coords.lst <- list(coords.mat)
# Create the sf object by specifying the coordinates
coords.pol = st_sfc(st_polygon(coords.lst))
# Assign the value z = 1 to the cell of the polygon
Set41bdry = st_sf(z = 1, coords.pol)
st_crs(Set41bdry) <- 32601
plot(Set41bdry)
# Don't forget to rund setwd() first!
st_write(Set41bdry, "created\\Set419697bdry.shp")

