library(spdep)
library(maptools)

data.Set4.2 <- read.csv("Set4\\Set4.296sample.csv", header = TRUE)
data.disp <- data.Set4.2
coordinates(data.disp) <- c("Easting", "Northing")
library(spatstat)
N <- 4267868
S <- 4267513
E <- 592867
W <- 592082
cell.ppp <- ppp(data.Set4.2$Easting, data.Set4.2$Northing,
     window = owin(c(W, E), c(S, N)))
thsn.pp <- dirichlet(cell.ppp)
thsn.sp <- as(thsn.pp, "SpatialPolygons")
row.names(thsn.sp) <- row.names(data.Set4.2)
thsn.spdf <- SpatialPolygonsDataFrame(thsn.sp, data.Set4.2)
greys <- grey(255:50 / 255)
spplot(thsn.spdf, "SoilpH", col.regions = greys,  scales = list(draw = TRUE),
  xlab = "Easting", ylab = "Northing", main = "Field 4.2 SoilpH") # Figl 16.5

sort(unique(data.Set4.2$Easting))
data.Set4.2W <- data.Set4.2[which(data.Set4.2$Easting < 592450),]
data.Set4.2E <- data.Set4.2[which(data.Set4.2$Easting > 592500),]

Y1 <- data.Set4.2W$SoilpH
coordinates(data.Set4.2W) <- c("Easting", "Northing")
nlist.1 <- dnearneigh(data.Set4.2W, d1 = 0, d2 = 70)
W.1 <- nb2listw(nlist.1, style = "W")
Y2 <- data.Set4.2E$SoilpH
coordinates(data.Set4.2E) <- c("Easting", "Northing")
nlist.2 <- dnearneigh(data.Set4.2E, d1 = 0, d2 = 70)
W.2 <- nb2listw(nlist.1, style = "W")

mean(data.Set4.2W$SoilpH)
mean(data.Set4.2E$SoilpH)
t.test(data.Set4.2W$SoilpH, data.Set4.2E$SoilpH, "two.sided")


para.samp <- function(Y1, Y2, IrWinv1.mod, IrWinv2.mod){
  Y1.samp <- sample(Y1, length(Y1), replace = TRUE)
  Y1.boot <- IrWinv1.mod %*% Y1.samp
  Y2.samp <- sample(Y2, length(Y2), replace = TRUE)
  Y2.boot <- IrWinv2.mod %*% Y2.samp
  Z <- c(rep(1,length(Y1)), rep(0,length(Y2)))
  Y <- c(Y1.boot, Y2.boot)
  r.hat <- cor(Y,Z)
}

# Fit a spatial error to the data
Y1.mod <- errorsarlm(SoilpH ~ 1, data = data.Set4.2W, listw = W.1)
e1.hat <- residuals(Y1.mod)
print(lambda1.hat <- Y1.mod$lambda, digits = 2)
Y2.mod <- errorsarlm(SoilpH ~ 1, data = data.Set4.2E, listw = W.2)
e2.hat <- residuals(Y2.mod)
print(lambda2.hat <- Y2.mod$lambda, digits = 2)
IrWinv1.mod <- invIrM(nlist.1, lambda1.hat)
IrWinv2.mod <- invIrM(nlist.2, lambda2.hat)
set.seed(123)
U <- replicate(200, para.samp(e1.hat, e2.hat, IrWinv1.mod,
   IrWinv2.mod))
print(sigmar.hat <- var(U), digits = 3)

print(ne.hat <- 1 / (2*sigmar.hat), digits = 3)
Z <- c(rep(1,length(Y1)), rep(0,length(Y2)))
Y <- c(Y1, Y2)
rw <- cor(Y, Z)
t.corr <- rw*sqrt(2*ne.hat - 2) / (1 - rw^2)
print(p.corr <- 2 * (1 - pt(q = abs(t.corr), df = 2*ne.hat - 2)),
   digits = 3)
