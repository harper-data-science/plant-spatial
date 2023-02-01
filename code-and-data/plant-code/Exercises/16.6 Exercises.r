# 16.1
# This assumes that the code in 16.3.r has been run

sort(tapply(plots.spdf@data$Yield,
  plots.spdf@data$trtmt.CRD, mean))
CRD.aov <- aov(Yield ~ trtmt.CRD,
  data = plots.spdf@data)
summary(CRD.aov)

align.x <- factor(row.names(sort(tapply(plots.spdf@data$Yield,
   plots.spdf@data$trtmt.CRD, mean))))
x.order <- numeric(72)
for (i in 1:18) x.order[which(plots.spdf@data$trtmt.CRD == align.x[i])] <- i
with(plots.spdf@data, plot(x.order, Yield,
    cex.lab = 1.5, xaxt = "n", xlab = "Treatment", ylab = "Yield",
    main = "CRD Yields and Treatment Means", cex.main = 2))
axis(side = 1, at = 1:18,
   labels = as.character(align.x))
text(1:18, sort(tapply(plots.spdf@data$Yield,
  plots.spdf@data$trtmt.CRD, mean)), "-", cex = 2)

sort(tapply(plots.spdf@data$Yield,
  plots.spdf@data$trtmt.RCB, mean))
sort(tapply(plots.spdf@data$Yield,
  plots.spdf@data$block, mean))

CRD.lm <- lm(Yield ~ trtmt.CRD,
  data = plots.spdf@data)
print(CRD.sum <- summary(CRD.lm))

contr.CRD <- coef(CRD.sum)
tau.CRD <- contr.CRD[1:18,1]
tau.CRD[2:18] <- tau.CRD[1] + tau.CRD[2:18]
tau.distCRD <- vegdist(tau.CRD, method = "euclidean")

# Computation of EMP and PRE
print(EMP.CRD <- 2 * sum(tau.distCRD^2) / (18*17))
print(PRE.CRD <- contr.CRD[2,2]^2)

set.seed(123)
CRD.form <- as.formula(Yield ~ trtmt.CRD)
U <- replicate(100, EMP.calc(plots.spdf@data, CRD.form))
print(EMP.CRD <- mean(U))
print(EMPbar.CRD <- 100 * EMP.CRD / EMP.RCB)


# 16.2
# This assumes that the code from Section 16.3.4 has been run
EMP.gls <- function(trt.data, a, b){
   trt.data$trtmt <- factor(unlist(tapply(rep(1:18,4),
      sort(rep(1:4,18)), sample)))
   trt.sum <- summary(gls(Yield ~ trtmt + Trend,
      corr = corSpher(value = c(a, b),
      form = ~ x + y, nugget = TRUE), data = trt.data))
   contr <- coef(trt.sum)
   tau.hat <- contr[1:18]
   tau.hat[2:18] <- tau.hat[1] + tau.hat[2:18]
   tau.dist <- vegdist(tau.hat, method = "euclidean")
   EMP <- 2 * sum(tau.dist^2) / (18*17)
   return(EMP)
}

set.seed(123)
U <- replicate(100, EMP.gls(trend.RCB, 0.5, 0.7))
print(EMP.gls <- mean(U))
print(EMPbar.gls <- 100 * EMP.gls / EMP.RCB)

#16.3
# Simulation of sampling yield monitor data from Set 4.2

library(spdep)
library(sf)
pop.sf <- st_read("Created\\Set42pop.shp")
pop.data <- as(pop.sf, "Spatial")

pop.mean <- mean(pop.data$Yield)
pop.sd <- sd(pop.data$Yield)

W <- (slot(pop.data, "bbox"))[1,1]
E <- (slot(pop.data, "bbox"))[1,2]
S <- (slot(pop.data, "bbox"))[2,1]
N <- (slot(pop.data, "bbox"))[2,2]
E <- E - 2.5
W <- W + 2.5
coords.mat <- matrix(c(W,E,E,W,W,N,N,S,S,N), ncol = 2)
library(maptools)
sampbdry.vec <- vector(mode="list", length=1)
sampbdry.vec[[1]] <- Polygons(list(Polygon(coords.mat)), ID="1")
sampbdry.poly <- SpatialPolygons(sampbdry.vec,
   proj4string = CRS("+proj=utm +zone=10 +ellps=WGS84"))
sampbdry.shp <- SpatialPolygonsDataFrame(sampbdry.poly,
 data = data.frame(ID ="1", row.names = "1"))


print(x.size <- (E - W) / 12)
print(y.size <- (N - S) / 6)
x.offset <- W + 0.5 * x.size
y.offset <- S + 0.5 * y.size
plot.gt <- GridTopology(c(x.offset,y.offset),
   c(x.size,y.size), c(12,6))
plot.poly <- as(plot.gt,"SpatialPolygons")
row.names(plot.poly) = as.character(1:72)

set.seed(123)
plot.shp <- SpatialPolygonsDataFrame(plot.poly,
  data = data.frame(ID = as.character(1:72),
  row.names = as.character(1:72)))
plot.shp@data$trtmt.CRD <- factor(sample(rep((1:18), 4)))
plot.shp@data$block <- factor(sort(rep(1:4, 18)))
plot.shp@data$trtmt.RCB <- factor(unlist(tapply(rep(1:18,4),
   sort(rep(1:4,18)), sample)))

par(mai = c(1,1,1,1))
plot(pop.data, pch = 16,
   col = grey(pop.data@data$Yield/max(pop.data@data$Yield)),
   axes = TRUE)
plot(plot.poly, axes = FALSE, add = TRUE)
title(main = "CRD Plot Numbers", xlab = "Easting",
  ylab = "Northing", cex.main = 2, font.lab = 2, cex.lab = 1.5)
y <- lapply(plot.poly@polygons, slot, "labpt")
y.loc <- matrix(0, length(y), 2)
for (i in 1:length(y)) y.loc[i,] <- unlist(y[[i]])
text(y.loc, labels = as.character(plot.shp@data$trtmt.CRD))

plot(pop.data, pch = 16,
   col = grey(pop.data@data$Yield/max(pop.data@data$Yield)),
   axes = TRUE)
plot(plot.poly, axes = FALSE, add = TRUE)
title(main = "RCB Plot Numbers", xlab = "Easting",
  ylab = "Northing", cex.main = 2, font.lab = 2, cex.lab = 1.5)
text(y.loc,labels = as.character(plot.shp@data$trtmt.RCB))

greys <- grey(c(130,160,190,220)/255)
plot.nos <- list("sp.text", y.loc,
   as.character(plot.shp@data$trtmt.RCB))
spplot(plot.shp, zcol = "block", col.regions = greys,
   scales = list(draw = TRUE), xlab = "Easting", sp.layout = plot.nos,
   ylab = "Northing", main = "RCB Blocks and Plots")

proj4string(plot.shp) <- proj4string(pop.data)
plot.shp@data$Yield <- over(plot.shp, pop.data, fn = mean)$Yield

# Standard CRD and RCB analysis
sort(tapply(plot.shp@data$Yield,
  plot.shp@data$trtmt.CRD, mean))
CRD.aov <- aov(Yield ~ trtmt.CRD,
  data = plot.shp@data)
summary(CRD.aov)

align.x <- factor(row.names(sort(tapply(plot.shp@data$Yield,
   plot.shp@data$trtmt.CRD, mean))))
x.order <- numeric(72)
for (i in 1:18) x.order[which(plot.shp@data$trtmt.CRD == align.x[i])] <- i
with(plot.shp@data, plot(x.order, Yield,
    cex.lab = 1.5, xaxt = "n", xlab = "Treatment", ylab = "Yield",
    main = "CRD Yields and Treatment Means", cex.main = 2))
axis(side = 1, at = 1:18,
   labels = as.character(align.x))
text(1:18, sort(tapply(plot.shp@data$Yield,
  plot.shp@data$trtmt.CRD, mean)), "-", cex = 2)

sort(tapply(plot.shp@data$Yield,
  plot.shp@data$trtmt.RCB, mean))
sort(tapply(plot.shp@data$Yield,
  plot.shp@data$block, mean))

RCB.aov <- aov(Yield ~ block + trtmt.RCB,
  data = plot.shp@data)
summary(RCB.aov)
align.x <- factor(row.names(sort(tapply(plot.shp@data$Yield,
   plot.shp@data$trtmt.RCB, mean))))
x.order <- numeric(72)
for (i in 1:18) x.order[which(plot.shp@data$trtmt.RCB == align.x[i])] <- i
with(plot.shp@data, plot(x.order, Yield,
    cex.lab = 1.5, xaxt = "n", xlab = "Treatment", ylab = "Yield",
    main = "RCB Yields and Treatment Means", cex.main = 2))
axis(side = 1, at = 1:18,
   labels = as.character(align.x))
text(1:18, sort(tapply(plot.shp@data$Yield,
  plot.shp@data$trtmt.RCB, mean)), "-", cex = 2)

CRD.lm <- lm(Yield ~ trtmt.CRD,
  data = plot.shp@data)
print(CRD.sum <- summary(CRD.lm))

print(trt.means <- tapply(plot.shp@data$Yield,
  plot.shp@data$trtmt.CRD, mean))
trt.means[2] - trt.means[1]
trt.means[3] - trt.means[1]
library(vegan)
contr.CRD <- coef(CRD.sum)
# Computation of EMP and PRE
tau.hat <- contr.CRD[,1]
tau.hat[2:18] <- tau.hat[1] + tau.hat[2:18]
tau.dist <- vegdist(tau.hat, method = "euclidean")
print(EMP.CRD <- 2 * sum(tau.dist^2) / (18*17))

print(PRE <- contr.CRD[2,2]^2)

RCB.lm <- lm(Yield ~ trtmt.RCB + block,
  data = plot.shp@data)
RCB.sum <- summary(RCB.lm)
contr.RCB <- coef(RCB.sum)
tau.RCB <- contr.RCB[,1]
tau.RCB[2:18] <- tau.RCB[1] + tau.RCB[2:18]
taudist.RCB <- vegdist(tau.RCB, method = "euclidean")
print(EMP.RCB <- 2 * sum(taudist.RCB^2) / (18*17))

MSE <- contr.RCB[2,2]^2
print(PRE.RCB <- MSE / 2)


# Papadakis method
library(spdep)
nb6x12 <- cell2nb(6,12)
W <- nb2listw(nb6x12)

#CRD analysis
trt.mean <- tapply(plot.shp@data$Yield,
  plot.shp@data$trtmt.CRD, mean)
Yield.e <- plot.shp@data$Yield -
    sapply(plot.shp@data$trtmt.CRD, function(x) trt.mean[x])
X <- listw2mat(W) %*% Yield.e
papa.CRD <- with(plot.shp@data, data.frame(Yield = Yield,
   trtmt = trtmt.CRD, X = X))
summary(lm(Yield ~ trtmt + X, data = papa.CRD))


CRD.lm <- l(Yield ~ trtmt.CRD,
  data = plot.shp@data)
summary(CRD.lm)

RCB.lm <- lm(Yield ~ block + trtmt.RCB,
  data = plot.shp@data)
summary(RCB.lm)

# 16.4 This assumes that the code from  Section 16.3.4 has been run
library(mgcv)
x <- (cell.ctrs[,1] - W) / (E - W)
y <- (cell.ctrs[,2] - S) / (N - S)
Yield.T <- gam(Yield ~ s(x, bs = "cr", k = 5) + s(y, bs = "cr", k = 5),
  data = plots.spdf@data, family = gaussian)
trend.RCB <- with(plots.spdf@data, data.frame(Yield = Yield,
   trtmt = trtmt.RCB, Trend = predict(Yield.T)))
trend.form <- as.formula(Yield ~ trtmt + Trend)
library(nlme)
trend.RCB$x <- x
trend.RCB$y <- y
model.gls <- gls(Yield ~ trtmt + Trend, data = trend.RCB)
# Check that the trend looks reasonable
trellis.device(color = FALSE)
plot(Variogram(model.gls, form = ~ x + y,
   maxDist = 1), xlim = c(0,1),
   main = "Variogram of Residuals")
model.gls2 <- update(model.gls,
   corr = corSpher(value = c(0.35, 0.5),
   form = ~ x + y, nugget = TRUE))
summary(model.gls2)
coef(summary(model.gls2))

EMP.calc <- function(trt.data, form.lm){
   trt.data$trtmt <- factor(unlist(tapply(rep(1:18,4),
      sort(rep(1:4,18)), sample)))
   trt.sum <- summary(lm(form.lm, data = trt.data))
   contr <- coef(trt.sum)
   tau.hat <- contr[1:18,1]
   tau.hat[2:18] <- tau.hat[1] + tau.hat[2:18]
   tau.dist <- vegdist(tau.hat, method = "euclidean")
   EMP <- 2 * sum(tau.dist^2) / (18*17)
   return(EMP)
}

set.seed(123)
RCB.form <- as.formula(Yield ~ trtmt.RCB + block)
U <- replicate(100, EMP.calc(plots.spdf@data, RCB.form))
print(EMP.RCB <- mean(U))


EMP.gls <- function(trt.data){
   trt.data$trtmt <- factor(unlist(tapply(rep(1:18,4),
      sort(rep(1:4,18)), sample)))
   trt.sum <- summary(gls(Yield ~ trtmt + Trend,
      corr = corSpher(value = c(0.35, 0.5),
      form = ~ x + y, nugget = TRUE), data = trt.data))
   contr <- coef(trt.sum)
   tau.hat <- c(0, contr[2:18])
   tau.dist <- vegdist(tau.hat, method = "euclidean")
   EMP <- 2 * sum(tau.dist^2) / (18*17)
   return(EMP)
}

set.seed(123)
U <- replicate(100, EMP.gls(trend.RCB))
print(EMP.gls <- mean(U))
print(EMPbar.gls <- 100 * EMP.gls / EMP.RCB)



#16.5
library(spdep)
lambda <- 0.6
nlist.14 <- cell2nb(14, 14)
IrWinv.14 <- invIrM(nlist.14, lambda)
nlist <- cell2nb(10, 10)
W <- nb2listw(nlist)

para.samp <- function(Y1, Y2, IrWinv1.mod, IrWinv2.mod){
  Y1.samp <- sample(Y1, length(Y1), replace = TRUE)
  Y1.boot <- IrWinv1.mod %*% Y1.samp
  Y2.samp <- sample(Y2, length(Y2), replace = TRUE)
  Y2.boot <- IrWinv2.mod %*% Y2.samp
  Z <- c(rep(1,100), rep(0,100))
  Y <- c(Y1.boot, Y2.boot)
  r.hat <- cor(Y,Z) 
}

mc.sim <- function(){
   Y1.plus <- IrWinv.14 %*% rnorm(14^2)
   Y1 <- matrix(Y1.plus, nrow = 14, byrow = TRUE)[3:12,3:12]
   Y2.plus <- IrWinv.14 %*% rnorm(14^2)
   Y2 <- matrix(Y2.plus, nrow = 14, byrow = TRUE)[3:12,3:12]
# Fit a spatial error to the data
   Y1.mod <- errorsarlm(as.vector(Y1) ~ 1, listw = W)
   e1.hat <- residuals(Y1.mod)
   lambda1.hat <- Y1.mod$lambda
   Y2.mod <- errorsarlm(as.vector(Y2) ~ 1, listw = W)
   e2.hat <- residuals(Y2.mod)
   lambda2.hat <- Y2.mod$lambda
   IrWinv1.mod <- invIrM(nlist, lambda1.hat)
   IrWinv2.mod <- invIrM(nlist, lambda2.hat)
   U <- replicate(200, para.samp(e1.hat, e2.hat, IrWinv1.mod,
      IrWinv2.mod))
   sigmar.hat <- var(U)
   p.uncorr <- t.test(Y1, Y2, "two.sided")$p.value
   TypeI.uncorr <- as.numeric(p.uncorr < 0.05)
   ne.hat <- 1 / (2*sigmar.hat)
   Z <- c(rep(1,100), rep(0,100))
   Y <- c(Y1, Y2)
   rw <- cor(Y, Z)
   t.corr <- rw*sqrt(2*ne.hat - 2) / (1 - rw^2)
   p.corr <- 2 * (1 - pt(q = abs(t.corr), df = 2*ne.hat - 2))
   TypeI.corr <- as.numeric(p.corr < 0.05)
   return(c(TypeI.uncorr, TypeI.corr))
}

set.seed(123)
U <- replicate(1000, mc.sim())
print(mean(U[1,]), digits = 3)
print(mean(U[2,]), digits = 3)

#16.6
# This assumes that the code from Section 16.4.3 is in memory
# Fit a spatial lag to the data
Y1.mod <- lagsarlm(SoilpH ~ 1, data = data.Set4.2W, listw = W.1)
e1.hat <- residuals(Y1.mod)
print(rho1.hat <- Y1.mod$rho, digits = 2)
Y2.mod <- lagsarlm(SoilpH ~ 1, data = data.Set4.2E, listw = W.2)
e2.hat <- residuals(Y2.mod)
print(rho2.hat <- Y2.mod$rho, digits = 2)
IrWinv1.mod <- invIrM(nlist.1, rho1.hat)
IrWinv2.mod <- invIrM(nlist.2, rho2.hat)
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





