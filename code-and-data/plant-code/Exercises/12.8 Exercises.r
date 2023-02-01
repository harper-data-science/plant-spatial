#12.1
# This assumes 12.2 is in memory
mean.yield <- unlist(tapply(data.Set3a$Yield,data.Set3a$Field,mean))
x <- cbind(coef(Yld.lmeF2)[,1],mean.yield)
plot(x[,2],x[,1])

# 12.2
data.Set4.2 <- read.csv("Set4\\Set4.296sample.csv", header = TRUE)
yield.pts <- read.csv("created\\Set4.2yld96ptsidw.csv")
data.Set4.2$Yield <- yield.pts$Yield

model.lm <- lm(Yield ~ Sand + SoilTN + Weeds, data = data.Set4.2)
summary(model.lm)

library(nlme)
model.gls1 <- gls(Yield ~ Sand + SoilTN + Weeds, data = data.Set4.2)
summary(model.gls1)

library(lattice)
trellis.device(color = FALSE)
plot(Variogram(model.gls1, form = ~ Easting + Northing,
   maxDist = 300), xlim = c(0,300),
   main = "Variogram of Residuals, Field 4.2 North")
   
model.gls2 <- update(model.gls1,
   corr = corSpher(form = ~ Easting + Northing, nugget = TRUE))
anova(model.gls1, model.gls2)
plot(model.gls2, resid(., type = "n") ~ fitted(.),
   abline = 0)
qqnorm(model.gls2, ~ resid(., type = "n"))
summary(model.gls1)
summary(model.gls2)




# 12.4
library(maptools)
library(sf)
data.Set2S.sf <- st_read("created\\set2sierra.shp")
data.Set2S <- as(data.Set2S.sf, "Spatial")
# Compute clusters

Set2.kmeans <- with(data.Set2S@data, data.frame(scale(Longitude),
  scale(Latitude), 0.5 * scale(Elevation)))
cluster.k <- 50
set.seed(123)
cl <- kmeans(Set2.kmeans, cluster.k, iter.max = 100)
data.Set2S$clusID <- cl$cluster
data.Set2Spts <- with(data.Set2S@data, data.frame(MAT, TempR, GS32,
   Precip, PE, ET, Texture, AWCAvg, Permeab, SolRad6, SolRad12,
   SolRad, QUDO, clusID))
data.Set2Spts$PM100 <- as.numeric(data.Set2S$PM100 > 0)
data.Set2Spts$PM200 <- as.numeric(data.Set2S$PM200 > 0)
data.Set2Spts$PM300 <- as.numeric(data.Set2S$PM300 > 0)
data.Set2Spts$PM400 <- as.numeric(data.Set2S$PM400 > 0)
data.Set2Spts$PM500 <- as.numeric(data.Set2S$PM500 > 0)
data.Set2Spts$PM600 <- as.numeric(data.Set2S$PM600 > 0)

data.Set2S.mat <- matrix(nrow = length(unique(data.Set2Spts$clusID)),
   ncol = ncol(data.Set2Spts))
for (i in 1:ncol(data.Set2Spts)){
   data.Set2S.mat[,i] <-  tapply(data.Set2Spts[,i],
      data.Set2Spts$clusID, mean)
}
data.Set2Sclus <- data.frame(data.Set2S.mat)
names(data.Set2Sclus) <- names(data.Set2Spts)

nrow(data.Set2Spts)
nrow(data.Set2Sclus)

model.glmSfull <- glm(QUDO ~ ., data = data.Set2Sclus,
  family = quasibinomial)
summary(model.glmSfull)

model.glmS1clus <- glm(QUDO ~ Precip + MAT + SolRad +
   AWCAvg + Permeab, data = data.Set2Sclus,
   family = quasibinomial)
summary(model.glmS1clus)
anova(model.glmS1clus, model.glmSfull, test = "Chisq")

model.glmSfullpts <- glm(QUDO ~ ., data = data.Set2Spts,
  family = binomial)

model.glmS1pts <- glm(QUDO ~ Precip + MAT + SolRad +
   AWCAvg + Permeab, data = data.Set2Spts,
   family = binomial)
anova(model.glmS1pts, model.glmSfullpts, test = "Chisq")

# Pointwise predictions
data.Set2Sclus$predict.clus <- predict(model.glmS1clus, type = "response")
# Clusterise predictions
data.Set2Spts$predict.pts <- predict(model.glmS1pts, type = "response")
# Means over clusters of pointwise predictions
predict.pt2clus <- tapply(data.Set2Spts$predict.pts,
   data.Set2Spts$clusID, mean)
# Clusterwise predictions corresponding to each pointwise prediction
predict.clus2pt <- numeric(nrow(data.Set2Spts))
for (i in 1:nrow(data.Set2Sclus)){
   predict.clus2pt[which(data.Set2Spts$clusID ==
      data.Set2Sclus$clusID[i])] <- data.Set2Sclus$predict.clus[i]
}
data.Set2Spts$predict.clus <- predict.clus2pt
par(mai = c(1,1,1,1))
with(data.Set2Spts, plot(predict.clus2pt, predict.pts,
   xlab = "Cluster Model", ylab = "Mean Pointwise Model",
   cex.lab = 1.5, cex = 0.5))
lines(c(0,1), c(0,1))
points(data.Set2Sclus$predict.clus, predict.pt2clus, , pch = 16,
  cex = 1.5)
title(main = "Model Predictions", cex.main = 2)

with(data.Set2Sclus, plot(predict.clus, QUDO,
   xlab = "Cluster Model Prediction", ylab = "Fraction of Sites with a Blue Oak",
   cex.lab = 1.5))
title(main = "Predicted vs. Observed Blue Oak Presence",
   cex.main = 1.5)
lines(c(0,1), c(0,1))

# 12.5
library(maptools)
library(rgdal)
library(sf)
# From 12.6.1
data.Set2C.sf <- st_read("created\\set2coast.shp")
data.Set2C <- as(data.Set2C.sf, "Spatial")
Set2.kmeans <- with(data.Set2C@data, data.frame(scale(Longitude),
  scale(Latitude), 0.5 * scale(Elevation)))
cluster.k <- 50
set.seed(123)
cl <- kmeans(Set2.kmeans, cluster.k, iter.max = 100)
data.Set2C$clusID <- cl$cluster
data.Set2Cpts <- with(data.Set2C@data, data.frame(MAT, TempR, GS32,
   Precip, PE, ET, Texture, AWCAvg, Permeab, SolRad6, SolRad12,
   SolRad, QUDO, Elevation, Longitude, Latitude, clusID))
coordinates(data.Set2Cpts) <- c("Longitude", "Latitude")
proj4string(data.Set2Cpts) <- CRS("+proj=longlat +datum=WGS84")
data.Set2Cutm <- spTransform(data.Set2Cpts,
  CRS("+proj=utm +zone=10 +ellps=WGS84"))
data.Set2Cpts$Easting <- coordinates(data.Set2Cutm)[,1]
data.Set2Cpts$Northing <- coordinates(data.Set2Cutm)[,2]

data.Set2C.mat <- matrix(nrow = length(unique(data.Set2Cpts$clusID)),
   ncol = ncol(data.Set2Cpts))
for (i in 1:ncol(data.Set2Cpts)){
   data.Set2C.mat[,i] <-  tapply(data.Set2Cpts@data[,i],
      data.Set2Cpts$clusID, mean)
}
data.Set2Cclus <- data.frame(data.Set2C.mat)
names(data.Set2Cclus) <- names(data.Set2Cpts)


data.Set2C.mat <- matrix(nrow = length(unique(data.Set2Cpts$clusID)),
   ncol = ncol(data.Set2Cpts))
for (i in 1:ncol(data.Set2Cpts)){
   data.Set2C.mat[,i] <-  tapply(data.Set2Cpts@data[,i],
      data.Set2Cpts$clusID, mean)
}
data.Set2Cclus <- data.frame(data.Set2C.mat)
names(data.Set2Cclus) <- names(data.Set2Cpts)

logit.ftn <- deriv( ~ 1 / (1 + exp(-(b0 + b1 * X))), c("b0", "b1"),
   function(b0, b1, X){} )
   
oaks.glm <- glm(QUDO ~ Elevation, data = data.Set2Cclus,
   family = quasibinomial)
coef(oaks.glm)

library(nlme)
oaks.nls <- nls(QUDO ~ logit.ftn(b0, b1, Elevation),  # P&B p. 401
   data = data.Set2Cclus, start = c(b0 = -1.1, b1 = 0.001))

X <- seq(0, 2000, 50)
logistic <- function(b, X) 1 / (1 + exp(-(b[1] + b[2] * X)))


logistic(coef(oaks.glm), X)
logistic(coef(oaks.nls), X)
