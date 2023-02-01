library(sf)
library(gstat)
library(stars)
library(starsExtra)

data.Set2C.sf <- st_read("created\\set2coast.shp")
# Compute clusters
Set2.kmeans <- with(data.Set2C.sf, data.frame(scale(Longitude),
  scale(Latitude), 0.5 * scale(Elevation)))
cluster.k <- 50
set.seed(123)
cl <- kmeans(Set2.kmeans, cluster.k, iter.max = 100)
data.Set2C.sf$clusID <- cl$cluster

# Map display of southern tip (Fig. 12.7)
data.disp <- data.Set2C.sf[which(data.Set2C.sf$Latitude < 35.5),]
sort(unique(data.disp$clusID))
data.disp$clusID[which(data.disp$clusID == 48)] <- 0
data.disp$clusID[which(data.disp$clusID == 19)] <- 3
data.disp$clusID[which(data.disp$clusID == 44)] <- 25
data.disp$clusID[which(data.disp$clusID == 45)] <- 16
data.disp$clusID[which(data.disp$clusID == 47)] <- 7
#data.disp$clusID[which(data.disp$clusID == 12)] <- 23
data.disp$clusID[which(data.disp$clusID == 38)] <- 10
#data.disp$clusID[which(data.disp$clusID == 10)] <- 12
#data.disp$clusID[which(data.disp$clusID == 7)] <- 13
#data.disp$clusID[which(data.disp$clusID == 9)] <- 6
#data.disp$clusID[which(data.disp$clusID == 14)] <- 16
#data.disp$clusID[which(data.disp$clusID == 27)] <- 21
data.disp$clusID[which(data.disp$clusID == 46)] <- 9
data.disp$clusID[which(data.disp$clusID == 50)] <- 4
data.disp$clusID[which(data.disp$clusID == 26)] <- 5
sort(unique(data.disp$clusID))
data.disp.sf <- data.disp
st_crs(data.disp.sf) <- "+proj=longlat +datum=WGS84"

data.utm.sf <- st_transform(data.disp.sf, crs = st_crs("EPSG:32610"))
grid.xy <- make_grid(data.utm.sf, res = 1000, buffer = 0)
st_crs(grid.xy) <- "EPSG:32610" # UTM Zone 10N
elev.idw <- krige(Elevation ~ 1, data.utm.sf, grid.xy)
par(mai = c(1,1,1,1))
plot(st_geometry(data.utm.sf), pch = data.utm.sf$clusID, axes = TRUE, cex = 0.8)

library(raster)
elev.idw.ras <- as(elev.idw["var1.pred"], "Raster")
library(terra)
elev.idw.ter <- rast(elev.idw.ras)
contour(elev.idw.ter, add = TRUE, nlevels = 5)

library(maps)
data(stateMapEnv)
cal.map <- map("state", "california",
   fill=TRUE, col="transparent",  plot = FALSE)
cal.poly.sf <- st_as_sf(cal.map, "California")
st_crs(cal.poly.sf) <- "EPSG:4326" # WGS 84
cal.utm.sf <- st_transform(cal.poly.sf, crs = st_crs("EPSG:32610"))
plot(st_geometry(cal.utm.sf), add = TRUE, lwd = 3)
title(main = "Cluster Regions (Southern Coast Range)",
   cex.main = 2, xlab = "Easting (m)",
   ylab = "Northing (m)", cex.lab = 1.5)   # End of Fig. 12.7

# Color version
plot(st_geometry(data.utm.sf),
  col = data.utm.sf$clusID, axes = TRUE, cex = 0.8, pch = 16)
contour(elev.idw.ter, add = TRUE, nlevels = 5)
plot(st_geometry(cal.utm.sf), add = TRUE, lwd = 3)
title(main = "Cluster Regions (Southern Coast Range)",
   cex.main = 2, xlab = "Easting (m)",
   ylab = "Northing (m)", cex.lab = 1.5)   

   
data.Set2Cpts <- with(data.Set2C.sf, data.frame(MAT, TempR, GS32,
   Precip, PE, ET, Texture, AWCAvg, Permeab, SolRad6, SolRad12,
   SolRad, QUDO, clusID))
data.Set2Cpts$PM100 <- as.numeric(data.Set2C.sf$PM100 > 0)
data.Set2Cpts$PM200 <- as.numeric(data.Set2C.sf$PM200 > 0)
data.Set2Cpts$PM300 <- as.numeric(data.Set2C.sf$PM300 > 0)
data.Set2Cpts$PM400 <- as.numeric(data.Set2C.sf$PM400 > 0)
data.Set2Cpts$PM500 <- as.numeric(data.Set2C.sf$PM500 > 0)
data.Set2Cpts$PM600 <- as.numeric(data.Set2C.sf$PM600 > 0)

data.Set2C.mat <- matrix(nrow = length(unique(data.Set2Cpts$clusID)),
   ncol = ncol(data.Set2Cpts))
for (i in 1:ncol(data.Set2Cpts)){
   data.Set2C.mat[,i] <-  tapply(data.Set2Cpts[,i],
      data.Set2Cpts$clusID, mean)
}
data.Set2Cclus <- data.frame(data.Set2C.mat)
names(data.Set2Cclus) <- names(data.Set2Cpts)

nrow(data.Set2Cpts)
nrow(data.Set2Cclus)

model.glmCfull <- glm(QUDO ~ ., data = data.Set2Cclus,
  family = quasibinomial)
summary(model.glmCfull)

model.glmC6clus <- glm(QUDO ~ TempR + Permeab + Precip +
   GS32 + PE + SolRad6, data = data.Set2Cclus,
   family = quasibinomial)
print(coef(model.glmC6clus), digits = 2)
model.glmC6pts <- glm(QUDO ~ TempR + Permeab + Precip +
   GS32 + PE + SolRad6, data = data.Set2Cpts,
   family = binomial)
print(coef(model.glmC6pts), digits = 2)

model.glmC3clus <- update(model.glmC6clus,
   formula = as.formula("QUDO ~ Permeab +
   GS32 + PE"))
model.glmC3pts <- update(model.glmC6pts,
   formula = as.formula("QUDO ~ Permeab +
   GS32 + PE"))
anova(model.glmC3clus, model.glmC6clus, test = "Chisq")
anova(model.glmC3pts, model.glmC6pts, test = "Chisq")

model.glmC2clus <- update(model.glmC3clus,
   formula = as.formula("QUDO ~ Permeab + GS32"))
anova(model.glmC2clus, model.glmC3clus, test = "Chisq")

# Check for outliers and higher order terms
library(car)
avPlots(model.glmC3clus)
crPlots(model.glmC3clus)

model.glmC3clusA <- update(model.glmC3clus,
   formula = as.formula("QUDO ~ Permeab +  I(Permeab^2) +
   GS32 + PE"))
anova(model.glmC3clus, model.glmC3clusA, test = "Chisq")

# Pointwise predictions
data.Set2Cclus$predict.clus <- predict(model.glmC3clus, type = "response")
# Clusterise predictions
data.Set2Cpts$predict.pts <- predict(model.glmC3pts, type = "response")
# Means over clusters of pointwise predictions
predict.pt2clus <- tapply(data.Set2Cpts$predict.pts,
   data.Set2Cpts$clusID, mean)
# Clusterwise predictions corresponding to each pointwise prediction
predict.clus2pt <- numeric(nrow(data.Set2Cpts))
for (i in 1:nrow(data.Set2Cclus)){
   predict.clus2pt[which(data.Set2Cpts$clusID ==
      data.Set2Cclus$clusID[i])] <- data.Set2Cclus$predict.clus[i]
}
data.Set2Cpts$predict.clus <- predict.clus2pt
par(mai = c(1,1,1,1))
with(data.Set2Cpts, plot(predict.clus2pt, predict.pts,
   xlab = "Cluster Model", ylab = "Mean Pointwise Model",
   cex.lab = 1.5, cex = 0.5))
lines(c(0,1), c(0,1))
points(data.Set2Cclus$predict.clus, predict.pt2clus, , pch = 16,
  cex = 1.5)
title(main = "Model Predictions", cex.main = 2)

with(data.Set2Cclus, plot(predict.clus, QUDO,
   xlab = "Cluster Model Prediction", ylab = "Fraction of Sites with a Blue Oak",
   cex.lab = 1.5))
title(main = "Predicted vs. Observed Blue Oak Presence",
   cex.main = 1.5)
# One low and one high QUDO value seem anomalous
# Find them
df <- with(data.Set2Cclus, cbind(predict.clus, QUDO,
  clusID))
df[order(df[,2]),] # We could also use the function identify()
text(0.5, 0.03, "ID = 36")
text(0.55, 0.97, "ID = 26")

