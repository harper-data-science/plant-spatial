library(sf)
data.Set2S.sf <- st_read("created\\set2Sierra.shp")
data.Set2S.glm <- with(data.Set2S.sf, data.frame(MAT, TempR,
   Precip, PE, ET, Texture, AWCAvg, Permeab, SolRad6, SolRad12,
   SolRad, CoastDist, QUDO))
data.Set2S.glm$PM100 <- as.numeric(data.Set2S.sf$PM100 > 0)
data.Set2S.glm$PM200 <- as.numeric(data.Set2S.sf$PM200 > 0)
data.Set2S.glm$PM300 <- as.numeric(data.Set2S.sf$PM300 > 0)
data.Set2S.glm$PM400 <- as.numeric(data.Set2S.sf$PM400 > 0)
data.Set2S.glm$PM500 <- as.numeric(data.Set2S.sf$PM500 > 0)
data.Set2S.glm$PM600 <- as.numeric(data.Set2S.sf$PM600 > 0)

model.glmSfull <- glm(QUDO ~ ., data = data.Set2S.glm,
  family = binomial)
AIC(model.glmSfull, k = log(nrow(data.Set2S.glm)))
# Take a peak (this is not in the text)
d1 <- drop1(model.glmSfull)
d1[order(d1[,3], decreasing = TRUE),]

model.formula <- as.formula("QUDO ~ MAT + TempR + Precip + PE + ET +
  Texture + AWCAvg + Permeab + SolRad6 + SolRad12 + SolRad +
  PM100 + PM200 + PM300 + PM400 + PM500 + PM600 + CoastDist")
model.glmS0 <- glm(QUDO ~ 1, data = data.Set2S.glm, family = binomial)
a1 <- add1(model.glmS0, model.formula)
a1[order(a1[,3]),]

model.glmS1 <- update(model.glmS0,
   formula = as.formula("QUDO ~ Precip"))
model.formula2 <- as.formula("QUDO ~ MAT + Precip + PE + ET +
  Texture + AWCAvg + Permeab + SolRad6 + SolRad12 + SolRad + CoastDist")
a1 <- add1(model.glmS1, model.formula)
a1[order(a1[,3]),]

with(data.Set2S.glm, cor(Precip, MAT))

##########
# Not in text. Again, try a parallel anaysis with drop1
model.glmSfull2 <- glm(QUDO ~ MAT + PE + ET + Texture + AWCAvg +
  Permeab + SolRad6 + SolRad12 + SolRad + CoastDist, data = data.Set2S.glm,
  family = binomial)
d1 <- drop1(model.glmSfull2)
d1[order(d1[,3], decreasing = TRUE),]
########

model.glmS2 <- update(model.glmS0,
   formula = as.formula("QUDO ~ Precip + MAT"))
summary(model.glmS1)
summary(model.glmS2)

library(car)
# Discussed but not shown in text
avPlots(model.glmS2,
   main = "Added Variable Plot for QUDO ~ Precip + MAT")
crPlots(model.glmS2,
   main = "Partial Residual Plot for QUDO ~ Precip + MAT")
   
model.glmS1sq <- update(model.glmS0,
   formula = as.formula("QUDO ~ Precip + I(Precip^2)"))
summary(model.glmS1sq)

model.glmS1sq <- update(model.glmS0,
   formula = as.formula("QUDO ~ Precip + I(Precip^2)"))
anova(model.glmS1, model.glmS1sq, test = "Chisq")

model.glmS2sq <- update(model.glmS0,
   formula = as.formula("QUDO ~ Precip + I(Precip^2) + MAT"))
coef(model.glmS2sq)
anova(model.glmS2, model.glmS2sq, test = "Chisq")

model.glmS2int <- update(model.glmS0,
   formula = as.formula("QUDO ~ Precip + I(Precip*MAT) +MAT"))
anova(model.glmS2, model.glmS2int, test = "Chisq")

AIC(model.glmS1, k = log(nrow(data.Set2S.glm)))
AIC(model.glmS2, k = log(nrow(data.Set2S.glm)))

 
library(maps)
library(maptools)
data(stateMapEnv)
cal.map <- map("state", "california",
   fill=TRUE, col="transparent",  plot = FALSE)
cal.poly <- map2SpatialPolygons(cal.map, "California")
proj4string(cal.poly) <- CRS("+proj=longlat +datum=WGS84")
sierra.spdf <- as(data.Set2S.sf, "Spatial")
proj4string(sierra.spdf) <- CRS("+proj=longlat +datum=WGS84")

cal = list("sp.polygons", cal.poly,	fill = "transparent")
deg.per.km <- 360 / 40075
lat <- 38.3
long <- -122.3
n.deg <- 1
deg.per.km <- deg.per.km * cos(pi*lat / 180)
scale <- list("SpatialPolygonsRescale", layout.scale.bar(),
	offset = c(long+0.5,lat-0.5), scale = 100 * deg.per.km, fill=c("transparent","black"))
text1 <- list("sp.text", c(long+0.5,lat-0.32), "0")
text2 <- list("sp.text", c(long+0.32+n.deg,lat-0.32), "100 km")
arrow <- list("SpatialPolygonsRescale", layout.north.arrow(),
	offset = c(long+0.7, lat-1.3), scale = 0.5)
greys <- grey(0:200 / 255)
sierra.spdf$QUDOFac <- factor(data.Set2S.sf$QUDO, labels =
   c("Absent", "Present"))
sierra.spdf$P1 <- predict(model.glmS1, type = "response")
sierra.spdf$DP2 <- factor(predict(model.glmS2, type = "response") -
   predict(model.glmS1, type = "response") >= 0,
   labels = c("Decreased Probability", "Increased Probability"))

spplot(sierra.spdf, "QUDOFac", col.regions = greys, cex = 0.5,
   key.space = "right",   # Fig. 8.10a
   sp.layout=list(cal,scale,text1,text2, arrow),
   main = "Blue Oak Presence/Absence: Precip Only")

spplot(sierra.spdf, "DP2", col.regions = greys, cex = 0.5,
   key.space = "right",   # Fig. 8.10b
   sp.layout=list(cal,scale,text1,text2, arrow),
   main = "Effect of Adding MAT")

model.formula2 <- as.formula("QUDO ~ MAT + Precip + PE + ET +
  Texture + AWCAvg + Permeab + SolRad6 + SolRad12 + SolRad")
a1 <- add1(model.glmS2, model.formula2)
a1[order(a1[,3]),]

#######
#Not in text. Continue with parallel dropping
model.glmSfull3 <- glm(QUDO ~ PE + ET + Texture + AWCAvg +
  Permeab + SolRad6 + SolRad12 + SolRad, data = data.Set2S.glm,
  family = binomial)
d1 <- drop1(model.glmSfull3)
d1[order(d1[,3], decreasing = TRUE),]
############

model.glmS5 <- update(model.glmS0,
   formula = as.formula("QUDO ~ Precip + MAT + SolRad +
   AWCAvg + Permeab"))

# Consider the effect of including Elevation
data.Set2SglmE <- data.frame(cbind(data.Set2S.glm,
   Elevation = data.Set2S.sf$Elevation))
model.glmS5E <- glm(QUDO ~ Precip + MAT + SolRad +
   AWCAvg + Permeab + Elevation, data = data.Set2SglmE,
   family = binomial)
AIC(model.glmS5, k = log(nrow(data.Set2S.glm)))
AIC(model.glmS5E, k = log(nrow(data.Set2S.glm)))
summary(model.glmS5E)

print(d1 <- drop1(model.glmS5E))


