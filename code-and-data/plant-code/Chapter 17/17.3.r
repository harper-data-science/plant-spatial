library(sf)
data.Set2C.sf <- st_read("created\\set2coast.shp")

par(mai = c(1,1,1,1))
plot(data.Set2C.sf$Precip, data.Set2C.sf$JuMean, # Fig. 17.2a
   pch = 1+15*data.Set2C.sf$QUDO,
   xlab = "Mean Annual Precipitation (mm)", cex.lab = 1.5,
   ylab = expression(Mean~July~Temperature~"("*degree*C*")"))
title(main = "Coast Range Blue Oaks", cex.main = 2)
lines(c(200, 1400), c(19.8,19.8))
legend(1200, 26, pch = c(1,16), legend = c("Absent", "Present"))

plot(data.Set2C.sf$Precip, data.Set2C.sf$JaMean, # Fig. 17.2b
   pch = 1+15*data.Set2C.sf$QUDO,
   xlab = "Mean Annual Precipitation (mm)", cex.lab = 1.5,
   ylab = expression(paste(Mean~Jan~Temperature~"(",degree,C,")")))
title(main = "Coast Range Blue Oaks", cex.main = 2)
lines(c(200, 1400), c(9.2,9.2))
legend(1200, 26, pch = c(1,16), legend = c("Absent", "Present"))

Set2.JuLow <- data.Set2C.sf[which((data.Set2C.sf$JuMean < 19.8) &
   (data.Set2C.sf$QUDO == 1)),]
Set2.JaHi <- data.Set2C.sf[which((data.Set2C.sf$JaMean > 9.2) &
   (data.Set2C.sf$QUDO == 1)),]
Set2.Oaks.sf <- data.Set2C.sf[which(data.Set2C.sf$QUDO == 1),]
st_crs(Set2.Oaks.sf) <- "EPSG:4326"
plot(st_geometry(Set2.Oaks.sf), axes = TRUE, pch = 1, cex = 0.8)  # Fig. 17.3a
title("Low JuMean Blue Oaks", cex.main = 2,
   xlab = "Longitude", cex.lab = 1.5, ylab = "Latitude")
Set2.Oaks <- data.Set2C.sf[which(data.Set2C.sf$QUDO == 1),]
plot(st_geometry(Set2.JuLow), pch = 16, cex = 1.5, add = TRUE)
Set2.No <- data.Set2C.sf[which(data.Set2C.sf$QUDO == 0),]
plot(st_geometry(Set2.No), add = TRUE, pch = 1, cex = 0.2)
legend(-123, 35.5, pt.cex = c(0.8,1.5,0.2), pch = c(1,16,1),
   legend = c("Hi July Temp","Low July Temp", "No Blue Oaks"))

plot(st_geometry(Set2.Oaks.sf), axes = TRUE, pch = 1, cex = .8)  # Fig. 17.3b
title("High JaMean Blue Oaks", cex.main = 2,
   xlab = "Longitude", cex.lab = 1.5, ylab = "Latitude")
Set2.Oaks.sf <- data.Set2C.sf[which(data.Set2C.sf$QUDO == 1),]
plot(st_geometry(Set2.JaHi), pch = 16,, cex = 1.5, add = TRUE)
Set2.No <- data.Set2C.sf[which(data.Set2C.sf$QUDO == 0),]
plot(st_geometry(Set2.No), add = TRUE, pch = 1, cex = 0.2)
legend(-123, 35.5, pt.cex = c(0.8,1.5,0.2), pch = c(1,16,1),
   legend = c("Low Jan Temp","Hi Jan Temp", "No Blue Oaks"))

cor(data.Set2C.sf$JaMean, data.Set2C.sf$JuMean)

plot(data.Set2C.sf$JaMean, data.Set2C.sf$JuMean, cex = 0.3,
   xlab = expression(paste(Mean~Jan~Temperature~"(",degree,C,")")),
   ylab = expression(paste(Mean~July~Temperature~"(",degree,C,")")),
   cex.lab = 1.5) # Fig 17.4
title(main = "Anomalous Temperature Sites", cex.main = 2)
points(Set2.JuLow$JaMean, Set2.JuLow$JuMean, pch = 3, cex = 2)
points(Set2.JaHi$JaMean, Set2.JaHi$JuMean, pch = 16)
lines(c(9.2,9.2), c(14,27))
lines(c(6,11), c(19.8,19.8))
text(6,15.5, "Ja OK, Ju Low")
text(11,15.5, "Ja High, Ju Low")
text(6,25, "Ja OK, Ju OK")
text(11,25, "Ja Hi, Ju OK")

data.Set2K.sf <- st_read("created\\set2klamath.shp")
Set2.glmK <- glm(QUDO ~ Precip + JuMean, data = data.Set2K.sf, family = binomial)
summary(Set2.glmK)
plot(data.Set2K.sf$Precip, data.Set2K.sf$JuMean, pch = 1+15*data.Set2K.sf$QUDO,
   xlab = "Mean Annual Precipitation (mm)", cex.lab = 1.5,
   ylab = expression(paste(Mean~July~Temperature~"(",degree,C,")")))
title(main = "Klamath Blue Oaks", cex.main = 2)  # Fig. 17.5a
legend(1000, 22, pch = c(1,16), legend = c("Absent", "Present"))

data.Set2T.sf <- st_read("created\\set2transverse.shp")
Set2.glmT <- glm(QUDO ~ Precip + JuMean, data = data.Set2T.sf, family = binomial)
summary(Set2.glmT)
plot(data.Set2T.sf$Precip, data.Set2T.sf$JuMean, pch = 1+15*data.Set2T.sf$QUDO,
   xlab = "Mean Annual Precipitation (mm)", cex.lab = 1.5,
   ylab = expression(paste(Mean~July~Temperature~"(",degree,C,")")))
title(main = "Traverse Range Blue Oaks", cex.main = 2)  # Fig. 17.5b
legend(700, 19, pch = c(1,16), legend = c("Absent", "Present"))

# Regional data

make.clusters <- function(data.Set){
   Set2.kmeans <- with(data.Set, data.frame(scale(Longitude),
     scale(Latitude), 0.5 * scale(Elevation)))
   cluster.k <- 50
   set.seed(123)
   cl <- kmeans(Set2.kmeans, cluster.k, iter.max = 100)$cluster
   QUDO <- tapply(data.Set$QUDO, cl, mean)
   Precip <- tapply(data.Set$Precip, cl, mean)
   JuMean <- tapply(data.Set$JuMean, cl, mean)
   Permeab <- tapply(data.Set$Permeab, cl, mean)
   Set.pts <- data.Set
   Set.pts$clusID <- cl
   st_crs(Set.pts) <-"EPSG:4326"
   Set.utm <- st_transform(Set.pts, crs = "EPSG:32610")
   Easting <- tapply(st_coordinates(Set.utm)[,1], cl, mean)
   Northing <- tapply(st_coordinates(Set.utm)[,2], cl, mean)
   Set.cl <- data.frame(QUDO, Precip, JuMean, Permeab,
     Easting, Northing)
}
data.Set2Cclus <- make.clusters(data.Set2C.sf)
data.Set2S.sf <- st_read("created\\set2sierra.shp")
data.Set2Sclus <- make.clusters(data.Set2S.sf)
data.Set2clus <- rbind(data.Set2Cclus, data.Set2Sclus)
par(mai = c(1,1,1,1))
with(data.Set2Cclus, plot(Permeab, QUDO, pch = 16,  # Fig. 17.6a
   xlab = "Mean Soil Permeability", cex.lab = 1.5,
   ylab = "Fraction of Sites with a Blue Oak"))
title(main = "Regional Data", cex.main = 2)
legend(2.1, 0.65, pch = c(1,16), legend = c("Sierra Nevada", "Coast Range"))
with(data.Set2Sclus, points(Permeab, QUDO))

with(data.Set2Cclus, plot(Precip, QUDO, pch = 16,  # Fig. 17.6b
   xlab = "Mean Annual Precipitation (mm)", cex.lab = 1.5,
   ylab = "Fraction of Sites with a Blue Oak"))
title(main = "Regional Data", cex.main = 2)
legend(850, 0.75, pch = c(1,16), legend = c("Sierra Nevada", "Coast Range"))
with(data.Set2Sclus, points(Precip, QUDO))

coordinates(data.Set2clus) <- c("Easting", "Northing")

