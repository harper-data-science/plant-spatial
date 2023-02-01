# 7.1
# This is a partial test. To do it right, you need a GIS
library(maptools)
library(sf)
data.obs1 <- read.csv("set1\\obspts.csv", header = TRUE)
coordinates(data.obs1) <- c("Easting", "Northing")
data.Set1.sf <- st_read("set1\\set1data.shp")
data.Set1 <- as(data.Set1.sf, "Spatial")
Set1.obs <- over(data.obs1, data.Set1)
Set1.obs$obsID <- data.obs1$ID
cbind(data.obs1@data$ID, Set1.obs$obsID)


# 7.3 Compute age ratio
OldArea <- data.frame(PatchID = data.Set1.sf$PatchID)
OldArea$Area <- 0
for (i in 1:nrow(OldArea)){
  {if (data.Set1.sf$Age[i] >= 60)
      OldArea$Area[i] <- data.Set1.sf$AgeArea[i] }}
YoungArea <- data.frame(PatchID = data.Set1.sf$PatchID)
YoungArea$Area <- 0
for (i in 1:nrow(OldArea)){
  {if (data.Set1.sf$Age[i] < 60)
      YoungArea$Area[i] <- data.Set1.sf$AgeArea[i] }}
Ratio <- AreaRatio(YoungArea, OldArea)
AgeRatio.df <- data.frame(PatchID = Ratio[,1], AgeRatio = Ratio[,2])

# 7.4a Age suitability score
age.score <- function(x, cat.val){
   score <- 0
   if(x >= cat.val[5]) score <- 0.33
   if(x < cat.val[4] & x >= cat.val[3]) score <- 0.66
   if(x < cat.val[3] & x >= cat.val[2]) score <- 1.0
   if(x < cat.val[3] & x >= cat.val[2]) score <- 0.66
   if(x < cat.val[2] & x >= cat.val[1]) score <- 0.33
   return(score)
}
AgeScore <- numeric(nrow(Set1.obs3))
for (i in 1:nrow(Set1.obs3)){
 AgeScore[i] <-
   age.score(Set1.obs3$AgeRatio[i],
     c(0.375,0.5,0.67,0.8,0.875))
}
# 7.4b 
height.score <- function(x, cat.val){
   score <- 0
   if(x < cat.val[4] & x >= cat.val[3]) score <- 0.66
   if(x < cat.val[3] & x >= cat.val[2]) score <- 1
   if(x < cat.val[2] & x >= cat.val[1]) score <- 0.66
   if(x < cat.val[1]) score <- 0.33
   return(score)
}
HeightScore <- numeric(nrow(Set1.obs3))
for (i in 1:nrow(Set1.obs3)){
 HeightScore[i] <-
    height.score(Set1.obs3$HtRatio[i], c(0.2,0.45,0.55,0.67))
}

# 7.6
#PatchArea and PatchWidth only
print(HSItest <- as.numeric(apply(scores.corr[,1:2], 1, prod) > 0))
UA1 <- which(HSItest == 0 & PAtest == 0)
UP1 <- which(HSItest == 0 & PAtest == 1)
SA1 <- which(HSItest > 0 & PAtest == 0)
SP1 <- which(HSItest > 0 & PAtest == 1)
print(cont.table <- matrix(c(length(SP1),length(SA1),
   length(UP1),length(UA1)), nrow = 2, byrow = TRUE,
   dimnames = list(c("Suit.", "Unsuit."),c("Pres.", "Abs."))))

#PatchArea, PatchWidth, and AgeRatio
print(HSItest <- as.numeric(apply(scores.corr[,1:3], 1, prod) > 0))
UA1 <- which(HSItest == 0 & PAtest == 0)
UP1 <- which(HSItest == 0 & PAtest == 1)
SA1 <- which(HSItest > 0 & PAtest == 0)
SP1 <- which(HSItest > 0 & PAtest == 1)
print(cont.table <- matrix(c(length(SP1),length(SA1),
   length(UP1),length(UA1)), nrow = 2, byrow = TRUE,
   dimnames = list(c("Suit.", "Unsuit."),c("Pres.", "Abs."))))

# PatchArea and AgeRatio only
print(HSItest <- as.numeric(apply(scores.corr[,c(1,3)], 1, prod) > 0))
UA1 <- which(HSItest == 0 & PAtest == 0)
UP1 <- which(HSItest == 0 & PAtest == 1)
SA1 <- which(HSItest > 0 & PAtest == 0)
SP1 <- which(HSItest > 0 & PAtest == 1)
print(cont.table <- matrix(c(length(SP1),length(SA1),
   length(UP1),length(UA1)), nrow = 2, byrow = TRUE,
   dimnames = list(c("Suit.", "Unsuit."),c("Pres.", "Abs."))))
   
#PatchWidth and AgeRatio only
HSItest <- as.numeric(apply(scores.corr[,2:3], 1, prod) > 0)
UA1 <- which(HSItest == 0 & PAtest == 0)
UP1 <- which(HSItest == 0 & PAtest == 1)
SA1 <- which(HSItest > 0 & PAtest == 0)
SP1 <- which(HSItest > 0 & PAtest == 1)
print(cont.table <- matrix(c(length(SP1),length(SA1),
   length(UP1),length(UA1)), nrow = 2, byrow = TRUE,
   dimnames = list(c("Suit.", "Unsuit."),c("Pres.", "Abs."))))

# AgeRatio only
print(HSItest <- as.numeric(scores.corr[,3] > 0))
UA1 <- which(HSItest == 0 & PAtest == 0)
UP1 <- which(HSItest == 0 & PAtest == 1)
SA1 <- which(HSItest > 0 & PAtest == 0)
SP1 <- which(HSItest > 0 & PAtest == 1)
print(cont.table <- matrix(c(length(SP1),length(SA1),
   length(UP1),length(UA1)), nrow = 2, byrow = TRUE,
   dimnames = list(c("Suit.", "Unsuit."),c("Pres.", "Abs."))))

#PatchArea only
print(HSItest <- as.numeric(scores.corr[,1] > 0))
UA1 <- which(HSItest == 0 & PAtest == 0)
UP1 <- which(HSItest == 0 & PAtest == 1)
SA1 <- which(HSItest > 0 & PAtest == 0)
SP1 <- which(HSItest > 0 & PAtest == 1)
print(cont.table <- matrix(c(length(SP1),length(SA1),
   length(UP1),length(UA1)), nrow = 2, byrow = TRUE,
   dimnames = list(c("Suit.", "Unsuit."),c("Pres.", "Abs."))))

# Exercises 7.7 through 7.10 assume that the data from Section 7.3 is loaded 
# 7.7
bdry.pts <- locator(type = "o")
coords.mat <- matrix(unlist(bdry.pts), ncol = 2)
coords.mat <- rbind(coords.mat,coords.mat[1,])
bdry.poly <- vector(mode="list", length=1)
bdry.poly[[1]] <- Polygons(list(Polygon(coords.mat)), ID="1")
bdry.sp <- SpatialPolygons(bdry.poly,
 proj4string = CRS("+proj=utm +zone=10 +ellps=WGS84"))
bdry.spdf <- SpatialPolygonsDataFrame(bdry.sp,
 data = data.frame(ID ="1", row.names = "1"))
proj4string(bdry.spdf) = CRS("+proj=utm +zone=10 +ellps=WGS84")
proj4string(data.Set2) = CRS("+proj=utm +zone=10 +ellps=WGS84")
data.ol <- over(data.Set2,bdry.spdf)
region.spdf <- data.Set2[which(is.na(data.ol$ID) == FALSE),]

# 7.8
pres <- numeric(22)
absent <- numeric(22)
for(i in 0:22) pres[i] <- with(slot(region.spdf, "data"),
    n.oaks(Elevation, QUDO, i*100,(i+1)*100, 1))
for(i in 0:22) absent[i] <- with(slot(region.spdf, "data"),
    n.oaks(Elevation, QUDO, i*100,(i+1)*100, 0))
pa.coast <- pres / (pres + absent + 0.00001)
x <- seq(100,2200,100)
par(mai = c(1,1,1,1))
plot(x, pa.coast, type = "o", xlab = "Elevation (m)",
   ylab = "Portion with Blue Oak", cex.lab = 1.5,
   main = "Blue Oak Presence vs. Elevation",
   cex.main = 2, lty = 1)

# 7.9
sierra.pres <- sierra.spdf@data[which(sierra.spdf@data$QUDO == 1),]
sierra.abs <- sierra.spdf@data[which(sierra.spdf@data$QUDO == 0),]
coast.pres <- coast.spdf@data[which(coast.spdf@data$QUDO == 1),]
coast.abs <- coast.spdf@data[which(coast.spdf@data$QUDO == 0),]
with(sierra.pres, plot(Elevation, Precip))
with(coast.pres, points(Elevation, Precip, pch = 16))

# 7.10
with(data.Set2@data, table(Texture, Permeab))

# 7.11
# This assumes 7.3 has been run
library(hexbin)
sierra.800 <- sierra.spdf@data[which(sierra.spdf@data$Precip <= 800),]
coast.800 <- coast.spdf@data[which(coast.spdf@data$Precip <= 800),]

plot(hexbin(sierra.800$Precip, sierra.800$MAT),
   xlab = "Precipitation",
   ylab = "Mean Annual Temperature",
   main = "Sierra Nevada")
   
plot(hexbin(coast.800$Precip, coast.800$MAT),
   xlab = "Precipitation",
   ylab = "Mean Annual Temperature",
   main = "Coast Range")


plot(hexbin(coast.800$Precip, coast.800$JuMax),
   xlab = "Precipitation",
   ylab = "Max July Temperature",
   main = "Coast Range")
   
plot(hexbin(sierra.800$Precip, sierra.800$JuMax),
   xlab = "Precipitation",
   ylab = "Max July Temperature",
   main = "Sierra Nevada")
   

# 7.12
library(maptools)
library(spatstat)
library(sf)
sampbdry.sf <- st_read("created\\Set42sampbdry.shp")
sampbdry.sp <- as(sampbdry.sf, "Spatial")
# a) Random sample
set.seed(123)
spsamp.pts <- spsample(sampbdry.sp, 32, type = "random")
spsamp.ppp <- as.ppp(spsamp.pts)
plot(spsamp.ppp)
plot(envelope(spsamp.ppp, Kest))

#b) Grid sampling
pop.sf <- st_read("Created\\Set42pop.shp")
pop.data <- as(pop.sf, "Spatial")
W <- bbox(pop.data)[1,1]
E <- bbox(pop.data)[1,2]
S <- bbox(pop.data)[2,1]
N <- bbox(pop.data)[2,2]
N <- N + 2.5
S <- S - 2.5
E <- E + 2.5
W <- W - 2.5
nrows <- 4
ncols <- 8
grid.size <- (E - W) / ncols
grid.offset <- 0.5 * grid.size
spsamp.pts <- expand.grid(x = seq(W+grid.offset,E,grid.size),
                          y = seq(N - grid.offset,S,-grid.size))
spsamp.ppp <- as.ppp(spsamp.pts, owin(c(W,E), c(S,N)))
plot(spsamp.ppp)
plot(envelope(spsamp.ppp, Kest))

# c) Cluster sample
nrows <- 4
ncols <- 8
grid.size <- (E - W) / ncols
grid.offset <- 0.5 * grid.size
cluster.size <- 10
n <- 0
for (i in seq(W + grid.offset,E,grid.size))
  for (j in seq(N - grid.offset,S,-grid.size)){
    spsamp.clst <- expand.grid(x = seq(i-cluster.size, i+cluster.size, cluster.size),
                               y = seq(j-cluster.size, j+cluster.size, cluster.size))
    if (n == 0)
      spsamp.pts <- spsamp.clst
    else
      spsamp.pts <- rbind(spsamp.pts, spsamp.clst)
    n <- n + 1
  }
spsamp.ppp <- as.ppp(spsamp.pts, owin(c(W,E), c(S,N)))
plot(spsamp.ppp)
plot(envelope(spsamp.ppp, Kest))



# 7.13
xyplot(Yield ~ Silt | FieldFac, data = data.Set3,
   main = "Yield vs. Silt by Field")
xyplot(Yield ~ pH | FieldFac, data = data.Set3,
   main = "Yield vs. pH by Field")
xyplot(Yield ~ Clay | Field, data = data.Set3)
xyplot(Yield ~ Silt | Field, data = data.Set3)
xyplot(Yield ~ Irrig | Field, data = data.Set3)
xyplot(Yield ~ Weeds | Field, data = data.Set3)
bwplot(Clay ~ Location | YearFac, data = data.Set3)
xyplot(Yield ~ Fert | Location + YearFac, data = data.Set3)
xyplot(Clay ~ Fert | Location + YearFac, data = data.Set3)
xyplot(Yield ~ Clay | Location + YearFac, data = data.Set3)

   
# 7.14
xyplot(Yield ~ Cont | FieldFac, data = data.Set3,
   main = "Yield vs. Weed Control by Field")
xyplot(Yield ~ Irrig | FieldFac, data = data.Set3,
   main = "Yield vs. Irrigation by Field")
xyplot(Yield ~ Fert | FieldFac, data = data.Set3,
   main = "Yield vs. Fertilization by Field")
   
# 7.15
# Compute C:N ratio
CNRatio <- data.Set4.1$SoilTOC / data.Set4.1$SoilTN
max(CNRatio)
median(CNRatio)
hist(CNRatio)
data.Set4.1@data$CNRatio <- CNRatio
greys <- grey(255:150 / 255)
spplot(data.Set4.1, "CNRatio", col.regions = greys,  scales = list(draw = TRUE),
  xlab = "Easting", ylab = "Northing", main = "C:N Ratio") 
  
# 7.16
# a
library(rgdal)
library(maptools)
library(sf)
data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
Set4.1.spdf <- data.Set4.1
coordinates(Set4.1.spdf) <- c("Easting", "Northing")
data.4.1.Dec <- readGDAL("set4\\set4.11295.tif")
IR.samp <- over(Set4.1.spdf, data.4.1.Dec)
IR.samp$Clay <- data.Set4.1$Clay
IRclay.lm <- lm(Clay ~ band1, data = IR.samp)
par(mai = c(1,1,1,1))
plot(IR.samp$band1, IR.samp$Clay,  
   main = "Percent Clay vs. December IR", cex.main = 2,
   ylab = "Percent Clay", xlab = "Dec IR Digital Number", cex.lab = 1.5)
abline(IRclay.lm)

img.df <- data.frame(IR = slot(data.4.1.Dec, "data")$band1)
img.df$ClayPred <- predict(IRclay.lm, data.frame(band1 = img.df$IR))
img.df$Clay30 <- factor(img.df$ClayPred <= 30,
  labels = c("Greater than 30%", "Less than 30%"))
img.coords <- coordinates(data.4.1.Dec)
img.df$x <- img.coords[,1]
img.df$y <- img.coords[,2]
coordinates(img.df) <- c("x", "y")
bdry.sf <- st_read("created\\set419697bdry.shp")
bdry.spdf <- as(bdry.sf, "Spatial")
proj4string(bdry.spdf) <- proj4string(img.df)
in.f <- over(img.df,bdry.spdf)
f.img <- img.df[which(!is.na(in.f)),]
spplot(f.img, zcol = "Clay30", 
   main = "Field 4.1 Clay Categories Based on Dec IR",
   key.space = "right")
   
# 7.17
# a) Star plot of weeds, disease, and yield
data.yield <- read.csv("created\\set4.1yld96ptsidw.csv", header = TRUE)
star.data <- with(data.Set4.1, data.frame(Weeds,
   Disease, Yield = data.yield$Yield))
star.loc <- 2* cbind(data.Set4.1$Column, 13 - data.Set4.1$Row)
stars(star.data, locations = star.loc,
  labels = NULL, key.loc = c(18,2),
  main = "Weed and Disease Star Plot", cex.main = 2) # Fig. 7.24c

# b) Star plot of N, K, and yield
star.data <- with(data.Set4.1, data.frame(SoilK, FLN,
   SoilTN, Yield = data.yield$Yield))
star.loc <- 2* cbind(data.Set4.1$Column, 13 - data.Set4.1$Row)
stars(star.data, locations = star.loc,
  labels = NULL, key.loc = c(18,2),
  main = "Soil K and Plant N Star Plot", cex.main = 2) # Fig. 7.24d

# 7.18
data.Set4.2 <- read.csv("set4\\set4.296sample.csv", header = TRUE)
data.yield41 <- read.csv("created\\set4.1yld96ptsidw.csv", header = TRUE)
data.yield42 <- read.csv("created\\set4.2yld96ptsidw.csv", header = TRUE)
data.Set4.1$Yield <- data.yield41$Yield
data.Set4.2$Yield <- data.yield42$Yield
stem(data.Set4.1$Clay)
stem(data.Set4.2$Clay)
with(data.Set4.1, plot(Clay, Yield))
with(data.Set4.2, points(Clay, Yield, pch = 16))

# 7.19
library(lattice)
agron.data <- subset(data.Set4.2,
   select = c(LeafN, SoilP, SoilTN, Weeds, Disease, Yield))
splom(agron.data, par.settings = list(fontsize=list(text=9),
  plot.symbol = list(col = "black")), pscales = 0,
  main = "Agronomic Relationships")
set4.2 <- data.Set4.2
coordinates(set4.2) <- c("Easting", "Northing")
plot(set4.2, pch = set4.2$Weeds)
plot(set4.2, pch = round(set4.2$Yield/max(set4.2$Yield)))

# 7.20
apply(with(data.Set4.2, cbind(LeafN, SoilP)), 2, stem)
apply(with(data.Set4.2, cbind(LeafN, SoilP)), 2, min)


# 7.21  
# a Star plot of response and endogenous variable
data.Set4.1$GrnMoist <- data.yield41$GrnMoist
data.Set4.2$GrnMoist <- data.yield42$GrnMoist
star.data <- with(data.Set4.2, data.frame(GrnMoist,
   LeafN, Yield, GrnMoist))
data.Set4.2$Row <- sort(rep(1:6, 13))
data.Set4.2$Column <- rep(1:13, 6)
star.loc <- 2* cbind(data.Set4.2$Column, 6 - data.Set4.2$Row)
# Winsorize outlier in GrnMoist
MoistID1 <- which (star.data$GrnMoist == max(star.data$GrnMoist))
MoistID2 <- which (star.data$GrnMoist ==
   max(star.data$GrnMoist[-MoistID1]))
star.data$GrnMoist[MoistID1] <- star.data$GrnMoist[MoistID2]
stars(star.data, locations = star.loc,
  labels = NULL, key.loc = c(18,-4),
  main = "Response Variable Star Plot", cex.main = 2) # Fig. 7.24b
  
# b Star plot of weeds, disease, and yield
star.data <- with(data.Set4.2, data.frame(Weeds,
   Disease, Yield = Yield))
stars(star.data, locations = star.loc,
  labels = NULL, key.loc = c(18,-4),
  main = "Weed and Disease Star Plot", cex.main = 2)


