# Creation of Fig. 7.2
library(maptools)
library(sf)
library(ggplot2)

# Read in the files and set the projections
data.Set1.patches.sf <- st_read("set1\\habitatpatches.shp")
st_crs(data.Set1.patches.sf) <- "+proj=utm +zone=10 +ellps=WGS84"
data.Set1.patches.sp <- as(data.Set1.patches.sf, "Spatial")
data.Set1.obs <- read.csv("set1\\obspts.csv", header = TRUE)
coordinates(data.Set1.obs) <- c("Easting", "Northing")
proj4string(data.Set1.obs) <- CRS("+proj=utm +zone=10 +ellps=WGS84 +units=m +no_defs")
data.Set1.sf <- st_read("set1\\set1data.shp")
st_crs(data.Set1.sf) <- "+proj=utm +zone=10 +ellps=WGS84"
data.Set1.sp <- as(data.Set1.sf, "Spatial")


# For explotatory purposes use the sf plot function
plot(data.Set1.patches.sf)

# To draw figures for the book use the sp plot function
par(mai = c(1,1,1,1))
plot(data.Set1.patches.sp, axes = TRUE, # Fig. 7.2a
   xlim = c(577000,578500), ylim = c(4417500,4419400))
title(xlab = "Easting", ylab = "Northing", cex.lab = 1.5)
points(data.Set1.obs, pch = 19, cex = 2)
text(coordinates(data.Set1.obs)[,1] + 100, coordinates(data.Set1.obs)[,2],
  labels=as.character(data.Set1.obs$Abund), cex = 2, font = 2)
y <- lapply(data.Set1.patches.sp@polygons, slot, "labpt")
patches.loc <- matrix(0, length(y), 2)
for (i in 1:length(y)) patches.loc[i,] <- unlist(y[[i]])
text(patches.loc,
  labels=as.character(data.Set1.patches.sp$VegType), cex = 2, font = 2)


plot(data.Set1.patches.sp, axes = TRUE, # Fig. 7.2b
   xlim = c(578385,583080), ylim = c(4408875,4415552))
title(xlab = "Easting", ylab = "Northing", cex.lab = 1.5)
points(data.Set1.obs, pch = 19, cex = 2)
text(coordinates(data.Set1.obs)[,1] + 250, coordinates(data.Set1.obs)[,2],
  labels=as.character(data.Set1.obs$Abund), cex = 2, font = 2)

plot(data.Set1.patches.sp, axes = TRUE,   # Fig. 7.2c
   xlim = c(578385,584780), ylim = c(4401344,4408900))
title(xlab = "Easting", ylab = "Northing", cex.lab = 1.5)
points(data.Set1.obs, pch = 19, cex = 2)
text(coordinates(data.Set1.obs)[,1] + 250, coordinates(data.Set1.obs)[,2],
  labels=as.character(data.Set1.obs$Abund), cex = 2, font = 2)

plot(data.Set1.patches.sp, axes = TRUE,  # Fig. 7.2d
   xlim = c(581971,589940), ylim = c(4400000, 4401344))
title(xlab = "Easting", ylab = "Northing", cex.lab = 1.5)
points(data.Set1.obs, pch = 19, cex = 2)
text(coordinates(data.Set1.obs)[,1] + 500, coordinates(data.Set1.obs)[,2],
  labels=as.character(data.Set1.obs$Abund), cex = 2, font = 2)

# Creation of Fig. 7.3
levels(data.Set1.sp$HtClass)
data.Set1.sp@data$HtClass2 <-
   ordered(as.character(data.Set1.sp@data$HtClass),
   levels = c("0", "l", "m", "h"),
   labels = c("No data", "Low", "Medium", "High"))
greys <- grey(c(250, 100, 150, 200) / 255)
obs.list = list("sp.points", data.Set1.obs, pch = 19, col = "black",
   cex = 2)
map.plot <- spplot(data.Set1.sp, "HtClass2", col.regions = greys, # Fig. 7.3a
   scales = list(draw = TRUE), xlab = "Easting", ylab = "Northing",
   main = "Vegetation Height Class", sp.layout = list(obs.list),
   xlim = c(577000,578500), ylim = c(4417500,4418800))
names(map.plot$legend) <- "inside"
map.plot$legend$inside$x <- 0.05
map.plot$legend$inside$y <- 0.15
print(map.plot)

# Color version
color <- topo.colors(n = 4)  
spplot(data.Set1.sp, "HtClass2", col.regions = color, 
   scales = list(draw = TRUE), xlab = "Easting", ylab = "Northing",
   main = "Vegetation Height Class", sp.layout = list(obs.list),
   xlim = c(577000,578500), ylim = c(4417500,4418800))
   
levels(data.Set1.sp$CoverClass)
data.Set1.sp@data$CoverClass2 <-
   ordered(as.character(slot(data.Set1.sp, "data")$CoverClass),
   levels = c("0", "s", "p", "m", "d"),
   labels = c("No data", "10-24%", "24-39%", "40-59%", "60-100%"))
greys <- grey(c(250, 50, 100, 150, 175) / 255)
map.plot <- spplot(data.Set1.sp, "CoverClass2", col.regions = greys, # Fig. 7.3b
   scales = list(draw = TRUE), xlab = "Easting", ylab = "Northing",
   main = "Vegetation Cover Class", sp.layout = list(obs.list),
   xlim = c(577000,578500), ylim = c(4417500,4418800))
names(map.plot$legend) <- "inside"
map.plot$legend$inside$x <- 0.05
map.plot$legend$inside$y <- 0.15
print(map.plot)  # Fig. 7.3b 

spplot(data.Set1.sp, "CoverClass2",   # Fig. 7.3b in native color
   scales = list(draw = TRUE), xlab = "Easting", ylab = "Northing",
   main = "Vegetation Cover Class", sp.layout = list(obs.list),
   xlim = c(577000,578500), ylim = c(4417500,4418800))


data.Set1.sp@data$AgeLT60 <- as.factor(slot(data.Set1.sp,"data")$Age < 60)
greys <- grey(c(175, 50) / 255)
map.plot <- spplot(data.Set1.sp, "AgeLT60", col.regions = greys, # Fig. 7.3c
   scales = list(draw = TRUE), xlab = "Easting", ylab = "Northing",
   main = "Age Less Than 60 Years", sp.layout = list(obs.list),
   xlim = c(577000,578500), ylim = c(4417500,4418800))
names(map.plot$legend) <- "inside"
map.plot$legend$inside$x <- 0.05
map.plot$legend$inside$y <- 0.15
print(map.plot)

spplot(data.Set1.sp, "AgeLT60", # Fig. 7.3c in color
   scales = list(draw = TRUE), xlab = "Easting", ylab = "Northing",
   main = "Age Less Than 60 Years", sp.layout = list(obs.list),
   xlim = c(577000,578500), ylim = c(4417500,4418800))


# Computations to generate the data set used in the exploratory analysis

# Eliminate patches in which no observation point exists
Set1.obs <- over(data.Set1.obs, data.Set1.sp)
Set1.obs$PresAbs <- data.Set1.obs$PresAbs
Set1.obs$obsID <- data.Set1.obs$ID
Set1.obs$Abund <- data.Set1.obs$Abund
Set1.obs$Easting <- coordinates(data.Set1.obs)[,1]
Set1.obs$Northing <- coordinates(data.Set1.obs)[,2]
names(Set1.obs)  # Polygons containing an obs point
# Display some of the data
with(Set1.obs, cbind(PatchID, PatchArea, PatchWidth, obsID, PresAbs))


# Create the Medium/Low data frame with ID data field
MLArea <- data.frame(PatchID = data.Set1.sf$PatchID)
# Initially set ML area of each patch to 0
MLArea$Area <- 0
# Insert the patch area if the height class is m or l
for (i in 1:nrow(MLArea)){
  {if ((data.Set1.sf$HtClass[i] == "m") |
      (data.Set1.sf$HtClass[i] == "l"))
      MLArea$Area[i] <- data.Set1.sf$HtClArea[i] }}

# Repeat the process for the high height class
HArea <- data.frame(PatchID = data.Set1.sf$PatchID)
HArea$Area <- 0
for (i in 1:nrow(HArea)){
  {if ((data.Set1.sf$HtClass[i] == "h"))
      HArea$Area[i] <- data.Set1.sf$HtClArea[i] }}

AreaRatio <- function(num, denom){
# Add the num argument values by PatchID
   patch.num <- aggregate(num,
      by = list(AggID = num$PatchID), FUN = sum)
# Add the denom argument values by PatchID
   patch.denom <- aggregate(denom,
      by = list(AggID = denom$PatchID), FUN = sum)
# If the num and denom PatchID values are not equal, return NULL
   ratio <- NULL
# Otherwise, compute the ratio (and avoid 0/0)
   if (all.equal(patch.num$PatchID, patch.denom$PatchID)){
      ratio <- patch.num$Area /
        (patch.num$Area + patch.denom$Area + 0.000001)}
   return(cbind(patch.num$AggID,ratio))
}

# Carry out the computation using AreaRatio()
Ratio <- AreaRatio(HArea, MLArea)
HtRatio.df <- data.frame(PatchID = Ratio[,1], HtRatio = Ratio[,2])
Set1.obs1 <- merge(x = Set1.obs, y = HtRatio.df,
  by.x = "PatchID", by.y = "PatchID")

# AgeRatio is computed in Exercise 7.3
# In order to continue, you must do this exercise
Set1.obs2 <- merge(x = Set1.obs1, y = AgeRatio.df,
  by.x = "PatchID", by.y = "PatchID")


# Repeat the procedure for cover class
DenseArea <- data.frame(PatchID = data.Set1.sf$PatchID)
DenseArea$Area <- 0
for (i in 1:nrow(DenseArea)){
  {if (data.Set1.sf$CoverClass[i] == "d")
      DenseArea$Area[i] <- data.Set1.sf$HtClArea[i] }}
SparseArea <- data.frame(PatchID = data.Set1.sf$PatchID)
SparseArea$Area <- 0
for (i in 1:nrow(SparseArea)){
  {if (data.Set1.sf$CoverClass[i] != "d")
      SparseArea$Area[i] <- data.Set1.sf$HtClArea[i] }}
Ratio <- AreaRatio(DenseArea, SparseArea)
CoverRatio.df <- data.frame(PatchID = Ratio[,1], CoverRatio = Ratio[,2])
Set1.obs3 <- merge(x = Set1.obs2, y = CoverRatio.df,
  by.x = "PatchID", by.y = "PatchID")

# Example of the use of ggplot()
ggplot(data = Set1.obs3) +
   geom_boxplot(mapping = aes(as.factor(PresAbs), PatchArea))

with(Set1.obs3, boxplot(PatchArea ~ PresAbs)) # Fig. 7.4a
title(main = "Patch Area", cex.main = 2,
  xlab = "Presence/Absence",
  ylab = expression(Area~"("*m^2*")"), cex.lab = 1.5)
with(Set1.obs3, boxplot(PatchWidth ~ PresAbs)) # Fig. 7.4b
title(main = "Patch Width", cex.main = 2,
  xlab = "Presence/Absence", ylab = "Patch Width (m)", cex.lab = 1.5)
with(Set1.obs3, boxplot(HtRatio ~ PresAbs)) # Fig. 7.4c
title(main = "Height Ratio", cex.main = 2,
  xlab = "Presence/Absence", ylab = "Height Ratio", cex.lab = 1.5)
with(Set1.obs3, boxplot(AgeRatio ~ PresAbs)) # Fig. 7.4d
title(main = "Age Ratio", cex.main = 2,
  xlab = "Presence/Absence", ylab = "Age Ratio", cex.lab = 1.5)
with(Set1.obs3, boxplot(CoverRatio ~ PresAbs)) # Fig. 7.4e
title(main = "Cover Ratio", cex.main = 2,
  xlab = "Presence/Absence", ylab = "Cover Ratio", cex.lab = 1.5)

# Aligned dot plots of the presence and absence
row.names(Set1.obs3) <- as.character(1:nrow(Set1.obs3))
obs.trans <- data.frame(t(scale(with(Set1.obs3,
   cbind(HtRatio, AgeRatio, CoverRatio, PatchArea, PatchWidth)))))
n.trans <- nrow(obs.trans)
obs.trans[n.trans + 1,] <- t(Set1.obs3$PresAbs)
obs.YPres <- obs.trans[1:n.trans,(obs.trans[n.trans+1,] == 1)]
obs.YAbs <- obs.trans[1:n.trans,(obs.trans[n.trans+1,] == 0)]
matplot(obs.YPres, type = "o", col = "black", xlim = c(0.8,5),
   ylim = c(-3,3), main = "Birds Present", ylab = "",
   cex.main = 2, xaxt = "n") # Fig. 7.5a
text(1.1,-2.5, "Height", font = 2, cex = 1.5)
text(1.1,-2.75, "Ratio", font = 2, cex = 1.5)
text(2,-2.5, "Age", font = 2, cex = 1.5)
text(2,-2.75, "Ratio", font = 2, cex = 1.5)
text(3,-2.5, "Cover", font = 2, cex = 1.5)
text(3,-2.75, "Ratio", font = 2, cex = 1.5)
text(4,-2.5, "Patch", font = 2, cex = 1.5)
text(4,-2.75, "Width", font = 2, cex = 1.5)
text(4.8,-2.5, "Patch", font = 2, cex = 1.5)
text(4.8,-2.75, "Area", font = 2, cex = 1.5)

matplot(obs.YAbs, type = "o", col = "black", xlim = c(0.8,5),
   ylim = c(-3,3), main = "Birds Absent", ylab = "",
   cex.main = 2, xaxt = "n") # Fig. 7.5b
text(1.1,-2.5, "Height", font = 2, cex = 1.5)
text(1.1,-2.75, "Ratio", font = 2, cex = 1.5)
text(2,-2.5, "Age", font = 2, cex = 1.5)
text(2,-2.75, "Ratio", font = 2, cex = 1.5)
text(3,-2.5, "Cover", font = 2, cex = 1.5)
text(3,-2.75, "Ratio", font = 2, cex = 1.5)
text(4,-2.5, "Patch", font = 2, cex = 1.5)
text(4,-2.75, "Width", font = 2, cex = 1.5)
text(4.8,-2.5, "Patch", font = 2, cex = 1.5)
text(4.8,-2.75, "Area", font = 2, cex = 1.5)

with(Set1.obs3, (cor(cbind(HtRatio, AgeRatio,
   CoverRatio, PatchArea, PatchWidth))))
# Compute suitabiliy scores for all the X variable
# Area and width scores can be computed with one function
suitability.score <- function(x, cat.val){
   score <- 0
   if(x >= cat.val[3]) score <- 1
   if(x < cat.val[3] & x >= cat.val[2]) score <- 0.66
   if(x < cat.val[2] & x >= cat.val[1]) score <- 0.33
   return(score)
}

AreaScore <- numeric(nrow(Set1.obs3))
for (i in 1:nrow(Set1.obs3)){
 AreaScore[i] <-
    suitability.score(Set1.obs3$PatchArea[i] / 10000, c(17,40,80))
}
Set1.obs3$AreaScore <- AreaScore

WidthScore <- numeric(nrow(Set1.obs3))
for (i in 1:nrow(Set1.obs3)){
 WidthScore[i] <-
    suitability.score(Set1.obs3$PatchWidth[i], c(100,200,600))
}
Set1.obs3$WidthScore <- WidthScore

# AgeScore and HeightScore are computed in Exercise 7.4
# In order to continue, you must do this exercise
Set1.obs3$AgeScore <- AgeScore
Set1.obs3$HeightScore <- HeightScore

plot(1,1,type = "n", xlim = c(0,1), ylim = c(0,1), # Fig. 7.6
   xlab = "Age Ratio", ylab = "Suitability Score", cex.lab = 1.5)
lines(c(0, 0.375), c(0,0), lwd = 2)
lines(c(0.375, 0.375), c(0, 0.33), lwd = 2)
lines(c(0.375, 0.5), c(0.33, 0.33), lwd = 2)
lines(c(0.5, 0.5), c(0.33, 0.67), lwd = 2)
lines(c(0.5, 0.67), c(0.67, 0.67), lwd = 2)
lines(c(0.67, 0.67), c(0.67, 1.0), lwd = 2)
lines(c(0.67, 0.8), c(1.0, 1.0), lwd = 2)
lines(c(0.8, 0.8), c(1.0, 0.67), lwd = 2)
lines(c(0.8, 0.875), c(0.67, 0.67), lwd = 2)
lines(c(0.875, 0.875), c(0.67, 0.33), lwd = 2)
lines(c(0.875, 1.0), c(0.33, 0.33), lwd = 2)
title(main = "Floodplain Age Ratio Suitability Score", cex.main = 1.5)


scores <- with(Set1.obs3, cbind(AreaScore, WidthScore,
  AgeScore, HeightScore))
print(Set1.obs3$HabitatScore <- apply(scores[,1:4], 1, prod)^(1/4),
  digits = 2)
print(Set1.obs3$HSIPred <- as.numeric(Set1.obs3$HabitatScore > 0))
#Disqualify obs point 1
Set1.corrected <- Set1.obs3[-which(Set1.obs3$PatchID == 191),]

UA <- with(Set1.corrected, which(HSIPred == 0 & PresAbs == 0))
UP <- with(Set1.corrected, which(HSIPred == 0 & PresAbs == 1))
SA <- with(Set1.corrected, which(HSIPred == 1 & PresAbs == 0))
SP <- with(Set1.corrected, which(HSIPred == 1 & PresAbs == 1))
print(cont.table <- matrix(c(length(SP),length(SA),
   length(UP),length(UA)), nrow = 2, byrow = TRUE,
   dimnames = list(c("Suit.", "Unsuit."),c("Pres.", "Abs."))))

scores.corr <- with(Set1.corrected, cbind(AreaScore,
   WidthScore, AgeScore, HeightScore))
print(HSItest <- as.numeric(apply(scores.corr[,1:4], 1, prod) > 0))
PAtest <- Set1.corrected$PresAbs

# This is the full set of variables
UA1 <- which(HSItest == 0 & PAtest == 0)
UP1 <- which(HSItest == 0 & PAtest == 1)
SA1 <- which(HSItest > 0 & PAtest == 0)
SP1 <- which(HSItest > 0 & PAtest == 1)
print(cont.table <- matrix(c(length(SP1),length(SA1),
   length(UP1),length(UA1)), nrow = 2, byrow = TRUE,
   dimnames = list(c("Suit.", "Unsuit."),c("Pres.", "Abs."))))
