library(maptools)
library(rgdal)
library(sf)

##### Data Setup from Section 7.2
data.Set1.obs <- read.csv("set1\\obspts.csv", header = TRUE)
coordinates(data.Set1.obs) <- c("Easting", "Northing")
proj4string(data.Set1.obs) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
data.Set1.sf <- st_read("set1\\set1data.shp")
data.Set1 <- as(data.Set1.sf, "Spatial")
proj4string(data.Set1) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
Set1.obs <- over(data.Set1.obs,data.Set1)
Set1.obs$PresAbs <- data.Set1.obs$PresAbs
Set1.obs$obsID <- data.Set1.obs$ID
Set1.obs$Abund <- data.Set1.obs$Abund
Set1.obs$Easting <- coordinates(data.Set1.obs)[,1]
Set1.obs$Northing <- coordinates(data.Set1.obs)[,2]

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

# Create the Medium/Low data frame with ID data field
MLArea <- data.frame(PatchID = data.Set1$PatchID)
# Initially set ML area of each patch to 0
MLArea$Area <- 0
# Insert the patch area if the height class is m or l
for (i in 1:nrow(MLArea)){
  {if ((data.Set1$HtClass[i] == "m") | (data.Set1$HtClass[i] == "l"))
      MLArea$Area[i] <- data.Set1$HtClArea[i] }}

# Repeat the process for the high height class
HArea <- data.frame(PatchID = data.Set1$PatchID)
HArea$Area <- 0
for (i in 1:nrow(HArea)){
  {if ((data.Set1$HtClass[i] == "h"))
      HArea$Area[i] <- data.Set1$HtClArea[i] }}

# Carry out the computation using AreaRatio()
Ratio <- AreaRatio(HArea, MLArea)
HtRatio.df <- data.frame(PatchID = Ratio[,1], HtRatio = Ratio[,2])
Set1.obs1 <- merge(x = Set1.obs, y = HtRatio.df,
  by.x = "PatchID", by.y = "PatchID")

# AgeRatio 
OldArea <- data.frame(PatchID = data.Set1$PatchID)
OldArea$Area <- 0
for (i in 1:nrow(OldArea)){
  {if (data.Set1$Age[i] >= 60)
      OldArea$Area[i] <- data.Set1$AgeArea[i] }}
YoungArea <- data.frame(PatchID = data.Set1$PatchID)
YoungArea$Area <- 0
for (i in 1:nrow(OldArea)){
  {if (data.Set1$Age[i] < 60)
      YoungArea$Area[i] <- data.Set1$AgeArea[i] }}
Ratio <- AreaRatio(YoungArea, OldArea)
AgeRatio.df <- data.frame(PatchID = Ratio[,1], AgeRatio = Ratio[,2])

Set1.obs2 <- merge(x = Set1.obs1, y = AgeRatio.df,
  by.x = "PatchID", by.y = "PatchID")

# Repeat the procedure for cover class
DenseArea <- data.frame(PatchID = data.Set1$PatchID)
DenseArea$Area <- 0
for (i in 1:nrow(DenseArea)){
  {if (data.Set1$CoverClass[i] == "d")
      DenseArea$Area[i] <- data.Set1$HtClArea[i] }}
SparseArea <- data.frame(PatchID = data.Set1$PatchID)
SparseArea$Area <- 0
for (i in 1:nrow(SparseArea)){
  {if (data.Set1$CoverClass[i] != "d")
      SparseArea$Area[i] <- data.Set1$HtClArea[i] }}
Ratio <- AreaRatio(DenseArea, SparseArea)
CoverRatio.df <- data.frame(PatchID = Ratio[,1], CoverRatio = Ratio[,2])
Set1.obs3 <- merge(x = Set1.obs2, y = CoverRatio.df,
  by.x = "PatchID", by.y = "PatchID")
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

# Age suitability score
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
Set1.obs3$AgeScore <- AgeScore
Set1.obs3$HeightScore <- HeightScore
scores <- with(Set1.obs3, cbind(AreaScore, WidthScore, AgeScore, HeightScore))
Set1.obs3$HabitatScore <- apply(scores[,1:4], 1, prod)^(1/4)
Set1.obs3$HSIPred <- as.numeric(Set1.obs3$HabitatScore > 0)

#Disqualify obs point 1
Set1.corrected <- Set1.obs3[-which(Set1.obs3$PatchID == 191),]
######################
## End of setup

Set1.corrected$HSIPred
Set1.corrected$PresAbs
UA <- with(Set1.corrected, which(HSIPred == 0 & PresAbs == 0))
UP <- with(Set1.corrected, which(HSIPred == 0 & PresAbs == 1))
SA <- with(Set1.corrected, which(HSIPred == 1 & PresAbs == 0))
SP <- with(Set1.corrected, which(HSIPred == 1 & PresAbs == 1))
print(cont.table <- matrix(c(length(SP),length(SA),
   length(UP),length(UA)), nrow = 2, byrow = TRUE,
   dimnames = list(c("Suit.", "Unsuit."),c("Pres.", "Abs."))))

chisq.test(cont.table, simulate.p.value = TRUE)
chisq.test(cont.table, simulate.p.value = FALSE)
fisher.test(cont.table)

Set1.corrected$Symbol <- 1
Set1.corrected$Symbol[which(Set1.corrected$HabitatScore == 0
 & Set1.corrected$PresAbs == 1)] <- 10
Set1.corrected$Symbol[which(Set1.corrected$HabitatScore > 0
 & Set1.corrected$PresAbs == 0)] <- 12
Set1.corrected$Symbol[which(Set1.corrected$HabitatScore > 0
 & Set1.corrected$PresAbs == 1)] <- 16
coordinates(Set1.corrected) <- c("Easting", "Northing")
proj4string(Set1.corrected) <- CRS("+proj=utm +zone=10 +ellps=WGS84")

# Color version
Set1.corrected$Color <- "red"
Set1.corrected$Color[which(Set1.corrected$HabitatScore == 0
 & Set1.corrected$PresAbs == 1)] <- "blue"
Set1.corrected$Color[which(Set1.corrected$HabitatScore > 0
 & Set1.corrected$PresAbs == 0)] <- "green"
Set1.corrected$Color[which(Set1.corrected$HabitatScore > 0
 & Set1.corrected$PresAbs == 1)] <- "purple"

plot(Set1.corrected, axes = TRUE, col = Set1.corrected$Color,
   pch = 16) # Fig. 11.4
title(main = "Data Set 1 Groups",
   xlab = "Easting", ylab = "Northing",
   cex.lab = 1.5, cex.main = 2)
text(coordinates(Set1.corrected)[,1] + 500, coordinates(Set1.corrected)[,2],
  labels=as.character(Set1.corrected$obsID), cex = 0.8)
lines(c(577300,579500), c(4417300,4417300))
lines(c(577300,579500), c(4419000,4419000))
lines(c(577300,577300), c(4417300,4419000))
lines(c(579500,579500), c(4417300,4419000))

lines(c(579000,583000), c(4415700,4415700))
lines(c(579000,583000), c(4413000,4413000))
lines(c(579000,579000), c(4413000,4415700))
lines(c(583000,583000), c(4413000,4415700))

lines(c(578500,582500), c(4412700,4412700))
lines(c(578500,582500), c(4408500,4408500))
lines(c(578500,578500), c(4408500,4412700))
lines(c(582500,582500), c(4408500,4412700))

lines(c(579500,585000), c(4404000,4404000))
lines(c(579500,585000), c(4408300,4408300))
lines(c(579500,579500), c(4404000,4408300))
lines(c(585000,585000), c(4404000,4408300))

lines(c(582500,589200), c(4403000,4403000))
lines(c(582500,589200), c(4398300,4398300))
lines(c(582500,582500), c(4403000,4398300))
lines(c(589200,589200), c(4403000,4398300))

legend(585000, 4415000, c("Unsuit. - Abs.", "Unsuit - Pres.",
  "Suit. - Abs.", "Suit. - Pres."), pch = 16,
  col = c("red", "blue", "green", "purple"))

# Create the blocks of observation points as in Fig. 11.4
obs.block1 <- Set1.corrected$PresAbs[1:2]
obs.block2 <- Set1.corrected$PresAbs[3:5]
obs.block3 <- Set1.corrected$PresAbs[6:11]
obs.block4 <- Set1.corrected$PresAbs[12:17]
obs.block5 <- Set1.corrected$PresAbs[18:20]


sample.blocks<- function(){
   s1 <- sample(obs.block1)
   s2 <- sample(obs.block2)
   s3 <- sample(obs.block3)
   s4 <- sample(obs.block4)
   s5 <- sample(obs.block5)
   c(s1,s2,s3,s4,s5)
}

calc.fisher <- function(){
   PA <- sample.blocks()
   UA <- sum(Set1.corrected$HSIPred == 0 & PA == 0)
   UP <- sum(Set1.corrected$HSIPred == 0 & PA == 1)
   SA <- sum(Set1.corrected$HSIPred == 1 & PA == 0)
   SP <- sum(Set1.corrected$HSIPred == 1 & PA == 1)
   n <- matrix(c(SP, SA, UP, UA), nrow = 2, byrow = TRUE)
   odds.ratio <- fisher.test(n)$estimate
}

obs.test <- fisher.test(cont.table)
print(obs.stat <- obs.test$estimate)

set.seed(123)
u <- replicate(1999,calc.fisher())
print(p <- sum(u >= obs.stat - 0.001) / 2000)
