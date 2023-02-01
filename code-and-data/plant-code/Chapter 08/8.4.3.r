library(maptools)
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

Set1.norm1 <- with(Set1.corrected, data.frame(PresAbs = PresAbs,
     PatchArea = scale(PatchArea), PatchWidth = scale(PatchWidth),
     HtRatio = HtRatio, AgeRatio = AgeRatio, AgeScore = AgeScore,
     obsID = obsID))

Set1.norm1$Display <- 1
Set1.norm1$Display[which(Set1.corrected$HSIPred == 0
  & Set1.corrected$PresAbs == 1)] <- 10
Set1.norm1$Display[which(Set1.corrected$HSIPred == 1
  & Set1.corrected$PresAbs == 0)] <- 12
Set1.norm1$Display[which(Set1.corrected$HSIPred == 1
  & Set1.corrected$PresAbs == 1)] <- 16

par(mai = c(1,1,1,1))
with(Set1.norm1, plot(PatchArea, PatchWidth, # Fig. 8.11
   pch = Display, xlab = "Scaled Patch Area",
   ylab = "Scaled Patch Width", cex.lab = 1.5))
with(Set1.norm1, text(PatchArea + 0.1,
     PatchWidth - 0.07,
     labels=as.character(obsID), cex = 1))
legend(1, 0, c("Unsuit. - Abs.", "Unsuit - Pres.",
  "Suit. - Abs.", "Suit. - Pres."), pch = c(1,10,12,16))

# Color version
Set1.norm1$Color <- "red"
Set1.norm1$Color[which(Set1.corrected$HSIPred == 0
  & Set1.corrected$PresAbs == 1)] <- "dark green"
Set1.norm1$Color[which(Set1.corrected$HSIPred == 1
  & Set1.corrected$PresAbs == 0)] <- "green"
Set1.norm1$Color[which(Set1.corrected$HSIPred == 1
  & Set1.corrected$PresAbs == 1)] <- "blue"
  
with(Set1.norm1, plot(PatchArea, PatchWidth, pch = 16, 
   col = Color, xlab = "Scaled Patch Area",
   ylab = "Scaled Patch Width", cex.lab = 1.5))
with(Set1.norm1, text(PatchArea + 0.1,
     PatchWidth - 0.07,
     labels=as.character(obsID), cex = 0.8))
legend(1, 0, c("Unsuit. - Abs.", "Unsuit - Pres.",
  "Suit. - Abs.", "Suit. - Pres."), pch = 16, 
  col = c("red", "dark green", "green", "blue"))
  

  
d.f <- with(Set1.obs3[-which(Set1.obs3$PatchID == 191),],
   data.frame(obsID, AreaScore, WidthScore, AgeScore, HeightScore, PresAbs))
d.f[order(d.f$obsID),]


Set1.logmodel0 <- glm(PresAbs ~ 1, data = Set1.norm1,
   family = binomial)

Set1.logArea <- update(Set1.logmodel0,
  formula = as.formula("PresAbs ~ PatchArea"))
AIC(Set1.logArea)
anova(Set1.logmodel0, Set1.logArea, test = "Chisq")

Set1.logAreaWidth <- update(Set1.logmodel0,
  formula = as.formula("PresAbs ~ PatchArea + PatchWidth"))
AIC(Set1.logAreaWidth)
anova(Set1.logArea,Set1.logAreaWidth, test = "Chisq")

Set1.logWidth <- update(Set1.logmodel0,
  formula = as.formula("PresAbs ~ PatchWidth"))
AIC(Set1.logWidth)

Set1.logAge <- update(Set1.logmodel0,
  formula = as.formula("PresAbs ~ AgeRatio + I(AgeRatio^2)"))
summary(Set1.logAge)
anova(Set1.logmodel0, Set1.logAge, test = "Chisq")

Set1.logAgeScore <- update(Set1.logmodel0,
  formula = as.formula("PresAbs ~ AgeScore"))
AIC(Set1.logAgeScore)
anova(Set1.logmodel0, Set1.logAgeScore, test = "Chisq")

Set1.logAreaAge <- update(Set1.logmodel0,
  formula = as.formula("PresAbs ~ PatchArea + AgeScore"))
summary(Set1.logAreaAge)
anova(Set1.logAgeScore, Set1.logAreaAge, test = "Chisq")

Set1.logAreaAgeInt <- update(Set1.logmodel0,
  formula = as.formula("PresAbs ~ PatchArea + AgeScore +
  I(PatchArea * AgeScore)"))
summary(Set1.logAreaAgeInt)
anova(Set1.logAreaAge, Set1.logAreaAgeInt, test = "Chisq")

Set1.logAreaAge2 <- update(Set1.logmodel0,
  formula = as.formula("PresAbs ~ PatchArea + AgeRatio +
  I(PatchArea * AgeRatio)"))
anova(Set1.logArea, Set1.logAreaAge2, test = "Chisq")


Set1.logmodelF <- glm(PresAbs ~ PatchArea + PatchWidth + HtRatio +
  I(HtRatio^2) + AgeRatio + I(AgeRatio^2), data = Set1.norm1,
   family = binomial)
anova(Set1.logArea, Set1.logmodelF, test = "Chisq")
