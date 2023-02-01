# Creation of Fig. 7.2
library(sf)
library(ggplot2)

# Read in the files and set the projections
data.Set1.patches.sf <- st_read("set1\\habitatpatches.shp")
st_crs(data.Set1.patches.sf) <- "EPSG:32610" # UTM Zone 10N
data.Set1.obs <- read.csv("set1\\obspts.csv", header = TRUE)
data.Set1.obs.sf <- st_as_sf(data.Set1.obs,
   coords = c("Easting", "Northing"))
st_crs(data.Set1.obs.sf) <- "EPSG:32610" # UTM Zone 10N
data.Set1.sf <- st_read("set1\\set1data.shp")
st_crs(data.Set1.sf) <- "EPSG:32610" # UTM Zone 10N
str(data.Set1.patches.sf)
ggplot() +       # Fig. 7.2a
    geom_sf(data = data.Set1.patches.sf ) +
        scale_fill_manual(values = "white") + 
       coord_sf(xlim = c(576000, 580500),ylim = c(4417400, 4421000)) +
       ggtitle("Data Set 1, Northern End, 1997") +
       geom_point(data = data.Set1.obs,
          mapping = aes(x = Easting, y = Northing), color = "black") +
       geom_text(data = data.Set1.obs,
          mapping = aes(x = Easting, y = Northing), color = "red",
          label = as.character(data.Set1.obs$Abund)) +
       geom_sf_text(data = data.Set1.patches.sf,
          color = "black", label = data.Set1.patches.sf$VegType) #+ 
#       theme(panel.grid.major = element_blank(), 
#          panel.grid.minor = element_blank(),
#          panel.background = element_blank()) 
# Removing the comment marks will get rid of the gray

ggplot() +       # Fig. 7.2b
    geom_sf(data = data.Set1.patches.sf ) +
        scale_fill_manual(values = "white") + 
       coord_sf(xlim = c(578385,583080), ylim = c(4408875,4415552)) +
       ggtitle("Data Set 1, North Center, 1997") +
       geom_point(data = data.Set1.obs,
          mapping = aes(x = Easting, y = Northing), color = "black") +
       geom_text(data = data.Set1.obs,
          mapping = aes(x = Easting, y = Northing), color = "red",
          label = as.character(data.Set1.obs$Abund)) 

ggplot() +       # Fig. 7.2c
    geom_sf(data = data.Set1.patches.sf ) +
        scale_fill_manual(values = "white") + 
       coord_sf(xlim = c(578385,584780), ylim = c(4401344,4408900)) +
       ggtitle("Data Set 1, South Center, 1997") +
       geom_point(data = data.Set1.obs,
          mapping = aes(x = Easting, y = Northing), color = "black") +
       geom_text(data = data.Set1.obs,
          mapping = aes(x = Easting, y = Northing), color = "red",
          label = as.character(data.Set1.obs$Abund)) 

ggplot() +       # Fig. 7.2d
    geom_sf(data = data.Set1.patches.sf ) +
        scale_fill_manual(values = "white") + 
       coord_sf(xlim = c(581971,589940), ylim = c(4397000, 4401344)) +
       ggtitle("Data Set 1, South, 1997") +
       geom_point(data = data.Set1.obs,
          mapping = aes(x = Easting, y = Northing), color = "black") +
       geom_text(data = data.Set1.obs,
          mapping = aes(x = Easting, y = Northing), color = "red",
          label = as.character(data.Set1.obs$Abund)) 



# Creation of Fig. 7.3
unique(data.Set1.sf$HtClass)
data.Set1.sf$Ht_Class <-
   ordered(as.character(data.Set1.sf$HtClass),
   levels = c("0", "l", "m", "h"),
   labels = c("No data", "Low", "Medium", "High"))
greys <- grey(c(250, 100, 150, 200) / 255)
ggplot() + 
    geom_sf(data = data.Set1.sf, aes(fill = Ht_Class)) +
        scale_fill_manual(values = greys) +
       geom_point(data = data.Set1.obs,
          mapping = aes(x = Easting, y = Northing), color = "black") +
       coord_sf(xlim = c(576800, 579000), ylim = c(4417400, 4418800)) +
       ggtitle("Vegetation Height Class") 

# Color version
color <- topo.colors(n = 4)  
ggplot() + 
    geom_sf(data = data.Set1.sf, aes(fill = Ht_Class)) +
        scale_fill_manual(values = color) +
       geom_point(data = data.Set1.obs,
          mapping = aes(x = Easting, y = Northing), color = "black") +
       coord_sf(xlim = c(576800, 579000), ylim = c(4417400, 4418800)) +
       ggtitle("Vegetation Height Class")  # Fig. 7.3a

unique(data.Set1.sf$CoverClass)
data.Set1.sf$Cover_Class <-
   ordered(as.character(data.Set1.sf$CoverClass),
   levels = c("0", "s", "p", "m", "d"),
   labels = c("No data", "10-24%", "24-39%", "40-59%", "60-100%"))
greys <- grey(c(250, 50, 100, 150, 175) / 255)
ggplot() + 
    geom_sf(data = data.Set1.sf, aes(fill = Cover_Class)) +
        scale_fill_manual(values = greys) +
       geom_point(data = data.Set1.obs,
          mapping = aes(x = Easting, y = Northing), color = "black") +
       coord_sf(xlim = c(576800, 579000), ylim = c(4417400, 4418800)) +
       ggtitle("Vegetation Cover Class")  # Fig. 7.3b

# Color version
color <- topo.colors(n = 5)  
ggplot() + 
    geom_sf(data = data.Set1.sf, aes(fill = Cover_Class)) +
        scale_fill_manual(values = color) +
       geom_point(data = data.Set1.obs,
          mapping = aes(x = Easting, y = Northing), color = "black") +
       coord_sf(xlim = c(576800, 579000), ylim = c(4417400, 4418800)) +
       ggtitle("Vegetation Cover Class")  # Fig. 7.3b

data.Set1.sf$AgeLT60 <- as.factor(data.Set1.sf$Age < 60)
greys <- grey(c(175, 50) / 255)
ggplot() + 
    geom_sf(data = data.Set1.sf, aes(fill = AgeLT60)) +
        scale_fill_manual(values = greys) +
       geom_point(data = data.Set1.obs,
          mapping = aes(x = Easting, y = Northing), color = "black") +
       coord_sf(xlim = c(576800, 579000), ylim = c(4417400, 4418800)) +
       ggtitle("Vegetation Age Less Than 60 Years")  # Fig. 7.3c

color <- topo.colors(n = 2)  
ggplot() + 
    geom_sf(data = data.Set1.sf, aes(fill = AgeLT60)) +
        scale_fill_manual(values = color) +
       geom_point(data = data.Set1.obs,
          mapping = aes(x = Easting, y = Northing), color = "black") +
       coord_sf(xlim = c(576800, 579000), ylim = c(4417400, 4418800)) +
       ggtitle("Vegetation Age Less Than 60 Years")  # Fig. 7.3c


# Computations to generate the data set used in the exploratory analysis

# Eliminate patches in which no observation point exists
obs.pts <- st_intersects(data.Set1.obs.sf, data.Set1.sf)
Set1.obs.sf <- data.Set1.sf[unlist(obs.pts),]
Set1.obs.sf$PresAbs <- data.Set1.obs$PresAbs
Set1.obs.sf$obsID <- data.Set1.obs$ID
Set1.obs.sf$Abund <- data.Set1.obs$Abund

names(Set1.obs.sf)  # Polygons containing an obs point
# Display some of the data
with(Set1.obs.sf, cbind(PatchID, PatchArea, PatchWidth, obsID, PresAbs))

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
Set1.obs1.sf <- merge(x = Set1.obs.sf, y = HtRatio.df,
  by.x = "PatchID", by.y = "PatchID")

# AgeRatio is computed in Exercise 7.3
# In order to continue, you must do this exercise
Set1.obs2.sf <- merge(x = Set1.obs1.sf, y = AgeRatio.df,
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
Set1.obs3.sf <- merge(x = Set1.obs2.sf, y = CoverRatio.df,
  by.x = "PatchID", by.y = "PatchID")

# Example of the use of ggplot()
ggplot(data = Set1.obs3.sf) +
   geom_boxplot(mapping = aes(as.factor(PresAbs), PatchArea))

with(Set1.obs3.sf, boxplot(PatchArea ~ PresAbs)) # Fig. 7.4a
title(main = "Patch Area", cex.main = 2,
  xlab = "Presence/Absence",
  ylab = expression(Area~"("*m^2*")"), cex.lab = 1.5)
with(Set1.obs3.sf, boxplot(PatchWidth ~ PresAbs)) # Fig. 7.4b
title(main = "Patch Width", cex.main = 2,
  xlab = "Presence/Absence", ylab = "Patch Width (m)", cex.lab = 1.5)
with(Set1.obs3.sf, boxplot(HtRatio ~ PresAbs)) # Fig. 7.4c
title(main = "Height Ratio", cex.main = 2,
  xlab = "Presence/Absence", ylab = "Height Ratio", cex.lab = 1.5)
with(Set1.obs3.sf, boxplot(AgeRatio ~ PresAbs)) # Fig. 7.4d
title(main = "Age Ratio", cex.main = 2,
  xlab = "Presence/Absence", ylab = "Age Ratio", cex.lab = 1.5)
with(Set1.obs3.sf, boxplot(CoverRatio ~ PresAbs)) # Fig. 7.4e
title(main = "Cover Ratio", cex.main = 2,
  xlab = "Presence/Absence", ylab = "Cover Ratio", cex.lab = 1.5)

# Aligned dot plots of the presence and absence
row.names(Set1.obs3.sf) <- as.character(1:nrow(Set1.obs3))
obs.trans <- data.frame(t(scale(with(Set1.obs3.sf,
   cbind(HtRatio, AgeRatio, CoverRatio, PatchArea, PatchWidth)))))
n.trans <- nrow(obs.trans)
obs.trans[n.trans + 1,] <- t(Set1.obs3.sf$PresAbs)
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

with(Set1.obs3.sf, (cor(cbind(HtRatio, AgeRatio,
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

AreaScore <- numeric(nrow(Set1.obs3.sf))
for (i in 1:nrow(Set1..sf)){
 AreaScore[i] <-
    suitability.score(Set1.obs3.sf$PatchArea[i] / 10000, c(17,40,80))
}
Set1.obs3$AreaScore <- AreaScore

WidthScore <- numeric(nrow(Set1.obs3.sf))
for (i in 1:nrow(Set1.obs3)){
 WidthScore[i] <-
    suitability.score(Set1.obs3.sf$PatchWidth[i], c(100,200,600))
}
Set1.obs3$WidthScore <- WidthScore

# AgeScore and HeightScore are computed in Exercise 7.4
# In order to continue, you must do this exercise
Set1.obs3.sf$AgeScore <- AgeScore
Set1.obs3.sf$HeightScore <- HeightScore

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


scores <- with(Set1.obs3.sf, cbind(AreaScore, WidthScore,
  AgeScore, HeightScore))
print(Set1.obs3.sf$HabitatScore <- apply(scores[,1:4], 1, prod)^(1/4),
  digits = 2)
print(Set1.obs.sf3$HSIPred <- as.numeric(Set1.obs3$HabitatScore > 0))
#Disqualify obs point 1
Set1.corrected <- Set1.obs3.sf[-which(Set1.obs3.sf$PatchID == 191),]

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
