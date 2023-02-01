library(rpart)
library(sf)
library(terra)

# Set up data as specified in Appendix B.4
data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
data.Yield4.1idw <- read.csv("created\\set4.1yld96ptsidw.csv", header = TRUE)
data.Set4.1$Yield <-  data.Yield4.1idw$Yield


Set4.1.model <- "Yield ~ Clay + Silt + Sand + SoilpH + SoilTOC + SoilTN +
   SoilP + SoilK + Weeds + Disease"
cont.parms <- rpart.control(minsplit = 2,cp = 0.01)
Set4.1.rp <-  rpart(Set4.1.model, data = data.Set4.1, control = cont.parms,
   method = "anova")
plotcp(Set4.1.rp)
printcp(Set4.1.rp)

cont.parms <- rpart.control(minsplit = 5, cp = 0.02)
Set4.1.rp <-  rpart(Set4.1.model, data = data.Set4.1,
   control = cont.parms, method = "anova")
plot(Set4.1.rp,branch = 0.4,uniform = T,margin = 0.1, # Fig. 9.15
   main = "Field 4.1 Yield Regression Tree", cex.main = 1)
text(Set4.1.rp,use.n = T,all = T, cex = 0.65)
summary(Set4.1.rp)

# This file can be created in ArcGIS or GeoDa
thsn.sf <- st_read("auxiliary\\set4.1thiessen.shp")
#thsn.sp <- as(thsn.sf, "Spatial")
Node <- numeric(nrow(data.Set4.1))
Node[with(data.Set4.1, which(Clay >= 35.42 &
  SoilTN < 0.1065))] <- 1
Node[with(data.Set4.1, which(Clay >= 35.42 &
  SoilTN >= 0.1065 & SoilK >= 235.6))] <- 2
Node[with(data.Set4.1, which(Clay >= 35.42 &
  SoilTN >= 0.1065 & SoilK < 235.6))] <- 3
Node[with(data.Set4.1, which(Clay < 35.42 &
  SoilTOC >= 1.113 & SoilTOC < 1.232))] <- 4
Node[with(data.Set4.1, which(Clay < 35.42 &
  SoilTOC >= 1.113 & SoilTOC >= 1.232))] <- 5
  Node[with(data.Set4.1, which(Clay < 35.42 &
  SoilTOC < 1.113))] <- 6
thsn.sf["Node"] <-
   factor(Node, labels = c("High Clay, Low SoilTN",
   "High Clay, High SoilTN, Low SoilK", "High Clay, High SoilTN, High SoilK",
   "Low Clay, High SoilTOC", "Low Clay, Moderate SoilTOC", "Low Clay, Low SoilTOC"))
thsn.ter <- vect(thsn.sf)

greys <- grey(c(40, 70, 110, 140, 180, 200) / 255)
plot(thsn.ter, y = "Node", col = greys,
  main = "Field 4.1 Recursive Partitioning Categories") 

# Color version
color <- rainbow(6)
plot(thsn.ter, y = "Node", col = color,
   main = "Field 4.1 Recursive Partitioning Categories") 

thsn.ter$Yield <- data.Set4.1$Yield
greys <- grey(50:250 / 255)
plot(thsn.ter, y = "Yield", col = greys,
   main = "Field 4.1 Yield (kg/ha)") # Fig. 9.16b

# Color version
color <- terrain.colors(200)
plot(thsn.ter, y = "Yield", col = color,
   main = "Field 4.1 Yield (kg/ha)") 


# Randomly perturb trees
set.seed(123)
n <- 0
data.Perturb <- with(data.Set4.1, data.frame(Yield, Clay, Silt, Sand,
   SoilpH, SoilTOC, SoilTN, SoilP, SoilK, CropDens, Weeds, Disease))
epsilon <- 0.05
# Repeat the following 9 times
mm <- matrix(rnorm(nrow(data.Perturb) * ncol(data.Perturb)),
  nrow = nrow(data.Perturb))
print(n <- n + 1)
df2 <-  data.Perturb * (1 + epsilon * mm)
Perturb.rp <-  rpart(Set4.1.model, data = df2, control = cont.parms,
   method = "anova")
plot(Perturb.rp,branch = 0.4,uniform = T,margin = 0.1,
   main = paste("Field 4-1 Yield Regression Tree ",
   as.character(n)), cex.main = 2)
text(Perturb.rp,use.n = T,all = F)
# End of repeated code

# Map tree #3
Node <- numeric(nrow(data.Set4.1))
Node[with(data.Set4.1, which(Clay >= 29.17 &
  SoilP < 7.628))] <- 1
Node[with(data.Set4.1, which(Clay >= 29.17 &
  SoilP >= 7.628  & Weeds > 3.5))]  <- 2
Node[with(data.Set4.1, which(Clay >= 29.17 &
  SoilP >= 7.628  & Weeds < 3.5))]  <- 3
Node[with(data.Set4.1, which(Clay < 29.17 &
  SoilTOC >= 1.178))] <- 4
Node[with(data.Set4.1, which(Clay < 29.17 &
  SoilTOC < 1.178))] <- 5
thsn.ter$Node <-
   factor(Node, labels = c("Hi Clay, Lo SoilP",
   "Hi Clay, Hi SoilP, Hi Weeds",
   "Hi Clay, Hi SoilP, Lo Weeds",
   "Lo Clay, Hi SoilTOC",
   "Lo Clay, Lo SoilTOC"))
greys <- grey(c(40, 70, 100, 130, 160) / 255)
plot(thsn.ter, y = "Node", col = greys,
   main = "Field 4.1 Most Popular Categories") # Fig. 9.16c


