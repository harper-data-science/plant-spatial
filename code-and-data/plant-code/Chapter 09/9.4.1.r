data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
data.Yield4.1idw <- read.csv("created\\set4.1yld96ptsidw.csv", header = TRUE)
data.Set4.1$Yield <-  data.Yield4.1idw$Yield

library(randomForest)
data.rf <- with(data.Set4.1, data.frame(Yield, Clay, Silt,
   SoilpH, SoilTOC, SoilTN, SoilP, SoilK, CropDens, Weeds, Disease))
Set4.1.rf <- randomForest(Yield ~ ., data = data.rf,
  importance = TRUE, proximity = TRUE)
varImpPlot(Set4.1.rf,
   main = "Data Set 4.1 Variable Importance") # Fig. 9.17

closest <- function(x){
   x[which(x > 0.999)] <- 0
   which(x == max(x))
}
apply(Set4.1.rf$proximity, 1, closest)