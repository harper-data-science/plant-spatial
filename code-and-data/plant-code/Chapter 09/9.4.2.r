library(sf)
library(randomForest)
data.Set2S.sf <- st_read("created\\set2Sierra.shp")

library(randomForest)
attr.S <- data.Set2S.sf
data.rfS <- with(data.Set2S.sf, data.frame(QUDO = factor(QUDO),
  MAT, Precip, SolRad6, SolRad12, Texture, AWCAvg,
  Permeab, PM100, PM200, PM300, PM400, PM500, PM600))
set.seed(123)
Set2.rf <- randomForest(QUDO ~ ., data = data.rfS,
  importance = TRUE)
varImpPlot(Set2.rf,   # Fig. 9.18a
  main = "Data Set 2 Sierra Variable Importance")
Set2.rf

set.seed(123)
Set2.rf2 <- randomForest(QUDO ~ MAT + Precip + SolRad6 +
  SolRad12 + Texture + AWCAvg + Permeab, data = data.rfS,
  importance = TRUE)
Set2.rf2

set.seed(123)
Set2.rf3 <- randomForest(QUDO ~ MAT + Precip + SolRad6 +
  SolRad12 + AWCAvg + Permeab, data = data.rfS,
  importance = TRUE)
Set2.rf3

set.seed(123)
Set2.rf4 <- randomForest(QUDO ~ MAT + Precip + SolRad6 +
  AWCAvg + Permeab, data = data.rfS,
  importance = TRUE)
Set2.rf4

