library(sf)
library(rpart)

data.Set2S.sf <- st_read("created\\set2sierra.shp")

data.Set2Srp <- with(data.Set2S.sf, data.frame(MAT,
   Precip, JuMin, JuMax, JuMean, JaMin, JaMax, JaMean, TempR, GS32,
   GS28, PE, ET, Texture, AWCAvg, Permeab, SolRad6, SolRad12,
   SolRad, QUDO))
data.Set2Srp$PM100 <- as.numeric(data.Set2S.sf$PM100 > 0)
data.Set2Srp$PM200 <- as.numeric(data.Set2S.sf$PM200 > 0)
data.Set2Srp$PM300 <- as.numeric(data.Set2S.sf$PM300 > 0)
data.Set2Srp$PM400 <- as.numeric(data.Set2S.sf$PM400 > 0)
data.Set2Srp$PM500 <- as.numeric(data.Set2S.sf$PM500 > 0)
data.Set2Srp$PM600 <- as.numeric(data.Set2S.sf$PM600 > 0)

# Start with small values of control parameters

cont.parms <- rpart.control(minsplit = 20,cp = 0.002)
Set2S.rp <-  rpart(QUDO ~ ., data = data.Set2Srp,
  control = cont.parms, method = "anova")
plotcp(Set2S.rp) # Fig. 9.9
printcp(Set2S.rp)

# Control parameters based on output of plotcp() and printcp()
cont.parms <- rpart.control(minsplit = 20, cp = 0.0078)
Set2S.rp <-  rpart(QUDO ~ ., data = data.Set2Srp,
  control = cont.parms, method = "anova")
plot(Set2S.rp,branch = 0.4,uniform = T,margin = 0.1,
   main = "Data Set 2 Sierra Nevada Regression Tree",
   cex.main = 2) # Fig. 9.10a
text(Set2S.rp,use.n = T,all = T, cex = 0.65)
summary(Set2S.rp)

