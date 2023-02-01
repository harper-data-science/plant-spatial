# 9.1
library(sf)
library(splines)
coast.sf <- st_read("created\\set2coast.shp")
coast.cor <- coast.sf[-which((coast.sf$Elevation >= 1800) &
  (coast.sf$Elevation <= 2000)),]
pres <- numeric(22)
absent <- numeric(22)
n.oaks <- function(var.name, oak, low, high, PresAbs){
  length(which(var.name >= low & var.name < high & oak == PresAbs))}

for(i in 0:22) pres[i] <- with(coast.cor, n.oaks(Elevation, QUDO, i*100,(i+1)*100, 1))
for(i in 0:22) absent[i] <- with(coast.cor,
    n.oaks(Elevation, QUDO, i*100,(i+1)*100, 0))
pa.coast <- pres / (pres + absent + 0.00001)
ID <- 1:nrow(coast.sf)
data.plot <- coast.sf[which(ID %% 15 == 0),]
data.plot <- data.plot[-which((data.plot$JuMin > 11.111) & (data.plot$JuMin < 11.12)),]
knots <- bs(data.plot$JuMin, df = 4)
str(knots)
print(k1 <- attr(knots, "knots"))
class(k1)

# 9.2
knots <- bs(data.plot$JuMin, df = 7)
str(knots)
print(k3 <- attr(knots, "knots"))
diff(k3)

# 9.3
# From Section 3.1
#Correlated data
library(spdep)
set.seed(123)
sigma.eta <- 1.0  # Variance of autocorrelated component
lambda <- 0.4     # Autocorrelation coefficient
nlist <- cell2nb(20, 20)
e <- rnorm(20^2)
eta <- sigma.eta * invIrM(nlist, lambda) %*% e
#Random data
sigma.eps <- 0.4 # Magnitude of random component
eps <- sigma.eps * rnorm(20^2)
# Create the correlated component plus uncorrelated component
Y.df <- data.frame(eta)  # Start with autocorr. comp.
Y.df$eps <- eps  # Add uncorrelated component
#Add deterministic logistic trend
a <- 2
b.eps <- 0.5  # Variance factor for uncorr. component
b.eta <- 0.15 # Variance factor for autocorr. component
c <- 1
f2 <- 20 / 2
xymax <- 20 - 0.5
coords.xy <- expand.grid(x = 0.5:xymax, y = xymax:0.5)
x <- coords.xy[,1]
y <- coords.xy[,2]
Y.trend <- a * exp(c * (x - f2)) / (1 + exp(c * (x - f2)))
Y <- Y.trend + b.eps * eps + b.eta* eta
Y.df$trend <- Y.trend
Y.df$Y <- Y
Y.df$x <- x
Y.df$y <- y
library(mgcv)
model.gam <- gam(Y ~ s(x, bs = "cr", k = 9) + s(y, bs = "cr", k = 9),
  data = Y.df, family = gaussian)
coef(model.gam)
trend.gam <- predict(model.gam)
Y.gam <- matrix(trend.gam, nrow = 20)
x <- (1:20) + 1.5
y <- x
Fit <- 4 * Y.gam
persp(x, y, Fit, theta = 225, phi = 15, scale = FALSE)
 
# 9.4
# Recursive partitioning analysis of Fisher's iris data
data(iris)
iris
iris$symbol <- "X" # Create symbols for each species
iris$symbol[(iris$Species == "versicolor")] <- 0
iris$symbol[(iris$Species == "virginica")] <- "+"
library(rpart)
iris.rp <- rpart(Species ~ Sepal.Length + Sepal.Width, data = iris,
   method = "class")
plot(iris.rp)
text(iris.rp)

par(mai = c(1,1,1,1))
plot(iris$Sepal.Length, iris$Sepal.Width, type = "n",
   main = "Classification of Fisher's Iris Data", cex.main = 2, cex.lab = 1.5,
   xlab = "Sepal Length", ylab = "Sepal Width")
text(iris$Sepal.Length, iris$Sepal.Width, labels = iris$symbol)
segments(5.45, 2.0, 5.45, 4.4)
segments(4.3, 2.8, 5.45, 2.8)
segments(6.15, 2.0, 6.15, 4.4)
segments(5.45, 3.1, 6.15, 3.1)
text(4.5,4.0, "setosa")
text(5.8,3.5, "setosa")
text(7.0, 3.8, "virginica")
text(5.8, 2.1, "versicolor")
text(4.5, 2.6, "versicolor")


# 9.5
# Coast Range
library(sf)
library(rpart)
data.Set2C <- st_read("created\\set2coast.shp")
data.Set2Crp <- with(data.Set2C, data.frame(MAT,
   Precip, JuMin, JuMax, JuMean, JaMin, JaMax, JaMean, TempR, GS32,
   GS28, PE, ET, Texture, AWCAvg, Permeab, SolRad6, SolRad12,
   SolRad, QUDO))
data.Set2Crp$PM100 <- as.numeric(data.Set2C$PM100 > 0)
data.Set2Crp$PM200 <- as.numeric(data.Set2C$PM200 > 0)
data.Set2Crp$PM300 <- as.numeric(data.Set2C$PM300 > 0)
data.Set2Crp$PM400 <- as.numeric(data.Set2C$PM400 > 0)
data.Set2Crp$PM500 <- as.numeric(data.Set2C$PM500 > 0)
data.Set2Crp$PM600 <- as.numeric(data.Set2C$PM600 > 0)
cont.parms <- rpart.control(minsplit = 20, cp = 0.006)
Set2C.rp <-  rpart("QUDO ~ .", data = data.Set2Crp,
  control = cont.parms, method = "anova")
plot(Set2C.rp,branch = 0.4,uniform = T,margin = 0.1,
   main = "Data Set 2 Coast Ranges Regression Tree",
   cex.main = 2) # Fig. 9.10b
text(Set2C.rp,use.n = T,all = T, cex = 0.65)
summary(Set2C.rp)

# 9.6
data.Set3 <- read.csv("Set3\\Set3data.csv", header = TRUE)
Yield.m <- with(data.Set3, tapply(Yield, Farmer, mean))
sort(Yield.m)
Silt.m <- with(data.Set3, tapply(Silt, Farmer, mean))
sort(Silt.m)
pH.m <- with(data.Set3, tapply(pH, Farmer, mean))
sort(pH.m)

# 9.7
data.Set3$Location <- 2
data.Set3$Location[(data.Set3$Northing > 6340000)] <- 1
data.Set3$Location[(data.Set3$Northing < 6280000)] <- 3
par(mai = c(1,1,1,1))
with(data.Set3, plot(N, Yield, cex.main = 2,
   main = "Yield vs. Nitrogen Application Rate",
   xlab = "N Fertilization Rate, kg/ha", cex.lab = 1.5,
   ylab = "Yield, kg/ha", pch = Location))
legend(45, 13000, c("Northen", "Central", "Southern"),
   pch = 1:3, title = "Region")

# 9.8
library(rpart)
cont.parms <- rpart.control(minsplit = 10,cp = 0.002)
Set3.rp2T <-  rpart("Yield ~ DPL + Cont + Irrig +
    N + P + K + pH + Var + Corg + SoilP + SoilK + Sand +
   Silt + Clay", data = data.Set3,
  control = cont.parms)
plotcp(Set3.rp2T)
printcp(Set3.rp2T)
cont.parms <- rpart.control(minsplit = 20,cp = 0.003)
Set3.rp2 <- rpart("Yield ~ DPL + Cont + Irrig +
    N + P + K + Var ", data = data.Set3, control = cont.parms)
plot(Set3.rp2,branch = 0.4,uniform = T,margin = 0.1,
   main = "Data Set 3 Yield Regression Tree", cex.main = 2)
text(Set3.rp2,use.n = T,all = T, cex = 0.8)

# 9.9
data.Set3 <- read.csv("Set3\\Set3data.csv", header = TRUE)
Yield <- with(data.Set3, ave(Yield, Farmer, Season, RiceYear, Field))
pH <- with(data.Set3, ave(pH, Farmer, Season, RiceYear, Field))
Corg <- with(data.Set3, ave(Corg, Farmer, Season, RiceYear, Field))
SoilP <- with(data.Set3, ave(SoilP, Farmer, Season, RiceYear, Field))
SoilK <- with(data.Set3, ave(SoilK, Farmer, Season, RiceYear, Field))
Sand <- with(data.Set3, ave(Sand, Farmer, Season, RiceYear, Field))
Silt <- with(data.Set3, ave(Silt, Farmer, Season, RiceYear, Field))
Clay <- with(data.Set3, ave(Clay, Farmer, Season, RiceYear, Field))
df.C <- data.frame(pH, Corg, SoilP, SoilK, Sand, Silt, Clay, Yield)
library(cwhmisc)
Table.C <- remove.dup.rows(df.C)
print(Table.C[order(Table.C$Yield, decreasing = TRUE),],
   digits = 3, right = TRUE)

# 9.10
library(rpart)
data.Set4.2 <- read.csv("set4\\set4.296sample.csv", header = TRUE)
yield.pts <- read.csv("created\\set4.2yld96ptsidw.csv")
data.Set4.2$Yield <- yield.pts$Yield

Set4.2.model <- "Yield ~ Clay + Silt + Sand + SoilpH + SoilTOC + SoilTN +
   SoilP + Weeds + Disease"
cont.parms <- rpart.control(minsplit = 2,cp = 0.01)
Set4.2.rp <-  rpart(Set4.2.model, data = data.Set4.2, control = cont.parms,
   method = "anova")
plotcp(Set4.2.rp)
printcp(Set4.2.rp)

cont.parms <- rpart.control(minsplit = 5, cp = 0.025)
Set4.2.rp <-  rpart(Set4.2.model, data = data.Set4.2, control = cont.parms,
   method = "anova")
plot(Set4.2.rp,branch = 0.4,uniform = T,margin = 0.1,
   main = "Field 4.2 Yield Regression Tree", cex.main = 2)
text(Set4.2.rp,use.n = T,all = T)
summary(Set4.2.rp)


# 9.11
library(rpart)
data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
yield.pts <- read.csv("created\\set4.1yld96ptsidw.csv")
data.Set4.1$Yield <- yield.pts$Yield
Set4.1.model <- "Yield ~ Clay + Silt + Sand + SoilpH + SoilTOC + SoilTN +
   SoilP + SoilK + Weeds + Disease"
cont.parms <- rpart.control(minsplit = 5, cp = 0.025)
set.seed(123)
n <- 0
data.Perturb <- with(data.Set4.1, data.frame(Yield, Clay, Silt, Sand,
   SoilpH, SoilTOC, SoilTN, SoilP, SoilK, CropDens, Weeds, Disease))
df2 <-  data.frame(apply(data.Perturb, 2, sample, replace = T))
Perturb.rp <-  rpart(Set4.1.model, data = df2, control = cont.parms,
   method = "anova")
n <- n + 1
plot(Perturb.rp,branch = 0.4,uniform = T,margin = 0.1,
   main = paste("Field 4-1 Yield Regression Tree ",
   as.character(n)), cex.main = 2)
text(Perturb.rp,use.n = T,all = F)

# 9.12
library(sf)
data.Set2Sierra <- st_read("created\\set2sierra.shp")
data.Set2Srp <- with(data.Set2Sierra, data.frame(MAT,
   Precip, JuMin, JuMax, JuMean, JaMin, JaMax, JaMean, TempR, GS32,
   GS28, PE, ET, Texture, AWCAvg, Permeab, SolRad6, SolRad12,
   SolRad, QUDO))
data.Set2Srp$PM100 <- as.numeric(data.Set2Sierra$PM100 > 0)
data.Set2Srp$PM200 <- as.numeric(data.Set2Sierra$PM200 > 0)
data.Set2Srp$PM300 <- as.numeric(data.Set2Sierra$PM300 > 0)
data.Set2Srp$PM400 <- as.numeric(data.Set2Sierra$PM400 > 0)
data.Set2Srp$PM500 <- as.numeric(data.Set2Sierra$PM500 > 0)
data.Set2Srp$PM600 <- as.numeric(data.Set2Sierra$PM600 > 0)
library(rpart)
n <- 0
data.Perturb <- with(data.Set2Sierra, data.frame(MAT,
   Precip, JuMin, JuMax, JuMean, JaMin, JaMax, JaMean, TempR, GS32,
   GS28, PE, ET, Texture, AWCAvg, Permeab, SolRad6, SolRad12,
   SolRad, QUDO))
ID <- 1:nrow(data.Perturb)
cont.parms <- rpart.control(minsplit = 20, cp = 0.0078)
n <- n + 1
ID.boot <- sample(ID, replace = TRUE)
df2 <-  data.Perturb[ID.boot,]
Perturb.rp <-  rpart("QUDO ~ .", data = df2, control = cont.parms,
   method = "anova")
plot(Perturb.rp,branch = 0.4,uniform = T,margin = 0.1,
   main = "Data Set 2 Sierra Nevada Regression Tree",
   cex.main = 2)
text(Perturb.rp,use.n = T,all = T, cex = 0.8)


# 9.13
data.Set4.2 <- read.csv("Set4\\Set4.296sample.csv", header = TRUE)
yield.pts <- read.csv("created\\Set4.2yld96ptsidw.csv")
data.Set4.2$Yield <- yield.pts$Yield
library(randomForest)
data.rf <- with(data.Set4.2, data.frame(Yield, Clay, Silt, Sand,
   SoilpH, SoilTOC, SoilTN, SoilP, Weeds, Disease))
Set4.2.rf <- randomForest(Yield ~ ., data = data.rf,
  importance = TRUE, proximity = TRUE)
varImpPlot(Set4.2.rf, main = "Data Set 4.2 Variable Importance")

# 9.14
library(sf)
library(randomForest)
data.Set2C <- st_read("created\\set2Coast.shp")
set.seed(123)
data.rfC <- with(data.Set2C, data.frame(QUDO = factor(QUDO),
  JuMax, JuMean, JaMean, GS32, GS28, ET,
  MAT, Precip, SolRad6, SolRad12, Texture, AWCAvg,
  Permeab, PM100, PM200, PM300, PM400, PM500, PM600))
Set2.rf <- randomForest(QUDO ~ ., data = data.rfC,
  importance = TRUE)
varImpPlot(Set2.rf,
  main = "Data Set 2 Coast Variable Importance")
Set2.rf

set.seed(123)
Set2.rf2 <- randomForest(QUDO ~ JuMax + JuMean + JaMean + GS32 +
    GS28 + JaMean + Precip, data = data.rfC,
   importance = TRUE)
varImpPlot(Set2.rf2,
  main = "Data Set 2 Coast Variable Importance")
Set2.rf2

set.seed(123)
Set2.rf3 <- randomForest(QUDO ~ JuMean + JaMean + GS32, data = data.rfC,
  importance = TRUE)
Set2.rf3

