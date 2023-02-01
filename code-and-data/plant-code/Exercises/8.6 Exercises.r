# 8.1
data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
yield.pts <- read.csv("created\\set4.1yld96ptsidw.csv")
data.Set4.1$Yield <- yield.pts$Yield
library(car)
avPlots(lm(Yield ~ Clay +  SoilK, data = data.Set4.1))
crPlots(lm(Yield ~ Clay +  SoilK, data = data.Set4.1))

# 8.2
library(MASS)
data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
yield.pts <- read.csv("created\\set4.1yld96ptsidw.csv")
data.Set4.1$Yield <- yield.pts$Yield
agron.data <- subset(data.Set4.1,
   select = c(Clay, Silt, SoilP, SoilK, SoilpH, SoilTOC, SoilTN,
   LeafN, Weeds, Disease, Yield))
model.4.1 <- as.formula("Yield ~ Clay + Silt + SoilpH +
   SoilTN + SoilP + Disease + Weeds + LeafN")
model.lm <- lm(model.4.1, data = agron.data)
stepAIC(model.lm, model.4.1, direction = "both")
stepAIC(lm(Yield ~ 1, data = data.Set4.1), model.4.1, direction = "forward")
stepAIC(lm(Yield ~ 1, data = data.Set4.1), model.4.1, direction = "both")

# 8.3
library(lattice)
data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
yield.pts <- read.csv("created\\set4.1yld96ptsidw.csv")
data.Set4.1$Yield <- yield.pts$Yield
agron.data <- subset(data.Set4.1,
   select = c(Clay, Silt, SoilP, SoilK, SoilpH, SoilTOC, SoilTN,
   LeafN, FLN, GrainProt, Yield))
agron.dataN <- agron.data[1:62,]
agron.dataS <- agron.data[63:86,]
splom(agron.dataN, par.settings = list(fontsize=list(text=11),
  plot.symbol = list(col = "black")), pscales = 0,
  main = "Agronomic Variables, South")
splom(agron.dataS, par.settings = list(fontsize=list(text=11),
  plot.symbol = list(col = "black")), pscales = 0,
  main = "Agronomic Variables, South")
  
splom(agron.dataS, par.settings = list(fontsize=list(text=11),
  plot.symbol = list(col = "black")), pscales = 0,
  main = "Agronomic Variables, South")

  
# 8.4
data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
yield.pts <- read.csv("created\\set4.1yld96ptsidw.csv")
data.Set4.1$Yield <- yield.pts$Yield
agron.data <- subset(data.Set4.1,
   select = c(Clay, Silt, SoilP, SoilK, SoilpH, SoilTOC, SoilTN,
   LeafN, Weeds, Disease, Yield))
X <- as.matrix(agron.dataN[,-which(names(agron.dataN) == "Yield")])
Y <- agron.dataN$Yield
library(leaps)
model.Cp <- leaps(X, Y, method = "Cp",
   names = names(agron.dataN[,-which(names(agron.dataN)
      == "Yield")]))
par(mai = c(1,1,1,1))
plot(model.Cp$size,model.Cp$Cp, cex.lab = 1.5,
   xlab = "Model size p", ylab = expression(C[p]),
   main = expression(Field~4*"."*1~C[p]), cex.main = 2)
lines(1:10,1:10)
model.Cp$which

# 8.5
# Northern portion only - southern is similar
data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
yield.pts <- read.csv("created\\set4.1yld96ptsidw.csv")
data.Set4.1$Yield <- yield.pts$Yield
agron.data <- subset(data.Set4.1,
   select = c(Clay, Silt, SoilP, SoilK, SoilpH, SoilTOC, SoilTN,
   LeafN, Weeds, Disease, Yield))
model.full  <- lm(Yield ~ ., data = agron.data)
d1 <- drop1(model.full)
d1[order(d1[,4]),]

model.test <- update(model.full, Yield ~ Clay + SoilP + SoilK +
    SoilpH + SoilTOC + LeafN + Weeds)
anova(model.test, model.full)
d1 <- drop1(model.test)
d1[order(d1[,4]),]

model.test <- update(model.full, Yield ~ Clay + SoilP + 
    SoilpH +LeafN + Weeds)
anova(model.test, model.full) # Not sig different
d1 <- drop1(model.test)
d1[order(d1[,4]),]

model.test <- update(model.full, Yield ~ SoilP + Weeds)
anova(model.test, model.full)  # Sig. different p <0.1

model.test <- update(model.full, Yield ~ Clay + SoilP + Weeds)
anova(model.test, model.full)  # Sig. different p <0.1

cor(agron.dataN)

# 8.6
data.Set4.1 <- read.csv("Set4\\Set4.196sample.csv", header = TRUE)
Yield4.1 <- read.csv("created\\set4.1yld96ptsidw.csv", header = TRUE)
print(dy <- max(Yield4.1) - min(Yield4.1))
eps <- 0.01
Yield.jit <- jitter(Yield4.1$Yield, amount = eps * dy)
data.Set4.1$Yield <-  Yield.jit
# Then proceed as in the text

# 8.7
# a
set.seed(123)
X <- c(rep(0,20), rep(2,20)) + 0.1 * rnorm(40)
Y <- X + 0.25 * rnorm(40)
plot(X, Y)
coef(lm(Y ~ X))
# b
coef(lm(Y[1:20] ~ X[1:20]))
coef(lm(Y[21:40] ~ X[21:40]))

# 8.8
agron.data <- with(data.Set4.1, data.frame(Clay, Silt,
   SoilpH, SoilTN, SoilK, SoilP, Disease, Weeds, Yield))
# Interaction terms
agron.data$ClayP <- with(agron.data, Clay*SoilP)
agron.data$ClayK <- with(agron.data, Clay*SoilK)
agron.data$ClaypH <- with(agron.data, Clay*SoilpH)
agron.data$ClayTN <- with(agron.data, Clay*SoilTN)
agron.data$SoilPpH <- with(agron.data, SoilpH*SoilP)
model.test <- update(model.full, Yield ~ Clay + SoilpH + ClaypH +
   SoilP + ClayP + Weeds + SoilPpH)
AIC(model.full)
AIC(model.test)
anova(model.test, model.full)
summary(model.test) # SoilPpH not significant
model.4 <- model.test

# 8.9
#a
cor(cbind(data.Set4.1$Yield,data.Set4.1$FLN,data.Set4.1$LeafN))
summary(lm(Yield ~ FLN + LeafN, data = data.Set4.1))
data.8.7 <- data.Set4.1[,-which(names(agron.data) == "Yield" |
   names(agron.data) == "FLN")]
splom(data.8.7)
model.N.full <- lm(LeafN ~ ., data = data.8.7)
d1 <- drop1(model.N.full)
d1[order(d1[,4]),] # Clay explains LeafN

#b
cor(cbind(data.Set4.1$Yield,data.Set4.1$CropDens))
model.CD.full <- lm(CropDens ~ ., data = data.9.7)
d1 <- drop1(model.CD.full)
d1[order(d1[,4]),] # LeafN explains CropDens

# 8.10
model.GP <- lm(GrainProt ~ Silt + Clay + SoilpH + SOM +
  SoilTN + SoilP + SoilK + Weeds + Disease, data = data.Set4.1)
d1 <- drop1(model.GP)
d1[order(d1[,4]),] # Weeds Clay, SoilK, SoilpH

data.Set4.1$GrnMoist <- yield.pts$GrnMoist
model.GM <- lm(GrnMoist ~ Silt + Clay + SoilpH + SOM +
  SoilTN + SoilP + SoilK + Weeds + Disease, data = data.Set4.1)
d1 <- drop1(model.GM)
d1[order(d1[,4]),] # Weeds, Clay

# 8.11
#a
data.Set4.2 <- read.csv("set4\\set4.296sample.csv", header = TRUE)
yield.pts <- read.csv("created\\set4.2yld96ptsidw.csv")
data.Set4.2$Yield <- yield.pts$Yield

library(lattice)
agron.data <- with(data.Set4.2, data.frame(Clay, Silt, Sand,
   SoilpH, SoilTN, SoilP, LeafN, Disease, Weeds, Yield))
splom(agron.data, par.settings = list(fontsize=list(text=11),
  plot.symbol = list(col = "black")), pscales = 0,
  main = "Agronomic Variables, Field 4.2")

agron.dataHW <- agron.data[which(agron.data$Weeds == 5),]
splom(agron.dataHW, par.settings = list(fontsize=list(text=11),
  plot.symbol = list(col = "black")), pscales = 0,
  main = "Agronomic Variables, Field 4.2 High Weeds")

agron.dataLW <- agron.data[which(agron.data$Weeds < 5),]
splom(agron.dataLW, par.settings = list(fontsize=list(text=11),
  plot.symbol = list(col = "black")), pscales = 0,
  main = "Agronomic Variables, Field 4.2 Low Weeds")

# b
library(leaps)
X <- as.matrix(agron.data[,-ncol(agron.data)])
Y <- agron.data$Yield
model.Cp <- leaps(X, Y, method = "Cp",
   names = names(agron.dataLW[,-ncol(agron.dataLW)]))
par(mai = c(1,1,1,1))
plot(model.Cp$size,model.Cp$Cp, cex.lab = 1.5,
   xlab = "Model size p", ylab = expression(C[p]))
lines(1:10,1:10)
model.Cp$which

## Full data set
model.full  <- lm(Yield ~ Clay + Silt + SoilpH + SoilP + SoilTN + Disease + Weeds, data = agron.data)
d1 <- drop1(model.full)
d1[order(d1[,4]),]

model  <- lm(Yield ~ Clay + Silt + SoilpH + SoilP + SoilTN + Weeds, data = agron.data)
d1 <- drop1(model)
d1[order(d1[,4]),]

model.4  <- lm(Yield ~ Clay + Silt + SoilTN + Weeds, data = agron.data)
anova(model.4, model.full)
d1 <- drop1(model.4)
d1[order(d1[,4]),]

cor(agron.data)

AIC(lm(Yield ~ Clay + Silt + SoilpH + SoilP + SoilTN + Disease + Weeds, data = agron.data))
AIC(lm(Yield ~ Sand + SoilpH + SoilP + SoilTN + Disease + Weeds, data = agron.data))

model.3  <- lm(Yield ~ Sand + SoilTN + Weeds, data = agron.data)
anova(model.3, model.full)

summary(model.3)


# Check for multicollinearity
library(car)
vif(model.3)

# Check for outliers
which(rowSums(influence.measures(model.3)$is.inf) > 0)
outlierTest(model.3)
coef(model.3)
coef(lm(Yield ~ SoilpH + Weeds, data = agron.data[-9,]))


# Check for heteroscedasticity
par(mai = c(1,1,1,1))
plot(fitted(model.3), residuals(model.2S),
   main = "Residuals vs. Fits", cex.main = 2,
   xlab = "Fitted Values", ylab = "Residuals",
   cex.lab = 1.5)
fits.med <- median(fitted(model.2S))
fits.groups <- fitted(model.3) <= fits.med
leveneTest(residuals(model.3),factor(fits.groups))
bptest(model.3)

qqnorm(residuals(model.3), cex.main = 2,
   cex.lab = 1.5)
qqline(residuals(model.3))
shapiro.test(residuals(model.3))

crPlot(model.3S2, variable = "Weeds")
crPlot(model.3S2, variable = "SoilpH")

# 8.12
library(sf)
data.Set2C.sf <- st_read("created\\set2coast.shp")
data.Set2C.glm <- with(data.Set2C.sf, data.frame(MAT, JuMax, JuMean, JaMin,
   TempR, GS32, Precip, PE, ET, Texture, AWCAvg, Permeab, SolRad6, SolRad12,
   SolRad, CoastDist, QUDO))
data.Set2C.glm$PM100 <- as.numeric(data.Set2C.sf$PM100 > 0)
data.Set2C.glm$PM200 <- as.numeric(data.Set2C.sf$PM200 > 0)
data.Set2C.glm$PM300 <- as.numeric(data.Set2C.sf$PM300 > 0)
data.Set2C.glm$PM400 <- as.numeric(data.Set2C.sf$PM400 > 0)
data.Set2C.glm$PM500 <- as.numeric(data.Set2C.sf$PM500 > 0)
data.Set2C.glm$PM600 <- as.numeric(data.Set2C.sf$PM600 > 0)

model.glmCfull <- glm(QUDO ~ ., data = data.Set2C.glm,
  family = binomial)
AIC(model.glmCfull, k = log(nrow(data.Set2C.glm)))
d1 <- drop1(model.glmCfull)
d1[order(d1[,3], decreasing = TRUE),]


model.formula <- as.formula("QUDO ~ MAT + JuMax + JuMean + JaMin + TempR +
  GS32 + Precip + PE + ET + Texture + AWCAvg + Permeab + SolRad6 + 
  SolRad12 + SolRad + PM100 + PM200 + PM300 + PM400 + PM500 + PM600 +
  CoastDist")
model.glmC0 <- glm(QUDO ~ 1, data = data.Set2C.glm, family = binomial)
a1 <- add1(model.glmC0, model.formula)
a1[order(a1[,3]),]
with(data.Set2C.glm, cor(TempR, JuMean))

library(maptools)
model.glmC1 <- update(model.glmC0,
   formula = as.formula("QUDO ~ TempR"))
data.Set2C.sf$QUDOFac <- factor(data.Set2C.sf$QUDO, labels =
   c("Absent", "Present"))
data.Set2C.sf$P1 <- predict(model.glmC1, type = "response")
data.Set2C.sp <- as(data.Set2C.sf, "Spatial")
spplot(data.Set2C.sp, zcol = "QUDOFac")
spplot(data.Set2C.sp, zcol = "P1")
summary(model.glmC1)

a1 <- add1(model.glmC1, model.formula)
a1[order(a1[,3]),]
with(data.Set2C.glm, cor(TempR, Permeab))
model.glmC2 <- update(model.glmC0,
   formula = as.formula("QUDO ~ TempR + Permeab"))
summary(model.glmC1)
summary(model.glmC2)
library(car)
avPlots(model.glmC2,col = "black")
crPlots(model.glmC2, col = "black")
data.Set2C.sp$P2 <- predict(model.glmC2, type = "response") # Stay with sp object
data.Set2C.sp$DP2 <- predict(model.glmC2, type = "response") -
   predict(model.glmC1, type = "response")
spplot(data.Set2C.sp, zcol = "QUDOFac")
spplot(data.Set2C.sp, zcol = "P1")
# Model 1 fits better in the south than the north
# P values a little too high in the north
spplot(data.Set2C.sp, zcol = "P2")
# Possibly a slight improvement in the northwest and south
spplot(data.Set2C.sp, zcol = "DP2")
# Generally raises the P values

a1 <- add1(model.glmC2, model.formula)
a1[order(a1[,3]),]

names(data.Set2C.glm)
cor(data.Set2C.glm[,c(2,3,4,9)])
model.glmC3A <- update(model.glmC0,
   formula = as.formula("QUDO ~ TempR + Permeab + Precip"))
AIC(model.glmC2, k = log(nrow(data.Set2C.glm)))
AIC(model.glmC3A, k = log(nrow(data.Set2C.glm)))
crPlots(model.glmC3A, col = "black",
   main = "Partial Residual Plot")
model.glmC3B <- update(model.glmC0,
   formula = as.formula("QUDO ~ TempR + Permeab + GS32"))
AIC(model.glmC2, k = log(nrow(data.Set2C.glm)))
AIC(model.glmC3B, k = log(nrow(data.Set2C.glm)))
coef(model.glmC2)
coef(model.glmC3A)
coef(model.glmC3B)
data.Set2C.sp$P3A <- predict(model.glmC3A, type = "response")
data.Set2C.sp$P3B <- predict(model.glmC3B, type = "response")
spplot(data.Set2C.sp, zcol = "QUDOFac")
spplot(data.Set2C.sp, zcol = "P3A")
spplot(data.Set2C.sp, zcol = "P3B")

a1 <- add1(model.glmC3A, model.formula)
a1[order(a1[,3]),]
a1 <- add1(model.glmC3B, model.formula)
a1[order(a1[,3]),]
# When Precip is in the model, GS32 is the next to enter
# When GS32 is in, PE is the next to enter

names(data.Set2C.glm)
cor(data.Set2C.glm[,c(2,3,4,5,9)])
model.glmC4A <- update(model.glmC0,
   formula = as.formula("QUDO ~ TempR + Permeab + Precip +
   GS32"))
model.glmC4B <- update(model.glmC0,
   formula = as.formula("QUDO ~ TempR + Permeab + GS32 +
   PE"))
AIC(model.glmC3B, k = log(nrow(data.Set2C.glm)))
AIC(model.glmC4A, k = log(nrow(data.Set2C.glm)))
AIC(model.glmC4B, k = log(nrow(data.Set2C.glm)))
data.Set2C.sp$P4A <- predict(model.glmC4A, type = "response")
data.Set2C.sp$P4B <- predict(model.glmC4B, type = "response")
spplot(data.Set2C.sp, zcol = "QUDOFac")
spplot(data.Set2C.sp, zcol = "P3A")
spplot(data.Set2C.sp, zcol = "P3B")
# Model A appears to extend the higher P further west

a1 <- add1(model.glmC4A, model.formula)
a1[order(a1[,3]),]  #PE
a1 <- add1(model.glmC4B, model.formula)
a1[order(a1[,3]),]  #ET or Precip (tie)
model.glmC5 <- update(model.glmC0,
   formula = as.formula("QUDO ~ TempR + Permeab + Precip +
   GS32 + PE + CoastDist"))
AIC(model.glmC4B, k = log(nrow(data.Set2C.glm)))
AIC(model.glmC5, k = log(nrow(data.Set2C.glm)))
avPlots(model.glmC5, col = "black",
   main = "Partial Residual Plot") # Point 734 is an outlier
crPlots(model.glmC5, col = "black",
   main = "Partial Residual Plot")

a1 <- add1(model.glmC5, model.formula)
a1[order(a1[,3]),]
names(data.Set2C.glm)
cor(data.Set2C.glm[,c(2,3,4,5,8,9,16)])
model.glmC6 <- update(model.glmC0,
   formula = as.formula("QUDO ~ TempR + Permeab + Precip +
   GS32 + PE + SolRad6"))
AIC(model.glmC5, k = log(nrow(data.Set2C.glm)))
AIC(model.glmC6, k = log(nrow(data.Set2C.glm)))

model.glmCS <- update(model.glmC0,
   formula = as.formula("QUDO ~ Precip + MAT + SolRad +
   AWCAvg + Permeab"))
AIC(model.glmC6, k = log(nrow(data.Set2C.glm)))
AIC(model.glmCS, k = log(nrow(data.Set2C.glm)))
 
# Try other correlates with TempR
model.glmCJ <- update(model.glmC0,
   formula = as.formula("QUDO ~ JuMean + Permeab + Precip +
   GS32 + PE + SolRad6"))
AIC(model.glmCJ, k = log(nrow(data.Set2C.glm)))
AIC(model.glmC6, k = log(nrow(data.Set2C.glm)))

model.glmCJ <- update(model.glmC0,
   formula = as.formula("QUDO ~ JuMax + Permeab + Precip +
   GS32 + PE + SolRad6"))
AIC(model.glmCJ, k = log(nrow(data.Set2C.glm)))
AIC(model.glmC6, k = log(nrow(data.Set2C.glm)))

model.glmCD <-update(model.glmC0,
   formula = as.formula("QUDO ~ CoastDist + Permeab + Precip +
   GS32 + PE + SolRad6"))
AIC(model.glmCD, k = log(nrow(data.Set2C.glm)))
AIC(model.glmC6, k = log(nrow(data.Set2C.glm)))

# CoastDist is a better predictor

summary(model.glmC6)
summary(model.glmCD) 
# Relatively small bias of other coefficients


# 8.13
Set1.logAge <- update(Set1.logmodel0,
  formula = as.formula("PresAbs ~ AgeRatio + I(AgeRatio^2)"))
summary(Set1.logAge)
anova(Set1.logmodel0, Set1.logAge, test = "Chisq")

# 8.14

Set1.logHt <- update(Set1.logmodel0,
  formula = as.formula("PresAbs ~ HtRatio + I(HtRatio^2)"))
summary(Set1.logHt)
anova(Set1.logHt,Set1.logmodel0, test = "Chisq")

# 8.15
Set1.poismodel <- glm(Abund ~ ., data = Set1.norm3,
   family = poisson)
summary(Set1.poismodel)
plot(Set1.norm3$Abund, predict(Set1.poismodel))


