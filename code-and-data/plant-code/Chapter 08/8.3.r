library(lattice)
library(sf)
library(car) # For vif() and outlierTest() 
library(MPV)  # For PRESS()
library(leaps) # For leaps()
library(lmtest) # For lmtest()

data.Set4.1 <- read.csv("Set4\\Set4.196sample.csv", header = TRUE)
data.Yield4.1idw <- read.csv("created\\set4.1yld96ptsidw.csv", header = TRUE)
data.Set4.1$Yield <-  data.Yield4.1idw$Yield

ord.dotplot <- function(data.Set){
   data.Weeds <- with(data.Set, data.frame(Yield, Level = as.ordered(Weeds)))
   data.Weeds$Type <- as.factor("Weeds")
   data.Disease <- with(data.Set, data.frame(Yield, Level = as.ordered(Disease)))
   data.Disease$Type <- as.factor("Disease")
   data.CropDens <- with(data.Set, data.frame(Yield, Level = as.ordered(CropDens)))
   data.CropDens$Type <- as.factor("CropDens")
   data.ord <- rbind(data.Weeds, data.Disease, data.CropDens)
   trellis.device(color = FALSE)
   dotplot(Yield ~ Level | Type, data = data.ord,
      main = "Field 4.1",
      xlab = "Level", ylab = "Yield, kg/ha",
      layout = c(3,1), aspect = c(1.0))
}
ord.dotplot(data.Set4.1) # Fig. 8.5

# Eliminate endogenous and gauge variables
agron.data <- with(data.Set4.1, data.frame(Clay, Silt,
   SoilpH, SoilTN, SoilK, SoilP, Disease, Weeds, Yield))
# Interaction terms
agron.data$ClayP <- with(agron.data, Clay*SoilP)
agron.data$ClayK <- with(agron.data, Clay*SoilK)
agron.data$ClaypH <- with(agron.data, Clay*SoilpH)
agron.data$ClayTN <- with(agron.data, Clay*SoilTN)

# Create terms for leaps():
X <- as.matrix(agron.data[,-which(names(agron.data) == "Yield")])
Y <- agron.data$Yield
model.Cp <- leaps(X, Y, method = "Cp",
   names = names(agron.data[,-which(names(agron.data)
      == "Yield")]))

print(cbind(model.Cp$Cp, model.Cp$which), digits = 3)

par(mai = c(1,1,1,1))
plot(model.Cp$size-1,model.Cp$Cp, cex.lab = 1.5, # Fig. 8.6
   xlab = "Number of variables p-1", ylab = expression(C[p]),
   main = expression(Field~4*"."*1~C[p]), cex.main = 2,
   ylim = c(0,50))
lines(1:12,1:12)
arrows(10,20, 8.05,6, length = 0.15, angle = 15)
text(10, 22, "model.3")
arrows(9,30, 6.05,4, length = 0.15, angle = 15)
text(9, 33, "model.4")
text(7, 28, "model.5")
arrows(7,25, 5.05,4, length = 0.15, angle = 15)

model.full  <- lm(Yield ~ ., data = agron.data)
d1 <- drop1(model.full)
d1[order(d1[,4]),]

model.1 = lm(Yield ~ Clay + Silt + SoilpH + SoilTN + SoilK +
    SoilP + Disease + Weeds + ClayP + ClayK + ClaypH + ClayTN,
    data = agron.data)

model.test <- update(model.full, Yield ~ Clay + Silt + SoilpH + SoilTN + 
    SoilP + Disease + Weeds + ClayP + ClaypH + ClayTN,)
anova(model.test, model.full)
AIC(model.full)
AIC(model.test)
d1 <- drop1(model.test)
d1[order(d1[,4]),]

model.test <- update(model.full, Yield ~ Clay + Silt + SoilTN + 
    SoilP + Disease + Weeds + ClayP + ClayTN)
anova(model.test, model.full)
AIC(model.full)
AIC(model.test)
d1 <- drop1(model.test)
d1[order(d1[,4]),]

model.test <- update(model.full, Yield ~ Clay + Silt + SoilTN +
    Disease + Weeds + ClayTN)
AIC(model.full)
AIC(model.test)
anova(model.test, model.full)
d1 <- drop1(model.test)
d1[order(d1[,4]),]

model.test <- update(model.full, Yield ~ Clay + Silt + SoilTN +
    Weeds + ClayTN)
AIC(model.full)
AIC(model.test)
anova(model.test, model.full)
d1 <- drop1(model.test)
d1[order(d1[,4]),]
model.2 <- model.test

# Add three models based on leaps() results
model.3 <- update(model.full, Yield ~ Clay + SoilpH +
   SoilP + Weeds + ClayP + ClaypH)
model.4 <- update(model.full, Yield ~ Clay + SoilpH + SoilP +
   Weeds + ClayP)
model.5 <- update(model.full, Yield ~ Clay + SoilP +
   Weeds + ClayP)   

# Not in text
par(mai = c(1,1,1,1))
plot(fitted(model.5), residuals(model.5),
   main = "Residuals vs. Fits", cex.main = 2,
   xlab = "Fitted Values", ylab = "Residuals",
   cex.lab = 1.5)
 
# Check for multicollinearity
vif(model.5)

# Check for outliers
which(rowSums(influence.measures(model.5)$is.inf) > 0)
avPlots(model.5, id.n = 1)
outlierTest(model.5)

crPlot(model.5, variable = "Clay")
crPlot(model.5, variable = "Weeds")


# Check for heteroscedasticity
bptest(model.5)

qqnorm(residuals(model.5), cex.main = 2,
   cex.lab = 1.5)
qqline(residuals(model.5))
shapiro.test(residuals(model.5))

PRESS(model.full)
PRESS(model.5)
PRESS(model.5) / deviance(model.5)

# This file can be created in ArcGIS or GeoDa
thsn.sf <- st_read("auxiliary\\set4.1thiessen.shp")
thsn.spdf <- as(thsn.sf, "Spatial")
# Check that IDs are the same
all.equal(thsn.spdf$ThPolyID, data.Set4.1$ID)
resids <- residuals(model.5)
slot(thsn.spdf, "data")$resids <- resids
greys <- grey(1:255 / 255)
spplot(thsn.spdf, zcol = "resids", col.regions = greys,
   main = "Field 4.1 model.5 Residuals") # Fig. 8.7

# Final models
# Full model: model.1
model.1
model.2
model.3
model.4
model.5
