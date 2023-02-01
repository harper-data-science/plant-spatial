data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
data.Yield4.1idw <- read.csv("created\\set4.1yld96ptsidw.csv", header = TRUE)
data.Set4.1$Yield <- scale(data.Yield4.1idw$Yield)

formula.5 <- as.formula("Yield ~ Clay + SoilP +
   I(Clay*SoilP) + Weeds")
model.5 <- lm(formula.5, data = data.Set4.1)
summary(model.5)

library(car)
library(lmtest)
bptest(model.5)
fits.med <- median(fitted(model.5))
fits.groups <- fitted(model.5) <= fits.med
leveneTest(residuals(model.5),factor(fits.groups))

plot(fitted(model.5), residuals(model.5)) # Not in text

library(spdep)
coordinates(data.Set4.1) <- c("Easting", "Northing")

nlist <- dnearneigh(data.Set4.1, d1 = 0, d2 = 61)
W <- nb2listw(nlist, style = "W")


model.5.lag <- lagsarlm(formula.5,
   data = data.Set4.1, listw = W)
summary(model.5.lag)
model.5.err <- errorsarlm(formula.5,
   data = data.Set4.1, listw = W)
summary(model.5.err)
# Same as model.5.err
summary(spautolm(formula.5,
   data = data.Set4.1, listw = W))
   
# Unscaled data (doesn't work)
data.Set4.1$YieldUS <- data.Yield4.1idw$Yield
lagsarlm(YieldUS ~ Clay + SoilP +
   I(Clay*SoilP) + Weeds, data = data.Set4.1, listw = W)

# CAR model
# This produces an error
model.N.car <- spautolm(formula.5,
   data = data.Set4.1, family = "CAR", listw = W)
W.B <- nb2listw(nlist, style = "B")
model.5.car <- spautolm(formula.5,
   data = data.Set4.1, family = "CAR", listw = W.B)
summary(model.5.car)
   
print(coef(model.5), digits = 2)
print(coef(model.5.lag), digits = 2)
print(coef(model.5.err), digits = 2)
print(coef(model.5.car), digits = 2)