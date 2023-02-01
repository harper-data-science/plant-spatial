library(nlme)
library(lattice)
data.Set3 <- read.csv("Set3\\Set3data.csv", header = TRUE)
data.Set3a <- data.Set3[-which(data.Set3$Field == 2),]

Yld.lmeF2 <- lme(Yield ~ Irrig, data = data.Set3a,
   random = ~ Irrig | Field)

var.lmeF <- Variogram(Yld.lmeF2, form = ~ Easting + Northing)

# This doesn't work
plot(var.lmeF, col = "black")

trellis.device(color = FALSE)
plot(var.lmeF,
   main = "Variogram of residuals of Yield vs. Irrig")  # Fig. 12.4

# This produces an error
Yld.lmeF3 <- update(Yld.lmeF2, correlation
  = corSpher(form = ~ Easting + Northing, nugget = TRUE))

set.seed(456)
data.Set3a$EAST2 <- jitter(data.Set3a$Easting)
data.Set3a$NORTH2 <- jitter(data.Set3a$Northing)
# Redo the model with the new data.Set3a
Yld.lmeF2a <- update(Yld.lmeF2, data = data.Set3a)
var.lmeF2a <- Variogram(Yld.lmeF2a, form = ~ EAST2 + NORTH2)
# Check that the variogram hasn't changed visibly
plot(var.lmeF2a)

Yld.lmeF3 <- update(Yld.lmeF2a, correlation
  = corSpher(form = ~ EAST2 + NORTH2, nugget = TRUE))
anova(Yld.lmeF2, Yld.lmeF3)
summary(Yld.lmeF2)
VarCorr(Yld.lmeF2)

trellis.device(color = FALSE)
plot(augPred(Yld.lmeF2, as.formula("~Irrig"))) # Fig. 12.5

data.lis <- lmList(Yield ~ Irrig | Field, data = data.Set3a)
coef(data.lis)
coef(Yld.lmeF2)





