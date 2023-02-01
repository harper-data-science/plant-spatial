# 13.2 
library(spdep)
data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
yield.pts <- read.csv("created\\set4.1yld96ptsidw.csv")
data.Set4.1$Yield <- yield.pts$Yield
model.4 <- lm(Yield ~ Clay + SoilpH + I(Clay*SoilpH) +
   SoilP + I(Clay*SoilP) + Weeds,
   data = data.Set4.1)
coordinates(data.Set4.1) <- c("Easting", "Northing")
nlist <- dnearneigh(data.Set4.1, d1 = 0, d2 = 61)
W <- nb2listw(nlist, style = "W")
lm.morantest(model.4, W)  # p=0.03
model.3 <- lm(Yield ~ Clay + SoilpH + I(Clay*SoilpH) +
   Silt + Weeds , data = data.Set4.1)
lm.morantest(model.3, W)  # p=0.002
model.2 <- lm(Yield ~ Clay + SoilpH + I(Clay*SoilpH) +
   Silt + Disease + Weeds , data = data.Set4.1)
lm.morantest(model.2, W)  # p=0.001
model.1 <-  lm(Yield ~ Clay + SoilpH + I(Clay*SoilpH) +
   Silt + SoilP + I(Clay*SoilP) + Disease + Weeds , data = data.Set4.1)
lm.morantest(model.2, W)  # p=0.06

#13.3
data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
yield.pts <- read.csv("created\\set4.1yld96ptsidw.csv")
data.Set4.1$Yield <- scale(yield.pts$Yield)
library(spdep)
coordinates(data.Set4.1) <- c("Easting", "Northing")
nlist <- dnearneigh(data.Set4.1, d1 = 0, d2 = 61)
W <- nb2listw(nlist, style = "W")
model.1 <-  lm(Yield ~ Clay + SoilpH + I(Clay*SoilpH) +
   Silt + SoilP + I(Clay*SoilP) + Disease + Weeds , data = data.Set4.1)
lm.LMtests(model.1, W, test = "all")

# 13.3
library(spdep)
data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
yield.pts <- read.csv("created\\set4.1yld96ptsidw.csv")
data.Set4.1$Yield <- scale(yield.pts$Yield)
coordinates(data.Set4.1) <- c("Easting", "Northing")
nlist <- dnearneigh(data.Set4.1, d1 = 0, d2 = 61)
W <- nb2listw(nlist, style = "W")
formula.3 <- as.formula("Yield ~ Clay + SoilpH +
    SoilP + Weeds + I(Clay*SoilP) + I(Clay*SoilpH)")
formula.5 <- as.formula("Yield ~ Clay + SoilP + I(Clay*SoilP) +
   Weeds")
summary(lm(formula.3, data = data.Set4.1))
summary(lagsarlm(formula.3, data = data.Set4.1, listw = W))
summary(lm(formula.5, data = data.Set4.1))
summary(lagsarlm(formula.5, data = data.Set4.1, listw = W))

# 13.4
library(geoR)
coords <- expand.grid(1:20,20:1)
X <- grf(400, cbind(coords[,1], coords[,2]), cov.model = "exponential",
   xlims = c(0,20), ylims = c(0,20), cov.pars = c(1,2))
X$data
plot(variog(X))

# 13.5
library(geoR)
set.seed(123)
coords <- expand.grid(1:20,20:1)
X.grf <- grf(400, cbind(coords[,1], coords[,2]), cov.model = "exponential",
   xlims = c(0,20), ylims = c(0,20), cov.pars = c(1,2))
X <- X$data
p <- 1 / (1 + exp(-X))
Y <- rbinom(length(p), 1, p)
# GLM model
Y.glm <- glm(Y ~ X, family = "binomial")
# Compute the squared bias for the model
print (B.glm <- (sum(c(0,1) - coef(Y.glm)))^2)

