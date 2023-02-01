library(maptools)
library(sf)
library(splines)
library(mgcv)

coast.sf <- st_read("created\\set2coast.shp")
coast.cor <- coast.sf[-which((coast.sf$Elevation >= 1800) &
                               (coast.sf$Elevation <= 2000)),]
model.glm1 <- glm(QUDO ~ Elevation + I(Elevation^2), data = coast.cor,
    family = binomial)
summary(model.glm1)
model.glm2 <- glm(QUDO ~ Elevation + I(Elevation^2) +I(Elevation^3), data = coast.cor,
    family = binomial)

n.oaks <- function(var.name, oak, low, high, PresAbs){
  length(which(var.name >= low & var.name < high & oak == PresAbs))}

pres <- numeric(22)
absent <- numeric(22)
for(i in 0:22) pres[i] <- with(coast.cor, n.oaks(Elevation, QUDO, i*100,(i+1)*100, 1))
for(i in 0:22) absent[i] <- with(coast.cor,
                                 n.oaks(Elevation, QUDO, i*100,(i+1)*100, 0))
pa.coast <- pres / (pres + absent + 0.00001)
x <- seq(100,2200,100)
par(mai = c(1,1,1,1))
plot(x, pa.coast, type = "o", xlab = "Elevation (m)",  # Fig. 9.1
     ylab = "Portion with Blue Oak", cex.lab = 1.5,
     main = "Blue Oak Presence vs. Elevation",
     cex.main = 2, lty = 0) 
elev.seq <- data.frame(Elevation = seq(0,2000,50))
lines(seq(0,2000,50), predict(model.glm1, elev.seq, type = "response"))
lines(seq(0,2000,50), predict(model.glm2, elev.seq, type = "response"), lty = 2)
legend(1500, 0.5, c("Quadratic", "Cubic"), lty = 1:2,
       title = "Polynomial")

x <- c(1, 1.5, 2, 4.1, 5)
y <- c(1, -1, 1, -1, 1)
s <- spline(x, y, n = 81, method = "natural")
s
plot(x, y, xlab = "x", ylab = "y", ylim = c(-1,2)) # Fig. 9.2
lines(s$x, s$y, lty = 1)

ID <- 1:nrow(coast.sf)
data.plot <- coast.sf[which(ID %% 15 == 0),]
# Remove a set of anomalous data values
data.plot <- data.plot[-which((data.plot$JuMin > 11.111) & (data.plot$JuMin < 11.12)),]
with(data.plot, plot(JuMin, JaMean, xlab = "June Minimum Temperature", ylab = "Jan Mean Temperature"))
spl <- lm(JaMean ~ bs(JuMin, df = 4), data = data.plot)  #Fig. 9.3a
x <- seq(min(data.plot$JuMin), max(data.plot$JuMin), length.out = 100)
lines(x, predict(spl, data.frame(JuMin = x)))
v1 <- attr(terms(spl), "predvars")[[3]]
v1
x = c(min(data.plot$JuMin),11.577775,max(data.plot$JuMin))
points(x = x, y = predict(spl, data.frame(JuMin = x)), pch = 19) 

with(data.plot, plot(JuMin, JaMean, xlab = "June Minimum Temperature", ylab = "Jan Mean Temperature"))
spl <- lm(JaMean ~ bs(JuMin, df = 7), data = data.plot)  #Fig. 9.3b
x <- seq(min(data.plot$JuMin), max(data.plot$JuMin), length.out = 100)
lines(x, predict(spl, data.frame(JuMin = x)))
v1 <- attr(terms(spl), "predvars")[[3]]
v1
x = c(min(data.plot$JuMin),10.163332, 11.29444, 11.925552, 13.336664,max(data.plot$JuMin))
points(x = x, y = predict(spl, data.frame(JuMin = x)), pch = 19) 

with(data.plot, plot(JuMin, JaMean, xlab = "June Minimum Temperature", ylab = "Jan Mean Temperature"))
spl <- lm(JaMean ~ bs(JuMin, df = 10), data = data.plot)  #Fig. 9.3c
x <- seq(min(data.plot$JuMin), max(data.plot$JuMin), length.out = 100)
lines(x, predict(spl, data.frame(JuMin = x)))
v1 <- attr(terms(spl), "predvars")[[3]]
v1
x = c(min(data.plot$JuMin),c(9.85486, 10.4388875, 11.2395825, 11.577775,
    11.94444, 12.77222, 14.3861125),max(data.plot$JuMin))
points(x = x, y = predict(spl, data.frame(JuMin = x)), pch = 19) 

spl.sm <- with(data.plot, smooth.spline(JuMin, JaMean))
round(spl.sm$df,0)

model.glm1
model.gam1 <- gam(QUDO ~ s(Elevation, bs = "cr", k = 8), data = coast.cor, family = binomial)
model.gam2 <- gam(QUDO ~ s(Elevation, bs = "cr", k = 9), data = coast.cor, family = binomial)
summary(model.gam1)

pres <- numeric(22)
absent <- numeric(22)
for(i in 0:22) pres[i] <- with(coast.cor, n.oaks(Elevation, QUDO, i*100,(i+1)*100, 1))
for(i in 0:22) absent[i] <- with(coast.cor,
                                 n.oaks(Elevation, QUDO, i*100,(i+1)*100, 0))
pa.coast <- pres / (pres + absent + 0.00001)
x <- seq(100,2200,100)
par(mai = c(1,1,1,1))
plot(x, pa.coast, type = "o", xlab = "Elevation (m)",  # Fig. 9.1
     ylab = "Portion with Blue Oak", cex.lab = 1.5,
     main = "Blue Oak Presence vs. Elevation",
     cex.main = 2, lty = 0) 
elev.seq <- data.frame(Elevation = seq(0,2000,50))
lines(seq(0,2000,50), predict(model.glm1, elev.seq, type = "response"))
lines(seq(0,2000,50), predict(model.gam1, elev.seq, type = "response"), lty = 2)
lines(seq(0,2000,50), predict(model.gam2, elev.seq, type = "response"), lty = 3)
legend(1500, 0.5, c("GLM", "6 Knots", "7 Knots"), lty = 1:3,
       title = "Type of Fit")

sierra.sf <- st_read("created\\set2sierra.shp")
model.glmS2 <- glm(QUDO ~ MAT + Precip, data = sierra.sf, family = binomial)
model.gamS2 <- gam(QUDO ~ s(MAT, bs = "cr", k = 9) + s(Precip, bs = "cr", k = 9), 
                 data = sierra.sf, family = binomial)

# This should be considered as only a rough approximation
anova(model.glmS2, model.gamS2, test = "Chisq")

hist(residuals(model.glmS2), main = "", xlab = "GLM Residuals") #Fig. 9.5a
hist(residuals(model.gamS2), main = "", xlab = "GAM Residuals") #Fig. 9.5b

min(sierra.sf$Precip)
max(sierra.sf$Precip)
length(which(sierra.sf$Precip > 900 & sierra.sf$Precip < 1100))
# Approximate cross section of Precip
sierra.xsec <- sierra.sf[which(sierra.sf$Precip > 900 & sierra.sf$Precip < 1100),]
sierra.xsec$Precip <- 1000

min(sierra.sf$MAT)
max(sierra.sf$MAT)

pres <- numeric(13)
absent <- numeric(13)
for(i in 5:17) pres[i-4] <- with(sierra.xsec, n.oaks(MAT, QUDO, i,i + 1, 1))
for(i in 5:17) absent[i-4] <- with(sierra.xsec, n.oaks(MAT, QUDO, i,i + 1, 0))
pa.xsec <- pres / (pres + absent + 0.00001)
x <- seq(5,17)
par(mai = c(1,1,1,1))
plot(x, pa.xsec, type = "o", xlab = "Mean Annual Temperature",  # Fig. 9.5
     ylab = "Portion with Blue Oak", cex.lab = 1.5,
     main = "QUDO vs. MAT at 1000 mm Precip",
     cex.main = 2, lty = 0) 
x.seq <- data.frame(MAT = seq(5,18,0.5), Precip = 1000)
lines(seq(5,18,0.5), predict(model.glmS2, x.seq, type = "response"))
lines(seq(5,18,0.5), predict(model.gamS2, x.seq, type = "response"), lty = 2)
legend(6, 0.4, c("GLM", "GAM"), lty = 1:2, title = "Model")

