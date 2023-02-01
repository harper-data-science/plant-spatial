data.Set4.1 <- read.csv("Set4\\Set4.196sample.csv", header = TRUE)
yield.pts <- read.csv("created\\Set4.1yld96ptsidw.csv")
data.Set4.1$Yield <- yield.pts$Yield

model.5lm <- lm(Yield ~ Clay + SoilP + I(Clay*SoilP) + Weeds,
   data = data.Set4.1)
summary(model.5lm)

library(nlme)
model.5gls1 <- gls(Yield ~ Clay + SoilP + I(Clay*SoilP) +
   Weeds, data = data.Set4.1)

library(lattice)
trellis.device(color = FALSE)
plot(Variogram(model.5gls1, form = ~ Easting + Northing,
   maxDist = 300), xlim = c(0,300),
   main = "Variogram of Residuals, model.5, Field 4.1") # Fig. 12.6
   
model.5gls2 <- update(model.5gls1,
   corr = corSpher(form = ~ Easting + Northing, nugget = TRUE))
anova(model.5gls1, model.5gls2)
plot(model.5gls2, resid(., type = "n") ~ fitted(.),
   abline = 0)
qqnorm(model.5gls2, ~ resid(., type = "n"))
summary(model.5gls1)
summary(model.5gls2)

