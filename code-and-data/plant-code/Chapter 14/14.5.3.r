library(spBayes)
library(sf)

data.Set2S.sf <- st_read("created\\set2sierra.shp")
data.Set2S.sf$ElevN <-
  data.Set2S.sf$Elevation / max(data.Set2S.sf$Elevation)
data.Set2S.sf$x <- scale(data.Set2S.sf$Longitude)
data.Set2S.sf$y <- scale(data.Set2S.sf$Latitude)

ID <- 1:nrow(data.Set2S.sf)
mod.fac <- 10
data.Set2Sr.sf <- data.Set2S.sf[which(ID %% mod.fac == 0),]
nrow(data.Set2Sr.sf)

glm.small <- glm(QUDO ~ ElevN, data = data.Set2Sr.sf, family = binomial)
coef(glm.small)

beta.start <- as.numeric(coef(glm.small))
beta.tuning <- t(chol(vcov(glm.small)))
Set2Sr.coords <- as.matrix(data.Set2Sr.sf[,c("x","y")])[,1:2]

n.samples <- 100000
model.glm.bayes <- spGLM(QUDO ~ ElevN, data = data.Set2Sr.sf, family = "binomial", coords = Set2Sr.coords,
   starting = list("beta" = beta.start, "phi"=0.05,"sigma.sq"=0.1,"w" = 1),
   tuning = list("beta" = beta.tuning, "phi"=0.05, "sigma.sq"=0.1, "w"=0.01),
   priors=list("beta.Flat", "phi.Unif"=c(0.01, 0.50), "sigma.sq.IG"=c(2, 1)), 
   cov.model = "exponential", verbose = FALSE, n.samples = n.samples ) 

names(model.glm.bayes)
str(model.glm.bayes)

burn.in <- 0.5 * n.samples
plot(window(model.glm.bayes$p.beta.theta.samples, start = burn.in))
summary(window(model.glm.bayes$p.beta.theta.samples, start = burn.in))

