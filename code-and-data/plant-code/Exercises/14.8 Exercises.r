# 14.1
library(R2WinBUGS)
set.seed(123)
n <- 20
print(X <- rnorm(n))
print(Y <- X + 0.1*rnorm(n))

Ex1.model <- function(){
beta0 ~ dnorm(0, 0.01)
beta1 ~ dnorm(0, 0.01)
tau ~ dnorm(1, 0.01)
for (i in 1:n)
  {
  Y.hat[i] <- beta0 + beta1 * X[i]
  Y[i] ~ dnorm(Y.hat[i], tau)
  }
}

write.model(Ex1.model,
   paste(mybugsdir,"\\Ex1model.bug", sep = ""))

XY.data <- list(X = X, Y = Y, n = n)
XY.inits <- function(){
   list(beta0 = rnorm(1), beta1 = rnorm(1), tau = exp(rnorm(1)))
}

demo.test <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Ex1model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 1,
   n.iter = 10, n.burnin = 2, debug = TRUE,
   bugs.directory=mybugsdir)

library(coda)
demo.coda <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Ex1model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 1,
   n.iter = 5000, n.burnin = 1000, codaPkg = TRUE,
   bugs.directory=mybugsdir)
demo.mcmc <- read.bugs(demo.coda)
codamenu()

demo.sim <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Ex1model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 1,
   n.iter = 5000, n.burnin = 1000,
   bugs.directory=mybugsdir)
print(demo.sim, digits = 3)

# 14.2
n <- 100
set.seed(123)
XA <- cbind(rnorm(100), rnorm(100))
eps <- rnorm(100)
YA <- rowSums(XA) + 0.1 * eps
summary(lm(YA ~ XA))
XB <- cbind(XA[,1], XA[,1] + 0.0001 * XA[,2])
YB <- rowSums(XB) + 0.1 * eps
summary(lm(YB ~ XB))

Ex2.model <- function(){
beta0 ~ dnorm(0, 0.01)
beta1 ~ dnorm(0, 0.01)
beta2 ~ dnorm(0, 0.01)
tau ~ dnorm(1, 0.01)
for (i in 1:n)
  {
  Y.hat[i] <- beta0 + beta1 * X1[i] + beta2 * X2[i]
  Y[i] ~ dnorm(Y.hat[i], tau)
  }
}

write.model(Ex2.model,
   paste(mybugsdir,"\\Ex2model.bug", sep = ""))
# part a
XY.data <- list(X1 = XA[,1], X2 = XA[,2], Y = YA, n = n)
XY.inits <- function(){
   list(beta0 = rnorm(1), beta1 = rnorm(1), beta2 = rnorm(1),
   tau = exp(rnorm(1)))
}

demo.test <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Ex2model.bug", sep = ""),
   parameters = c("beta0", "beta1", "beta2", "tau"), n.chains = 1,
   n.iter = 10, n.burnin = 2, debug = TRUE,
   bugs.directory=mybugsdir)


demo.sim <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Ex2model.bug", sep = ""),
   parameters = c("beta0", "beta1", "beta2", "tau"), n.chains = 5,
   n.iter = 5000, n.burnin = 1000,
   bugs.directory=mybugsdir)
print(demo.sim, digits = 3)

# part b
XY.data <- list(X1 = XB[,1], X2 = XB[,2], Y = YA, n = n)
XY.inits <- function(){
   list(beta0 = rnorm(1), beta1 = rnorm(1), beta2 = rnorm(1),
   tau = exp(rnorm(1)))
}

demo.sim <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Ex2model.bug", sep = ""),
   parameters = c("beta0", "beta1", "beta2", "tau"), n.chains = 5,
   n.iter = 5000, n.burnin = 1000,
   bugs.directory=mybugsdir)
print(demo.sim, digits = 3)

# 14.3
library(R2WinBUGS)
data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
yield.pts <- read.csv("created\\set4.1yld96ptsidw.csv", header = TRUE)
data.Set4.1$YieldN <- with(yield.pts, Yield / max(Yield))
data.Set4.1$ClayN <- with(data.Set4.1, Clay / max(Clay))
data.Set4.1$SoilPN <- with(data.Set4.1, SoilP / max(SoilP))
data.Set4.1$WeedsN <- with(data.Set4.1, Weeds / max(Weeds))

Set4.1.model <- function(){
beta0 ~ dnorm(0, 0.01)
beta1 ~ dnorm(0, 0.01)
beta2 ~ dnorm(0, 0.01)
beta3 ~ dnorm(0, 0.01)
beta4 ~ dnorm(0, 0.01)
tau ~ dgamma(0.01, 0.01)
for (i in 1:n)
  {
  Y.hat[i] <- beta0 + beta1 * Clay[i] + beta2 * SoilP[i] +
     beta3 * SoilP[i] * Clay[i] + beta4 * Weeds[i]
  Yield[i] ~ dnorm(Y.hat[i], tau)
  }
}

write.model(Set4.1.model,
   paste(mybugsdir,"\\Set4.1model.bug", sep = ""))

XY.data <- list(Clay = data.Set4.1$ClayN, SoilP = data.Set4.1$SoilPN,
   Weeds = data.Set4.1$WeedsN, Yield = data.Set4.1$YieldN,
   n = nrow(data.Set4.1))
XY.inits <- function(){
   list(beta0 = rnorm(1), beta1 = rnorm(1), beta2 = rnorm(1),
   beta3 = rnorm(1), beta4 = rnorm(1), tau = exp(rnorm(1)))
}

Set4.1.test <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set4.1model.bug", sep = ""),
   parameters = c("beta0", "beta1", "beta2", "beta3", "beta4",
   "tau"), n.chains = 1, n.iter = 10, n.burnin = 2, debug = TRUE,
   bugs.directory=mybugsdir)

library(coda)
Set4.1.coda <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set4.1model.bug", sep = ""),
   parameters = c("beta0","beta1","beta2","beta3","beta4","tau"), n.chains=5,
   n.iter = 5000, n.burnin = 500, codaPkg = TRUE,
   bugs.directory=mybugsdir)
Set4.1.mcmc <- read.bugs(Set4.1.coda)
# Select 2 here:
codamenu()

#Second try

library(coda)
Set4.1.coda <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set4.1model.bug", sep = ""),
   parameters = c("beta0","beta1","beta2","beta3","beta4","tau"), n.chains=5,
   n.iter = 10000, n.burnin = 1000, codaPkg = TRUE,
   bugs.directory=mybugsdir)
Set4.1.mcmc <- read.bugs(Set4.1.coda)
codamenu()

Set4.1.mcmc <- read.bugs(Set4.1.coda)
# Select 2 here:
codamenu()


Set4.1.sim <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set4.1model.bug", sep = ""),
   parameters = c("beta0","beta1","beta2","beta3","beta4","tau"), n.chains=5,
   n.iter = 10000, n.burnin = 1000,
   bugs.directory=mybugsdir)
print(Set4.1.sim, digits = 3)

Set4.1.lm <- lm(YieldN ~ ClayN + SoilPN +
  I(ClayN * SoilPN) + WeedsN, data = data.Set4.1)
coef(Set4.1.lm)
t(Set4.1.sim$mean)



#14.4

library(maptools)
data.Set2S <- readShapePoints("created\\set2sierra")

# Run the standard GLM for comparison
glm.Set2S <- glm(QUDO ~ Precip + MAT + SolRad + AWCAvg +
   Permeab , data = data.Set2S, family = binomial)


# Now set up the WinBUGS analysis
library(R2WinBUGS)
Set2S.model <- function(){
beta0 ~ dnorm(0, 0.01)
betaPre ~ dnorm(0, 0.01)
betaMAT ~ dnorm(0, 0.01)
betaSol ~ dnorm(0, 0.01)
betaAWC ~ dnorm(0, 0.01)
betaPerm ~ dnorm(0, 0.01)
for (i in 1:n)
  {
  logit(p.QUDO[i]) <- beta0 + betaPre*Precip[i] +
     betaMAT*MAT[i] + betaSol*SolRad[i] +
     betaAWC*AWCAvg[i] + betaPerm*Permeab[i]
  QUDO[i] ~ dbin(p.bound[i], 1)
  p.bound[i] <- max(0, min(1, p.QUDO[i]))
  }
}

XY.data <- list(QUDO = data.Set2S@data$QUDO,
 Precip = data.Set2S@data$Precip, MAT = data.Set2S@data$MAT,
 SolRad = data.Set2S@data$SolRad, AWCAvg = data.Set2S@data$AWCAvg,
 Permeab = data.Set2S@data$Permeab, n = nrow(data.Set2S))
XY.inits <- list(list(beta0 = 0, betaPre = 0,
   betaMAT = 0, betaSol = 0, betaAWC = 0, betaPerm = 0))

write.model(Set2S.model,
   paste(mybugsdir,"\\demoglm.bug", sep = ""))


Set2S.sim <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\demoglm.bug", sep = ""),
   parameters = c("beta0", "betaPre", "betaMAT", "betaSol",
   "betaAWC", "betaPerm"), n.chains = 1,
   n.iter = 5000, n.burnin = 1000,
   bugs.directory=mybugsdir)

print(Set2S.sim)
plot(Set2S.sim)

coef(glm.Set2S)
t(Set2S.sim$mean)

#14.5
library(R2WinBUGS)
data.Set3 <- read.csv("Set3\\Set3data.csv", header = TRUE)
data.Set3$YieldN <- data.Set3$Yield / max(data.Set3$Yield)

n.F <- length(unique(data.Set3$Field))

Set3.model <- function(){
for (i in 1:n.F){
   beta0[i] ~ dnorm(mu.0, tau.0)
   beta1[i] ~ dnorm(mu.1, tau.1)
}
mu.0 ~ dnorm(0, 0.001)
tau.0 ~ dgamma(0.01, 0.01)
mu.1 ~ dnorm(0, 0.01)
tau.1 ~ dgamma(0.01, 0.001)
tau ~ dgamma(0.01, 0.01)
for (i in 1:n)
  {
  Y.hat[i] <- beta0[Field[i]] + beta1[Field[i]]*Irrig[i]
  Yield[i] ~ dnorm(Y.hat[i], tau)
  }
}

write.model(Set3.model,
   paste(mybugsdir,"\\Set3model.bug", sep = ""))  

XY.data <- list(Irrig = data.Set3$Irrig, Yield = data.Set3$YieldN,
   n = nrow(data.Set3), n.F = length(unique(data.Set3$Field)),
   Field = data.Set3$Field)
XY.inits <- function(){
   list(beta0 = rnorm(n.F), beta1 = rnorm(n.F), tau = exp(rnorm(1)),
   mu.0 = rnorm(1), tau.0 = runif(1), mu.1 = rnorm(1),
   tau.1 = exp(rnorm(1)))}

Set3.test <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau", "mu.0", "tau.0", "mu.1", "tau.1"), n.chains = 5,
   n.iter = 10, n.burnin = 2, debug = TRUE,
   bugs.directory=mybugsdir)

Set3.sim <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau", "mu.0", "tau.0", "mu.1", "tau.1"), n.chains = 5,
   n.iter = 10000, n.burnin = 2000, bugs.directory=mybugsdir)

print(Set3.sim)
param.est <- t(unlist(Set3.sim$mean))
print(b0 <- mean(param.est[1:15]))
print(b1 <- mean(param.est[16:30]))



# 14.6
library(spdep)
lambda <- 0
nlist <- cell2nb(10,10)
IrWinv <- invIrM(nlist, lambda)
set.seed(123)
X <- rnorm(100)
eps <- as.numeric(IrWinv %*% rnorm(100))
sig2 <- 0.1
Y <- X + sig2*eps
W <- nb2listw(nlist, style = "B")
Y.SAR <- spautolm(Y ~ X, listw = W)
print(coef(Y.SAR), digits = 3)
Y.CAR <- spautolm(Y ~ X, listw = W, family = "CAR")
print(coef(Y.CAR), digits = 3)

demo.model <- function(){
beta0 ~ dnorm(0, 0.01)
beta1 ~ dnorm(0, 0.01)
tau ~ dgamma(0.01, 0.01)
for (i in 1:100)
  {
  Y.hat[i] <- beta0 + beta1*X[i]
  Y[i] ~ dnorm(Y.hat[i], tau)
  }
}
write.model(demo.model,
   paste(mybugsdir,"\\demomodel.bug", sep = ""))

data.XY <- data.frame(X = X, Y = Y)
XY.data <- list(X = data.XY$X, Y = data.XY$Y)
XY.inits <- function(){
   list(beta0 = rnorm(1), beta1 = rnorm(1), tau = exp(rnorm(1)))}

demo.sim <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\demomodel.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 5,
   n.iter = 10000, n.burnin = 1000, n.thin = 10,
   bugs.directory=mybugsdir)
print(demo.sim, digits = 2)

# CAR model
W.WB <- nb2WB(nlist)
demo.model <- function(){
beta0 ~ dnorm(0, 0.01)
beta1 ~ dnorm(0, 0.01)
tau ~ dgamma(0.01, 0.01)
# This line is modified.
v[1:n] ~ car.normal(adj[], weights[], num[], tau)
for (i in 1:n)
  {
# This line is modified.
  Y.hat[i] <- beta0 + beta1*X[i] + v[i]
  Y[i] ~ dnorm(Y.hat[i], tau)
  }
}

write.model(demo.model,
   paste(mybugsdir,"\\demomodel.bug", sep = ""))

XY.data <- list(X = data.XY$X, Y = data.XY$Y,
  adj = W.WB$adj, weights = W.WB$weights,
  num = W.WB$num, n = nrow(data.XY))
XY.inits <- function(){
   list(beta0 = rnorm(1), beta1 = rnorm(1), tau = exp(rnorm(1)),
   v = rep(0.01,nrow(data.XY)))}


coda.file <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\demomodel.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau", "v"), n.chains = 5,
   n.iter = 5000, n.burnin = 1000, n.thin = 10, codaPkg = TRUE,
   bugs.directory=mybugsdir)
demo.mcmc <- read.bugs(coda.file)
# Select 2 here:
codamenu()

demo.sim <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\demomodel.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau", "v"), n.chains = 5,
   n.iter = 10000, n.burnin = 1000, n.thin = 10,
   bugs.directory=mybugsdir)
print(demo.sim, digits = 2)

#14.7
library(spBayes)
data.Set4.1 <- read.csv("Set4\\Set4.196sample.csv", header = TRUE)
data.Yield4.1idw <- read.csv("created\\set4.1yld96ptsidw.csv", header = TRUE)
data.Set4.1$Yield <-  data.Yield4.1idw$Yield
model.start <- lm(Yield ~ Clay, data = data.Set4.1)
coords.1 <- as.matrix(cbind(data.Set4.1$Easting, data.Set4.1$Northing), nrow <- nrow(d), ncol = 2)
sigma2.ini <- 0.5
tau2.ini <- 0.05
phi.ini <- 0.0015
n.samples <- 50000

beta.ini <- as.numeric(coef(model.start))

# a)
model.lm <- spLM(Yield ~ Clay, data = data.Set4.1,
   coords=coords.1,starting=list("phi"=phi.ini,"sigma.sq"=sigma2.ini, "tau.sq"=tau2.ini,"beta"=beta.ini),
   tuning=list("phi"=0.005, "sigma.sq"=0.1, "tau.sq"=0.05),
   priors=list("phi.Unif"=c(0.001, 0.01), "sigma.sq.IG"=c(2, 0.4),
   "tau.sq.IG"=c(2, 0.1),"beta.Flat"), cov.model="exponential",
   n.samples=n.samples, verbose=FALSE)


names(model.lm)
burn.in <- 0.5*n.samples
plot(window(model.lm$p.theta.samples, start = burn.in))
summary(window(model.lm$p.theta.samples, start = burn.in))

# b)
model.lm.params <- spRecover(model.lm, start=burn.in, verbose=FALSE)
names(model.lm.params)

plot(window(model.lm.params$p.theta.samples, start = burn.in))
summary(model.lm.params$p.beta.recover.samples)
summary(model.lm.params$p.beta.recover.samples)$quantiles[,3]


# c)
mod.1 <- lm(Yield ~ Clay, data = data.Set4.1)
plot(data.Set4.1$Clay, data.Set4.1$Yield)
abline(as.numeric(coef(mod.1)))
abline(as.numeric(summary(model.lm.params$p.beta.recover.samples)$quantiles[,3]), lty = 2)



