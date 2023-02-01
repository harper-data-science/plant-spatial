library(R2WinBUGS)
beta.true <- 1
set.seed(123)
n <- 20
X <- rnorm(n)
Y <- beta.true * X + rnorm(n)

demo.model <- function(){
beta ~ dnorm(0, 0.01)
tau ~ dgamma(0.01, 0.01)
for (i in 1:n)
  {
  Y.hat[i] <- beta * X[i]
  Y[i] ~ dnorm(Y.hat[i], tau)
  }
}

write.model(demo.model,
   paste(mybugsdir,"demomodel.bug", sep = ""))

XY.data <- list(X = X, Y = Y, n = n)
XY.inits <- function(){
   list(beta = rnorm(1), tau = exp(rnorm(1)))
}

demo.test <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"demomodel.bug", sep = ""),
   parameters = c("beta", "tau"), n.chains = 1,
   n.iter = 10, n.burnin = 2, debug = TRUE,
   bugs.directory=mybugsdir)

library(coda)
demo.coda <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"demomodel.bug", sep = ""),
   parameters = c("beta", "tau"), n.chains = 1,
   n.iter = 5000, n.burnin = 1000, codaPkg = TRUE,
   bugs.directory=mybugsdir)

demo.mcmc <- read.bugs(demo.coda)
# Select 2 here:
codamenu()

demo.sim <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"demomodel.bug", sep = ""),
   parameters = c("beta", "tau"), n.chains = 1,
   n.iter = 5000, n.burnin = 1000,
   bugs.directory=mybugsdir)
print(demo.sim, digits = 3)


demo.sim <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"demomodel.bug", sep = ""),
   parameters = c("beta", "tau"), n.chains = 5,
   n.iter = 5000, n.burnin = 1000,
   bugs.directory=mybugsdir)
print(demo.sim, digits = 3)

data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
yield.pts <- read.csv("created\\set4.1yld96ptsidw.csv", header = TRUE)
data.Set4.1$YieldN <- with(yield.pts, Yield / max(Yield))
data.Set4.1$ClayN <- with(data.Set4.1, Clay / max(Clay))
data.Set4.1$SoilKN <- with(data.Set4.1, SoilK / max(SoilK))

Set4.1.model <- function(){
beta0 ~ dnorm(0, 0.01)
beta1 ~ dnorm(0, 0.01)
beta2 ~ dnorm(0, 0.01)
tau ~ dgamma(0.01, 0.01)
for (i in 1:n)
  {
  Y.hat[i] <- beta0 + beta1 * Clay[i] + beta2 * SoilK[i]
  Yield[i] ~ dnorm(Y.hat[i], tau)
  }
}

write.model(Set4.1.model,
   paste(mybugsdir,"\\Set4.1model.bug", sep = ""))

XY.data <- list(Clay = data.Set4.1$ClayN, SoilK = data.Set4.1$SoilKN,
   Yield = data.Set4.1$YieldN, n = nrow(data.Set4.1))
XY.inits <- function(){
   list(beta0 = rnorm(1), beta1 = rnorm(1), beta2 = rnorm(1),
   tau = exp(rnorm(1)))
}

Set4.1.test <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set4.1model.bug", sep = ""),
   parameters = c("beta0", "beta1", "beta2", "tau"), n.chains = 1,
   n.iter = 10, n.burnin = 2, debug = TRUE,
   bugs.directory=mybugsdir)

library(coda)
Set4.1.coda <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set4.1model.bug", sep = ""),
   parameters = c("beta0", "beta1", "beta2", "tau"), n.chains = 5,
   n.iter = 5000, n.burnin = 500, codaPkg = TRUE,
   bugs.directory=mybugsdir)
Set4.1.mcmc <- read.bugs(Set4.1.coda)
# Select 2 here:
codamenu()

#Second try

Set4.1.coda <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set4.1model.bug", sep = ""),
   parameters = c("beta0", "beta1", "beta2", "tau"), n.chains = 5,
   n.iter = 10000, n.burnin = 1000, n.thin = 5, codaPkg = TRUE,
   bugs.directory=mybugsdir)
Set4.1.mcmc <- read.bugs(Set4.1.coda)
# Select 2 here:
codamenu()
 
Set4.1.sim <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set4.1model.bug", sep = ""),
   parameters = c("beta0", "beta1", "beta2", "tau"), n.chains = 5,
   n.iter = 10000, n.burnin = 1000, n.thin = 5,
   bugs.directory=mybugsdir)
print(Set4.1.sim, digits = 3)

Set4.1.lm <- lm(YieldN ~ ClayN + SoilKN, data = data.Set4.1)
coef(Set4.1.lm)
t(Set4.1.sim$mean)

1 / (sum(residuals(Set4.1.lm)^2) / (nrow(data.Set4.1) - 3))
