library(R2WinBUGS)
library(spdep)

data.Set3 <- read.csv("Set3\\Set3data.csv", header = TRUE)
tapply(data.Set3$Irrig, data.Set3$Field, unique)
data.Set3a <- data.Set3[-which(data.Set3$Field == 2),]

data.Set3a$YieldN <- data.Set3a$Yield / max(data.Set3a$Yield)


coordinates(data.Set3a) <- c("Easting", "Northing")
nlist <- dnearneigh(data.Set3a, d1 = 0, d2 = 600)
min(card(nlist))
max(card(nlist))
W.WB <- nb2WB(nlist)

# First WinBUGS model (pooled data)
# Same as 14.5.1

Set3.model <- function(){
beta0 ~ dnorm(0, 0.01)
beta1 ~ dnorm(0, 0.01)
tau ~ dgamma(0.01, 0.01)
v[1:n] ~ car.normal(adj[], weights[], num[], tau)
for (i in 1:n)
  {
  Y.hat[i] <- beta0 + beta1*X[i] + v[i]
  Y[i] ~ dnorm(Y.hat[i], tau)
  }
}

write.model(Set3.model,
   paste(mybugsdir,"\\Set3model.bug", sep = ""))

data.XY <- data.Set3a
XY.data <- list(X = data.XY$Irrig, Y = data.XY$YieldN,
  adj = W.WB$adj, weights = W.WB$weights, num = W.WB$num,
  n = nrow(data.XY))
XY.inits <- function(){
   list(beta0 = rnorm(1), beta1 = rnorm(1), tau = exp(rnorm(1)),
   v = rep(0.01,nrow(data.XY)))}
   
test.file <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau", "v"), n.chains = 1,
   n.iter = 10, n.burnin = 2, debug = TRUE,
   bugs.directory = mybugsdir)

coda.file <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau", "v"), n.chains = 5,
   n.iter = 5000, n.burnin = 1000, n.thin = 10, codaPkg = TRUE,
   bugs.directory = mybugsdir)

Set3.mcmc <- read.bugs(coda.file)
# Select 2 here:
codamenu()

Set3.sim <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau", "v"), n.chains = 5,
   n.iter = 5000, n.burnin = 1000, n.thin = 10,
   bugs.directory = mybugsdir)
print(Set3.sim, digits = 2)

Y.lm <- lm(YieldN ~ Irrig, data = data.Set3a)
W.B <- nb2listw(nlist, style = "B")
Y.SAR <- spautolm(YieldN ~ Irrig,
   data = data.Set3a, listw = W.B)
Y.CAR <- spautolm(YieldN ~ Irrig,
   data = data.Set3a, family = "CAR", listw = W.B)
print(coef(Y.lm), digits = 3)
print(coef(Y.SAR), digits = 3)
print(coef(Y.CAR), digits = 3)
print(t(Set3.sim$mean[1:3]), digits = 3)

# Multilevel intercept

data.Set3a$Field[which(data.Set3a$Field == 16)] <- 2

Set3.model <- function(){
# Priors for beta0
for (i in 1:n.F){
   beta0[i] ~ dnorm(mu.0, tau.0)
}
# Parameters for the priors
mu.0 ~ dnorm(0, 0.001)
tau.0 ~ dgamma(0.01, 0.01)
beta1 ~ dnorm(0, 0.001)
tau ~ dgamma(0.01, 0.01)
# This line is modified.
v[1:n] ~ car.normal(adj[], weights[], num[], tau)
for (i in 1:n)
  {
  Y.hat[i] <- beta0[Field[i]] + beta1*Irrig[i] + v[i]
  Yield[i] ~ dnorm(Y.hat[i], tau)
  }
}

write.model(Set3.model,
   paste(mybugsdir,"\\Set3model.bug", sep = ""))

XY.data <- list(Irrig = data.Set3a$Irrig, Yield = data.Set3a$YieldN,
   n = nrow(data.Set3a), n.F = length(unique(data.Set3a$Field)),
   adj = W.WB$adj, weights = W.WB$weights,
   num = W.WB$num, n = nrow(data.Set3a), Field = data.Set3a$Field)
n.F <- length(unique(data.Set3a$Field))
XY.inits <- function(){
   list(beta0 = rnorm(n.F), beta1 = rnorm(1), tau = exp(rnorm(1)),
   mu.0 = rnorm(1), tau.0 = exp(rnorm(1)),
   v = rep(0.01,nrow(data.Set3a)))}

Set3.test <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau", "mu.0", "tau.0", "v"), n.chains = 1,
   n.iter = 10, n.burnin = 2, debug = TRUE,
   bugs.directory = mybugsdir)


Set3.coda <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau", "mu.0", "tau.0", "v"), n.chains = 5,
   n.iter = 10000, n.burnin = 2000, n.thin = 10, codaPkg = TRUE,
   bugs.directory = mybugsdir)
Set3.mcmc <- read.bugs(Set3.coda)
# Select 2 here:
codamenu()

# Second try
Set3.coda <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau", "mu.0", "tau.0", "v"), n.chains = 5,
   n.iter = 20000, n.burnin = 10000, n.thin = 20, codaPkg = TRUE,
   bugs.directory = mybugsdir)
Set3.mcmc <- read.bugs(Set3.coda)
# Select 2 here:
codamenu()

Set3.sim <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau", "mu.0", "tau.0", "v"), n.chains = 5,
   n.iter = 20000, n.burnin = 10000, n.thin = 20,
   bugs.directory = mybugsdir)
print(Set3.sim)  
# Small n.eff

Set3.sim <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau", "mu.0", "tau.0", "v"), n.chains = 5,
   n.iter = 30000, n.burnin = 20000, n.thin = 25,
   bugs.directory = mybugsdir)
print(Set3.sim, digits = 2)

library(nlme)
print(t(coef(lme(YieldN ~ Irrig, data = data.Set3a,
   random = ~ 1 | Field)))[1,1:5], digits = 3)
print(t(unlist(Set3.sim$mean))[1:5], digits = 3)

