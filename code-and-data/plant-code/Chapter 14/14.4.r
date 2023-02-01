library(R2WinBUGS)

data.Set3 <- read.csv("Set3\\Set3data.csv", header = TRUE)
tapply(data.Set3$Irrig, data.Set3$Field, unique)
data.Set3a <- data.Set3[-which(data.Set3$Field == 2),]

data.Set3a$YieldN <- data.Set3a$Yield / max(data.Set3a$Yield)

# First WinBUGS model (pooled data)
Set3.model <- function(){
beta0 ~ dnorm(0, 0.01)
beta1 ~ dnorm(0, 0.01)
tau ~ dgamma(0.01, 0.01)
for (i in 1:n)
  {
  Y.hat[i] <- beta0 + beta1*Irrig[i]
  Yield[i] ~ dnorm(Y.hat[i], tau)
  }
}

write.model(Set3.model,
   paste(mybugsdir,"\\Set3model.bug", sep = ""))

XY.data <- list(Irrig = data.Set3a$Irrig, Yield = data.Set3a$YieldN,
   n = nrow(data.Set3a))
XY.inits <- function(){
   list(beta0 = rnorm(1), beta1 = rnorm(1), tau = exp(rnorm(1)))}

Set3.test <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 1,
   n.iter = 10, debug = TRUE,
   bugs.directory=mybugsdir)

Set3.coda <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 5,
   n.iter = 1000, n.burnin = 100,  codaPkg  = TRUE,
   bugs.directory=mybugsdir)
Set3.mcmc <- read.bugs(Set3.coda)
# Select 2 here:
codamenu()

# Second try
Set3.coda <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 5,
   n.iter = 5000, n.burnin = 500, n.thin = 5, codaPkg  = TRUE,
   bugs.directory=mybugsdir)
Set3.mcmc <- read.bugs(Set3.coda)
# Select 2 here:
codamenu()


Set3.sim <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 5,
   n.iter = 5000, n.burnin = 500,
   bugs.directory=mybugsdir)
print(Set3.sim, digits = 2)

print(coef(lm(YieldN ~ Irrig, data = data.Set3a)),
   digits = 3)

# Multilevel intercept

data.Set3a$Field[which(data.Set3a$Field == 16)] <- 2
n.F <- length(unique(data.Set3a$Field))

Set3.model2 <- function(){
# Priors for beta0
for (i in 1:n.F){
   beta0[i] ~ dnorm(mu.0, tau.0)
}
# Parameters for the priors
mu.0 ~ dnorm(0, 0.001)
tau.0 ~ dgamma(0.01, 0.01)
beta1 ~ dnorm(0, 0.001)
tau ~ dgamma(0.01, 0.01)
for (i in 1:n)
  {
  Y.hat[i] <- beta0[Field[i]] + beta1*Irrig[i]
  Yield[i] ~ dnorm(Y.hat[i], tau)
  }
}

write.model(Set3.model2,
   paste(mybugsdir,"\\Set3model.bug", sep = ""))

XY.data2 <- list(Irrig = data.Set3a$Irrig, Yield = data.Set3a$YieldN,
   n = nrow(data.Set3a), n.F = length(unique(data.Set3a$Field)),
   Field = data.Set3a$Field)
XY.inits2 <- function(){
   list(beta0 = rnorm(n.F), beta1 = rnorm(1), tau = exp(rnorm(1)),
   mu.0 = rnorm(1), tau.0 = exp(rnorm(1)))}
   
Set3.test2 <- bugs(data = XY.data2, inits = XY.inits2,
   model.file = paste(mybugsdir,"\\Set3model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau", "mu.0", "tau.0"), n.chains = 1,
   n.iter = 10, n.burnin = 2, debug = TRUE,
   bugs.directory=mybugsdir)
   
Set3.coda2 <- bugs(data = XY.data2, inits = XY.inits2,
   model.file = paste(mybugsdir,"\\Set3model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau", "mu.0", "tau.0"), n.chains = 5,
   n.iter = 5000, n.burnin = 500, codaPkg = TRUE,
   bugs.directory=mybugsdir)
Set3.mcmc <- read.bugs(Set3.coda2)
# Select 2 here:
codamenu()

# Second try
Set3.coda2 <- bugs(data = XY.data2, inits = XY.inits2,
   model.file = paste(mybugsdir,"\\Set3model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau", "mu.0", "tau.0"), n.chains = 5,
   n.iter = 10000, n.burnin = 2000, n.thin = 10, codaPkg = TRUE,
   bugs.directory=mybugsdir)
Set3.mcmc <- read.bugs(Set3.coda2)
# Select 2 here:
codamenu()

Set3.sim2 <- bugs(data = XY.data2, inits = XY.inits2,
   model.file = paste(mybugsdir,"\\Set3model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau", "mu.0", "tau.0"), n.chains = 5,
   n.iter = 10000, n.burnin = 2000, n.thin = 20,
   bugs.directory=mybugsdir)
print(Set3.sim2, digits = 2)
t(Set3.sim$mean)
t(unlist(Set3.sim$mean))

# Multilevel intercept and slope

Set3.model3 <- function(){
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

write.model(Set3.model3,
   paste(mybugsdir,"\\Set3model.bug", sep = ""))

XY.data3 <- list(Irrig = data.Set3a$Irrig, Yield = data.Set3a$YieldN,
   n = nrow(data.Set3a), n.F = length(unique(data.Set3a$Field)),
   Field = data.Set3a$Field)

XY.inits3 <- function(){
   list(beta0 = rnorm(n.F), beta1 = rnorm(n.F), tau = exp(rnorm(1)),
   mu.0 = rnorm(1), tau.0 = runif(1), mu.1 = rnorm(1),
   tau.1 = exp(rnorm(1)))}

Set3.test3 <- bugs(data = XY.data3, inits = XY.inits3,
   model.file = paste(mybugsdir,"\\Set3model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau", "mu.0", "tau.0", "mu.1", "tau.1"), n.chains = 5,
   n.iter = 10, n.burnin = 2, debug = TRUE,
   bugs.directory=mybugsdir)

Set3.coda3 <- bugs(data = XY.data3, inits = XY.inits3,
   model.file = paste(mybugsdir,"\\Set3model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau", "mu.0", "tau.0", "mu.1", "tau.1"), n.chains = 5,
   n.iter = 10000, n.burnin = 2000, n.thin = 10, codaPkg = TRUE,
   bugs.directory=mybugsdir)
Set3.mcmc <- read.bugs(Set3.coda3)
# Select 2 here:
codamenu()

Set3.sim3 <- bugs(data = XY.data3, inits = XY.inits3,
   model.file = paste(mybugsdir,"\\Set3model.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau", "mu.0",
      "tau.0", "mu.1", "tau.1"), n.chains = 5,
   n.iter = 10000, n.burnin = 2000, n.thin = 20,
   bugs.directory=mybugsdir)
print(Set3.sim3, digits = 2)

param.est <- t(unlist(Set3.sim3$mean))
b1 <- param.est[16:30]

print(b1NS <- b1[c(1:4, 15)], digits = 2)
print(b1C <- b1[5:14], digits = 2)
mean(b1NS)
mean(b1C)
mean(b1NS) - mean(b1C)

# Permutation test
b <- c(b1NS, b1C)
print(d0 <- mean(b[1:5] - b[6:length(b)]))
u <- numeric(1000)
sample.d <- function(b){
    b.samp <- sample(b)
    mean(b.samp[1:5] - b.samp[6:length(b)])
}
set.seed(123)
u <- replicate(999, sample.d(b))
u[1000] <- d0
print(p <- length(which(u > d0)) / length(u))

