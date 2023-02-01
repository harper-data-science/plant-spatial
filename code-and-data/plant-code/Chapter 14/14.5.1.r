library(spdep)
library(R2WinBUGS)

lambda <- 0.4
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

# WinBugs formulation
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

library(coda)
coda.file <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\demomodel.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 5,
   n.iter = 5000, n.burnin = 500, codaPkg  = TRUE,
   bugs.directory = mybugsdir)
   
demo.mcmc <- read.bugs(coda.file)
# Select 2 here:
codamenu()


demo.sim <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\demomodel.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 5,
   n.iter = 10000, n.burnin = 1000, n.thin = 10,
   bugs.directory = mybugsdir)
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
   bugs.directory = mybugsdir)
demo.mcmc <- read.bugs(coda.file)
# Select 2 here:
codamenu()

demo.sim <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\demomodel.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau", "v"), n.chains = 5,
   n.iter = 10000, n.burnin = 1000, n.thin = 10,
   bugs.directory = mybugsdir)
print(demo.sim, digits = 2)
