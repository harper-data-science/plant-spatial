library(R2WinBUGS)
set.seed(123)
n <- 20
X1 <- rnorm(n)
Y1 <- X1 + rnorm(n)
print(coef(lm(Y1 ~ X1)), digits = 3)

demo.model <- function(){
beta0 ~ dnorm(0, 0.01)
beta1 ~ dnorm(0, 0.01)
tau ~ dgamma(0.01, 0.01)
for (i in 1:n)
  {
  Y.hat[i] <- beta0 + beta1 * X[i]
  Y[i] ~ dnorm(Y.hat[i], tau)
  }
}

write.model(demo.model,
   paste(mybugsdir,"\\demomodel.bug", sep = ""))

XY.data <- list(X = X1, Y = Y1, n = n)
XY.inits <- function(){
   list(beta0 = rnorm(1), beta1 = rnorm(1), tau = exp(rnorm(1)))
}

demo.test <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\demomodel.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 1,
   n.iter = 10, n.burnin = 2, debug = TRUE,
   bugs.directory=mybugsdir)

demo.sim1 <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\demomodel.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 1,
   n.iter = 5000, n.burnin = 1000,
   bugs.directory=mybugsdir)
print(t(cbind(demo.sim1$mean, demo.sim1$sd)), digits = 3)


# Second run: different data
print(mu.0 <- demo.sim1$mean$beta0, digits = 3)
print(tau.0 <- 1/demo.sim1$sd$beta0^2, digits = 3)
print(mu.1 <- demo.sim1$mean$beta1, digits = 3)
print(tau.1 <- 1/demo.sim1$sd$beta1^2, digits = 3)
print(nu.t <- demo.sim1$mean$tau / demo.sim1$sd$tau^2, digits = 3)
print(mu.t <- demo.sim1$mean$tau * nu.t, digits = 4)

set.seed(456)
X2 <- rnorm(n)
Y2 <- 0.7 * X2 + rnorm(n)

demo.model <- function(){
beta0 ~ dnorm(mu.0, tau.0)
beta1 ~ dnorm(mu.1, tau.1)
tau ~ dgamma(mu.t, nu.t)
for (i in 1:n)
  {
  Y.hat[i] <- beta0 + beta1 * X[i]
  Y[i] ~ dnorm(Y.hat[i], tau)
  }
}

write.model(demo.model,
   paste(mybugsdir,"\\demomodel.bug", sep = ""))

XY.data <- list(X = X2, Y = Y2, n = n,
   mu.0 = mu.0, tau.0 = tau.0, mu.1 = mu.1, tau.1 = tau.1,
   mu.t = mu.t, nu.t = nu.t)
XY.inits <- function(){
   list(beta0 = mu.0, beta1 = mu.1, tau = mu.t)
}

demo.test <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\demomodel.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 1,
   n.iter = 10, n.burnin = 2, debug = TRUE,
   bugs.directory=mybugsdir)

demo.sim1.2 <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\demomodel.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 1,
   n.iter = 5000, n.burnin = 1000,
   bugs.directory=mybugsdir)
print(t(cbind(demo.sim1.2$mean, demo.sim1.2$sd)), digits = 3)

plot(Y1 ~ X1)
points(X2, Y2, pch = 16)
abline(lm(Y1~X1))
abline(lm(Y2 ~ X2), lty = 2)


# Compare with OLS solutions
print(coef(lm(Y1 ~ X1)), digits = 3)
Y12 <- c(Y1,Y2)
X12 <- c(X1,X2)
print(coef(lm.12 <- lm(Y12 ~ X12)), digits = 3)
