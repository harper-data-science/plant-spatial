# 10.1
set.seed(123)
pop.Y <- rnorm(10000)
print(Y.bar <- mean(pop.Y))
print(sample.Y <- sample(pop.Y, size = 10))
library(boot)
mean.Y <- function(Y,i) mean(Y[i])
boot.Y <- boot(sample.Y, mean.Y, R = 50)
sqrt(sum((pop.Y - Y.bar)^2) / 10000) / sqrt(10)
sd(boot.Y$t)

# 10.2
sd(boot(sample.Y, mean.Y, R = 10)$t)
sd(boot(sample.Y, mean.Y, R = 50)$t)
sd(boot(sample.Y, mean.Y, R = 200)$t)
sd(boot(sample.Y, mean.Y, R = 1000)$t)
sd(boot(sample.Y, mean.Y, R = 10000)$t)

# 10.3
library(boot)
boot.test <-function(){
   pop.Y <- rnorm(10000)
   Y.bar <- mean(pop.Y)
   sample.Y <- sample(pop.Y, size = 10)
   mean.Y <- function(Y,i) mean(Y[i])
   boot.Y <- boot(sample.Y, mean.Y, R = 50)
   sqrt(sum((pop.Y - Y.bar)^2) / 10000) / sqrt(10) - sd(boot.Y$t)
}
U <- replicate(10000, boot.test())
hist(U)

# 10.4
set.seed(123)
pop.Y <- rlnorm(10000)
print(Y.bar <- mean(pop.Y))
print(sample.Y <- sample(pop.Y, size = 10))
library(boot)
mean.Y <- function(Y,i) mean(Y[i])
boot.Y <- boot(sample.Y, mean.Y, R = 50)
sqrt(sum((pop.Y - Y.bar)^2) / 10000) / sqrt(10)
sd(boot(sample.Y, mean.Y, R = 10)$t)
sd(boot(sample.Y, mean.Y, R = 50)$t)
sd(boot(sample.Y, mean.Y, R = 200)$t)
sd(boot(sample.Y, mean.Y, R = 1000)$t)
sd(boot(sample.Y, mean.Y, R = 10000)$t)


# 10.5
data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
para.samp <- function(e.hat, IrWinv.mod){
  e.samp <- sample(e.hat, length(e.hat), replace = TRUE)
  Y.boot <- IrWinv.mod %*% e.samp
  return(mean(Y.boot))
}

library(spdep)
set.seed(123)
coordinates(EM38.rect) <- c("Easting", "Northing")
nlist.w <- dnearneigh(EM38.rect, d1 = 0, d2 = 61)
W <- nb2listw(nlist.w, style = "W")
Y.mod <- lagsarlm(EMDiff ~ 1, data = EM38.rect, listw = W)
e.hat <- residuals(Y.mod)
print(lambda.hat <- Y.mod$lambda)
IrWinv.mod <- invIrM(nlist.w, lambda.hat)
# Bootstrap resampling
u <- replicate(200,para.samp(e.hat, IrWinv.mod))
# Carry out the t-test
se.para <- sd(u)
t.stat <- mean(EM38.rect$EMDiff) / se.para
print(p <- 2 * (1 - pt(q = abs(t.stat),
     df = length(EM38.rect$EMDiff) - 1)))
print(ne.hat.para <- var(EM38.rect$EMDiff) / se.para^2)

# 10.6
para.samp <- function(IrWinv.mod){
   Y <- IrWinv.mod %*% rnorm(20^2)
   mean(Y)
}
# Monte Carlo simulation of parametric bootstrap, use the bootstrap residuals
t.para.boot <- function(IrWinv, nlist.small, W){
# Generate the sample, eliminating the boundary
   Y <- IrWinv %*% rnorm(24^2)
   Y.samp <- matrix(Y, nrow = 24, byrow = TRUE)[3:22,3:22]
# Fit a spatial leg to the data
   Y.mod <- errorsarlm(as.vector(Y.samp) ~ 1, listw = W)
   lambda.hat <- Y.mod$lambda
   IrWinv.mod <- invIrM(nlist.small, lambda.hat)
# Bootstrap resampling
   u <- replicate(200,para.samp(IrWinv.mod))
# Carry out the t-test
   t.stat <- mean(Y.samp) / sd(u)
   p <- 2 * (1 - pt(q = abs(t.stat),
     df = nrow(Y.samp)*ncol(Y.samp) - 1))
   return(c(as.numeric(p < 0.05), sd(u)))
}

library(spdep)
set.seed(123)
lambda <- 0.6
nlist <- cell2nb(24, 24)
IrWinv <- invIrM(nlist, lambda)
nlist.small <- cell2nb(20, 20)
W <- nb2listw(nlist.small)
u <- replicate(1000, t.para.boot(IrWinv, nlist.small, W))
mean(u[1,])  # Error rate
mean(u[2,])  # Mean est. std. error

