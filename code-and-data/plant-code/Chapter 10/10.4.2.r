# Spatially autocorrelated data

library(spdep)

# Parametric bootstrap
# Simulate the lattice data
para.samp <- function(e.hat, IrWinv.mod){
  e.samp <- sample(e.hat, length(e.hat), replace = TRUE)
  Y.boot <- IrWinv.mod %*% e.samp
  return(mean(Y.boot))
}

# Monte Carlo simulation of parametric bootstrap
t.para.boot <- function(IrWinv, nlist.small, W){
# Generate the sample, eliminating the boundary
   Y <- IrWinv %*% rnorm(24^2)
   Y.samp <- matrix(Y, nrow = 24, byrow = TRUE)[3:22,3:22]
# Fit a spatial error model to the data
   Y.mod <- errorsarlm(as.vector(Y.samp) ~ 1, listw = W)
   e.hat <- residuals(Y.mod)
   lambda.hat <- Y.mod$lambda
   IrWinv.mod <- invIrM(nlist.small, lambda.hat)
# Bootstrap resampling
   u <- replicate(200,para.samp(e.hat, IrWinv.mod))
# Carry out the t-test
   t.stat <- mean(Y.samp) / sd(u)
   p <- 2 * (1 - pt(q = abs(t.stat),
     df = nrow(Y.samp)*ncol(Y.samp) - 1))
   return(c(as.numeric(p < 0.05), sd(u)))
}

set.seed(123)
lambda <- 0.4
nlist <- cell2nb(24, 24)
IrWinv <- invIrM(nlist, lambda)
nlist.small <- cell2nb(20, 20)
W <- nb2listw(nlist.small)
U <- replicate(10000, t.para.boot(IrWinv, nlist.small, W))
mean(U[1,]) # Error rate
mean(U[2,]) # Mean est. std. error

