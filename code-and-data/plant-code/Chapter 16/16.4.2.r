library(spdep)
lambda <- 0.6
nlist.14 <- cell2nb(14, 14)
IrWinv.14 <- invIrM(nlist.14, lambda)
nlist <- cell2nb(10, 10)
W <- nb2listw(nlist)

para.samp <- function(Y1, Y2, IrWinv1.mod, IrWinv2.mod){
  Y1.samp <- sample(Y1, length(Y1), replace = TRUE)
  Y1.boot <- IrWinv1.mod %*% Y1.samp
  Y2.samp <- sample(Y2, length(Y2), replace = TRUE)
  Y2.boot <- IrWinv2.mod %*% Y2.samp
  Z <- c(rep(1,100), rep(0,100))
  Y <- c(Y1.boot, Y2.boot)
  r.hat <- cor(Y,Z) 
}

# Generate the data
set.seed(123)
Y1.plus <- IrWinv.14 %*% rnorm(14^2)
Y1 <- matrix(Y1.plus, nrow = 14, byrow = TRUE)[3:12,3:12]
Y2.plus <- IrWinv.14 %*% rnorm(14^2)
Y2 <- matrix(Y2.plus, nrow = 14, byrow = TRUE)[3:12,3:12]

print(t.test(Y1, Y2, "two.sided")$p.value, digits = 3)

# Fit a spatial error to the data
Y1.mod <- errorsarlm(as.vector(Y1) ~ 1, listw = W)
e1.hat <- residuals(Y1.mod)
print(lambda1.hat <- Y1.mod$lambda, digits = 2)
Y2.mod <- errorsarlm(as.vector(Y2) ~ 1, listw = W)
e2.hat <- residuals(Y2.mod)
print(lambda2.hat <- Y2.mod$lambda, digits = 2)
IrWinv1.mod <- invIrM(nlist, lambda1.hat)
IrWinv2.mod <- invIrM(nlist, lambda2.hat)
U <- replicate(200, para.samp(e1.hat, e2.hat, IrWinv1.mod,
   IrWinv2.mod))
print(sigmar.hat <- var(U), digits = 3)

print(t.test(Y1, Y2, "two.sided")$p.value, digits = 3)

print(ne.hat <- 1 / (2*sigmar.hat), digits = 3)
Z <- c(rep(1,100), rep(0,100))
Y <- c(Y1, Y2)
rw <- cor(Y, Z)
t.corr <- rw*sqrt(2*ne.hat - 2) / (1 - rw^2)
print(p.corr <- 2 * (1 - pt(q = abs(t.corr), df = 2*ne.hat - 2)),
   digits = 3)



