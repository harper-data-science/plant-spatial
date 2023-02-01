# Demonstration of the effects of various factors on OLS regression
# Y = tree productivity
# X1 = light availability
# X2 = nutrient avaialbility

set.seed(123)
X1 <- rnorm(100)
X2 <- rnorm(100)
eps <- 0.01 * rnorm(100)
b <- c(0.5,0.3)
Y <- b[1]*X1 + b[2]*X2 + eps
print(coef(lm(Y ~ X1 + X2)), digits = 2)

# Interaction example
rho <- 0.6
library(spdep)
nlist <- cell2nb(10, 10)
IrWinv <- invIrM(nlist, rho)
Y <- IrWinv %*% (b[1]*X1 + b[2]*X2 + eps)
print(coef(lm(Y ~ X1 + X2)), digits = 2)

# Reaction example
set.seed(123)
coords <- expand.grid(1:10,10:1)
b <- c(0.5,0.3,0.8)
reg.mc <- function(coords, b, incl.X3){
  X1 <- rnorm(100)
  X2 <- coords[,1]+rnorm(100)
  X3 <- coords[,1]+rnorm(100)
  Y <- b[1]*X1 + b[2]*X2 + b[3]*X3  + rnorm(100)
  {if (incl.X3 == T)
     Y.lm <- lm(Y ~ X1 + X2 + X3)
   else
     Y.lm <- lm(Y ~ X1 + X2)}
   return(coef(Y.lm))
}
U <- replicate(1000,reg.mc(coords, b, T))
print(rowMeans(U), digits = 2)
U <- replicate(1000,reg.mc(coords, b, F))
print(rowMeans(U), digits = 2)


# Second example in Miron
library(spdep)
set.seed(123)
coords <- expand.grid(1:10,10:1)
b <- c(0.5,0.3,0.8)
lambda <- 0.6
nlist <- cell2nb(10, 10)
IrWinv <- invIrM(nlist, lambda)
reg.mc <- function(coords, b, incl.X3){
  X1 <- rnorm(100)
  X2 <- rnorm(100)
  X3 <- IrWinv %*% rnorm(100)
  Y <- b[1]*X1 + b[2]*X2 + b[3]*X3 + rnorm(100)
  {if (incl.X3 == T)
     Y.lm <- lm(Y ~ X1 + X2 + X3)
   else
     Y.lm <- lm(Y ~ X1 + X2)}
   return(coef(Y.lm))
}
U <- replicate(1000,reg.mc(coords, b, T))
print(rowMeans(U), digits = 2)
print(apply(U, 1, var), digits = 2)
U <- replicate(1000,reg.mc(coords, b, F))
print(rowMeans(U), digits = 2)
print(apply(U, 1, var), digits = 2)

# Third example in Miron: misspecified model
# Single run, not shown in text
library(spdep)
library(lmtest)
set.seed(123)
coords <- expand.grid(1:10,10:1)
b <- c(0.5,0.3)
nlist <- cell2nb(10,10)
IrWinv <- invIrM(nlist, 0.8)
W <- nb2listw(nlist)
X1 <- rnorm(100)
X2 <- IrWinv %*% rnorm(100)
moran.test(X2, W)
Y <- b[1]*X1 + b[2]*X2 + exp(1 + 2 * X1) * rnorm(100)
Y.lm <- lm(Y ~ X1 + X2)
lm.morantest(Y.lm, W)
coef(Y.lm)
bptest(Y.lm)

set.seed(123)
coords <- expand.grid(1:10,10:1)
b <- c(0.5,0.3)
nlist <- cell2nb(10,10)
IrWinv <- invIrM(nlist, 0.8)
W <- nb2listw(nlist)
reg.mc <- function(coords, b, W){
  X1 <- rnorm(100)
  X2 <- IrWinv %*% rnorm(100)
  Y1 <- b[1]*X1 + b[2]*X2 + rnorm(100)
  Y2 <- b[1]*X1 + b[2]*X2 + exp(1 + 2 * X2) * rnorm(100)
# Model correctly specified
  Y1.lm <- lm(Y1 ~ X1 + X2)
  t1 <- lm.morantest(Y1.lm, W)
  C1 <- as.numeric(t1$p.value < 0.05)
# Model mis-specified, heteroscedastic errors
  Y2.lm <- lm(Y2 ~ X1 + X2)
  t2 <- lm.morantest(Y2.lm, W)
  C2 <- as.numeric(t2$p.value < 0.05)
  return(c(C1, C2))
}
U <- replicate(1000,reg.mc(coords, b, W))
rowMeans(U)




