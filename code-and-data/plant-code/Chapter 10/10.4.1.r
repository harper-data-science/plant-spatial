# Spatially autocorrelated data

# Monte Carlo simulation of uncorrected bootstrap

t.boot <- function(Y){
   mean.Y <- function(Y,i) mean(Y[i])
   boot.Y <- boot(Y, mean.Y, R = 200)
   se <- sd(boot.Y$t)
   t.stat <- mean(Y) / se
   p <- 2 * (1 - pt(q = abs(t.stat),df = length(Y) - 1))
   return(c(p, se))
}

ttest <- function(lambda){
  Y <- IrWinv %*% rnorm(24^2)
  Ysamp <- matrix(Y, nrow = 24, byrow = TRUE)[3:22,3:22]
  t.ttest <- t.boot(as.vector(Ysamp))
  TypeI <- as.numeric(t.ttest[1] < 0.05)
  Yse <- sd(Ysamp) / 20
  Yse.boot <- t.ttest[2]
  return(c(TypeI, Yse, Yse.boot))
}

set.seed(123)
library(spdep)
library(boot)
lambda <- 0.6
nlist <- cell2nb(24, 24)
IrWinv <- invIrM(nlist, lambda)
U <- replicate(10000, ttest(lambda))
mean(U[1,])  # Error rate / Power
mean(U[2,])  # Avg std error
mean(U[3,])  # Avg bootstrap s.e.


n.row <- 6
n.block <- 2
print(A <- matrix(1:n.row^2,
   nrow = n.row, byrow = TRUE))


# -------------------
# Start material on block bootstrap
At <- t(A)

blk.size <- n.row / n.block
B <- array(0, c(blk.size,blk.size,n.block^2))
k <- 1
for(i in 1:n.block){
   for(j in 1:n.block){
      B[,,k] <- At[(1+(i-1)*blk.size):(i*blk.size),
         (1+(j-1)*blk.size):(j*blk.size)]
      k <- k+1
}}
B[,,1]

set.seed(123)
print(i.samp <- sample(1:n.block^2, replace = TRUE))

Ct <- matrix(0, nrow = n.row, ncol = n.row)
k <- 1
for(i in 1:n.block){
   for(j in 1:n.block){
      Ct[(1+(i-1)*blk.size):(i*blk.size),
         (1+(j-1)*blk.size):(j*blk.size)] <- B[,,i.samp[k]]
      k <- k+1
}}

print(C <- t(Ct))

# Block sample function
block.samp <- function(n.block, blocks){
   i.samp <- sample(1:n.block^2, replace = TRUE)
   Ct <- matrix(0, nrow = 20, ncol = 20)
   k <- 1
   blk.size <- 20 / n.block
   for(i in 1:n.block){
      for(j in 1:n.block){
         Ct[(1+(i-1)*blk.size):(i*blk.size),
            (1+(j-1)*blk.size):(j*blk.size)] <- blocks[,,i.samp[k]]
         k <- k+1
   }}
   return(t(Ct))
}

mean.b.boot <- function(n.block, blocks){
   mean(block.samp(n.block, blocks))
}

t.b.boot <- function(lambda){
# Generate the sample, eliminating the boundary
   Y <- IrWinv %*% rnorm(24^2)
   Ysamp <- matrix(Y, nrow = 24, byrow = TRUE)[3:22,3:22]
# Break into blocks
   Yt <- t(Ysamp)
   n.block <- 4
   blk.size <- 20 / n.block
   blocks <- array(0, c(blk.size,blk.size,n.block^2))
   k <- 1
   for(i in 1:n.block){
      for(j in 1:n.block){
         blocks[,,k] <- Yt[(1+(i-1)*blk.size):(i*blk.size),
            (1+(j-1)*blk.size):(j*blk.size)]
         k <- k+1
}}
#Compute the bootstrap resample
   U <- replicate(200, mean.b.boot(n.block, blocks))
# Carry out the t-test
   t.stat <- mean(blocks) / sd(U)
   p <- 2 * (1 - pt(q = abs(t.stat),
     df = nrow(Ysamp)*ncol(Ysamp) - 1))
   return(c(as.numeric(p < 0.05), sd(U)))
}

library(spdep)
set.seed(123)
lambda <- 0.4
nlist <- cell2nb(24, 24)
IrWinv <- invIrM(nlist, lambda)
U <- replicate(10000, t.b.boot(lambda))
mean(U[1,]) # Error rate
mean(U[2,]) # Mean est. std. error

print(rnorm(25), digits = 2)
print(matrix(rnorm(25), nrow = 5), digits = 2)