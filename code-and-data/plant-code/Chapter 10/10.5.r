data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)

# Block bootstrap test
block.samp <- function(n.blockx, n.blocky, blk.size, blocks){
# Resample the block index
   i.samp <- sample(1:(n.blockx*n.blocky), replace = TRUE)
# Construct the 3D array of blocks
   Ct <- matrix(0, nrow = n.blocky * blk.size,
      ncol = n.blockx * blk.size)
   k <- 1
# Place resampled blocks in the array
   for(i in 1:n.blocky){
      for(j in 1:n.blockx){
         Ct[(1+(i-1)*blk.size):(i*blk.size),
            (1+(j-1)*blk.size):(j*blk.size)] <- blocks[,,i.samp[k]]
         k <- k+1
   }}
# Since we are just computing the mean, we wouldn't realy need
# to compute the transpose here
   return(t(Ct))
}

mean.b.boot <- function(n.blockx, n.blocky, blk.size, blocks){
   mean(block.samp(n.blockx, n.blocky, blk.size, blocks))
}

EM38.rect <- data.Set4.1[which(data.Set4.1$Column <= 6 &
   data.Set4.1$Row <= 12),]
EM38.rect$EMDiff <- with(EM38.rect, EM38B425 - EM38F520)
Ysamp <- matrix(EM38.rect$EMDiff, nrow = 12, byrow = TRUE)
Yt <- t(Ysamp)
n.blockx <- 4
n.blocky <- 2
blk.size <- 3
# Create the blocks
blocks <- array(0, c(blk.size,blk.size,n.blockx * n.blocky))
k <- 1
for(i in 1:n.blocky){
   for(j in 1:n.blockx){
      blocks[,,k] <- Yt[(1+(i-1)*blk.size):(i*blk.size),
         (1+(j-1)*blk.size):(j*blk.size)]
         k <- k+1
}}
# Create the bootstrap resample
set.seed(123)
u <- replicate(200, mean.b.boot(n.blockx, n.blocky, blk.size, blocks))
# Carry out the t-test
Y.bar <- mean(Ysamp)
se.boot <-  sd(u)
t.stat <- Y.bar / se.boot
print(p <- 2 * (1 - pt(q = abs(t.stat),
   df = nrow(Ysamp)*ncol(Ysamp) - 1)))
print(ne.hat.bb <- var(EM38.rect$EMDiff) / se.boot^2)

# Parameetric bootstrap

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
Y.mod <- errorsarlm(EMDiff ~ 1, data = EM38.rect, listw = W)
e.hat <- residuals(Y.mod)
print(lambda.hat <- Y.mod$lambda)
IrWinv.mod <- invIrM(nlist.w, lambda.hat)
# Bootstrap resampling
U <- replicate(200,para.samp(e.hat, IrWinv.mod))
# Carry out the t-test
se.para <- sd(U)
t.stat <- mean(EM38.rect$EMDiff) / se.para
print(p <- 2 * (1 - pt(q = abs(t.stat),
     df = length(EM38.rect$EMDiff) - 1)))
print(ne.hat.para <- var(EM38.rect$EMDiff) / se.para^2)