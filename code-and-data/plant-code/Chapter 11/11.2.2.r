library(spdep)
library(spatial)

# Generate the artificial data
rho <- 0.6
nlist <- cell2nb(14, 14)
IrWinv <- invIrM(nlist, rho)
set.seed(123)
Y1 <- IrWinv %*% rnorm(14^2)
Y2 <- IrWinv %*% rnorm(14^2)
Y1samp <- matrix(Y1, nrow = 14, byrow = TRUE)[3:12,3:12]
Y2samp <- matrix(Y2, nrow = 14, byrow = TRUE)[3:12,3:12]
cortest <- cor.test(Y1samp, Y2samp,
   alternative = "two.sided", method = "pearson")
print(r <- cortest$estimate)
print(t.stat <- cortest$statistic)
print(p <- cortest$p.value)

# Generate the coordinates for surf.ls()
coords.xy <- expand.grid(1:10,10:1)
# Generate  vectors Y1 and Y2 and compute correlgrams
Y1.vec <- as.vector(t(Y1samp))
Y1.surf <- surf.ls(0,coords.xy[,1], coords.xy[,2], Y1.vec)
r1 <- correlogram(Y1.surf,20)
Y2.vec <- as.vector(t(Y2samp))
Y2.surf <- surf.ls(0,coords.xy[,1], coords.xy[,2], Y2.vec)
r2 <- correlogram(Y2.surf,20)
# Estimate sigmar
nr1r2 <- r1$cnt * r1$y * r2$y
print(sr.hat <- sum(nr1r2[1:10]) / 10^4, digits = 3)

# Estimate the effective sample se and the p value of the t statisic
print(ne.hat <- 1 + 1 / sr.hat, digits = 3)
print(t.corr <- r * sqrt(ne.hat - 2) / sqrt(1 - r^2), digits = 3)
print(p.corr <- 2 * (1 - pt(q = abs(t.corr),
   df = ne.hat - 2)), digits = 3)


# Monte Carlo simulation of Clifford correction

library(spdep)
library(spatial)
nlist <- cell2nb(14, 14)
MC <- function(rho,IrWinv){
   Y1 <- IrWinv %*% rnorm(14^2)
   Y2 <- IrWinv %*% rnorm(14^2)
   Y1samp <- matrix(Y1, nrow = 14, byrow = TRUE)[3:12,3:12]
   Y2samp <- matrix(Y2, nrow = 14, byrow = TRUE)[3:12,3:12]
   cortest <- cor.test(Y1samp, Y2samp,
     alternative = "two.sided", method = "pearson")
   r <- cortest$estimate
   t.uncorr <- cortest$statistic
   p.uncorr <- cortest$p.value
   coords.xy <- expand.grid(1:10,10:1)
   Y1.vec <- as.vector(t(Y1samp))
   Y1.kr <- surf.ls(0,coords.xy[,1], coords.xy[,2], Y1.vec)
   r1 <- correlogram(Y1.kr,20, plotit = FALSE)
   Y2.vec <- as.vector(t(Y2samp))
   Y2.kr <- surf.ls(0,coords.xy[,1], coords.xy[,2], Y2.vec)
   r2 <- correlogram(Y2.kr,20, plotit = FALSE)
# Estimate sigmar
   nr1r2 <- r1$cnt * r1$y * r2$y
   sr.hat <- sum(nr1r2[1:10]) / 10^4
# Estimate the effective sample size and the p value of the t statistic
   ne.hat <- 1 + 1 / sr.hat
   t.corr <- r * sqrt(ne.hat - 2) / sqrt(1 - r^2)
   p.corr <- 2 * (1 - pt(q = abs(t.corr),df = ne.hat - 2))
   TypeI.uncorr <- as.numeric(p.uncorr < 0.05)
   TypeI.corr <- as.numeric(p.corr < 0.05)
   return(c(TypeI.uncorr, TypeI.corr))
}

set.seed(123)
rho <- 0.6
IrWinv <- invIrM(nlist, rho)
U <- replicate(10000, MC(rho,IrWinv))

mean(U[1,])
mean(U[2,])














