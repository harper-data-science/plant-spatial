library(spdep)
library(spatial)

rho <- 0.6
nlist <- cell2nb(14, 14)
IrWinv <- invIrM(nlist, rho)
set.seed(123)
Y1 <- IrWinv %*% rnorm(14^2)
Y2 <- IrWinv %*% rnorm(14^2)
Y1samp <- matrix(Y1, nrow = 14, byrow = TRUE)[3:12,3:12]
Y2samp <- matrix(Y2, nrow = 14, byrow = TRUE)[3:12,3:12]

# Clifford-type correction using parametric bootstrap variance estimate

#Single run

# Generate the parametric bootstrap
nlist.10 <- cell2nb(10,10)
W <- nb2listw(nlist.10)
# Geneate the parametric models for Y1 and Y2
Y1.vec <- as.vector(t(Y1samp))
Y1.mod <- lagsarlm(Y1.vec ~ 1, data = data.frame(Y1.vec), W)
IrWinv.1 <- invIrM(nlist.10, max(Y1.mod$rho, 0), style = "W")
Y2.vec <- as.vector(t(Y2samp))
Y2.mod <- lagsarlm(Y2.vec ~ 1, data = data.frame(Y2.vec), W)
IrWinv.2 <- invIrM(nlist.10, max(Y2.mod$rho, 0), style = "W")
# Function to generate the boostrap resamples
boot.samp <- function(Y1.mod,Y2.mod,IrWinv.1,IrWinv.2){
   e <- sample(Y1.mod$residuals, replace = TRUE)
   Y1 <- IrWinv.1 %*% e
   e <- sample(Y2.mod$residuals, replace = TRUE)
   Y2 <- IrWinv.2 %*% e
   r.boot <- cor(Y1,Y2)
}
# Compute the boostrap resamples
U <- replicate(200, boot.samp(Y1.mod,Y2.mod,IrWinv.1,IrWinv.2))
sigmar.hat <- var(U)
cortest <- cor.test(Y1samp, Y2samp,
   alternative = "two.sided", method = "pearson")
r <- cortest$estimate
print(ne.hat <- 1 + 1 / sigmar.hat, digits = 3)
print(t.corr <- sqrt(ne.hat - 2) * r / sqrt(1 - r^2), digits = 3)
print(p.corr <- 2 * (1 - pt(q = abs(t.corr),
  df = ne.hat - 2)), digits = 3)


# Monte Carlo simulation of parametric bootstrap

library(spdep)
# Generate the parametric bootstrap
MC.corr <- function(rho,IrWinv){
   nlist.10 <- cell2nb(10,10)
   Y1 <- IrWinv %*% rnorm(14^2)
   Y2 <- IrWinv %*% rnorm(14^2)
   Y1samp <- matrix(Y1, nrow = 14, byrow = TRUE)[3:12,3:12]
   Y2samp <- matrix(Y2, nrow = 14, byrow = TRUE)[3:12,3:12]
   cortest <- cor.test(Y1samp, Y2samp,
     alternative = "two.sided", method = "pearson")
   r <- cortest$estimate
   W <- nb2listw(nlist.10)
# Generate the parametric models for Y1 and Y2
   Y1.vec <- as.vector(t(Y1samp))
   Y1.mod <- lagsarlm(Y1.vec ~ 1, data = data.frame(Y1.vec), W)
   IrWinv.1 <- invIrM(nlist.10, max(Y1.mod$rho, 0), style = "W")
   Y2.vec <- as.vector(t(Y2samp))
   Y2.mod <- lagsarlm(Y2.vec ~ 1, data = data.frame(Y2.vec), W)
   IrWinv.2 <- invIrM(nlist.10, max(Y2.mod$rho, 0), style = "W")
# Function to generate the boostrap resamples
   boot.samp <- function(Y1.mod,Y2.mod,IrWinv.1,IrWinv.2){
      e1 <- sample(Y1.mod$residuals, replace = TRUE)
      Y1 <- IrWinv.1 %*% e1
      e2 <- sample(Y2.mod$residuals, replace = TRUE)
      Y2 <- IrWinv.2 %*% e2
      r.boot <- cor(Y1,Y2)
   }
# Compute the boostrap resamples
   U <- replicate(200, boot.samp(Y1.mod,Y2.mod,IrWinv.1,IrWinv.2))
   sigmar.hat <- var(U)
   ne.hat <- 1 + 1 / sigmar.hat
   TypeI.uncorr <- as.numeric(cortest$p.value < 0.05)
   t.corr <- sqrt(ne.hat - 2) * r / sqrt(1 - r^2)
   p.corr <- 2 * (1 - pt(q = abs(t.corr), df = ne.hat - 2))
   TypeI.corr <- as.numeric(p.corr < 0.05)
   return(c(TypeI.uncorr, TypeI.corr))
}
rho <- 0.8
nlist <- cell2nb(14, 14)
IrWinv <- invIrM(nlist, rho)
set.seed(123)
U <- replicate(10000, MC.corr(rho,IrWinv))
mean(U[1,])
mean(U[2,])












