library(spdep)
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
print(r <- cortest$estimate, digits = 3)
print(t.stat <- cortest$statistic, digits = 3)
print(p <- cortest$p.value, digits = 3)
