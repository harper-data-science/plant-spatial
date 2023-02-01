# Construct a neighbor list for a 3 by 3 square lattice
library(sf)
library(nngeo)
library(spdep)
library(spatialreg)


nb3x3 <- cell2nb(3,3) #Rook's case by default
# Examine the structure of the neighbor list
str(nb3x3)

# ------------------------------------
# Generation of Fig. 3.9

Y.df <- expand.grid(x = seq(0.5,23.5),
  y = seq(23.5, 0.5, by = -1))
gen.Y <- function(lambda, nlist){
   IrWinv <- invIrM(nlist, lambda)
   eps <- rnorm(24^2)
   return(IrWinv %*% eps)
}
 
nlist <- cell2nb(24, 24)
set.seed(123)
Y.df$Y0 <- gen.Y(0, nlist)
set.seed(123)
Y.df$Y4 <- gen.Y(0.4, nlist)
set.seed(123)
Y.df$Y8 <- gen.Y(0.8, nlist)
Y.df <- Y.df[(Y.df$x > 2 & Y.df$x < 22
 & Y.df$y > 2 & Y.df$y < 22),]

library(lattice)

greys <- grey(0:255 / 255)
Y.plot <- data.frame(z = c(Y.df$Y0, Y.df$Y4, Y.df$Y8),
   x = rep(Y.df$x, 3), y = rep(Y.df$y, 3),
   lambda = as.factor(sort(rep(c(0,0.4,0.8),nrow(Y.df)))))
trellis.device(color = FALSE)
levelplot(z ~ x * y | lambda, Y.plot,
   col.regions = greys, main = expression("Three values of"~lambda),
   layout = c(3,1), aspect = "iso")  # Fig. 3.9

# Color version
trellis.device(color = TRUE)
levelplot(z ~ x * y | lambda, Y.plot,
   main = expression("Three values of"~lambda),
   layout = c(3,1), aspect = "iso")

# -------------------------------------------

# carry out a Monte Carlo simulation of a t test

lambda <- 0.4
nlist <- cell2nb(14,14)
IrWinv <- invIrM(nlist, lambda)
ttest <- function(IrWinv){
# Generate a vector representing the autocorrelataed data
# on a 14 by 14 grid
  Y.plus <- IrWinv %*% rnorm(14^2)
# Convert Y to a matrix and remove the outer two cells 
  Y <- matrix(Y.plus, nrow = 14,
     byrow = TRUE)[3:12,3:12]
  Ybar <- mean(Y)
# Carry out the test and return the outcome and the mean
  t.ttest <- t.test(Y,alternative = "two.sided")
  TypeI <- t.ttest$p.value < 0.05
  return(c(TypeI, Ybar))
}
set.seed(123)
U <- replicate(10000, ttest(IrWinv))
mean(U[1,])
mean(U[2,])
sqrt(var(U[2,]))

# Do the MC experiment for a set of lambda values

lambda <- numeric(5)
p.obs <- numeric(5)
nlist <- cell2nb(14,14)
for (i in 1:5){
   set.seed(123)
   lambda[i] <- 0.2 * (i - 1)
   IrWinv <- invIrM(nlist, lambda[i])
   U <- replicate(10000, ttest(IrWinv))
   p.obs[i] <- mean(U[1,])
}
par(mai = c(1,1,1,1))
plot(lambda, p.obs, xlab = expression(lambda),  # Fig. 3.10
  ylab = "Error Rate", ylim = c(0.0,0.6),
  cex.lab = 1.5, cex.main = 2,
  main = "Error Rate of t Test for Spatial Data")





















