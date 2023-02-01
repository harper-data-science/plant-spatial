# Monte Carlo simulation of t tests with autocorrelated data
library(boot)

#Ordinary data, one test
set.seed(123)
Y <- rnorm(100)
t.test(Y, alternative = "two.sided")$p.value

library(boot)
mean.Y <- function(Y,i) mean(Y[i])
boot.Y <- boot(Y, mean.Y, R = 1000)
t.boot <- mean(Y) / sd(boot.Y$t)
print(p <-  2 * (1 - pt(q = abs(t.boot),
   df = length(Y) - 1)))

#Ordinary data, Monte Carlo
set.seed(123)
ttest <- function(){
  Y <- rnorm(100)
  t.ttest <- t.test(Y,alternative = "two.sided")
  return(TypeI <- as.numeric(t.ttest$p.value < 0.05))
}
U <- replicate(10000, ttest())
mean(U)

# Autocorrelated data
lambda <- 0.4 #Autocorrelation term
set.seed(123)
ttest <- function(lambda){
  Y <- numeric(110)
  for (i in 2:110) Y[i] <- lambda * Y[i - 1] + rnorm(1, sd = 1)
  Ysamp <- Y[11:110]
  t.ttest <- t.test(Ysamp,alternative = "two.sided")
  return(TypeI <- as.numeric(t.ttest$p.value < 0.05))
}
U <- replicate(10000, ttest(lambda))
mean(U)

# Monte Carlo simulation of uncorrected bootstrap
t.boot <- function(Y){
   mean.Y <- function(Y,i) mean(Y[i])
# Bootstrap estimate of the variance
   boot.Y <- boot(Y, mean.Y, R = 200)
   se <- sd(boot.Y$t)
   t.stat <- mean(Y) / se
# t test based on the bootstrap s.e. estimate
   p <- 2 * (1 - pt(q = abs(t.stat),df = length(Y) - 1))
   return(c(p, se))
}

ttest <- function(lambda){
  Y <- numeric(110)
  for (i in 2:110) Y[i] <- lambda * Y[i - 1] + rnorm(1, sd = 1)
  Ysamp <- Y[11:110]
  t.ttest <- t.boot(Ysamp)
  TypeI <- as.numeric(t.ttest[1] < 0.05)
  Yse <- sd(Ysamp) / 10
  Yse.boot <- t.ttest[2]
  return(c(TypeI, Yse, Yse.boot))
}

set.seed(123)
lambda <- 0.4
U <- replicate(10000, ttest(lambda))
mean(U[1,]) # Error rate
mean(U[2,]) # Avg std error
mean(U[3,]) # Avg bootstrap s.e.

