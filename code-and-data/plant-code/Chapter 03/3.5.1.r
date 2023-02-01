#One run of a simulation of a t-test with autocorrelated data
lambda <- 0.4 #Autocorrelation term
set.seed(123)
Y <- numeric(20)
for (i in 2:20) Y[i] <- lambda * Y[i - 1] + rnorm(1)
  Y <- Y[11:20]

Y.ttest <- t.test(Y,alternative = "two.sided")
#Assign the value 1 to a Type I error
TypeI <- as.numeric(Y.ttest$p.value < 0.05)
Ybar <- mean(Y)
Yse <- sqrt(var(Y) / 10)
c(TypeI, Ybar, Yse)

#Monte Carl0 simulation of a t-test with autocorrelated data  
set.seed(123)
lambda <- 0.4
ttest <- function(lambda){
  Y <- numeric(20)
  for (i in 2:20) Y[i] <- lambda * Y[i - 1] + rnorm(1, sd = 1)
  Y <- Y[11:20]
  Y.ttest <- t.test(Y,alternative = "two.sided")
  TypeI <- as.numeric(Y.ttest$p.value < 0.05)
  Ybar <- mean(Y)
  Yse <- sqrt(var(Y) / 10)
  return(c(TypeI, Ybar, Yse))
}
U <- replicate(10000, ttest(lambda))
mean(U[1,]) # Type I error rate
mean(U[2,]) # Mean value of Ybar
mean(U[3,]) # Mean est. standard error
sd(U[2,]) # Sample std. dev. of Ybar

# Plot of Type I errors vs. lambda
error.rate <- numeric(10)
mean.Y <- numeric(10)
sd.Y <- numeric(10)
sem.Y <- numeric(10)
lambda <- seq(0.0, 0.9, 0.1)
ttest <- function(lambda){
  Y <- numeric(20)
  for (i in 2:20) Y[i] <- lambda * Y[i - 1] + rnorm(1, sd = 1)
  Y <- Y[11:20]
  t.ttest <- t.test(Y,alternative = "two.sided")
  TypeI <- as.numeric(t.ttest$p.value < 0.05)
  Ybar <- mean(Y)
  Yse <- sqrt(var(Y) / 10)
  return(c(TypeI, Ybar, Yse))
}
for (j in 1:10){
  set.seed(1)
  U <- replicate(10000, ttest(lambda[j]))
  error.rate[j] <- mean(U[1,])
  mean.Y[j] <- mean(U[2,])
  sem.Y[j] <- mean(U[3,])
  sd.Y[j] <- sd(U[2,])
}
par(mai = c(1,1,1,1))  #Bottom, Left, Top, Right
plot(lambda, error.rate, xlab = expression(lambda),
  ylab = "Error Rate", cex.lab = 1.5)   # Fig. 3.6a
title(main = "Error Rate", cex.main = 2)
plot(lambda, mean.Y, xlab = expression(lambda),
  ylab = expression(Mean~italic(bar(Y))),
  ylim = c(-0.1, 0.1), cex.lab = 1.5) # Fig. 3.6b
title(main = "Mean of Sample Means", cex.main = 2)
plot(lambda, sem.Y, xlab = expression(lambda),
  ylab = expression(Mean~"s{"*italic(bar(Y))*"}"),
  ylim = c(0,0.5), cex.lab = 1.5) # Fig. 3.6c
title(main = "Mean Estimated Standard Error", cex.main = 2)
plot(lambda,sd.Y, xlab = expression(lambda),
  ylab = expression("Std. Dev. of "*italic(bar(Y))),
  ylim = c(0,2), cex.lab = 1.5) # Fig. 3.6d
title(main = "Standard Deviation of Mean", cex.main = 2)


   