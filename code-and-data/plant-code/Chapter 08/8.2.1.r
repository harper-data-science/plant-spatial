set.seed(123)
X <- matrix(rnorm(50), ncol = 5, byrow = TRUE)
beta.lm <- matrix(c(1,1,1,0,0), ncol = 1)
mu <- X %*% beta.lm

regress <- function(X, mu, pminus1){
   Y <- mu + 0.25 * rnorm(10)
   Yhat <- predict(lm(Y ~ X[,1:pminus1]))
   return(Yhat)
}

b2 <- numeric(5)
v <- numeric(5)
for (pminus1 in 1:5){
   set.seed(123)
   Yhat <- replicate(1000, regress(X, mu, pminus1))
   EYhat <- rowMeans(Yhat)
   b2[pminus1] <- sum((EYhat - mu)^2) / 1000
   v[pminus1] <- sum((Yhat - EYhat)^2) / 1000
}

par(mai = c(1,1,1,1))
plot(1:5, b2, type = "l", xlab = "Number of Explanatory Variables",
   ylab = "Squared Bias and Scaled Variance", cex.lab = 1.5,
   main = "Squared Bias and Scaled Variance for 1 Through 5 X's",,
   cex.main = 1.5)   # Fig. 8.1
lines(1:5, max(b2) * v / max(v))
text(1.9, 0.008, expression(bold(Bias^2)))
text(4, 0.008, "Variance", font = 2)

set.seed(123)
Y <- X %*% beta.lm  + 0.25 * rnorm(10)
model.1 <- lm(Y ~ X[,1])
model.3 <- lm(Y ~ X[,1:3])
model.5 <- lm(Y ~ X[,1:5])
Yhat1 <- predict(model.1)
Yhat3 <- predict(model.3)
Yhat5 <- predict(model.5)
par(mai = c(1,1,1,1))
plot(Y, Yhat1, xlab = "Y",  ylab = expression(hat(Y)),
  cex.lab = 1.5, cex.main = 2,
  main = "Fitted Data Set")   # Fig. 8.2a
points(Y, Yhat3, pch = 3)
points(Y, Yhat5, pch = 16)
lines(c(-1.5,1.5), c(-1.5,1.5))
legend(x = -1.75, y = 1, pch = c(1,3,16),legend = c("p-1 = 1",
   "p-1 = 3", "p-1 = 5"))

# Color version
plot(Y, Yhat1, xlab = "Y",  ylab = expression(hat(Y)),
  cex.lab = 1.5, cex.main = 2, pch = 16, col = "blue",
  main = "Fitted Data Set")   # Fig. 8.2a
points(Y, Yhat3, pch = 16, col = "green")
points(Y, Yhat5, pch = 16, col = "red")
lines(c(-1.5,1.5), c(-1.5,1.5))
legend(x = -1.75, y = 1, pch = c(16,16,16),legend = c("p-1 = 1",
   "p-1 = 3", "p-1 = 5"), col = c("blue", "green", "red"))


X2 <- matrix(rnorm(50), ncol = 5, byrow = TRUE)
X2[,5] <- 10 * X2[,5]
Y2 <- X2 %*% beta.lm
Yhat1.2 <- cbind(rep(1,10),X2[,1]) %*% matrix(coef(model.1), ncol = 1)
Yhat3.2 <- cbind(rep(1,10),X2[,1:3]) %*% matrix(coef(model.3), ncol = 1)
Yhat5.2 <- cbind(rep(1,10),X2[,1:5]) %*% matrix(coef(model.5), ncol = 1)
plot(Y2, Yhat5.2, xlab = "Y",  ylab = expression(hat(Y)),
  cex.lab = 1.5, pch = 16, cex.main = 2,
  main = "Validation Data Set") # Fig. 8.2b
points(Y2, Yhat3.2, pch = 3)
points(Y2, Yhat1.2)
lines(c(-2.5,2.5), c(-2.5,2.5))
legend(x = -2, y = 1, pch = c(1,3,16),legend = c("p-1 = 1",
   "p-1 = 3", "p-1 = 5"))
