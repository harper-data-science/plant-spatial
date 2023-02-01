set.seed(123)
X <- rnorm(20)
Y <- X + 0.5 * rnorm(20)
par(mai = c(1,1,1,1))
plot(X,Y, cex.lab = 1.5, xlim = c(-2,4.5),
   ylim = c(-2,4))#Fig. A.2a
YX.lm <- lm(Y ~ X)
summary(YX.lm)
abline(YX.lm)
X.df <- data.frame(cbind(X = X, i = 1:20))
text(X+0.1, Y, as.character(X.df$i))
im.lm <- influence.measures(YX.lm)
im.lm$is.inf

# Compute the coeficients of the "good" model
b0 <- coef(YX.lm)[1]
b1 <- coef(YX.lm)[2]

set.seed(123)
X <- rnorm(20)
Y <- X + 0.5 * rnorm(20)
Y[2] <- Y[2] + 2
YX.lm <- lm(Y ~ X)
im.lm <- influence.measures(YX.lm)
im.lm$is.inf

plot(X,Y, cex.lab = 1.5, xlim = c(-2,4.5),
   ylim = c(-2,4)) # Fig. A.2b
summary(YX.lm)
abline(YX.lm)
lines(c(-2,4), c(b0-b1*2, b0+ b1*4), lty = 2)
X.df <- data.frame(cbind(X = X, i = 1:20))
text(X+0.1, Y, as.character(X.df$i))
legend(-1.75, 3, c("Orignal model", "Modified model"), lty = c(2,1))


set.seed(123)
X <- rnorm(20)
Y <- X + 0.5 * rnorm(20)
Y[18] <- Y[18] + 2
YX.lm <- lm(Y ~ X)
im.lm <- influence.measures(YX.lm)
im.lm$is.inf

plot(X,Y, cex.lab = 1.5, xlim = c(-2,4.5),
   ylim = c(-2,4))  #Fig. A.2c
summary(YX.lm)
abline(YX.lm)
lines(c(-2,4), c(b0-b1*2, b0+ b1*4), lty = 2)
X.df <- data.frame(cbind(X = X, i = 1:20))
text(X+0.1, Y, as.character(X.df$i))
legend(-1.75, 3, c("Orignal model", "Modified model"), lty = c(2,1))


set.seed(123)
X <- rnorm(20)
X[16] <- X[16] + 2
Y <- X + 0.5 * rnorm(20)
YX.lm <- lm(Y ~ X)
im.lm <- influence.measures(YX.lm)
im.lm$is.inf

plot(X,Y, cex.lab = 1.5, xlim = c(-2,4.5),
   ylim = c(-2,4)) # Fig. A.2d
summary(YX.lm)
abline(YX.lm)
lines(c(-2,4), c(b0-b1*2, b0+ b1*4), lty = 2)
X.df <- data.frame(cbind(X = X, i = 1:20))
text(X+0.1, Y, as.character(X.df$i))
legend(-1.75, 3, c("Orignal model", "Modified model"), lty = c(2,1))


set.seed(123)
X <- rnorm(20)
Y <- X + 0.5 * rnorm(20)
X[16] <- X[16] + 2
Y[16] <- Y[16] - 3
YX.lm <- lm(Y ~ X)
im.lm <- influence.measures(YX.lm)
im.lm$is.inf

plot(X,Y, cex.lab = 1.5, xlim = c(-2,4.5),
   ylim = c(-2,4))  # Fig. A.2e
summary(YX.lm)

abline(YX.lm)
lines(c(-2,4), c(b0-b1*2, b0+ b1*4), lty = 2)
X.df <- data.frame(cbind(X = X, i = 1:20))
text(X+0.2, Y, as.character(X.df$i))
legend(-1.75, 3, c("Orignal model", "Modified model"), lty = c(2,1))
