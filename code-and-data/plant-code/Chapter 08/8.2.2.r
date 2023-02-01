# Simple partial regression

# Model A
set.seed(123)
XA <- cbind(rnorm(100), rnorm(100))
par(mai = c(1,1,1,1))
plot(XA[,1], XA[,2],  # Fig. 8.3a
   main = expression("Data Set A:"~italic(X[i])~
      "Uncorrelated"), cex.main = 2,
   xlab = expression(italic(X)[1]),
   ylab = expression(italic(X)[2]),
   cex.lab = 1.5)

var(XA)

eps <- rnorm(100)
YA <- rowSums(XA) + 0.1 * eps
summary(lm(YA ~ XA))
summary(lm(YA ~ XA[,1]))

# Model B
XB <- cbind(XA[,1], XA[,1] + 0.0001 * XA[,2])
plot(XB[,1], XB[,2],  # Fig. 8.3b
   main = expression("Data Set B:"~italic(X[i])~
      "Correlated"), cex.main = 2,
   xlab = expression(italic(X)[1]),
   ylab = expression(italic(X)[2]),
   cex.lab = 1.5)

var(XB)
YB <- rowSums(XB) + 0.1 * eps
summary(lm(YB ~ XB))
summary(lm(YB ~ XB[,1]))

data.Set4.1 <- read.csv("Set4\\Set4.196sample.csv", header = TRUE)
data.Yield4.1idw <- read.csv("created\\set4.1yld96ptsidw.csv", header = TRUE)
data.Set4.1$Yield <-  data.Yield4.1idw$Yield
print(coef(lm(Yield ~ Clay, data = data.Set4.1)), digits = 4)
print(coef(lm(Yield ~ SoilK, data = data.Set4.1)), digits = 3)
print(coef(lm(Yield ~ Clay + SoilK, data = data.Set4.1)), digits = 2)


e.YA1 <- residuals(lm(YA ~ XA[,1]))
e.XA21 <- residuals(lm(XA[,2] ~ XA[,1]))
plot(e.XA21, e.YA1, #Fig. 8.4a
   main = "Added Variable Plot of Data Set A", cex.main = 2,
   xlab = expression(Residuals~of~italic(X)[2]),
   ylab = expression(Residuals~of~italic(Y)),
   cex.lab = 1.5)
   
e.YB1 <- residuals(lm(YB ~ XB[,1]))
e.XB21 <- residuals(lm(XB[,2] ~ XB[,1]))
plot(e.XB21, e.YB1, main = "Added Variable of Data Set B", # Fig. 8.4b
   cex.main = 2, xlab = expression(bold(Residuals~of~italic(X)[2])),
   ylab = expression(bold(Residuals~of~italic(Y))), cex.lab = 1.5)

cor(e.YA1,e.XA21)
coef(lm(YA ~ XA))
coef(lm(YA ~ XA[,1]))
coef(lm(e.YA1 ~ e.XA21))

cor(e.YB1,e.XB21)
coef(lm(YB ~ XB))
coef(lm(YB ~ XB[,1]))
coef(lm(e.YB1 ~ e.XB21))

library(car)
avPlots(lm(YA ~ XA))
avPlots(lm(YB ~ XB))

A.lm <- lm(YA ~ XA)
e.PRA <- residuals(A.lm)
Y.PRA <- e.PRA + coef(A.lm)["XA2"] * XA[,2]
plot(XA[,2], Y.PRA, main = "Partial Residual Plot of Data Set A",
   cex.main = 2, xlab = expression(italic(X)[2]),
   ylab = expression(Partial~Residuals~of~italic(X)[2]),
   cex.lab = 1.5)  # Fig. 8.4c
coef(lm(Y.PRA ~ XA[,2]))

B.lm <- lm(YB ~ XB)
e.PRB <- residuals(B.lm)
Y.PRB <- e.PRB + coef(B.lm)["XB2"] * XB[,2]
plot(XB[,2], Y.PRB, main = "Partial Residual Plot of Data Set B",
   cex.main = 2, xlab = expression(italic(X)[2]),
   ylab = expression(Partial~Residuals~of~italic(X)[2]),
   cex.lab = 1.5)  # Fig. 8.4d
coef(B.lm)
coef(lm(Y.PRB ~ XB[,2]))

#Added variable regression
summary(lm(e.YB1 ~ e.XB21))
#Partial residual regression
summary(lm(Y.PRB ~ XB[,2]))

