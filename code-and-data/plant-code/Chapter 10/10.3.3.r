# Parametric bootstrap 1 run, use bootstrap residuals
lambda <- 0.4 #Autocorrelation term
set.seed(123)
Y <- numeric(110)
for (i in 2:110) Y[i] <- lambda * Y[i - 1] + rnorm(1, sd = 1)
Ysamp <- Y[11:110]

# Fit a time series to the data
Ysamp.ts <- ts(Ysamp)
Ysamp.ar <- arima(Ysamp.ts, c(1,0,0))
print(lambda.hat <- coef(Ysamp.ar)[1], digits = 3)
e.hat <- residuals(Ysamp.ar)

# Simulate the time series
para.samp <- function(Ysamp, e.hat, lambda.hat){
   Y.sim <- numeric(length(Ysamp))
   Y.sim[1] <- Ysamp[1]
   for (i in 2:length(Ysamp)){
      ei.boot <- sample(e.hat, 1)
      Y.sim[i] <- lambda.hat * Y.sim[i - 1] + ei.boot
  }
  return(mean(Y.sim))
}

set.seed(123)
U <- replicate(200,para.samp(Ysamp, e.hat, lambda.hat))
print(sd(U), digits = 3)
print(sd(Ysamp) / 10, digits = 3)

# Monte Carlo simulation of parametric bootstrap, use the bootstrap residuals

t.para.boot <- function(lambda){
# Generate the sample, eliminating the initial transient
   Y <- numeric(110)
   for (i in 2:110) Y[i] <- lambda * Y[i - 1] + rnorm(1, sd = 1)
   Ysamp <- Y[11:110]
# Fit a time series to the data
   Ysamp.ts <- ts(Ysamp)
   Ysamp.ar <- arima(Ysamp.ts, c(1,0,0))
   lambda.hat <- coef(Ysamp.ar)[1]
   e.hat <- residuals(Ysamp.ar)
# Bootstrap resampling
   u <- replicate(200,para.samp(Ysamp, e.hat, lambda.hat))
# Carry out the t-test
   t.stat <- mean(Ysamp) / sd(u)
   p <- 2 * (1 - pt(q = abs(t.stat),
     df = length(Ysamp) - 1))
   return(c(as.numeric(p < 0.05), sd(u)))
}

# Monte Carlo replications
set.seed(123)
lambda <- 0.4
U <- replicate(10000, t.para.boot(lambda))
mean(U[1,])
mean(U[2,])


