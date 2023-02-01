# Generate artificial data
beta.true <- 1
set.seed(123)
n <- 20
print(X <- rnorm(n))
print(Y <- beta.true * X + rnorm(n))
summary(lm(Y ~ X))
par(mai = c(1,1,1,1))
plot(X, Y, main = "Data for Gibbs Sampler", cex.main = 2,
   cex.lab = 1.5) # Fig. 14.2

# Set prior parameters
s2.pri <- 100
beta.pri <- 0
mu.pri <- 0.01
nu.pri <- 0.01

# Initialize tau
cur.tau <- 0.01

MCMC.data <- numeric(5000)
for (i in 1:5000){
# Use  current tau to generate conditional posterior of beta
   sb2.post <- 1/(1/s2.pri + cur.tau*sum(X^2))
   beta.post <- sb2.post*(beta.pri/s2.pri+ cur.tau*sum(X*Y))
# Draw beta from the current conditional posterior
   cur.beta <- rnorm(1, beta.post, sqrt(sb2.post))
# Use  current beta to generate conditional posterior of tau
   mu.post <- n + mu.pri
   nu.post <- (nu.pri + sum((Y-cur.beta*X)^2)) / 2
# Draw tau from the current conditional posterior
   cur.tau <- rgamma(1, shape = mu.post, rate = nu.post)
   MCMC.data[i] <- c(cur.beta)
}

burn.in <- 1000
beta.MCMC <- MCMC.data[-(1:burn.in)]
mean(beta.MCMC)

plot(1:200, MCMC.data[1:200], type = "l",
   ylab = expression(beta^"(i)"), cex.lab = 1.5,
   xlab = "iteration i")  # Fig.  14.3


