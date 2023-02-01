library(boot)

# Generate a population
set.seed(123)
pop.Y <- rlnorm(1000)
print(Y.bar <- mean(pop.Y))
print(pop.var <- sum((pop.Y - Y.bar)^2) / 1000)

par(mai = c(1,1,1,1))
hist(pop.Y, main = "Population Histogram",
   cex.main = 2, xlab = expression(italic(Y)),
   cex.lab = 1.5) # Fig. 10.3a

# Take a sample of size 20 (without replacement)
set.seed(123)
print(sample.Y <- sample(pop.Y, size = 20), digits = 3)
print(mean(sample.Y), digits = 3)
print(sd(sample.Y) / sqrt(20), digits = 3)
print(sqrt(pop.var / 20), digits = 3)

boot.sample <- function(x) sample(x, size = length(x),
   replace = TRUE)
# One bootstrap resample
print(b <- boot.sample(sample.Y), digits = 3)
print(mean(b), digits = 3)
# A second bootstrap resample
print(b <- boot.sample(sample.Y), digits = 3)
print(mean(b), digits = 3)
#A third bootstrap resample
print(b <- boot.sample(sample.Y), digits = 3)
print(mean(b), digits = 3)


# Compute the bootstap sample of means
sample.mean <- function(x){
   Y <- sample(x, size = length(x), replace = TRUE)
   return(mean(Y))
}
set.seed(123)
boot.dist <- replicate(1000, sample.mean(sample.Y))
print(mean(boot.dist), digits = 3)
print(sd(boot.dist), digits = 3)
hist(boot.dist, main = "Bootstrap Resample Histogram",
   cex.main = 2, xlab = expression(italic(bar(Y))),
   cex.lab = 1.5)  # Fig 10.3b

# Now use the function boot
set.seed(123)
mean.Y <- function(Y,i) mean(Y[i])
boot.Y <- boot(sample.Y, mean.Y, R = 1000)
print(sd(boot.Y$t), digits = 3)


