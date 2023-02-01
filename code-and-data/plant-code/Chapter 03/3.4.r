# Monte Carlo simulation of t tests with autocorrelated data

#Ordinary data, one test
set.seed(123)
Y <- rnorm(10)
Y.ttest <- t.test(Y, alternative = "two.sided")
Y.ttest$p.value

# Monte Carlo simulation of a t-test
set.seed(123)
ttest <- function(){
  Y <- rnorm(10)
  t.ttest <- t.test(Y,alternative = "two.sided")
  TypeI <- t.ttest$p.value < 0.05
  return(TypeI)
}
U <- replicate(10000, ttest())
mean(U)


# Difference between two means
set.seed(123)
print(Y1 <- rnorm(5))
print(Y2 <- rnorm(5))
print(d <- mean(Y1) - mean(Y2))

# Single permutation test
Y <- c(Y1, Y2)
Ysamp <- sample(Y,length(Y))
print(Yprime1 <- Ysamp[1:5])
print(Yprime2 <- Ysamp[6:10])
print(dprime <- mean(Yprime1) - mean(Yprime2))

# Replicated permutation test with 10 permutations
perm.diff <- function(Y1, Y2){
  Y <- c(Y1, Y2)
  Ysamp <- sample(Y,length(Y))
  Yprime1 <- Ysamp[1:5]
  Yprime2 <- Ysamp[6:10]
  dprime <- mean(Yprime1) - mean(Yprime2)
  return(dprime)
}

set.seed(123)
U <- replicate(9, perm.diff(Y1, Y2))
sort(c(d,U))

# Replicated permutation test with 10,000 permutations
set.seed(123)
U <- replicate(9999, perm.diff(Y1, Y2))
U <- c(U, d) # Add original observation to get 10,000 
U.low <- U[U < d] # Obtain diff. values less than d
{if (d < median(U)) # Is d in the upper or lower tail?
   n.tail <- length(U.low)
 else
   n.tail <- 10000 - length(U.low)
}
print(p <- 2 * n.tail / 10000) # Two tail test

t.test(Y1, Y2, "two.sided")$p.value





