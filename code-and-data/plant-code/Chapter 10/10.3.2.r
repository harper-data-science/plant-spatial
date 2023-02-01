# Creating blocks for block bootstrap
set.seed(123)
lambda <- 0.4
Y <- numeric(110)
for (i in 2:110) Y[i] <- lambda * Y[i - 1] + rnorm(1, sd = 1)
Ysamp <- Y[11:110]
n.blocks <- 10
blocks <- matrix(Ysamp, ncol = n.blocks)

# Block sample function
block.samp <- function(n.blocks, blocks){
   cols <- sample(1:ncol(blocks), n.blocks, replace = TRUE)
   return(as.vector(blocks[,cols]))
}

set.seed(123)
print(A <- matrix(1:9,3))
print(B <- block.samp(6, A))

mean.b.boot <- function(n.blocks, blocks){
   mean(block.samp(n.blocks, blocks))
}

set.seed(123)
mean.b.boot(6, A)
mean(B)

mean(Ysamp)
mean(blocks)

set.seed(123)
u <- replicate(200, mean.b.boot(10, blocks))
t.stat <- mean(blocks) / sd(u)
print(p <- 2 * (1 - pt(q = abs(t.stat),
   df = nrow(blocks)*ncol(blocks) - 1)))

t.b.boot <- function(lambda){
# Generate the sample, eliminating the initial transient
   Y <- numeric(110)
   for (i in 2:110) Y[i] <- lambda * Y[i - 1] + rnorm(1, sd = 1)
   Ysamp <- Y[11:110]
# Break into blocks
   n.blocks <- 10
   blocks <- matrix(Ysamp, ncol = n.blocks)
#Compute the bootstrap resample
   U <- replicate(200, mean.b.boot(n.blocks, blocks))
# Carry out the t-test
   t.stat <- mean(blocks) / sd(U)
   p <- 2 * (1 - pt(q = abs(t.stat),
     df = nrow(blocks)*ncol(blocks) - 1))
   return(c(as.numeric(p < 0.05), sd(U)))
}


set.seed(123)
lambda <- 0.4
U <- replicate(10000, t.b.boot(lambda))
mean(U[1,]) # Error rate
mean(U[2,]) # Mean est. std. error

