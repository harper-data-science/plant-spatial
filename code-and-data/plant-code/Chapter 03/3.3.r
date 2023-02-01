coin.toss <- function (n.tosses, p){
  n.heads <- rbinom(1,n.tosses,p)
}

set.seed(123)
n.tosses <- 20
p <- 0.5
n.reps <- 10000
U <- replicate(n.reps, coin.toss(n.tosses, p))
mean(U)
var(U)
par(mai = c(1,1,1,1))
hist(U, cex.lab = 1.5,  # Fig. 3.5
   main = "Number of Heads in 20 Tosses",
   cex.main = 2, xlab = "Number of Heads")

