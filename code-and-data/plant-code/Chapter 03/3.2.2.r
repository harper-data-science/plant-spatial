# Monte Carlo simulation of a coin toss experiment

set.seed(123)
n.tosses <- 30
head <- numeric(n.tosses)
for (i in 1:n.tosses) head[i] <- rbinom(1,1,0.5)
head

set.seed(123)
n.tosses <- 30
head <- numeric(n.tosses)
p <- 0.5
for (i in 1:n.tosses){
  head[i] <- rbinom(1,1,p)
  p <- ifelse(head[i] > 0, 0.8, 0.2)
}
head

set.seed(123)
n.tosses <- 30
head <- numeric(n.tosses)
p <- 0.3
for (i in 1:n.tosses){
  head[i] <- rbinom(1,1,p)
  p <- p + 0.02
}
head


