library(sp)
library(spgwr)

# Artificial data set 
data.df <- expand.grid(x = seq(1,20),
  y = seq(20,1, by = -1))
set.seed(123)
data.df$X <- rnorm(400)
data.df$Y <- data.df$X + 0.1*rnorm(400)
coordinates(data.df) <- c("x", "y")
coef(lm(Y ~ X, data = data.df))
Y.bw <- gwr.sel(Y ~ X, data = data.df)
Y.gwr <- gwr(Y ~ X, data = data.df, bandwidth = Y.bw)
range(Y.gwr$SDF$X)
print(Y.sd <- sd(Y.gwr$SDF$X), digits = 3)

demo.perm <- function(){
   data.test <- data.df
   data.test@data$X <- sample(data.df@data$X,
      replace = FALSE)
   Y.bw <- gwr.sel(Y ~ X, data = data.df)
   Y.gwr <- gwr(Y ~ X, data = data.test,
      bandwidth = Y.bw)
   return(sd(Y.gwr$SDF$X))
}
set.seed(123)
U <- replicate(99, demo.perm())
length(which(U >= Y.sd)) / 100

data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
yield.pts <- read.csv("created\\set4.1yld96ptsidw.csv")
data.Set4.1$Yield <- yield.pts$Yield
coordinates(data.Set4.1) <- c("Easting", "Northing")
library(spgwr)
Sand.lm <- lm(Yield ~ Sand,  data = data.Set4.1)
coef(Sand.lm)
Sand.bw <- gwr.sel(Yield ~ Sand, data = data.Set4.1)
Sand.gwr <- gwr(Yield ~ Sand, data = data.Set4.1,
   bandwidth = Sand.bw)
range(Sand.gwr$SDF$Sand)
print(Sand.sd <- sd(Sand.gwr$SDF$Sand), digits = 3)

Sand.perm <- function(){
   data.test <- data.Set4.1
   data.test@data$Sand <- sample(data.Set4.1@data$Sand,
      replace = FALSE)
   Sand.bw <- gwr.sel(Yield ~ Sand, data = data.test)
   Sand.gwr <- gwr(Yield ~ Sand, data = data.test,
      bandwidth = Sand.bw)
   return(sd(Sand.gwr$SDF$Sand))
}
set.seed(123)
U <- replicate(99, Sand.perm())
length(which(U >= Sand.sd)) / 100
