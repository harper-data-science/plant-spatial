data.Set4.1.96 <- read.csv("created\\set4.1yld96ptsidw.csv")
data.Set4.1.97 <- read.csv("created\\set4.1yld97ptsidw.csv")
data.Set4.1.98 <- read.csv("created\\set4.1yld98ptsidw.csv")
data.Set4.1.99 <- read.csv("created\\set4.1yld99ptsidw.csv")

# Eliminate the last 12 rows of each data set
Y96 <- data.Set4.1.96[(1:74),]
Y97 <- data.Set4.1.97[(1:74),]
Y98 <- data.Set4.1.98[(1:74),]
Y99 <- data.Set4.1.99[(1:74),]

# Scale the data
Y96.scale <- 100 * Y96[,2] / max(Y96[,2])
Y97.scale <- 100 * Y97[,2] / max(Y97[,2])
Y98.scale <- 100 * Y98[,2] / max(Y98[,2])
Y99.scale <- 100 * Y99[,2] / max(Y99[,2])

Y.scale <- cbind(Y96.scale, Y97.scale, Y98.scale, Y99.scale)


dxx <- numeric(74)
dyy <- numeric(74)
comp.d <- function(i, Y, bdry1, bdry2, c){
  if (i %in% bdry1) return(-(Y[i] - Y[i+c]) / 2)
  if (i %in% bdry2) return(-(Y[i-c] - Y[i]) / 2)
  return((Y[i-1] - 2*Y[i] + Y[i+1])/ 2)
}

# First 8 rows (7 columns)
xlbdry <- c(1,8,15,22,29,36,43,50)
xrbdry <- c(7,14,21,28,35,42,49,56)
yubdry <- 1:7
ylbdry <- 49:56
for (i in 1:56)  dxx[i] <- comp.d(i, Y.scale[,1],xlbdry, xrbdry, 1)
for (i in 1:56)  dyy[i] <- comp.d(i, Y.scale[,1],yubdry, ylbdry, 4)

# Next 3 rows (6 columns)
xlbdry <- c(57,63,69)
xrbdry <- c(62,68,74)
yubdry <- 57:62


ylbdry <- 68:74
for (i in 57:74)  dxx[i] <- comp.d(i, Y.scale[,1],xlbdry, xrbdry, 1)
for (i in 57:74)  dyy[i] <- comp.d(i, Y.scale[,1],yubdry, ylbdry, 4)

MSE <- function(P){
  Yest <- numeric(74)
  for (i in 1:74) Yest[i] <- Y1[1] + P[1] * (dxx[i] + dyy[i]) + P[2] * Y1[i]
  E <- sum((Yest[i] - Yp1[i])^2) / length(Y1)
   return(E)
}

D <- numeric(3)
mu <- numeric(3)

for (n in 1:3){
   Yp1 <- Y.scale[,n+1]
   Y1 <- Y.scale[,n]
   soln <- nlm(MSE, c(0,0))
   D[n] <- soln[[2]][1]
   mu[n] <- soln[[2]][2]
}

D
mu


