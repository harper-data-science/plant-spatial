# Ex 15.1
library(spacetime)
library(gstat)
library(date) 
library(maptools) 

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
Y96.scale <- scale(Y96[,2])
Y97.scale <- scale(Y97[,2])
Y98.scale <- scale(Y98[,2])
Y99.scale <- scale(Y99[,2])
Yield.scale <- data.frame("Yield" = c(Y96.scale,
    Y97.scale, Y98.scale, Y99.scale))
# Create the attribute data
eps <- 5
lambda <- 0.4
nt <- 100
eta <- numeric(nt)
# Create the set of attribute values
set.seed(123)
v <- c(Y96.scale[1], Y97.scale[1], Y98.scale[1],
  Y99.scale[1])
t <- c(0, 1, 2, 3)
s <- spline(t, v, n = nt, method = "natural")
for (i in 2:nt) eta[i] <- lambda * eta[i - 1] + eps * rnorm(1)
v2 <- s$y + eta

Y <- v2
for (i in 2:74){
  v <- c(Y96.scale[i], Y97.scale[i], Y98.scale[i],
    Y99.scale[i])
  t <- c(96,97,98,99)
  s <- spline(t, v, n = nt, method = "natural")
  eta[1] <- 0
  for (i in 2:nt) eta[i] <- lambda * eta[i - 1] + eps * rnorm(1)
  v2 <- s$y + eta
  Y <-c(Y, v2)
}

Y.data <- data.frame(Y = Y)
nrow(Y.data)

Y.sites <- data.frame(Easting = Y96$Easting, Northing = Y96$Northing)
coordinates(Y.sites) <- c("Easting", "Northing")
proj4string(Y.sites) <- CRS("+proj=utm +zone=10 +ellps=WGS84")

Y.time <- as.POSIXct(as.date(0:(nt - 1)))

# Create the STFDF object
Y.stfdf <- STFDF(Y.sites, Y.time, data = Y.data)

#Simple empirical variogram
Y.empvgm <- variogramST(Y ~ 1, data = Y.stfdf, tlags = 0:7, cutoff = 400,
    width = 100)
greys <- grey(0:200 / 255)
plot(Y.empvgm, wireframe = TRUE, col = greys) 
print(st.ani <- estiStAni(Y.empvgm, interval = c(0,500)))
# Sum-metric fit
Y.smm <- vgmST("sumMetric",  space = vgm(20, "Sph", 150, 1),
               time = vgm(10, "Exp", 2, 0.5), joint = vgm(80, "Sph", 1500, 2.5),
               stAni = st.ani)

Y.smm.fit <- fitSumMetricModel <- fit.StVariogram(Y.empvgm, Y.smm, fit.method = 7, stAni=st.ani,
    method = "L-BFGS-B", lower = c(sill.s = 0,  range.s = 10,  nugget.s = 0,
    sill.t = 0,  range.t = 0.1,   nugget.t = 0,
    sill.st= 0, range.st = 10, nugget.st = 0, anis = 40),
    upper = c(sill.s = 200,  range.s = 1E3,  nugget.s = 20,
    sill.t = 200,  range.t = 75,   nugget.t = 20,
    sill.st= 200, range.st = 5E3, nugget.st = 20, anis = 500),
    control = list(parscale = c(1,100,1,1,0.5,1,1,100,1,100), maxit=1e4))

plot(Y.empvgm, Y.smm.fit, wireframe=T, col = greys) 
attr(Y.smm.fit, "MSE")

# Ex 15.2

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
Y96.scale <- scale(Y96[,2])
Y97.scale <- scale(Y97[,2])
Y98.scale <- scale(Y98[,2])
Y99.scale <- scale(Y99[,2])

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

# 15.3

Yield96 <- read.csv("created\\set4.1yld96ptsidw.csv", header = TRUE)[1:74,]
Yield97 <- read.csv("created\\set4.1yld97ptsidw.csv", header = TRUE)[1:74,]
Yield98 <- read.csv("created\\set4.1yld98ptsidw.csv", header = TRUE)[1:74,]
Yield99 <- read.csv("created\\set4.1yld99ptsidw.csv", header = TRUE)[1:74,]

Yield.4Yr <- data.frame(Yield96 = scale(Yield96$Yield),
   Yield97 = scale(Yield97$Yield), Yield98 = scale(Yield98$Yield),
   Yield99 = scale(Yield99$Yield))

library(e1071)

min.sum <- function(i, j, perms, CM){
   test <- numeric(nrow(CM))
   diff.sum <- numeric(ncol(perms))
   for (n in 1:length(diff.sum)){
# test replaces the IDs in rep i with those in column n of perms
      for (m in 1:nrow(perms)) test[which(CM[,j] == m)] <- perms[m,n]
      diff.sum[n] <- sum((test != CM[,i]))
      }
# which.min automatically selects the first in a tie
   n.min <- which.min(diff.sum)
   opt.id <- diff.sum[n.min]
   return(c(n.min, opt.id))
}

beta.clus <- function(x) as.numeric(sum(abs(x[2:length(x)] - x[1])) == 0)
gamma.clus <- function(CM, set, k){
   sum.beta <- sum(apply(CM[,set], 1, beta.clus))
   n <- length(set)
   p <- nrow(CM)
   return((sum.beta - p/n^k) / (p - p/n^k))
}

cluster.k <- 2
n.reps <- 25
n.sites <- nrow(Yield.4Yr)
CM <- matrix(nrow = n.sites, ncol = n.reps)
set.seed(123)
for (i in 1:n.reps) CM[,i] <- kmeans(Yield.4Yr, cluster.k)$cluster
# Realign the clusters
dist.mat <- matrix(nrow = n.reps, ncol = n.reps)
min.id <- matrix(nrow = n.reps, ncol = n.reps)
perms <- t(permutations(cluster.k))
for (i in 1:n.reps){
   for (j in 1:n.reps){
      x <- min.sum(i,j,perms,CM)
      min.id[i,j] <- x[1]
      dist.mat[i,j] <- x[2]
   }
}
# Display the distance matrix to determine cluster sets
dist.mat

ref.rep <- 1
best <- min.id[ref.rep,]
CM.align <- matrix(nrow = n.sites, ncol = n.reps)
for (i in 1:n.reps){
   for (j in 1:cluster.k){
     CM.align[which(CM[,i] == j),i] <- perms[j,best[i]]
     }
}
CM.align
gamma.clus(CM.align, 1:ncol(CM.align), 2)
clusters2 <- data.frame(CM.align)

# 15.3a
par(mai = c(1,1,1,1))
clusters.2mean <- matrix(nrow = 4, ncol = 2)
for (i in 1:4) clusters.2mean[i,] <-
   tapply(Yield.4Yr[,i], clusters2$X1, mean)
clusters.2sd <- matrix(nrow = 4, ncol = 2)
for (i in 1:4) clusters.2sd[i,] <-
   tapply(Yield.4Yr[,i], clusters2$X1, sd)
matplot(1:4, clusters.2mean, type = "o",
  pch = c(1,2), col = "black", lty = 1, cex = 1,
  main = "k = 2", cex.main = 2, cex.lab = 1.5,
  xaxt = "n", xlab = "Year", ylab = "Scaled Yield",)
matplot(1:4, clusters.2mean + clusters.2sd, type = "o",
  pch = c(1,2), col = "black", lty = 2, cex = 1,
  main = "k = 2", cex.main = 2, cex.lab = 1.5,
  xaxt = "n", xlab = "Year", ylab = "Scaled Yield",
  add = TRUE)
matplot(1:4, clusters.2mean - clusters.2sd, type = "o",
  pch = c(1,2), col = "black", lty = 3, cex = 1,
  main = "k = 2", cex.main = 2, cex.lab = 1.5,
  xaxt = "n", xlab = "Year", ylab = "Scaled Yield",
  add = TRUE)

# 15.3b
matplot(1:4, t(as.matrix(Yield.4Yr,)),
  type = "o", pch = 13*clusters2$X1 - 10, col = "black",
  lty = clusters2$X1, 
  main = "k = 2", 
  cex.main = 2, cex.lab = 1.5, ylim = c(0,1),
  xaxt = "n", xlab = "Year", ylab = "Scaled Yield")
axis(side = 1, at = c(1,2,3,4),
   labels = c("1996","1997","1998","1999"))
legend(1,0.2, c("Cluster 1", "Cluster 2"), pch = c(3,16))


# Ex 15.4
# This is also used for Ex. 15.5
data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)[1:74,]
data.Set4.1$Yield96 <- read.csv("created\\set4.1yld96ptsidw.csv", header = TRUE)[1:74,2]
data.Set4.1$Yield97 <- read.csv("created\\set4.1yld97ptsidw.csv", header = TRUE)[1:74,2]
data.Set4.1$Yield98 <- read.csv("created\\set4.1yld98ptsidw.csv", header = TRUE)[1:74,2]
data.Set4.1$Yield99 <- read.csv("created\\set4.1yld99ptsidw.csv", header = TRUE)[1:74,2]

library(lattice)
agron.data <- subset(data.Set4.1,
   select = c(Clay, Silt, SoilP, SoilK, SoilpH, SoilTOC, Yield96,
      Yield97, Yield98, Yield99))
splom(agron.data, par.settings = list(fontsize=list(text=9),
  plot.symbol = list(col = "black")), pscales = 0,
  main = "Yield Relationships, Field 4.1") 

# 15.5
######### Imputation of weather data in Section 15.4.2
weather.data <- read.csv("Set4\\dailyweatherdata.csv", header = TRUE)
names(weather.data)

season.1 <- 62:225
season.2 <- 548:701
season.3 <- 913:1066
season.4 <- 1278:1431

which(is.na(weather.data$CIMIS_Eto_mm))
ETo1997 <- sum(weather.data$CIMIS_Eto_mm[season.2])
ETo1998 <- sum(weather.data$CIMIS_Eto_mm[season.3])
ETo1999 <- sum(weather.data$CIMIS_Eto_mm[season.4])
c(ETo1997, ETo1998, ETo1999)

which(is.na(weather.data$MinAirTemp_C))

impute.temp <- function(temp.data){
   temp.data1 <- temp.data
   temp.na <- which(is.na(temp.data1))
   if (length(temp.na) == 0) return(temp.data1)
   min.day <- min(temp.na)
   quit.impute <- 0
   while (quit.impute == 0){
      day <- min.day
      while(is.na(temp.data1[day])) day <- day + 1
      impute.days <- min.day:(day-1)
      day.no <- impute.days - min.day + 1
      lin.interp <- day.no / (max(day.no) + 1)
      temp.data1[impute.days] <- temp.data[min.day-1] +
         (temp.data[day] - temp.data[min.day-1]) * lin.interp
      min.day <- day
      if (min.day > max(temp.na)) quit.impute <- 1 else
         min.day <- temp.na[min(which(temp.na > min.day))]
   }
   temp.data1
}
season.length <- 1431 - 1278 + 1
imputed.max1997 <- impute.temp(weather.data$MaxAirTemp_C[season.2])
imputed.min1997 <- impute.temp(weather.data$MinAirTemp_C[season.2])
imputed.max1998 <- impute.temp(weather.data$MaxAirTemp_C[season.3])
imputed.min1998 <- impute.temp(weather.data$MinAirTemp_C[season.3])
imputed.max1999 <- impute.temp(weather.data$MaxAirTemp_C[season.4])
imputed.min1999 <- impute.temp(weather.data$MinAirTemp_C[season.4])
DD.per.day <- function(max.temp, min.temp) (max.temp+min.temp)/2 - 10

# 15.6
# Assumes files from Exercise 15.5 are loaded
precip1997 <- cumsum(weather.data$Precip_mm[season.2])
precip1998 <- cumsum(weather.data$Precip_mm[season.3])
precip1999 <- cumsum(weather.data$Precip_mm[season.4])
plot(1:season.length, precip1998, type = "l",
   xlab = "Days after 1 April", cex.lab = 1.5,
   ylab = "Precip (mm)", main = "Cumulative Precipitation",
   cex.main = 2, lty = 2)
lines(1:season.length, precip1997, lty = 1)   # Fig. 15.14b
lines(1:season.length, precip1999, lty = 3)
legend(10, 85, c("1997", "1998", "1999"), lty = 1:3)



# 15.7
library(rpart)
library(maptools)
# Do for each year
year <- "1997"
data.Set4.1$Yield <- data.Set4.1$Yield97
year <- "1998"
data.Set4.1$Yield <- data.Set4.1$Yield98
year <- "1999"
data.Set4.1$Yield <- data.Set4.1$Yield99

cont.parms <- rpart.control(minsplit = 5, cp = 0.05)
Set4.1.model <- "Yield ~ Clay + Silt + SoilpH + SoilTOC +
   SoilP + SoilK"
Set4.1.rp <-  rpart(Set4.1.model, data = data.Set4.1, control = cont.parms,
   method = "anova")
plot(Set4.1.rp,branch = 0.4,uniform = T,margin = 0.1,
   main = paste("Field 4.1 1 Yield Regression Tree", year))
text(Set4.1.rp,use.n = T)
summary(Set4.1.rp)
data.disp <- data.Set4.1
coordinates(data.disp) <- c("Easting", "Northing")

#1997 & 1998
data.disp$pch <- 1
data.disp$pch[which(data.disp$SoilP >= 7.235)] <- 2
data.disp$pch[which(data.disp$SoilP >= 7.235 &
   data.disp$Clay <= 35.06)] <- 3
data.disp$pch[which(data.disp$SoilP >= 7.235 &
   data.disp$Clay <= 35.06 & data.disp$Silt > 39.42)] <- 4
 
plot(data.disp, pch = data.disp$pch)

# 15.8
Yield96 <- read.csv("created\\set4.1yld96ptsidw.csv", header = TRUE)[1:74,]
Yield97 <- read.csv("created\\set4.1yld97ptsidw.csv", header = TRUE)[1:74,]
Yield98 <- read.csv("created\\set4.1yld98ptsidw.csv", header = TRUE)[1:74,]
Yield99 <- read.csv("created\\set4.1yld99ptsidw.csv", header = TRUE)[1:74,]
Yield96.norm <- Yield96$Yield / max(Yield96$Yield)
Yield97.norm <- Yield97$Yield / max(Yield97$Yield)
Yield98.norm <- Yield98$Yield / max(Yield98$Yield)
Yield99.norm <- Yield99$Yield / max(Yield99$Yield)
Yield.4Yr <- data.frame(Yield96 = Yield96.norm,
   Yield97 = Yield97.norm, Yield98 = Yield98.norm,
   Yield99 = Yield99.norm)
data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)[1:74,]
data.Set4.1$Region <- 1
data.Set4.1$Region[which(data.disp$Clay < 35.52)] <- 17
data.Set4.1$Region[which(data.disp$Clay >= 35.2 & 
   data.disp$SoilTN >= 0.1065)] <- 2  
regions.mean <- matrix(nrow = 4, ncol = 3)
for (i in 1:4) regions.mean[i,] <-
   tapply(Yield.4Yr[,i], data.Set4.1$Region, mean)
matplot(regions.mean, type = "o",
  pch = c(1:5), col = "black", lty = 1:5,
  main = "Yield by region", cex.main = 2, cex.lab = 1.5,
  xaxt = "n", xlab = "Year", ylab = "Normalized Yield",
  ylim = c(0,1))
axis(side = 1, at = c(1,2,3,4),
   labels = c("1996","1997","1998","1999"))

# 15.9
library(R2WinBUGS)
set.seed(123)
n <- 20
X1 <- rnorm(n)
set.seed(456)
X2 <- rnorm(n)
Y2 <- 0.7 * X2 + rnorm(n)
Y1 <- X1 + rnorm(n)
Y12 <- c(Y1,Y2)
X12 <- c(X1,X2)

demo.model <- function(){
beta0 ~ dnorm(0, 0.01)
beta1 ~ dnorm(0, 0.01)
tau ~ dgamma(0.01, 0.01)
for (i in 1:n)
  {
  Y.hat[i] <- beta0 + beta1 * X[i]
  Y[i] ~ dnorm(Y.hat[i], tau)
  }
}

write.model(demo.model,
   paste(mybugsdir,"\\demomodel.bug", sep = ""))

XY.data <- list(X = X12, Y = Y12, n = 2*n)
XY.inits <- function(){
   list(beta0 = rnorm(1), beta1 = rnorm(1), tau = exp(rnorm(1)))
}

demo.sim12 <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\demomodel.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 1,
   n.iter = 5000, n.burnin = 1000,
   bugs.directory="c:\\Program Files\\WinBUGS14\\")
print(t(cbind(demo.sim12$mean, demo.sim12$sd)), digits = 3)



# 15.10
data.Set3 <- read.csv("Set3\\Set3data.csv", header = TRUE)
data.Set3$YieldN <- data.Set3$Yield / max(data.Set3$Yield)
data.Set3S2 <- data.Set3[which(data.Set3$Season == 2),]
data.Set3S3 <- data.Set3[which(data.Set3$Season == 3),]
data.Set3F3S2 <- data.Set3S2[which(data.Set3S2$Field == 3),]
data.Set3F3S3 <- data.Set3S3[which(data.Set3S3$Field == 3),]
library(maptools)
f3.spdf <- data.Set3F3S2
coordinates(f3.spdf) <- c("Easting", "Northing")
plot(f3.spdf, pch = f3.spdf$Irrig)

# 15.11 (Same data as 15.10
# Season 3
# a)
# Noninf prior, data from season 3
library(R2WinBUGS)
Set3model.ni <- function(){
beta0 ~ dnorm(0, 0.001)
beta1 ~ dnorm(0, 0.001)
tau ~ dgamma(0.01, 0.01)
for (i in 1:n)
  {
  Y.hat[i] <- beta0 + beta1*Irrig[i]
  Yield[i] ~ dnorm(Y.hat[i], tau)
  }
}

write.model(Set3model.ni,
   paste(mybugsdir,"\\Set3model.ni.bug", sep = ""))

XY.data.3S3 <- list(Irrig = data.Set3F3S3$Irrig, Yield = data.Set3F3S3$YieldN,
   n = nrow(data.Set3F3S3))
XY.inits <- function(){
   list(beta0 = rnorm(1), beta1 = rnorm(1), tau = exp(rnorm(1)))}

Set3.sim.S3ni <- bugs(data = XY.data.3S3, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.ni.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 5,
   n.iter = 10000, n.burnin = 1000, n.thin = 10,
   bugs.directory="c:\\Program Files\\WinBUGS14\\")
print(Set3.sim.S3ni, digits = 3)

# b)Third season, Field 3, informative prior from that field
# in season 2
XY.data.3S2 <- list(Irrig = data.Set3F3S2$Irrig, Yield = data.Set3F3S2$YieldN,
   n = nrow(data.Set3F3S2))
Set3.sim.S2ni <- bugs(data = XY.data.3S2, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.ni.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 5,
   n.iter = 10000, n.burnin = 1000, n.thin = 10,
   bugs.directory="c:\\Program Files\\WinBUGS14\\")

print(mu.0 <- Set3.sim.S2ni$mean$beta0, digits = 3)
print(tau.0 <- 1/Set3.sim.S2ni$sd$beta0^2, digits = 3)
print(mu.1 <- Set3.sim.S2ni$mean$beta1, digits = 3)
print(tau.1 <- 1/Set3.sim.S2ni$sd$beta1^2, digits = 3)
print(nu.t <- Set3.sim.S2ni$mean$tau / Set3.sim.S2ni$sd$tau^2,
  digits = 3)
print(mu.t <- Set3.sim.S2ni$mean$tau * nu.t, digits = 3)

Set3.model.ip <- function(){
beta0 ~ dnorm(mu.0, tau.0)
beta1 ~ dnorm(mu.1, tau.1)
tau ~ dgamma(mu.t, nu.t)
for (i in 1:n)
  {
  Y.hat[i] <- beta0 + beta1 * Irrig[i]
  Yield[i] ~ dnorm(Y.hat[i], tau)
  }
}

write.model(Set3.model.ip,
   paste(mybugsdir,"\\Set3model.ip.bug", sep = ""))

XY.data.F3S2.ip <- list(Irrig = data.Set3F3S2$Irrig, Yield = data.Set3F3S2$YieldN,
   n = nrow(data.Set3F3S2),
   mu.0 = mu.0, tau.0 = tau.0, mu.1 = mu.1, tau.1 = tau.1,
   mu.t = mu.t, nu.t = nu.t)

Set3.sim.3.ip <- bugs(data = XY.data.F3S2.ip, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.ip.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 5,
   n.iter = 10000, n.burnin = 1000, n.thin = 10,
   bugs.directory="c:\\Program Files\\WinBUGS14\\")
print(Set3.sim.3.ip, digits = 3)

# c)Third season, Field 3, informative prior from northern fields
# in season 2
data.Set3S2N <- data.Set3S2[which(data.Set3S2$Northing > 6340000),]

XY.data.S2N <- list(Irrig = data.Set3S2N$Irrig, Yield = data.Set3S2N$YieldN,
   n = nrow(data.Set3S2N))
   
Set3.sim.S2ni <- bugs(data = XY.data.S2N, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.ni.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 5,
   n.iter = 10000, n.burnin = 1000, n.thin = 10,
   bugs.directory="c:\\Program Files\\WinBUGS14\\")

print(mu.0 <- Set3.sim.S2ni$mean$beta0, digits = 3)
print(tau.0 <- 1/Set3.sim.S2ni$sd$beta0^2, digits = 3)
print(mu.1 <- Set3.sim.S2ni$mean$beta1, digits = 3)
print(tau.1 <- 1/Set3.sim.S2ni$sd$beta1^2, digits = 3)
print(nu.t <- Set3.sim.S2ni$mean$tau / Set3.sim.S2ni$sd$tau^2,
  digits = 3)
print(mu.t <- Set3.sim.S2ni$mean$tau * nu.t, digits = 3)

XY.data.F3S2.ip <- list(Irrig = data.Set3S2N$Irrig, Yield = data.Set3S2N$YieldN,
   n = nrow(data.Set3S2N),
   mu.0 = mu.0, tau.0 = tau.0, mu.1 = mu.1, tau.1 = tau.1,
   mu.t = mu.t, nu.t = nu.t)

Set3.sim.3.ip <- bugs(data = XY.data.F3S2.ip, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.ip.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 5,
   n.iter = 10000, n.burnin = 1000, n.thin = 10,
   bugs.directory="c:\\Program Files\\WinBUGS14\\")
print(Set3.sim.3.ip, digits = 3)

