library(R2WinBUGS)

data.Set3 <- read.csv("Set3\\Set3data.csv", header = TRUE)
data.Set3$YieldN <- data.Set3$Yield / max(data.Set3$Yield)
unique(data.Set3$Season)

data.Set3S12 <- data.Set3[-which(data.Set3$Season == 3),]
data.Set3S1 <- data.Set3[which(data.Set3$Season == 1),]
data.Set3S2 <- data.Set3[which(data.Set3$Season == 2),]
data.Set3S3 <- data.Set3[which(data.Set3$Season == 3),]
sort(unique(data.Set3S1$Field))
sort(unique(data.Set3S2$Field))
sort(unique(data.Set3S3$Field))
with(data.Set3, unique(Farmer[which(Field == 3)]))

data.Set3F3S2 <- data.Set3S2[which(data.Set3S2$Field == 3),]
data.Set3F3S3 <- data.Set3S3[which(data.Set3S3$Field == 3),]

with(data.Set3F3S2, tapply(Yield, Irrig, mean))


model.lm <- lm(YieldN ~ Irrig, data = data.Set3F3S2)
summary(model.lm)
par(mai = c(1,1,1,1))
with(data.Set3F3S2, plot(Irrig, YieldN,  # Fig. 15.15
   xlab = "Irrigation Effectiveness", cex.lab = 1.5,
   ylab = "Normalized Yield", cex.main = 2,
   main = "Field 3, Season 2"))
abline(model.lm)

# Noninformative prior model
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
# Data from Field 3 season 2
XY.data.F3S2 <- list(Irrig = data.Set3F3S2$Irrig,
   Yield = data.Set3F3S2$YieldN, n = nrow(data.Set3F3S2))
XY.inits <- function(){
   list(beta0 = rnorm(1), beta1 = rnorm(1), tau = exp(rnorm(1)))}

Set3.test <- bugs(data = XY.data.F3S2, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.ni.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 1,
   n.iter = 10, debug = TRUE,
   bugs.directory=mybugsdir)


Set3.coda <- bugs(data = XY.data.F3S2, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.ni.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 5,
   n.iter = 5000, n.burnin = 500, n.thin = 5, codaPkg  = TRUE,
   bugs.directory=mybugsdir)
Set3.mcmc <- read.bugs(Set3.coda)
# Select 2 here:
codamenu()

Set3.sim.F3S2ni <- bugs(data = XY.data.F3S2, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.ni.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 5,
   n.iter = 10000, n.burnin = 1000, n.thin = 10,
   bugs.directory=mybugsdir)
print(Set3.sim.F3S2ni, digits = 2)

# Data from all fields in season 1

XY.data.S1 <- list(Irrig = data.Set3S1$Irrig,
   Yield = data.Set3S1$YieldN, n = nrow(data.Set3S1))

Set3.test <- bugs(data = XY.data.S1, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.ni.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 1,
   n.iter = 10, n.burnin = 2, debug = TRUE,
   bugs.directory=mybugsdir)


Set3.sim.S1ni <- bugs(data = XY.data.S1, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.ni.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 5,
   n.iter = 10000, n.burnin = 1000, n.thin = 10,
   bugs.directory=mybugsdir)
print(Set3.sim.S1ni, digits = 2)

# Season 2, informative prior
#data from all fields in season 1

print(mu.0 <- Set3.sim.S1ni$mean$beta0, digits = 3)
print(tau.0 <- 1/Set3.sim.S1ni$sd$beta0^2, digits = 3)
print(mu.1 <- Set3.sim.S1ni$mean$beta1, digits = 3)
print(tau.1 <- 1/Set3.sim.S1ni$sd$beta1^2, digits = 3)
print(nu.t <- Set3.sim.S1ni$mean$tau / Set3.sim.S1ni$sd$tau^2,
  digits = 3)
print(mu.t <- Set3.sim.S1ni$mean$tau * nu.t, digits = 3)

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

XY.data.F3S2ip <- list(Irrig = data.Set3F3S2$Irrig,
   Yield = data.Set3F3S2$YieldN, n = nrow(data.Set3F3S2),
   mu.0 = mu.0, tau.0 = tau.0, mu.1 = mu.1, tau.1 = tau.1,
   mu.t = mu.t, nu.t = nu.t)
   
Set3.test <- bugs(data = XY.data.F3S2ip, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.ip.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 1,
   n.iter = 10, n.burnin = 2, debug = TRUE,
   bugs.directory=mybugsdir)

Set3.sim.F3S2ip <- bugs(data = XY.data.F3S2ip, inits = XY.inits,
   model.file = paste(mybugsdir,"\\Set3model.ip.bug", sep = ""),
   parameters = c("beta0", "beta1", "tau"), n.chains = 5,
   n.iter = 10000, n.burnin = 1000, n.thin = 10,
   bugs.directory=mybugsdir)
print(Set3.sim.F3S2ip, digits = 2)






