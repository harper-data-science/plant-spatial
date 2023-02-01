library(sf)
library(R2WinBUGS)

data.Set2S <- st_read("created\\set2sierra.shp")
data.Set2S$ElevN <-
   data.Set2S$Elevation / max(data.Set2S$Elevation)
glm.demo <- glm(QUDO ~ ElevN, data = data.Set2S, family = binomial)
coef(glm.demo)

par(mai = c(1,1,1,1))
plot(data.Set2S$ElevN, data.Set2S$QUDO,
   main = "Sierra Nevada Blue Oaks vs. Normalized Elevation",
   cex.main = 1.5, xlab = "Elevation",
   ylab = "Blue Oak Presence/Absence", cex.lab = 1.5)
elev.seq <- data.frame(ElevN = seq(0,1,0.05))
lines(seq(0,1,0.05), predict(glm.demo, elev.seq,
   type = "response"))

demo.model <- function(){
beta0 ~ dnorm(0, 0.01)
beta1 ~ dnorm(0, 0.01)
for (i in 1:n)
  {
  logit(p.QUDO[i]) <- beta0 + beta1 * ElevN[i]
  QUDO[i] ~ dbin(p.bound[i], 1)
  p.bound[i] <- max(0, min(1, p.QUDO[i]))
  }
}

XY.data <- list(QUDO = data.Set2S$QUDO,
  ElevN = data.Set2S$ElevN, n = nrow(data.Set2S))
XY.inits <- list(list(beta0 = 0, beta1 = 0))  

write.model(demo.model,
   paste(mybugsdir,"\\demoglm.bug", sep = ""))
   
demo.test <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\demoglm.bug", sep = ""),
   parameters = c("beta0", "beta1"), n.chains = 1,
   n.iter = 10, n.burnin = 1, debug = TRUE,
   bugs.directory= mybugsdir)

XY.inits <- function(){
   list(beta0 = rnorm(1), beta1 = rnorm(1))
}

demo.coda <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\demoglm.bug", sep = ""),
   parameters = c("beta0", "beta1"), n.chains = 5,
   n.iter = 5000, n.burnin = 1000, codaPkg = TRUE,
   bugs.directory = mybugsdir)
demo.mcmc <- read.bugs(demo.coda)
# Select 2 here:
codamenu()

#Second try

demo.coda <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\demoglm.bug", sep = ""),
   parameters = c("beta0", "beta1"), n.chains = 5,
   n.iter = 10000, n.burnin = 1000, n.thin = 10, codaPkg = TRUE,
   bugs.directory=mybugsdir)
demo.mcmc <- read.bugs(demo.coda)
# Select 2 here:
codamenu()

demo.sim <- bugs(data = XY.data, inits = XY.inits,
   model.file = paste(mybugsdir,"\\demoglm.bug", sep = ""),
   parameters = c("beta0", "beta1"), n.chains = 5,
   n.iter = 10000, n.burnin = 1000, n.thin = 10,
   bugs.directory = mybugsdir)
print(demo.sim, digits = 3)
t(demo.sim$mean)
coef(glm(QUDO ~ ElevN,
  data = data.Set2S, family = binomial))

