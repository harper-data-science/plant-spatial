data.Set3 <- read.csv("Set3\\Set3data.csv", header = TRUE)
data.Set3a <- data.Set3[-which(data.Set3$Field == 2),]

library(nlme)
Yld.lmeR1 <- lme(Yield ~ 1, data = data.Set3a,
   random = ~ 1 | Field, method = "ML")
Yld.lmeF1 <- lme(Yield ~ Irrig, data = data.Set3a,
   random = ~ 1 | Field, method = "ML")
anova(Yld.lmeR1, Yld.lmeF1)

Yld.lmeR2 <- lme(Yield ~ Irrig, data = data.Set3a,
   random = ~ 1 | Field)
Yld.lmeF2 <- update(Yld.lmeR2, random = ~ Irrig | Field)
anova(Yld.lmeR2, Yld.lmeF2)

logLik(Yld.lmeR1)
logLik(Yld.lmeF1)
sim.lme <- simulate.lme(Yld.lmeR1,Yld.lmeF1,
   nsim = 1000, seed = 123)
library(lattice)
trellis.device(color = FALSE)
plot(sim.lme, df = 1) # Fig. 12.2

logLik(Yld.lmeR2)
logLik(Yld.lmeF2)
sim.lme <- simulate.lme(Yld.lmeR2,Yld.lmeF2, nsim = 1000, seed = 123)
plot(sim.lme, df = c(1,2)) # Fig 12.3

