# This requires that the code in 8.4.3 be in memory

d.f <- data.frame(with(Set1.corrected, cbind(PatchID,
   AreaScore, AgeScore, HeightScore, obsID, HSIPred, PresAbs)))
d.f[order(d.f$obsID),]

with(Set1.corrected, plot(HtRatio, AgeRatio))
with(Set1.corrected, plot(HeightScore, AgeScore))


UA <- with(Set1.corrected, which(HSIPred == 0 & PresAbs == 0))
UP <- with(Set1.corrected, which(HSIPred == 0 & PresAbs == 1))
SA <- with(Set1.corrected, which(HSIPred == 1 & PresAbs == 0))
SP <- with(Set1.corrected, which(HSIPred == 1 & PresAbs == 1))
print(cont.table <- matrix(c(length(SP),length(SA),
   length(UP),length(UA)), nrow = 2, byrow = TRUE,
   dimnames = list(c("Suit.", "Unsuit."),c("Pres.", "Abs."))))

library(fmsb)
print(kappa.stat <- Kappa.test(cont.table))


calc.kappa <- function(){
   PA <- sample.blocks()
   UA <- sum(Set1.corrected$HSIPred == 0 & PA == 0)
   UP <- sum(Set1.corrected$HSIPred == 0 & PA == 1)
   SA <- sum(Set1.corrected$HSIPred == 1 & PA == 0)
   SP <- sum(Set1.corrected$HSIPred == 1 & PA == 1)
   n <- matrix(c(SP, SA, UP, UA), nrow = 2, byrow = TRUE)
   kappa.stat <- Kappa.test(n)$Result$estimate
}

# Permutation test without block sampling
set.seed(123)
sample.blocks <- function() sample(Set1.corrected$PresAbs)
u <- replicate(1999,calc.kappa())
print(obs.kappa <- Kappa.test(cont.table)$Result$estimate)
print(p <- sum(u >= obs.kappa - 0.001) / 2000)

obs.block1 <- Set1.corrected$PresAbs[1:2]
obs.block2 <- Set1.corrected$PresAbs[3:5]
obs.block3 <- Set1.corrected$PresAbs[6:11]
obs.block4 <- Set1.corrected$PresAbs[12:17]
obs.block5 <- Set1.corrected$PresAbs[18:20]

sample.blocks<- function(){
   s1 <- sample(obs.block1)
   s2 <- sample(obs.block2)
   s3 <- sample(obs.block3)
   s4 <- sample(obs.block4)
   s5 <- sample(obs.block5)
   c(s1,s2,s3,s4,s5)
}

set.seed(123)
u <- replicate(1999,calc.kappa())
print(p <- sum(u >= obs.kappa - 0.001) / 2000)

# From 8.4.3.r
Set1.norm1 <- with(Set1.corrected, data.frame(PresAbs = PresAbs,
     PatchArea = scale(PatchArea), PatchWidth = scale(PatchWidth),
     HtRatio = HtRatio, AgeRatio = AgeRatio, AgeScore = AgeScore,
     obsID = obsID))


Set1.logArea <- glm(PresAbs ~ PatchArea,
      data = Set1.norm1, family = binomial)
AIC(Set1.logArea)
Set1.logAge <- glm(PresAbs ~ AgeScore,
      data = Set1.norm1, family = binomial)
AIC(Set1.logAge)
Set1.logAreaAge <- glm(PresAbs ~ PatchArea + AgeScore,
      data = Set1.norm1, family = binomial)
anova(Set1.logAge,Set1.logAreaAge, test = "Chisq")
summary(Set1.logAreaAge)

par(mai = c(1,1,1,1))
with(Set1.norm1, plot(PatchArea, PresAbs, # Fig. 17.1
   pch = 1 + 15 * as.numeric(AgeScore > 0),
   xlab = "Scaled Patch Area",
   ylab = "PresAbs", cex.lab = 1.5))
legend(0.75, 0.8, c("AgeScore = 0", "AgeScore > 0"),
  pch = c(1,16))
legend(-0.5, 0.8, c("AgeScore = 0", "AgeScore = 1"),
  lty = c(2,1))
b <- as.numeric(coef(Set1.logAreaAge))
print(mod.0 <- 1 / (1 + exp(-(b[1] + b[2]*seq(-1, 2, 0.1)))))
print(mod.1 <- 1 / (1 + exp(-(b[1] + b[2]*seq(-1, 2, 0.1)
   + b[3]))))
lines(seq(-1, 2, 0.1), mod.0, lty = 2)
lines(seq(-1, 2, 0.1), mod.1, lty = 1)
