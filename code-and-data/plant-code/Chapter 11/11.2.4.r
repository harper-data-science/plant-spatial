library(spdep)
library(car)

data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
# Leaf N vs. SPAD
# Check to see whether a log log transformation will stabilize the variance
spadn.lin <- lm(LeafN ~ SPAD, data = data.Set4.1)
fits.med <- median(fitted(spadn.lin))
fits.groups <- fitted(spadn.lin) <= fits.med
leveneTest(residuals(spadn.lin),factor(fits.groups))
library(lmtest)
bptest(spadn.lin)
par(mai = c(1,1,1,1))
qqnorm(residuals(spadn.lin), # Not in text
  main = "Normal Q-Q Plot, Untransformed Data",
  cex.main = 2, cex.lab = 1.5)
qqline(residuals(spadn.lin))
spadn.log <- lm(log(LeafN) ~ log(SPAD), data = data.Set4.1)
bptest(spadn.log)
fits.med <- median(fitted(spadn.log))
fits.groups <- fitted(spadn.log) <= fits.med
leveneTest(residuals(spadn.log),factor(fits.groups))
qqnorm(residuals(spadn.log),  # Not in text
  main = "Normal Q-Q Plot, Log Transformed Data",
  cex.main = 2, cex.lab = 1.5)
qqline(residuals(spadn.log))
shapiro.test(residuals(spadn.lin))
shapiro.test(residuals(spadn.log))

# Compute correlation coefficient
log.SPAD <- log(data.Set4.1$SPAD)
log.LeafN <- log(data.Set4.1$LeafN)
cor.test(log.SPAD, log.LeafN, alternative = "two.sided",
   method = "pearson")
library(spatial)
spad.surf <- surf.ls(0, data.Set4.1$Easting,
   data.Set4.1$Northing,
   log.SPAD)
par(mai = c(1,1,1,1))
spad.cgr <- correlogram(spad.surf,10, cex.main = 2, # Fig. 11.2a
   main = "Log SPAD Correlogram", cex.lab = 1.5,
   xlab = "Lag Group", ylab = "Experimental Correlogram")
   
LeafN.surf <- surf.ls(0,data.Set4.1$Easting,
  data.Set4.1$Northing,log.LeafN)
LeafN.cgr <- correlogram(LeafN.surf,10, cex.main = 2, # Fig. 11.2b
   main = "Log Leaf N Correlogram", cex.lab = 1.5,
   xlab = "Lag Group", ylab = "Experimental Correlogram")
spad.var <- var(log.SPAD)
LeafN.var <- var(log.LeafN)
# Estimate variance
n <- nrow(data.Set4.1)
cgr.prod <- spad.cgr$y * LeafN.cgr$y * LeafN.cgr$cnt
sigr.hat <- sum(cgr.prod[1:3]) / n^2
# Estimate effective sample size
print(ne.hat <- 1 + 1 / sigr.hat, digits = 3)
print(r <- cor(log.SPAD,log.LeafN), digits = 3)
print(t.uncorr <- sqrt(n - 2) * r / sqrt(1 - r^2), digits = 3)
print(t.corr <- sqrt(ne.hat - 2) * r / sqrt(1 - r^2), digits = 3)
print(p.uncorr <- 2 * (1 - pt(q = t.uncorr, df = n - 2)), digits = 3)
print(p.corr <- 2 * (1 - pt(q = t.corr, df = ne.hat - 2)), digits = 3)

# Bootstrap estimate of cor(log.SPAD,log.LeafN)
coordinates(data.Set4.1) <- c("Easting", "Northing")
nlist.w <- dnearneigh(data.Set4.1, d1 = 0, d2 = 61)
W <- nb2listw(nlist.w)
Y1.mod <- lagsarlm(log.SPAD ~ 1, data = data.Set4.1, W)
Y1.mod$rho
IrWinv.1 <- invIrM(nlist.w, max(Y1.mod$rho, 0), style = "W")
Y2.mod <- lagsarlm(log.LeafN ~ 1, data = data.Set4.1, W)
Y2.mod$rho
IrWinv.2 <- invIrM(nlist.w, max(Y2.mod$rho, 0), style = "W")
boot.samp <- function(Y1.mod,Y2.mod,IrWinv.1,IrWinv.2){
  e <- sample(Y1.mod$residuals, replace = TRUE)
  Y1 <- IrWinv.1 %*% e
  e <- sample(Y2.mod$residuals, replace = TRUE)
  Y2 <- IrWinv.2 %*% e
  r.boot <- cor(Y1,Y2)
}

set.seed(123)
U <- replicate(200, boot.samp(Y1.mod,Y2.mod,IrWinv.1,IrWinv.2))
print(sigmar.boot <- var(U), digits = 3)
print(r <- cor(log.SPAD, log.LeafN), digits = 3)
print(ne.hat <- 1 + 1 / sigmar.boot, digits = 3)
print(t.corr <- sqrt(ne.hat - 2) * r / sqrt(1 - r^2), digits = 3)
print(p.corr <- 2 * (1 - pt(q = abs(t.corr), df = ne.hat - 2)),digits = 3)

