# 11.1
# This assumes 11.2.4.r is in memory
# Bootstrap estimate of cor(SPAD,GrainProt)
log.GrainProt <- log(data.Set4.1$GrainProt)
#coordinates(data.Set4.1) <- c("Easting", "Northing")
nlist.w <- dnearneigh(data.Set4.1, d1 = 0, d2 = 61)
W <- nb2listw(nlist.w)
Y1.mod <- lagsarlm(log.LeafN ~ 1, data = data.Set4.1, W)
IrWinv.1 <- invIrM(nlist.w, max(Y2.mod$rho, 0), style = "W")
Y2.mod <- lagsarlm(GrainProt ~ 1, data = data.Set4.1, W)
IrWinv.2 <- invIrM(nlist.w, max(Y1.mod$rho, 0), style = "W")
boot.samp <- function(Y1.mod,Y2.mod,IrWinv.1,IrWinv.2){
  e <- sample(Y1.mod$residuals, replace = TRUE)
  Y1 <- IrWinv.1 %*% e
  e <- sample(Y2.mod$residuals, replace = TRUE)
  Y2 <- IrWinv.2 %*% e
  r.boot <- cor(Y1,Y2)
}

set.seed(123)
u <- replicate(200, boot.samp(Y1.mod,Y2.mod,IrWinv.1,IrWinv.2))
print(sigmar.boot <- var(u))
print(r <- cor(log.SPAD, log.GrainProt))
print(ne.hat <- 1 + 1 / sigmar.boot)
print(t.corr <- sqrt(ne.hat - 2) * r / sqrt(1 - r^2))
print(p.corr <- 2 * (1 - pt(q = abs(t.corr), df = ne.hat - 2)))

# 11.2
library(sf)
coast.df <- st_read("created\\set2coast.shp")
coast.800 <- coast.df[which(coast.df$Precip <= 800),]

coast.800$LoJuMax <- 0
coast.800$LoJuMax[which(coast.800$JuMax <= 30)] <- 1

print(n.mat <- with(coast.800, matrix(c(
   sum(LoJuMax == 1 & QUDO == 1),
   sum(LoJuMax == 1 & QUDO == 0),
   sum(LoJuMax == 0 & QUDO == 1),
   sum(LoJuMax == 0 & QUDO == 0)),
   nrow = 2, byrow = TRUE,
   dimnames = list(c("LoJuMax", "HiJuMax"), c("Pres", "Abs")))))

print(n.tot <- sum(n.mat))
print(p1 <- (n.mat[1,1]+n.mat[1,2])/n.tot, digits = 2)
print(p2 <- (n.mat[2,1]+n.mat[2,2])/n.tot, digits = 2)
print(q1 <- (n.mat[1,1]+n.mat[2,1])/n.tot, digits = 2)
print(q2 <- (n.mat[1,2]+n.mat[2,2])/n.tot, digits = 2)
print(C2.hat <- (
   (n.mat[1,1]-n.tot*p1*q1)^2/(n.tot*p1*q1) +
   (n.mat[1,2]-n.tot*p1*q2)^2/(n.tot*p1*q2) +
   (n.mat[2,1]-n.tot*p2*q1)^2/(n.tot*p2*q1) +
   (n.mat[2,2]-n.tot*p2*q2)^2/(n.tot*p2*q2)))

chisq.test(n.mat, simulate.p.value = FALSE)
chisq.test(n.mat, simulate.p.value = TRUE)

print(theta.hat <- (n.mat[1,1]*n.mat[2,2]) / (n.mat[1,2]*n.mat[2,1]))
print(sig2.hat <- sum(1/(n.mat/n.tot)))
print(W <- n.tot * log(theta.hat)^2 / sig2.hat)

# Cerioli adjustment
library(spatial)
qudo.surf <- with(coast.800, surf.ls(2, Longitude, Latitude, QUDO))
qudo.vgr <- variogram(qudo.surf,200)
par(mai = c(1,1,1,1))
plot(qudo.vgr$x, qudo.vgr$y, cex.lab = 1.5,
   main = "Raw QUDO Variogram, All Lags", cex.main = 2,
   xlab = "Relative Lag Distance",
   ylab = "Experimental Variogram")
LoJuMax.surf <- with(coast.800, surf.ls(2, Longitude, Latitude, LoJuMax))
LoJuMax.vgr <- variogram(LoJuMax.surf,200)
plot(LoJuMax.vgr$x, LoJuMax.vgr$y, cex.lab = 1.5,
   main = "CWHS Variogram, All Lag Groups", cex.main = 2,
   xlab = "Relative Lag Distance",
   ylab = "Experimental Variogram", ylim = c(0, 0.3))

plot(qudo.vgr$x[1:20], qudo.vgr$y[1:20], cex.lab = 1.5,
   main = "QUDO Variogram, First 20 Lag Groups", cex.main = 2,
   xlab = "Relative Lag Distance", xlim = c(0, 1),
   ylab = "Experimental Variogram", ylim = c(0, 0.3))

plot(LoJuMax.vgr$x[1:20], LoJuMax.vgr$y[1:20], cex.lab = 1.5,
   main = "LoJuMax Variogram, First 20 Lag Groups", cex.main = 2,
   xlab = "Relative Lag Distance", xlim = c(0, 1),
   ylab = "Experimental Variogram", ylim = c(0, 0.3))

CX.hat <- max(qudo.vgr$y[1:20]) - qudo.vgr$y[1:20]
nkX <- qudo.vgr$cnt[1:20]
CY.hat <- max(LoJuMax.vgr$y[1:20]) - LoJuMax.vgr$y[1:20]
nkY <- LoJuMax.vgr$cnt[1:20]
all.equal(nkX,nkY)
lambda.sum <- sum(CX.hat * CY.hat * nkX)
print(lambda <- (2 * n.tot* lambda.sum)/(n.mat[1,1]*n.mat[2,2]))
print(W.adj <- W / (1 + lambda))
print(p <- 1 - pchisq(W.adj, df = 1))



# 11.3
data.Set2 <- read.csv("set2\\set2data.csv", header = TRUE)

JaMed <- median(data.Set2$JaMean)
JuMed <- median(data.Set2$JuMean)

data.Set2sub <- data.Set2[which(with(data.Set2,
    (JaMean <= JaMed & JuMean > JuMed) |
    (JaMean > JaMed & JuMean <= JuMed))),]
data.Set2sub$CWHS <- with(data.Set2sub,
   as.numeric(JaMean <= JaMed & JuMean > JuMed))

print(n.mat <- with(data.Set2sub, matrix(c(
   sum(CWHS == 1 & QUDO == 1),
   sum(CWHS == 1 & QUDO == 0),
   sum(CWHS == 0 & QUDO == 1),
   sum(CWHS == 0 & QUDO == 0)),
   nrow = 2, byrow = TRUE,
   dimnames = list(c("CW-HS", "WW-WS"), c("Pres", "Abs")))))

n.tot <- sum(n.mat)
print(p1 <- (n.mat[1,1]+n.mat[1,2])/n.tot)
print(p2 <- (n.mat[2,1]+n.mat[2,2])/n.tot)
print(q1 <- (n.mat[1,1]+n.mat[2,1])/n.tot)
print(q2 <- (n.mat[1,2]+n.mat[2,2])/n.tot)
print(C2.hat <- (
   (n.mat[1,1]-n.tot*p1*q1)^2/(n.tot*p1*q1) +
   (n.mat[1,2]-n.tot*p1*q2)^2/(n.tot*p1*q2) +
   (n.mat[2,1]-n.tot*p2*q1)^2/(n.tot*p2*q1) +
   (n.mat[2,2]-n.tot*p2*q2)^2/(n.tot*p2*q2)))

chisq.test(n.mat, simulate.p.value = FALSE)
chisq.test(n.mat, simulate.p.value = TRUE)

print(theta.hat <- (n.mat[1,1]*n.mat[2,2]) / (n.mat[1,2]*n.mat[2,1]))
print(sig2.hat <- sum(1/(n.mat/n.tot)))
print(W <- n.tot * log(theta.hat)^2 / sig2.hat)

# Cerioli adjustment

library(spatial)
qudo.surf <- with(data.Set2sub, surf.ls(2, Longitude, Latitude, QUDO))
qudo.vgr <- variogram(qudo.surf,200)
par(mai = c(1,1,1,1))
plot(qudo.vgr$x, qudo.vgr$y, cex.lab = 1.5,
   main = "Raw QUDO Variogram, All Lags", cex.main = 2,
   xlab = "Relative Lag Distance",
   ylab = "Experimental Variogram")
cwhs.surf <- with(data.Set2sub, surf.ls(2, Longitude, Latitude, CWHS))
cwhs.vgr <- variogram(cwhs.surf,200)
plot(cwhs.vgr$x, cwhs.vgr$y, cex.lab = 1.5,
   main = "CWHS Variogram, All Lag Groups", cex.main = 2,
   xlab = "Relative Lag Distance",
   ylab = "Experimental Variogram", ylim = c(0, 0.3))

plot(qudo.vgr$x[1:20], qudo.vgr$y[1:20], cex.lab = 1.5,
   main = "QUDO Variogram, First 20 Lag Groups", cex.main = 2,
   xlab = "Relative Lag Distance", xlim = c(0, 1),
   ylab = "Experimental Variogram", ylim = c(0, 0.3))

plot(cwhs.vgr$x[1:20], cwhs.vgr$y[1:20], cex.lab = 1.5,
   main = "CWHS Variogram, First 20 Lag Groups", cex.main = 2,
   xlab = "Relative Lag Distance", xlim = c(0, 1),
   ylab = "Experimental Variogram", ylim = c(0, 0.03))

CX.hat <- max(qudo.vgr$y[1:20]) - qudo.vgr$y[1:20]
nkX <- qudo.vgr$cnt[1:20]
CY.hat <- max(cwhs.vgr$y[1:20]) - cwhs.vgr$y[1:20]
nkY <- cwhs.vgr$cnt[1:20]
all.equal(nkX,nkY)
lambda.sum <- sum(CX.hat * CY.hat * nkX)
print(lambda <- (2 * n.tot* lambda.sum)/(n.mat[1,1]*n.mat[2,2]))
print(W.adj <- W / (1 + lambda))
print(p <- 1 - pchisq(W.adj, df = 1))


# 11.4
data.Set2 <- read.csv("set2\\set2data.csv", header = TRUE)
JaMed <- median(data.Set2$JaMean)
JuMed <- median(data.Set2$JuMean)

data.Set2$CW <- with(data.Set2,
   as.numeric(JaMean <= JaMed))

print(n.cw <- with(data.Set2, matrix(c(
   sum(CW == 1 & QUDO == 1),
   sum(CW == 1 & QUDO == 0),
   sum(CW == 0 & QUDO == 1),
   sum(CW == 0 & QUDO == 0)),
   nrow = 2, byrow = TRUE,
   dimnames = list(c("CW=1", "CW=0"), c("Pres", "Abs")))))



data.Set2$HS <- with(data.Set2,
   as.numeric(JuMean > JuMed))

print(n.hs <- with(data.Set2, matrix(c(
   sum(HS == 1 & QUDO == 1),
   sum(HS == 1 & QUDO == 0),
   sum(HS == 0 & QUDO == 1),
   sum(HS == 0 & QUDO == 0)),
   nrow = 2, byrow = TRUE,
   dimnames = list(c("HS=1", "HS=0"), c("Pres", "Abs")))))
library(sf)
sierra.df <- st_read("created\\set2sierra.shp")
sierra.df$CW <- with(sierra.df,
   as.numeric(JaMean <= JaMed))

print(n.cws <- with(sierra.df, matrix(c(
   sum(CW == 1 & QUDO == 1),
   sum(CW == 1 & QUDO == 0),
   sum(CW == 0 & QUDO == 1),
   sum(CW == 0 & QUDO == 0)),
   nrow = 2, byrow = TRUE,
   dimnames = list(c("CW=1", "CW=0"), c("Pres", "Abs")))))

sierra.df$HS <- with(sierra.df,
   as.numeric(JuMean > JuMed))

print(n.hss <- with(sierra.df, matrix(c(
   sum(HS == 1 & QUDO == 1),
   sum(HS == 1 & QUDO == 0),
   sum(HS == 0 & QUDO == 1),
   sum(HS == 0 & QUDO == 0)),
   nrow = 2, byrow = TRUE,
   dimnames = list(c("HS=1", "HS=0"), c("Pres", "Abs")))))
coast.df <- st_read("created\\set2coast.shp")
coast.df$CW <- with(coast.df,
   as.numeric(JaMean <= JaMed))

print(n.cwc <- with(coast.df, matrix(c(
   sum(CW == 1 & QUDO == 1),
   sum(CW == 1 & QUDO == 0),
   sum(CW == 0 & QUDO == 1),
   sum(CW == 0 & QUDO == 0)),
   nrow = 2, byrow = TRUE,
   dimnames = list(c("CW=1", "CW=0"), c("Pres", "Abs")))))

coast.df$HS <- with(coast.df,
   as.numeric(JuMean > JuMed))

print(n.hsc <- with(coast.df, matrix(c(
   sum(HS == 1 & QUDO == 1),
   sum(HS == 1 & QUDO == 0),
   sum(HS == 0 & QUDO == 1),
   sum(HS == 0 & QUDO == 0)),
   nrow = 2, byrow = TRUE,
   dimnames = list(c("HS=1", "HS=0"), c("Pres", "Abs")))))

print(n.cwsc <- n.cws + n.cwc)
print(n.hssc <- n.hss + n.hsc)


# 11.5
library(sf)
sierra.df <- st_read("created\\set2sierra.shp")
coast.df <- st_read("created\\set2coast.shp")
sierra.var4 <- with(sierra.df, data.frame(Precip, JuMean, JaMean, Elevation))
library(lattice)
trellis.par.get()
trellis.par.set(par.main.text = list(cex = 2))
splom(sierra.var4, par.settings = list(fontsize=list(text=11),
  plot.symbol = list(col = "black")),
  main = "Sierra Nevada")
  
coast.var4 <- with(coast.df, data.frame(Precip, JuMean, JaMean, Elevation))
splom(coast.var4, par.settings = list(fontsize=list(text=11),
  plot.symbol = list(col = "black")),
  main = "Coast Ranges")

# 11.6
data.Set2 <- read.csv("set2\\set2data.csv", header = TRUE)
data.Set2$CWHS <- with(data.Set2,
   as.numeric(JaMean <= JaMed & JuMean > JuMed))

data.Set2$HiPr <- as.numeric(data.Set2$Precip >
    median(data.Set2$Precip))
print(n.mat <- with(data.Set2, matrix(c(
   sum(CWHS == 1 & HiPr == 1),
   sum(CWHS == 1 & HiPr == 0),
   sum(CWHS == 0 & HiPr == 1),
   sum(CWHS == 0 & HiPr == 0)),
   nrow = 2, byrow = TRUE,
   dimnames = list(c("CW-HS", "WW-WS"), c("Hi Pr", "Lo Pr")))))

# 11.7
library(sf)
sierra.df <- st_read("created\\set2sierra.shp")
coast.df <- st_read("created\\set2coast.shp")

sierra.a1100 <- sierra.df[which(sierra.df$Elevation <= 1100),]
sierra.a1100$ID <- 1:nrow(sierra.a1100)
sierra.disp <- sierra.a1100[which(sierra.a1100$ID %% 4 == 0),]
with(sierra.disp, plot(Elevation, Precip, pch = 16 * QUDO))
with(sierra.disp, plot(Elevation, MAT, pch = 16 * QUDO))
with(sierra.disp, plot(Precip, MAT, pch = 16 * QUDO))
with(sierra.disp, plot(JaMean, JuMean, pch = 16 * QUDO))

coast.df$ID <- 1:nrow(coast.df)
coast.disp <- coast.df[which(coast.df$ID %% 4 == 0),]
with(coast.disp, plot(Elevation, Precip, pch = 16 * QUDO))
with(coast.disp, plot(Elevation, MAT, pch = 16 * QUDO))
with(coast.disp, plot(Precip, MAT, pch = 16 * QUDO))


# 11.8
# Assuming all of the code in Sectoin 11.3.2 has been run
set.seed(123)
u <- replicate(1999,calc.fisher())
unique(u)
obs.stat
# The inequality may not be satisfied due to roundoff error

# 11.9
# This assumes that the code from Section 7.2 has been run

scores.corr <- with(Set1.corrected, cbind(AreaScore, WidthScore, AgeScore, HeightScore))
print(HSItest <- as.numeric(apply(scores.corr[,1:4], 1, prod) > 0))
PAtest <- Set1.corrected$PresAbs

UA1 <- which(HSItest == 0 & PAtest == 0)
UP1 <- which(HSItest == 0 & PAtest == 1)
SA1 <- which(HSItest > 0 & PAtest == 0)
SP1 <- which(HSItest > 0 & PAtest == 1)
print(cont.table <- matrix(c(length(SP1),length(SA1),
   length(UP1),length(UA1)), nrow = 2, byrow = TRUE,
   dimnames = list(c("Suit.", "Unsuit."),c("Pres.", "Abs."))))

calc.fisher1 <- function(){
   PA <- sample.blocks()
   UA <- sum(HSItest == 0 & PA == 0)
   UP <- sum(HSItest == 0 & PA == 1)
   SA <- sum(HSItest == 1 & PA == 0)
   SP <- sum(HSItest == 1 & PA == 1)
   n <- matrix(c(SP, SA, UP, UA), nrow = 2, byrow = TRUE)
   p.value <- fisher.test(n)$estimate
}

obs.test <- fisher.test(cont.table)
print(obs.stat <- obs.test$estimate)

set.seed(123)
u <- replicate(1999,calc.fisher1())
print(p <- sum(u >= obs.stat - 0.001) / 2000)


print(HSItest <- as.numeric(apply(scores.corr[,2:3], 1, prod) > 0))
PAtest <- Set1.corrected$PresAbs

UA1 <- which(HSItest == 0 & PAtest == 0)
UP1 <- which(HSItest == 0 & PAtest == 1)
SA1 <- which(HSItest > 0 & PAtest == 0)
SP1 <- which(HSItest > 0 & PAtest == 1)
print(cont.table <- matrix(c(length(SP1),length(SA1),
   length(UP1),length(UA1)), nrow = 2, byrow = TRUE,
   dimnames = list(c("Suit.", "Unsuit."),c("Pres.", "Abs."))))

obs.test <- fisher.test(cont.table)
print(obs.stat <- obs.test$estimate)
obs.test$p.value

set.seed(123)
u <- replicate(1999,calc.fisher1())
print(p <- sum(u >= obs.stat - 0.001) / 2000)


print(HSItest <- as.numeric(scores.corr[,3] > 0))
PAtest <- Set1.corrected$PresAbs

UA1 <- which(HSItest == 0 & PAtest == 0)
UP1 <- which(HSItest == 0 & PAtest == 1)
SA1 <- which(HSItest > 0 & PAtest == 0)
SP1 <- which(HSItest > 0 & PAtest == 1)
print(cont.table <- matrix(c(length(SP1),length(SA1),
   length(UP1),length(UA1)), nrow = 2, byrow = TRUE,
   dimnames = list(c("Suit.", "Unsuit."),c("Pres.", "Abs."))))

obs.test <- fisher.test(cont.table)
print(obs.stat <- obs.test$estimate)
obs.test$p.value

set.seed(123)
u <- replicate(1999,calc.fisher1())
print(p <- sum(u >= obs.stat - 0.001) / 2000)

# 11.10
# This assumes 11.4.2 is in memory
LeafN.dist <- vegdist(log(data.Set4.1$LeafN), method = "euclidean")
GrainProt.dist <- vegdist(log(data.Set4.1$GrainProt), method = "euclidean")
mantel.partial(SPAD.dist, GrainProt.dist, dist.mat)
mantel.partial(SPAD.dist, GrainProt.dist, invdist.mat)

mantel.partial(SPAD.dist, GrainProt.dist, dist.mat)
mantel.partial(SPAD.dist, GrainProt.dist, invdist.mat)






