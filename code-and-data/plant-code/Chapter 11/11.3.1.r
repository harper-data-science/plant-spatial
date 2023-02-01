library(sf)
coast.sf <- st_read("created\\set2coast.shp")
coast.800 <- coast.sf[which(coast.sf$Precip <= 800),]

coast.800$LoMAT <- 0
coast.800$LoMAT[which(coast.800$MAT <= 15)] <- 1

print(n.mat <- with(coast.800, matrix(c(
   sum(LoMAT == 1 & QUDO == 1),
   sum(LoMAT == 1 & QUDO == 0),
   sum(LoMAT == 0 & QUDO == 1),
   sum(LoMAT == 0 & QUDO == 0)),
   nrow = 2, byrow = TRUE,
   dimnames = list(c("LoMAT", "HiMAT"), c("Pres", "Abs")))))

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

print(theta.hat <-
   (n.mat[1,1]*n.mat[2,2]) / (n.mat[1,2]*n.mat[2,1]), digits = 3)
print(sig2.hat <- sum(1/(n.mat/n.tot)), digits = 3)
print(W <- n.tot * log(theta.hat)^2 / sig2.hat, digits = 4)

# Cerioli adjustment

library(spatial)
qudo.surf <- with(coast.800, surf.ls(2, Longitude, Latitude, QUDO))
qudo.vgr <- variogram(qudo.surf,200)
par(mai = c(1,1,1,1))
plot(qudo.vgr$x, qudo.vgr$y, cex.lab = 1.5, 
   main = "Raw QUDO Variogram, All Lags", cex.main = 2,
   xlab = "Relative Lag Distance",
   ylab = "Experimental Variogram")  
lomat.surf <- with(coast.800, surf.ls(2, Longitude, Latitude, LoMAT))
lomat.vgr <- variogram(lomat.surf,200)
plot(lomat.vgr$x, lomat.vgr$y, cex.lab = 1.5, 
   main = "CWHS Variogram, All Lag Groups", cex.main = 2,
   xlab = "Relative Lag Distance",
   ylab = "Experimental Variogram", ylim = c(0, 0.3))
   
plot(qudo.vgr$x[1:20], qudo.vgr$y[1:20], cex.lab = 1.5,
   main = "QUDO Variogram, First 20 Lag Groups", cex.main = 2,
   xlab = "Relative Lag Distance", xlim = c(0, 0.5), # Fig. 11.3a
   ylab = "Experimental Variogram", ylim = c(0, 0.3))

plot(lomat.vgr$x[1:20], lomat.vgr$y[1:20], cex.lab = 1.5,
   main = "LoMAT Variogram, First 20 Lag Groups", cex.main = 2,
   xlab = "Relative Lag Distance", xlim = c(0, 0.5), # Fig. 11.3b
   ylab = "Experimental Variogram", ylim = c(0, 0.3))
   
CX.hat <- max(qudo.vgr$y[1:20]) - qudo.vgr$y[1:20]
nkX <- qudo.vgr$cnt[1:20]
CY.hat <- max(lomat.vgr$y[1:20]) - lomat.vgr$y[1:20]
nkY <- lomat.vgr$cnt[1:20]
all.equal(nkX,nkY)
lambda.sum <- sum(CX.hat * CY.hat * nkX)
print(lambda <-
   (2 * n.tot* lambda.sum)/(n.mat[1,1]*n.mat[2,2]), digits = 3)
print(W.adj <- W / (1 + lambda), digits = 3)
print(p <- 1 - pchisq(W.adj, df = 1), digits = 3)



