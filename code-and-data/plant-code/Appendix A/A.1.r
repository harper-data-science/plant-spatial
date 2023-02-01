set.seed(123)
# Generate the data
Y1 <- rnorm(30)
Y2 <- 2 * Y1 + 1.3 * rnorm(30)
# Center the variables
Y1 <- Y1 - mean(Y1)
Y2 <- Y2 - mean(Y2)
var(Y1)
var(Y2)
cov(Y1,Y2)
Y <- cbind(Y1,Y2)
var(Y)

# Generate Fig. A.1a
Y.df <- data.frame(Y)
Y.df[11,]
par(mai = c(1,1,1,1), pty = "s")
with(Y.df, plot(Y1, Y2, xlim = c(-4,4), ylim = c(-4,4),
   xlab = expression(italic(Y)[1]), cex.lab = 1.5,
   ylab = expression(italic(Y)[2]), # Fig. A.1a
   main = "Cartesian Coordinate System", cex.main = 2))
arrows(0, 0, 1, 0, lwd = 2, length = 0.1)
arrows(0, 0, 0, 1, lwd = 2, length = 0.1)
text(1, -0.25, expression(italic(bold(u))[1]))
text(-0.25, 1, expression(italic(bold(u))[2]))
points(Y[11,1], Y[11,2], pch = 19)
arrows(0, 0, Y[11,1], Y[11,2], lwd = 2, length = 0.1)
text(Y[11,1] + 0.3, Y[11,2], expression(italic(bold(P))[11]))
lines(c(-3,3), c(0,0))
lines(c(0,0), c(-4,4))
lines(c(Y[11,1],0), c(Y[11,2],Y[11,2]), lty = 3)
lines(c(Y[11,1],Y[11,1]), c(Y[11,2],0), lty = 3)
text(Y[11,1]+0.5, Y[11,2]/2, as.character(round(Y[11,2], 3)), font = 2)
text(Y[11,1]/2, Y[11,2]+0.2, as.character(round(Y[11,1], 3)), font = 2)

print(SY <- var(Y))
print(eigen.SY <- eigen(SY))
A <- t(eigen.SY$vectors)
print (Sc <- t(A) %*% SY %*% A)

W <- A
acos(W[1,1]) * 180 / pi
asin(W[2,1]) * 180 / pi

print(P11 <- Y[11,])
print(C <- A %*% P11)

# Create Fig. A.1b
with(Y.df, plot(Y1, Y2, xlim = c(-4,4), ylim = c(-4,4),
   xlab = expression(italic(Y)[1]), cex.lab = 1.5,
   ylab = expression(italic(Y)[2]),
   main = "Rotated Coordinate System", cex.main = 2))
with(eigen.SY,
  lines(vectors[1,1] * c(-4,4), vectors[2,1] * c(-4,4),
  xlim = c(-4,4), ylim = c(-4,4), lty = 2))
with(eigen.SY ,
  lines(vectors[1,2] * c(-2,2), vectors[2,2] * c(-2,2),
  xlim = c(-4,4), ylim = c(-4,4), lty = 2))
with(eigen.SY, arrows(0, 0, vectors[1,1], vectors[2,1], lwd = 2,
 length = 0.1))
with(eigen.SY, arrows(0, 0, vectors[1,2], vectors[2,2], lwd = 2,
  length = 0.1))
text(0.7, 0.9, expression(italic(bold(e))[1]))
text(-1.2, 0.25, expression(italic(bold(e))[2]))
lines(c(-3,3), c(0,0))
lines(c(0,0), c(-4,4))
text(0.7, 0.9, expression(italic(bold(e))[1]))
text(-1.2, 0.25, expression(italic(bold(e))[2]))
points(Y[11,1], Y[11,2], pch = 19)
arrows(0, 0, Y[11,1], Y[11,2], lwd = 2, length = 0.1)
text(Y[11,1] + 0.3, Y[11,2] - 0.1, expression(italic(bold(P))[11]))
lines(c(Y[11,1], C[1]*eigen.SY$vectors[1,1]),
   c(Y[11,2], C[1]*eigen.SY$vectors[2,1]), lty = 3)
text(Y[11,1] + 0.1, Y[11,2] + 0.3, as.character(round(C[2], 3)), font = 2)
lines(c(Y[11,1], -C[2]*eigen.SY$vectors[2,1]),
   c(Y[11,2], C[2]*eigen.SY$vectors[2,2]), lty = 3)
text(1.4, 0.9, as.character(round(C[1], 3)), font = 2)
with(eigen.SY, points(C[1]*vectors[1,1], C[1]*vectors[2,1], pch = 19))
with(eigen.SY, text(C[1]*vectors[1,1]/2,
  C[1]*vectors[2,1]+0.1, expression(italic(bold(Q))[11])))












