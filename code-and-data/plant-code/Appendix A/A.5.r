# Creation of Fig. A.3
d <- 200
xlo <- -5
xhi <- 5
x <- seq(5+xlo, 5+xhi, (xhi-xlo)/d)
ylo <- -5
yhi <- 10
y <- seq(4+ylo, 4+yhi, (yhi-ylo)/d)
z <- matrix(nrow = length(y), ncol = length(x))
fxy = function(x,y) (x-5)^2 + (y-4)^2 + (x-5)^4 + (y-4)^4
for (i in 1:length(x)){
   for (j in 1: length(y)) z[i,j] <- fxy(x[i], y[j])}
par(mai = c(1,1,1,1))
contour(x, y, z, levels = c(0.1, 2.1, 11, 50, 150, 300), drawlabels = FALSE,
   ylim = c(-2,9), xlab = expression(italic(Y)[1]), ylab = expression(italic(Y)[2]),
   cex.lab = 1.5)
points(5, 4, pch = 16)
arrows(2.1,0.2, 4.2,4.8, lwd = 2, length = 0.1)
arrows(4.2,4.8, 5.1,4.3, lwd = 2, length = 0.1)
arrows(5.1, 4.3, 5,4, lwd = 2, length = 0.1)
text(2, 0.8, "1", cex = 1.5)
text(4.2, 5.2, "2", cex = 1.5)
text(5.3, 4.5, "3", cex = 1.5) 
