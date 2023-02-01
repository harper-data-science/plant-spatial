# Fig. 14.1
priorA <- dnorm(seq(500, 1000), 900, 20)
priorB <- dnorm(seq(500, 1000), 800, 80)
par(mai = c(1,1,1,1))
plot(seq(500, 1000), priorA, type = "l",
   ylim = c(0, 0.025), xlab = expression(theta),
   ylab = "", cex.lab = 1.5) # Fig. 14.1
lines(seq(500, 1000), priorB)
likeY <- dnorm(seq(500, 1000), 850, 40)
lines(seq(500, 1000), likeY, lty = 2)
postA <- dnorm(seq(500, 1000), 890, 17.9)
postB <- dnorm(seq(500, 1000), 840, 35.7)
lines(seq(500, 1000), postA, lwd = 2)
lines(seq(500, 1000), postB, lwd = 2)
text(960, 0.015, expression(p[A](theta)), cex = 1.5)
text(820, 0.02, expression(p[A]*"("*theta*"|Y)"), cex = 1.5)
text(775, 0.012, expression(italic(l)[A]*"("*Y*"|"*theta*")"),
  cex = 1.5)
arrows(770, 0.0112, 830, 0.009, length = 0.07)
text(675, 0.003, expression(p[B](theta)), cex = 1.5)
text(760, 0.008, expression(p[B]*"("*theta*"|"*Y*")"), cex = 1.5)
   
# Color version
plot(seq(500, 1000), priorA, type = "l", lwd = 2, col = "light blue",
   ylim = c(0, 0.025), xlab = expression(theta),
   ylab = "", cex.lab = 1.5) 
lines(seq(500, 1000), priorB, lwd = 2, col = "pink")
lines(seq(500, 1000), likeY, lwd = 2, col = "green")
lines(seq(500, 1000), postA, lwd = 2, col = "dark blue")
lines(seq(500, 1000), postB, lwd = 2, col = "red")
text(960, 0.015, expression(p[A](theta)), cex = 1.5, col = "light blue")
text(820, 0.02, expression(p[A]*"("*theta*"|Y)"), cex = 1.5, col = "blue")
text(775, 0.012, expression(italic(l)[A]*"("*Y*"|"*theta*")"),
  cex = 1.5, col = "green")
arrows(770, 0.0112, 830, 0.009, length = 0.07, col = "green")
text(675, 0.003, expression(p[B](theta)), cex = 1.5, col = "pink")
text(760, 0.008, expression(p[B]*"("*theta*"|"*Y*")"), cex = 1.5, col = "red")


