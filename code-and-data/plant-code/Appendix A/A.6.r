# Creation of Fig. A.4
x.min <- 2
x.max <- 20
x <- seq(x.min, x.max, (x.max - x.min)/100)
f <- function(x) 2 * exp(0.5 * (x - 10)) / (1 + exp(0.5 * (x - 10)))
y <- f(x)
par(mai = c(1,1,1,1))
plot(x, y, type = "l", lwd = 2, cex.lab = 2,
   xlim = c(-1, 20), ylim = c(-1,2.5),
   axes = FALSE, xlab = expression(epsilon),
   ylab = expression(italic(Y)))
lines(c(-1, 20), c(-1,-1), lwd = 2)
lines(c(-1,-1), c(-1, 2.5), lwd = 2)
text(10, -0.8, expression(Delta), cex = 2)
text(10.8, -0.83, expression(epsilon), cex = 2)
x <- 8.5
lines(c(x,x), c(-1,f(x)))
lines(c(-1,x),c(f(x),f(x)))
x <- 12
lines(c(x,x), c(-1,f(x)))
lines(c(-1,x),c(f(x),f(x)))
text(0.4, f(10), expression(Delta*Y), cex = 2)
text(11, 2, expression(italic(Y)~"="~italic(h)*"("*epsilon*")"), cex = 2)
