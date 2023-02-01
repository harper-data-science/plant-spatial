# Fig. 5.3
x.pt <- 0.3
y.pt <- 0.6

plot( c(x.pt - 0.25, x.pt + 0.25), c(y.pt, y.pt), xlim = c(0,1.5), ylim = c(0,1), type = "l",
   axes = FALSE, lwd = 2, xlab = "", ylab = "") 
set.seed(123)
x <- x.pt + 0.05 * rnorm(5)
p <- cbind(x,rep(y.pt,5))
text(x.pt, y.pt, "I", font = 2, cex = 2)
text(p, "x", font = 2, cex = 1.5)
text(x.pt, y.pt - 0.1, "(a)", font = 2, cex = 1.5)
text(x.pt, y.pt + 0.05, expression(mu), font = 2, cex = 1.5)

x.pt <- 1.0
lines(c(x.pt-0.25, x.pt+0.25), c(y.pt,y.pt), lwd = 2)
x <- x.pt + 0.15 * rnorm(5)
p <- cbind(x,rep(y.pt,5))
text(x.pt, y.pt, "I", font = 2, cex = 2)
text(p, "x", font = 2, cex = 1.5)
text(x.pt, y.pt - 0.1, "(b)", font = 2, cex = 1.5)
text(x.pt, y.pt + 0.05, expression(mu), font = 2, cex = 1.5)

x.pt <- 0.3
y.pt <- 0.3
lines(c(x.pt-0.25, x.pt+0.25), c(y.pt,y.pt), lwd = 2)
x <- x.pt + 0.1 + 0.05 * rnorm(5)
p <- cbind(x,rep(y.pt,5))
text(x.pt, y.pt, "I", font = 2, cex = 2)
text(p, "x", font = 2, cex = 1.5)
text(x.pt, y.pt - 0.1, "(c)", font = 2, cex = 1.5)
text(x.pt, y.pt + 0.05, expression(mu), font = 2, cex = 1.5)

x.pt <- 1.0
y.pt <- 0.3
lines(c(x.pt-0.25, x.pt+0.25), c(y.pt,y.pt), lwd = 2)
x <- x.pt + 0.1 + 0.15 * rnorm(5)
p <- cbind(x,rep(y.pt,5))
text(x.pt, y.pt, "I", font = 2, cex = 2)
text(p, "x", font = 2, cex = 1.5)
text(x.pt, y.pt - 0.1, "(d)", font = 2, cex = 1.5)
text(x.pt, y.pt + 0.05, expression(mu), font = 2, cex = 1.5)
