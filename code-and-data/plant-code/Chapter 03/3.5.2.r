plot(1:6,1:6,type="n", axes = FALSE,  # Fig. 3.7
   xlab = "", ylab = "")
lines(c(2,2), c(2,5), lwd = 2)
lines(c(2,5), c(2,2), lwd = 2)
lines(c(5,5), c(2,5), lwd = 2)
lines(c(2,5), c(5,5), lwd = 2)
lines(c(2,5), c(4,4), lwd = 2)
lines(c(2,5), c(3,3), lwd = 2)
lines(c(4,4), c(2,5), lwd = 2)
lines(c(3,3), c(2,5), lwd = 2)
text(2.5,4.5, "1", font = 2, cex = 2)
text(3.5,4.5, "2", font = 2, cex = 2)
text(4.5,4.5, "3", font = 2, cex = 2)
text(2.5,3.5, "4", font = 2, cex = 2)
text(3.5,3.5, "5", font = 2, cex = 2)
text(4.5,3.5, "6", font = 2, cex = 2)
text(2.5,2.5, "7", font = 2, cex = 2)
text(3.5,2.5, "8", font = 2, cex = 2)
text(4.5,2.5, "9", font = 2, cex = 2)

# -----------------------------------------

# Creation of Fig. 3.8

plot(1:5,1:5,type="n", axes = FALSE,  # Fig. 3.8a
 xlab = "", ylab = "")
lines(c(2,2), c(2,4), lwd = 2)
lines(c(2,4), c(2,2), lwd = 2)
lines(c(4,4), c(2,4), lwd = 2)
lines(c(2,4), c(4,4), lwd = 2)
lines(c(3,3), c(2,4), lwd = 2)
lines(c(2,4), c(3,3), lwd = 2)
text(2.5,3.5, "1", font = 2, cex = 2)
text(3.5,3.5, "2", font = 2, cex = 2)
text(2.5,2.5, "3", font = 2, cex = 2)
text(3.5,2.5, "4", font = 2, cex = 2)


plot(1:5,1:5,type="n", axes = FALSE,  # Fig. 3.8b
 xlab = "", ylab = "")
symbols(3, 3, circles = 3, add = TRUE, lwd = 2)
library(car)
ellipse(center = c(3,3), shape = matrix(c(1,0,0,0.5), nrow = 2), radius = 0.3,
   center.cex = 0, col = "black")
lines(c(2.17,2.7), c(3,3), lwd = 2)
lines(c(3.3,3.82), c(3,3), lwd = 2)
lines(c(3,3), c(2.08,2.8), lwd = 2)
lines(c(3,3), c(3.23,3.95), lwd = 2)
text(2.7,3.5, "1", font = 2, cex = 2)
text(3.3,3.5, "2", font = 2, cex = 2)
text(2.7,2.5, "3", font = 2, cex = 2)
text(3.3,2.5, "4", font = 2, cex = 2)





