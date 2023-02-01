library(rpart)
data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
data.Yield4.1idw <- read.csv("created\\set4.1yld96ptsidw.csv")
data.Set4.1$Yield <- data.Yield4.1idw$Yield
min(data.Set4.1$SoilP)
max(data.Set4.1$SoilP)
par(mai = c(1,1,1,1))
with(data.Set4.1, plot(SoilP, Weeds, cex = 3 * Yield/max(Yield),
   cex.lab = 1.5))    # Fig. 9.7
lines(c(0,20), c(2.5,2.5), lwd = 3)   
lines(c(0,20), c(3.5,3.5), lwd = 2)
lines(c(7.36,7.36), c(2.5,3.5), lwd = 1)
lines(c(6.94,6.94), c(0,2.5))
text(10, 3.5, "1", cex = 2)
text(10, 1.5, "2", cex = 2)
text(10, 4.5, "3", cex = 2)
text(10, 3, "4", cex = 2)
text(4.75, 1.5, "5", cex = 2)
text(12.75, 1.5, "6", cex = 2)
text(5, 3, "7", cex = 2)
text(13, 3, "8", cex = 2)
title(main = "SoilP - Weeds Data Space", cex.main = 2)

Set4.1.rp1 <- rpart(Yield ~ SoilP + Weeds, data = data.Set4.1,
                    method = "anova")
plot(Set4.1.rp1) #Fig. 9.8
text(Set4.1.rp1)
