n <- 25
vr <- function(r){
  a <- 2*r/(1-r)
  b <- 1 - 1/n
  c <- (2/n)*(1-r^(n-1))
  d <- (r/(1-r))^2
  v <- 1 + a*b - c*d
  return(v)
}
rho <- as.matrix(seq(0.05,0.95,0.05))
var <- apply(rho,1,vr) / 25
par(mai = c(1,1,1,1))
plot(rho,var, main = "Sample Variance",  # Fig. 10.1a
   cex.main = 2, ylab = expression(var*"{"*rho*"}"),
   xlab = expression(rho), cex.lab = 2)
ne <- 1 / var
plot(rho, ne, main = "Effective Sample Size",
   cex.main = 2, ylab = expression(n[e]),
   xlab = expression(rho), cex.lab = 2) # Fig. 10.1b
   
data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
# Paired t-test
EM425B <- data.Set4.1$EM38B425[!is.na(data.Set4.1$EM38B425)]
EM520F <- data.Set4.1$EM38F520[!is.na(data.Set4.1$EM38B425)]

mean(EM425B)
mean(EM520F)

par(mai = c(1,1,1,1))
# This figure is not in the text
plot(EM425B, EM520F, main = "EM38 Readings: 4/25 vs. 5/20",
  xlim = c(50,100), ylim = c(50,100), cex.main = 2,
  xlab = "Bed EM38 Reading, 4/25/96",
  ylab = "Bed EM38 Reading, 5/20/96", cex.lab = 1.5)
  
h1 <- hist(EM425B, breaks = seq(50,100,5), plot = FALSE)
h2 <- hist(EM520F, breaks = seq(50,100,5), plot = FALSE)
plot(h1$mids,h1$density, xlim = c(50,100), ylim = c(0,0.06),
   type = "o", cex.main = 2, cex.lab = 1.5, # Fig. 10.2
   xlab = "EC (mS/M)", ylab = "Frequency",
   main = "Histograms of EM38 Readings")
lines(h2$mids,h2$density, xlim = c(50,100),
   type = "o", lty = 2)
legend(80, 0.05, c("4/25 Bed", "5/20 Furrow"), lty = c(1,2))

t.test(EM425B,EM520F, paired = TRUE, alternative = "two.sided")
