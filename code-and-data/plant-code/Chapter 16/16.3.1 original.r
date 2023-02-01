# This is from 16.2
library(spatstat)
library(maptools)
library(vegan)

data.Yield4.1 <- read.csv("created\\set4.1yld96cleaned.csv", header = TRUE)
coordinates(data.Yield4.1) <- c("Easting","Northing")
E <- 592400
W <- 592050
S <- 4270400
N <- 4271100
cell.size <- 350 / 6
xmin <- W + cell.size / 2
xmax <- E - cell.size / 2
ymin <- S + cell.size / 2
ymax <- N - cell.size / 2
cell.ctrs <- expand.grid(seq(xmax, xmin, -cell.size),
      seq(ymin, ymax, cell.size))
cell.ppp <- ppp(cell.ctrs[,1], cell.ctrs[,2],
     window = owin(c(W, E), c(S, N)))
plots.pp <- dirichlet(cell.ppp)
plots.sp <- as(plots.pp, "SpatialPolygons")
row.names(plots.sp) = as.character(1:72)
plots.spdf <- SpatialPolygonsDataFrame(plots.sp,
  data = data.frame(ID = as.character(72:1),
  row.names = as.character(1:72)))
plots.pts <- over(data.Yield4.1, plots.spdf)
Yield.pts <- data.Yield4.1[which(is.na(plots.pts)== FALSE),]
plots.spdf@data$Yield <-
   over(plots.spdf, Yield.pts, fn = mean)$Yield
set.seed(123)
plots.spdf@data$trtmt.CRD <- factor(sample(rep((1:18), 4)))
plots.spdf@data$block <- factor(sort(rep(1:4, 18)))
plots.spdf@data$trtmt.RCB <- factor(unlist(tapply(rep(1:18,4),
   sort(rep(1:4,18)), sample)))

trt.mean <- tapply(plots.spdf@data$Yield,
  plots.spdf@data$trtmt.RCB, mean)
print((trt.mean - trt.mean[1])[2:18], digits = 4)

RCB.lm <- lm(Yield ~ trtmt.RCB + block,
  data = plots.spdf@data)
print(RCB.sum <- summary(RCB.lm))

tau.dif1 <- coef(RCB.sum)[2:18,1]

tau.hat <- c(0, tau.dif1)

print(vegdist(tau.hat[1:4], method = "euclidean"), digits = 3)
tau.dist <- vegdist(tau.hat, method = "euclidean")

# Computation of EMP

print(EMP.RCB <- 2 * sum(tau.dist^2) / (18*17))

# Monte Carlo simuation
EMP.calc <- function(trt.data, form.lm){
   trt.data$trtmt <- factor(unlist(tapply(rep(1:18,4),
      sort(rep(1:4,18)), sample)))
   trt.sum <- summary(lm(form.lm, data = trt.data))
   tau.dif1 <- coef(trt.sum)[2:18,1]
   tau.hat <-  c(0, tau.dif1)
   tau.dist <- vegdist(tau.hat, method = "euclidean")
   EMP <- 2 * sum(tau.dist^2) / (18*17)
   return(EMP)
}


set.seed(123)
RCB.form <- as.formula(Yield ~ trtmt.RCB + block)
U <- replicate(100, EMP.calc(plots.spdf@data, RCB.form))
print(EMP.RCB <- mean(U))



