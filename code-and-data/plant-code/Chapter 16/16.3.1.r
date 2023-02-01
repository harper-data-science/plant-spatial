# This is from 16.2
library(spatstat)
library(sf)
library(vegan)

data.Yield4.1 <- read.csv("created\\set4.1yld96cleaned.csv", header = TRUE)
data.Yield4.1.sf <- st_as_sf(data.Yield4.1, coords = c("Easting","Northing"))
E <- 592400
W <- 592050
S <- 4270400
N <- 4271100
E - W
N - S
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
plots.sfc <- st_as_sfc(plots.pp)
plots.sf <- st_sf(plots.sfc)
plots.sf$row_name = as.character(1:72)

plots.pts <- st_intersects(data.Yield4.1.sf, plots.sf)
Yield.pts <- data.Yield4.1[which(is.na(plots.pts)== FALSE),]
Yield.pts.sf <- st_as_sf(Yield.pts, coords = c("Easting","Northing"))
set.seed(123)
trtmt.CRD <- factor(sample(rep((1:18), 4)))
plots.sf$trtmt.CRD <- trtmt.CRD
plots.sf$block <- factor(sort(rep(1:4, 18)))
plots.sf$trtmt.RCB <- factor(unlist(tapply(rep(1:18,4),
   sort(rep(1:4,18)), sample)))

# Use st_intersects() to obtain the mean yield for each plot
Y <- st_intersects(plots.sf, Yield.pts.sf)
Yield.means <- numeric(length(Y))
for (i in 1:length(Y)) Yield.means[i] <- mean(Yield.pts$Yield[Y[[i]]])
plots.sf$Yield <- Yield.means

trt.mean <- tapply(plots.sf$Yield, plots.sf$trtmt.RCB, mean)
print((trt.mean - trt.mean[1])[2:18], digits = 4)

RCB.lm <- lm(Yield ~ trtmt.RCB + block, data = plots.sf)
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
U <- replicate(100, EMP.calc(plots.sf, RCB.form))
print(EMP.RCB <- mean(U))



