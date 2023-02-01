# This is from 16.2
library(spatstat)
library(maptools)
library(spdep)
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

# Correlated errors (GLS)
x <- (cell.ctrs[,1] - W) / (E - W)
y <- (cell.ctrs[,2] - S) / (N - S)
Yield.T <- lm(Yield ~ x + y + I(x^2) +
+   I(y^2) + I(x*y), data = plots.spdf@data)
trend.RCB <- with(plots.spdf@data, data.frame(Yield = Yield,
   trtmt = trtmt.RCB, Trend = predict(Yield.T)))
trend.form <- as.formula(Yield ~ trtmt + Trend)

library(nlme)
trend.RCB$x <- x
trend.RCB$y <- y
model.gls <- gls(Yield ~ trtmt + Trend, data = trend.RCB)
# Check that the trend looks reasonable
trellis.device(color = FALSE)
plot(Variogram(model.gls, form = ~ x + y,
   maxDist = 1), xlim = c(0,1),
   main = "Variogram of Residuals")
model.gls2 <- update(model.gls,
   corr = corSpher(value = c(0.35, 0.5),
   form = ~ x + y, nugget = TRUE))
summary(model.gls2)
coef(summary(model.gls2))

EMP.calc <- function(trt.data, form.lm){
   trt.data$trtmt <- factor(unlist(tapply(rep(1:18,4),
      sort(rep(1:4,18)), sample)))
   trt.sum <- summary(lm(form.lm, data = trt.data))
   contr <- coef(trt.sum)
   tau.hat <- contr[1:18,1]
   tau.hat[2:18] <- tau.hat[1] + tau.hat[2:18]
   tau.dist <- vegdist(tau.hat, method = "euclidean")
   EMP <- 2 * sum(tau.dist^2) / (18*17)
   return(EMP)
}

set.seed(123)
RCB.form <- as.formula(Yield ~ trtmt.RCB + block)
U <- replicate(100, EMP.calc(plots.spdf@data, RCB.form))
print(EMP.RCB <- mean(U))


EMP.gls <- function(trt.data){
   trt.data$trtmt <- factor(unlist(tapply(rep(1:18,4),
      sort(rep(1:4,18)), sample)))
   trt.sum <- summary(gls(Yield ~ trtmt + Trend,
      corr = corSpher(value = c(0.35, 0.5),
      form = ~ x + y, nugget = TRUE), data = trt.data))
   contr <- coef(trt.sum)
   tau.hat <- c(0, contr[2:18])
   tau.dist <- vegdist(tau.hat, method = "euclidean")
   EMP <- 2 * sum(tau.dist^2) / (18*17)
   return(EMP)
}

set.seed(123)
U <- replicate(100, EMP.gls(trend.RCB))
print(EMP.gls <- mean(U))
print(EMPbar.gls <- 100 * EMP.gls / EMP.RCB)

