library(spatstat)
library(maptools)
data.Yield4.1 <- read.csv("created\\set4.1yld96cleaned.csv", header = TRUE)
coordinates(data.Yield4.1) <- c("Easting","Northing")
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
plots.sp <- as(plots.pp, "SpatialPolygons")
row.names(plots.sp) = as.character(1:72)
plots.spdf <- SpatialPolygonsDataFrame(plots.sp,
  data = data.frame(ID = as.character(72:1),
  row.names = as.character(1:72)))

plots.pts <- over(data.Yield4.1, plots.spdf)
Yield.pts <- data.Yield4.1[which(is.na(plots.pts)== FALSE),]
par(mai = c(1,1,1,1))
plot(Yield.pts, pch = 16,   # Fig. 16.1
   col = grey(0.7*(Yield.pts@data$Yield/max(Yield.pts@data$Yield)) + 0.3),
   axes = TRUE)
plot(plots.sp, axes = FALSE, add = TRUE)
title(main = "Plot Numbers", xlab = "Easting",
  ylab = "Northing", cex.main = 2, font.lab = 2, cex.lab = 1.5)
y <- lapply(plots.sp@polygons, slot, "labpt")
y.loc <- matrix(0, length(y), 2)
for (i in 1:length(y)) y.loc[i,] <- unlist(y[[i]])
text(y.loc,labels=lapply(plots.sp@polygons, slot, "ID"))


set.seed(123)
plots.spdf@data$trtmt.CRD <- factor(sample(rep((1:18), 4)))
plots.spdf@data$block <- factor(sort(rep(1:4, 18)))
plots.spdf@data$trtmt.RCB <- factor(unlist(tapply(rep(1:18,4),
   sort(rep(1:4,18)), sample)))

plot(Yield.pts, pch = 16,   # Fig. 16.2
   col = grey(0.7*(Yield.pts@data$Yield/max(Yield.pts@data$Yield)) + 0.3),
   axes = TRUE)
plot(plots.sp, axes = FALSE, add = TRUE)
title(main = "CRD Treatments", xlab = "Easting",
  ylab = "Northing", cex.main = 2, font.lab = 2, cex.lab = 1.5)
text(y.loc,labels=plots.spdf@data$trtmt.CRD, font = 2)

greys <- grey(c(130,160,190,220)/255)  # Fig. 16.3a
plot.nos <- list("sp.text", y.loc,
   as.character(plots.spdf@data$trtmt.RCB))
spplot(plots.spdf, zcol = "block", col.regions = greys,
   scales = list(draw = TRUE), xlab = "Easting", sp.layout = plot.nos,
   ylab = "Northing", main = "RCB Blocks and Plots")

plot(Yield.pts, pch = 16,   # Fig. 16.3b
   col = grey(0.7*(Yield.pts@data$Yield/max(Yield.pts@data$Yield)) + 0.3),
   axes = TRUE)
plot(plots.sp, axes = FALSE, add = TRUE)
title(main = "RCB Treatments", xlab = "Easting",
  ylab = "Northing", cex.main = 2, font.lab = 2, cex.lab = 1.5)
text(y.loc, labels = plots.spdf@data$trtmt.RCB, font = 2)

# Use over() to obtain the mean yield for each plot
plots.spdf@data$Yield <-
   over(plots.spdf, Yield.pts, fn = mean)$Yield

# RCB analysis
print(sort(tapply(plots.spdf@data$Yield,
  plots.spdf@data$trtmt.RCB, mean)), digits = 4)

RCB.aov <- aov(Yield ~ block + trtmt.RCB,
  data = plots.spdf@data)
summary(RCB.aov)
align.x <- factor(row.names(sort(tapply(plots.spdf@data$Yield,
   plots.spdf@data$trtmt.RCB, mean))))
x.order <- numeric(72)

for (i in 1:18) x.order[which(plots.spdf@data$trtmt.RCB == align.x[i])] <- i
with(plots.spdf@data, plot(x.order, Yield,  # Fig. 16.4
    cex.lab = 1.5, xaxt = "n", xlab = "Treatment", ylab = "Yield",
    main = "RCB Yields and Treatment Means", cex.main = 2))
axis(side = 1, at = 1:18,
   labels = as.character(align.x))
text(1:18, sort(tapply(plots.spdf@data$Yield,
  plots.spdf@data$trtmt.RCB, mean)), "-", cex = 2)
  
#################
print(tau.hat <- tapply(plots.spdf@data$Yield,
  plots.spdf@data$trtmt.RCB, mean), digits = 5)


RCB.lm <- lm(Yield ~ trtmt.RCB + block,
  data = plots.spdf@data)
print(RCB.sum <- summary(RCB.lm))

print(trt.means <- tapply(plots.spdf@data$Yield,
  plots.spdf@data$trtmt.RCB, mean), digits = 5)
print(trt.means[2] - trt.means[1], digits = 3)
print(trt.means[3] - trt.means[1], digits = 4)


