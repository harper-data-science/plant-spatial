library(spatstat)
library(sf)
library(terra)


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
par(mai = c(1,1,1,1))
plot(st_geometry(Yield.pts.sf), pch = 16,   # Fig. 16.1
   col = grey(0.7*(Yield.pts.sf$Yield/max(Yield.pts.sf$Yield)) + 0.3),
   axes = TRUE)
plot(st_geometry(plots.sf), axes = FALSE, add = TRUE)
title(main = "Plot Numbers", xlab = "Easting",
  ylab = "Northing", cex.main = 2, font.lab = 2, cex.lab = 1.5)
lab.pts <- st_coordinates(st_centroid(plots.sf))
plot.names <- unlist(dimnames(lab.pts))[1:72]
text(lab.pts[,1], lab.pts[,2],
   labels <- plot.names, cex = 0.8)

set.seed(123)
trtmt.CRD <- factor(sample(rep((1:18), 4)))
plots.sf$trtmt.CRD <- trtmt.CRD
plots.sf$block <- factor(sort(rep(1:4, 18)))
plots.sf$trtmt.RCB <- factor(unlist(tapply(rep(1:18,4),
   sort(rep(1:4,18)), sample)))
plot(st_geometry(Yield.pts.sf), pch = 16,   # Fig. 16.2
   col = grey(0.7*(Yield.pts.sf$Yield/max(Yield.pts.sf$Yield)) + 0.3),
   axes = TRUE)
plot(st_geometry(plots.sf), axes = FALSE, add = TRUE)
title(main = "CRD Treatments", cex.main = 2, font.lab = 2, cex.lab = 1.5,
   xlab = "Easting", ylab = "Northing")
text(lab.pts[,1], lab.pts[,2],
   labels <- trtmt.CRD, cex = 0.8)

greys <- grey(c(130,160,190,220)/255)  # Fig. 16.3a
plots.ter <- vect(plots.sf)
plot(plots.ter, y = "block", col = greys, main = "RCB Blocks and Plots")
text(y.loc, labels = plots.ter$trtmt.RCB, cex = 0.8)

plot(st_geometry(Yield.pts.sf), pch = 16,   # Fig. 16.3b
   col = grey(0.7*(Yield.pts.sf$Yield/max(Yield.pts.sf$Yield)) + 0.3),
   axes = TRUE)
plot(st_geometry(plots.sf), axes = FALSE, add = TRUE)
title(main = "RCB Treatments", cex.main = 2, font.lab = 2, cex.lab = 1.5,
   xlab = "Easting", ylab = "Northing")
text(lab.pts[,1], lab.pts[,2],
   labels <- as.character(plots.sf$trtmt.RCB, cex = 0.8))

# Use st_intersects() to obtain the mean yield for each plot
Y <- st_intersects(plots.sf, Yield.pts.sf)
Yield.means <- numeric(length(Y))
for (i in 1:length(Yield.means)) Yield.means[i] <- mean(Yield.pts$Yield[Y[[i]]])
plots.sf$Yield <- Yield.means

# RCB analysis
print(sort(tapply(Yield.means, plots.sf$trtmt.RCB, mean)), digits = 4)
RCB.aov <- aov(Yield ~ block + trtmt.RCB, data = plots.sf)
summary(RCB.aov)
align.x <- factor(row.names(sort(tapply(plots.sf$Yield,
   plots.sf$trtmt.RCB, mean))))
x.order <- numeric(72)

for (i in 1:18) x.order[which(plots.sf$trtmt.RCB == align.x[i])] <- i
with(plots.sf, plot(x.order, Yield,  # Fig. 16.4
    cex.lab = 1.5, xaxt = "n", xlab = "Treatment", ylab = "Yield",
    main = "RCB Yields and Treatment Means", cex.main = 2))
axis(side = 1, at = 1:18,
   labels = as.character(align.x))
text(1:18, sort(tapply(plots.sf$Yield,
  plots.sf$trtmt.RCB, mean)), "-", cex = 2)
  
#################
print(tau.hat <- tapply(plots.sf$Yield,
  plots.sf$trtmt.RCB, mean), digits = 5)


RCB.lm <- lm(Yield ~ trtmt.RCB + block, data = plots.sf)
print(RCB.sum <- summary(RCB.lm))

print(trt.means <- tapply(plots.sf$Yield,
  plots.sf$trtmt.RCB, mean), digits = 5)
print(trt.means[2] - trt.means[1], digits = 3)
print(trt.means[3] - trt.means[1], digits = 4)


