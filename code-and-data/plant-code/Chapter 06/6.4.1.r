
plot(0:9,0:9,type="n", axes = FALSE,  # Fig. 6.11
   xlab = "", ylab = "")
   
library(spdep)
# Grid in the lower right corner (position 4,2)
npts <- 10
dd <- 0.1
min.pt <- dd
max.pt <- 1.5 - dd
d.pt <- (max.pt - min.pt) / (npts - 1)
dx <- 5
dy <- 0
Y42.df <- expand.grid(x = dx + seq(min.pt, max.pt, d.pt),
  y = dy + seq(max.pt, min.pt, -d.pt))
nlist <- cell2nb(npts, npts)
set.seed(123)
lambda <- 0.8
IrWinv <- invIrM(nlist, lambda)
eps <- rnorm(npts^2)
Z <- IrWinv %*% eps
Y42.df$Y <- (Z - min(Z)) / (max(Z) - min(Z))
coordinates(Y42.df) <- c("x", "y")
plot(Y42.df,  pch = 15, col = grey(Y42.df$Y),
   add = TRUE, cex = 1.1)
   
# Grid one up from the bottom on right (position 3,2)
# Can't use the coordinates() extractor function here
Y32.df <- Y42.df
Y32.df@coords[,2] <- Y32.df@coords[,2] + 2.5
plot(Y32.df,  pch = 15, col = grey(Y32.df$Y),
   add = TRUE, cex = 1.1)

# Position 3,1
npts2 <- 25
dd2 <- 0.05
min.pt2 <- dd2
max.pt2 <- 1.5 - dd2
d.pt2 <- (max.pt2 - min.pt2) / (npts2 - 1)
dx <- 0
dy <- 2.5
Y31.df <- expand.grid(x = dx + seq(min.pt2, max.pt2, d.pt2),
  y = dy + seq(max.pt2, min.pt2, -d.pt2))
nlist2 <- cell2nb(npts2, npts2)
set.seed(123)
lambda <- 0.8
IrWinv2 <- invIrM(nlist2, lambda)
eps2 <- rnorm(npts2^2)
Z2 <- IrWinv2 %*% eps2
Y31.df$Y <- (Z2 - min(Z2)) / (max(Z2) - min(Z2))  
coordinates(Y31.df) <- c("x", "y")
plot(Y31.df,  pch = 15, col = grey(Y31.df$Y),
   add = TRUE, cex = 0.47)

#Position 2,2
Y22.df <- Y31.df
Y22.df@coords[,1] <- Y22.df@coords[,1] + 5
Y22.df@coords[,2] <- Y22.df@coords[,2] + 2.5
plot(Y22.df,  pch = 15, col = grey(Y31.df$Y),
   add = TRUE, cex = 0.47)
   
#Position 1,2
Y12.df <- Y22.df
Y12.df@coords[,2] <- Y12.df@coords[,2] + 2.5
plot(Y12.df,  pch = 15, col = grey(Y31.df$Y),
   add = TRUE, cex = 0.47)

  

# Position 4,1
min.pt41 <- min.pt2 + 5 * d.pt2
max.pt41 <- min.pt2 + 14 * d.pt2
Y41.df <- expand.grid(x = seq(min.pt41, max.pt41, 3*d.pt2),
  y = 2.5 + seq(min.pt41, max.pt41, 3*d.pt2))
closest.point <- function(sample.pt, grid.data){
   dist.sq <- (coordinates(grid.data)[,1]-sample.pt[1])^2 +
      (coordinates(grid.data)[,2]-sample.pt[2])^2
   return(which.min(dist.sq))
}

samp.pts <- apply(Y41.df, 1, closest.point, grid.data = Y31.df)
Y41.df$Y <- Y31.df$Y[samp.pts]
coordinates(Y41.df) <- c("x", "y")
Y41.df@coords[,2] <- Y41.df@coords[,2] - 2.5  
plot(Y41.df,  pch = 15, col = grey(Y41.df$Y - 0.1),
   add = TRUE, cex = 0.47)
   
# Position 2,1
Y21.df <- expand.grid(x = seq(min.pt2, max.pt2, 3*d.pt2),
  y = 2.5 + seq(min.pt2, max.pt2, 3*d.pt2))
samp.pts <- apply(Y21.df, 1, closest.point, grid.data = Y31.df)
Y21.df$Y <- Y31.df$Y[samp.pts]
coordinates(Y21.df) <- c("x", "y")
Y21.df@coords[,2] <- Y21.df@coords[,2] + 2.5  
plot(Y21.df,  pch = 15, col = grey(Y41.df$Y - 0.1),
   add = TRUE, cex = 0.47)

# Position 1,1
min.pt11 <- min.pt2 + 5 * d.pt2
max.pt11 <- min.pt2 + 14 * d.pt2
Y11.df <- expand.grid(x = seq(min.pt41, max.pt41, d.pt2),
  y = 2.5 + seq(min.pt11, max.pt11, d.pt2))
samp.pts <- apply(Y11.df, 1, closest.point, grid.data = Y31.df)
Y11.df$Y <- Y31.df$Y[samp.pts]
coordinates(Y11.df) <- c("x", "y")
Y11.df@coords[,2] <- Y11.df@coords[,2] + 5  
plot(Y11.df,  pch = 15, col = grey(Y11.df$Y - 0.1),
   add = TRUE, cex = 0.47)

lines(c(0,0), c(0,1.5), lwd = 3)
lines(c(0,1.5), c(1.5,1.5), lwd = 3)
lines(c(1.5,1.5), c(0,1.5), lwd = 3)
lines(c(0,1.5), c(0,0), lwd = 3)

dx <- 0
dy <- 2.5
lines(c(0+dx,0+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(1.5+dy,1.5+dy), lwd = 3)
lines(c(1.5+dx,1.5+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(0+dy,0+dy), lwd = 3)

dx <- 0
dy <- 5
lines(c(0+dx,0+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(1.5+dy,1.5+dy), lwd = 3)
lines(c(1.5+dx,1.5+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(0+dy,0+dy), lwd = 3)

dx <- 0
dy <- 7.5
lines(c(0+dx,0+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(1.5+dy,1.5+dy), lwd = 3)
lines(c(1.5+dx,1.5+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(0+dy,0+dy), lwd = 3)

dx <- 5
dy <- 0
lines(c(0+dx,0+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(1.5+dy,1.5+dy), lwd = 3)
lines(c(1.5+dx,1.5+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(0+dy,0+dy), lwd = 3)

dx <- 5
dy <- 2.5
lines(c(0+dx,0+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(1.5+dy,1.5+dy), lwd = 3)
lines(c(1.5+dx,1.5+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(0+dy,0+dy), lwd = 3)

dx <- 5
dy <- 5
lines(c(0+dx,0+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(1.5+dy,1.5+dy), lwd = 3)
lines(c(1.5+dx,1.5+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(0+dy,0+dy), lwd = 3)

dx <- 5
dy <- 7.5
lines(c(0+dx,0+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(1.5+dy,1.5+dy), lwd = 3)
lines(c(1.5+dx,1.5+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(0+dy,0+dy), lwd = 3)

lines(c(0,0), c(0,1.5), lwd = 3)
lines(c(0,1.5), c(1.5,1.5), lwd = 3)
lines(c(1.5,1.5), c(0,1.5), lwd = 3)
lines(c(0,1.5), c(0,0), lwd = 3)

dx <- 0
dy <- 2.5
lines(c(0+dx,0+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(1.5+dy,1.5+dy), lwd = 3)
lines(c(1.5+dx,1.5+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(0+dy,0+dy), lwd = 3)

dx <- 0
dy <- 5
lines(c(0+dx,0+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(1.5+dy,1.5+dy), lwd = 3)
lines(c(1.5+dx,1.5+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(0+dy,0+dy), lwd = 3)

dx <- 0
dy <- 7.5
lines(c(0+dx,0+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(1.5+dy,1.5+dy), lwd = 3)
lines(c(1.5+dx,1.5+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(0+dy,0+dy), lwd = 3)

dx <- 5
dy <- 0
lines(c(0+dx,0+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(1.5+dy,1.5+dy), lwd = 3)
lines(c(1.5+dx,1.5+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(0+dy,0+dy), lwd = 3)

dx <- 5
dy <- 2.5
lines(c(0+dx,0+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(1.5+dy,1.5+dy), lwd = 3)
lines(c(1.5+dx,1.5+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(0+dy,0+dy), lwd = 3)

dx <- 5
dy <- 5
lines(c(0+dx,0+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(1.5+dy,1.5+dy), lwd = 3)
lines(c(1.5+dx,1.5+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(0+dy,0+dy), lwd = 3)

dx <- 5
dy <- 7.5
lines(c(0+dx,0+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(1.5+dy,1.5+dy), lwd = 3)
lines(c(1.5+dx,1.5+dx), c(0+dy,1.5+dy), lwd = 3)
lines(c(0+dx,1.5+dx), c(0+dy,0+dy), lwd = 3)

text(3, 1.5, expression(bold(Combination)))
arrows(2,1, 4,1, length = 0.1, lwd = 2)
text(3, 0.8, expression(bold(extrapolation)), cex = 0.8)
text(3, 0.5, expression(bold(interpolation)), cex = 0.8)
text(3, 0.2, expression(bold(upscaling)), cex = 0.8)

text(3.2, 4, expression(bold(Change~of~Support)))
arrows(2,3.3, 4,3.3, length = 0.1, lwd = 2)
text(3, 3.5, expression(bold(upscaling)), cex = 0.8)
arrows(4,3.1, 2,3.1, length = 0.1, lwd = 2)
text(3, 2.9, expression(bold(downscaling)), cex = 0.8)

text(3.2, 6.5, expression(bold(Change~of~Coverage)))
arrows(2,5.8, 4,5.8, length = 0.1, lwd = 2)
text(3, 6, expression(bold(interpolation)), cex = 0.8)
arrows(4,5.6, 2,5.6, length = 0.1, lwd = 2)
text(3, 5.4, expression(bold(sampling)), cex = 0.8)

text(3.2, 9, expression(bold(Change~of~Extent)))
arrows(2,8.3, 4,8.3, length = 0.1, lwd = 2)
text(3, 8.5, expression(bold(extrapolation)), cex = 0.8)
arrows(4,8.1, 2,8.1, length = 0.1, lwd = 2)
text(3, 7.9, expression(bold(singling~out)), cex = 0.8)
