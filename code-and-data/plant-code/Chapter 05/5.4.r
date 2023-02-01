library(sf)
pop.data.sf <- st_read("Created\\Set42pop.shp")
library(gstat)
v0.var <- variogram(Yield ~ 1, pop.data.sf, cutoff = 400)

# Determine the member of the population closest to a sample point
closest.point <- function(sample.pt, grid.data){
  dist.sq <- (st_coordinates(grid.data)[,1] - sample.pt[1])^2 +
    (st_coordinates(grid.data)[,2] - sample.pt[2])^2
  return(which.min(dist.sq))
}


W <- st_bbox(pop.data.sf)[1]
E <- st_bbox(pop.data.sf)[3]
S <- st_bbox(pop.data.sf)[2]
N <- st_bbox(pop.data.sf)[4]
N <- N + 2.5
S <- S - 2.5
E <- E + 2.5
W <- W - 2.5

# Grid sample on a 12 by 24 = 288 grid
nrows <- 12
ncols <- 24
grid.size <- (E - W) / ncols
grid.offset <- 0.5 * grid.size
spsamp.pts <- expand.grid(x = seq(W + grid.offset,E,grid.size),
  y = seq(N - grid.offset,S,-grid.size))
samp.pts <- apply(spsamp.pts, 1, closest.point,
    grid.data = pop.data.sf)
data.samp <- pop.data.sf[samp.pts,]
v1.var <- variogram(Yield ~ 1, data.samp, cutoff = 400)
par(mai = c(1,1,1,1))
plot(v0.var$dist, v0.var$gamma, type = "l",  # Fig. 5.11a
   ylim = c(0,2000000), xlab = expression(bold("lag h")),
   ylab = expression(bold(hat(gamma)~"(h)")),
   main = "288 Point Grid Sample Variogram",
   cex.main = 2, cex.lab = 1.5, lwd = 2, font.axis = 2)
lines(v1.var$dist, v1.var$gamma, lty = 2, lwd = 2)
legend(250, 500000, c("Population", "Sample"),
  lty = c(1,2), lwd = 2)

# Grid sample on an 8 by 16 = 128 grid
nrows <- 8
ncols <- 16
grid.size <- (E - W) / ncols
grid.offset <- 0.5 * grid.size
spsamp.pts <- expand.grid(x = seq(W + grid.offset,E,grid.size),
  y = seq(N - grid.offset,S,-grid.size))
samp.pts <- apply(spsamp.pts, 1, closest.point,
      grid.data = pop.data.sf)
data.samp <- pop.data.sf[samp.pts,]
v1.var <- variogram(Yield ~ 1, data.samp, cutoff = 400)
plot(v0.var$dist, v0.var$gamma, type = "l",  # Fig. 5.11b
   ylim = c(0,2000000), xlab = expression(bold("lag h")),
   ylab = expression(bold(hat(gamma)~"(h)")),
   main = "128 Point Grid Sample Variogram",
   cex.main = 2, cex.lab = 1.5, lwd = 2, font.axis = 2)
lines(v1.var$dist, v1.var$gamma, lty = 2, lwd = 2)
legend(250, 500000, c("Population", "Sample"),
  lty = c(1,2), lwd = 2)

# Grid sample on a 6 by 12 = 72 grid
nrows <- 6
ncols <- 12
grid.size <- (E - W) / ncols
grid.offset <- 0.5 * grid.size
spsamp.pts <- expand.grid(x = seq(W + grid.offset,E,grid.size),
  y = seq(N - grid.offset,S,-grid.size))
samp.pts <- apply(spsamp.pts, 1, closest.point,
  grid.data = pop.data.sf)
data.samp <- pop.data.sf[samp.pts,]
v1.var <- variogram(Yield ~ 1, data.samp, cutoff = 400)
plot(v0.var$dist, v0.var$gamma, type = "l",  # Fig. 5.11c
   ylim = c(0,2000000), xlab = expression(bold("lag h")),
   ylab = expression(bold(hat(gamma)~"(h)")),
   main = "72 Point Grid Sample Variogram",
   cex.main = 2, cex.lab = 1.5, lwd = 2, font.axis = 2)
lines(v1.var$dist, v1.var$gamma, lty = 2, lwd = 2)
legend(250, 500000, c("Population", "Sample"),
  lty = c(1,2), lwd = 2)


# Random sample 288 points
samp.size <- 288
coords.mat <- matrix(c(W,E,E,W,W,N,N,S,S,N), ncol = 2)
coords.mat <- matrix(c(W,E,E,W,W,N,N,S,S,N), ncol = 2)
coords.lst <- list(coords.mat)
coords.pol = st_sfc(st_polygon(coords.lst))
sampbdry.sf = st_sf(z = 1, coords.pol)
st_crs(sampbdry.sf) <- "+proj=utm +zone=10 +ellps=WGS84"

set.seed(123)
spsamp.pts <- st_sample(sampbdry.sf, samp.size, type = "random")
sample.coords <- st_coordinates(spsamp.pts)
samp.pts <- apply(sample.coords, 1, closest.point,
  grid.data = pop.data.sf)
data.samp <- pop.data.sf[samp.pts,]
v1.var <- variogram(Yield ~ 1, data.samp, cutoff = 400)
plot(v0.var$dist, v0.var$gamma, type = "l",  # Fig. 5.11d
   ylim = c(0,2000000), xlab = expression(bold("lag h")),
   ylab = expression(bold(hat(gamma)~"(h)")),
   main = "288 Point Random Sample Variogram",
   cex.main = 2, cex.lab = 1.5, lwd = 2, font.axis = 2)
lines(v1.var$dist, v1.var$gamma, lty = 2, lwd = 2)
legend(250, 500000, c("Population", "Sample"),
  lty = c(1,2), lwd = 2)
  
# Cluster-based sampling
nrows <- 4
ncols <- 8
grid.size <- (E - W) / ncols
grid.offset <- 0.5 * grid.size
cluster.size <- 10
n <- 0
for (i in seq(W + grid.offset,E,grid.size))
  for (j in seq(N - grid.offset,S,-grid.size)){
     spsamp.clst <- expand.grid(x = seq(i-cluster.size, i+cluster.size, cluster.size),
        y = seq(j-cluster.size, j+cluster.size, cluster.size))
     if (n == 0)
       spsamp.pts <- spsamp.clst
     else
       spsamp.pts <- rbind(spsamp.pts, spsamp.clst)
     n <- n + 1
}
samp.pts <- apply(spsamp.pts, 1, closest.point,
    grid.data = pop.data.sf)
data.samp <- pop.data.sf[samp.pts,]
v1.var <- variogram(Yield ~ 1, data.samp, cutoff = 400)
plot(v0.var$dist, v0.var$gamma, type = "l", # Fig. 5.11e
   ylim = c(0,2000000), xlab = expression(bold("lag h")),
   ylab = expression(bold(hat(gamma)~"(h)")),
   main = "288 Point Cluster Sample Variogram",
   cex.main = 2, cex.lab = 1.5, lwd = 2, font.axis = 2)
lines(v1.var$dist, v1.var$gamma, lty = 2, lwd = 2)
legend(250, 500000, c("Population", "Sample"),
  lty = c(1,2), lwd = 2)


