# Simulation of sampling the artificial population
library(sf)
pop.data.sf <- st_read("Created\\Set42pop.shp")
sampbdry.sf <- st_read("created\\Set42sampbdry.shp")
pop.mean <- mean(pop.data.sf$Yield)
pop.sd <- sd(pop.data.sf$Yield)

# From Sec. 5.2.1
W <- st_bbox(pop.data.sf)[1]
E <- st_bbox(pop.data.sf)[3]
S <- st_bbox(pop.data.sf)[2]
N <- st_bbox(pop.data.sf)[4]
N <- N + 2.5
S <- S - 2.5
E <- E + 2.5
W <- W - 2.5
print(x.size <- (E - W) / 8)
print(y.size <- (N - S) / 4)
n.x <- 8
n.y <- 4
x.seq <- W + 0.5 * x.size + x.size * seq(0, n.x - 1)
y.seq <- S + 0.5 * y.size + y.size * seq(0, n.y - 1)
library(spatstat)
x.coords <- rep(x.seq, n.y)
y.coords <- sort(rep(y.seq, n.x))
cell.ppp <- ppp(x.coords, y.coords, window = owin(c(W, E), c(S, N)))
thsn.tess <- dirichlet(cell.ppp)
thsn.sfc <- st_as_sfc(thsn.tess)
thsn.sf <- st_sf(thsn.sfc)
thsn.sf$cell_no <- 1:32

# Stratified sample of locations

set.seed(123)
strat.samp <- function(E, W, N, S, x.size, y.size, n.per.cell){
   n.x <- (E - W) / x.size
   n.y <- (N - S) / y.size
   n.cell <- n.x * n.y
   n.tot <- n.per.cell * n.cell
   ret.M <- matrix(0, nrow = n.tot, ncol = 2)
   row.range <- seq(1, n.per.cell)
   for (i in 1:n.x)
      for(j in 1:n.y) {
         x.min <- W + (i - 1) * x.size
         x.max <- W + i * x.size
         y.min <- S + (j - 1) * y.size
         y.max <- S + j * y.size
         samp.pt.x <- runif(n.per.cell, x.min, x.max)
         samp.pt.y <- runif(n.per.cell, y.min, y.max)
         ret.M[row.range,] <- cbind(samp.pt.x, samp.pt.y)
         row.range <- seq(max(row.range) + 1,
             max(row.range) + n.per.cell)
      }
  return(ret.M)
}

closest.point <- function(sample.pt, grid.data){
   dist.sq <- (st_coordinates(grid.data)[,1] - sample.pt[1])^2 +
      (st_coordinates(grid.data)[,2] - sample.pt[2])^2
   return(which.min(dist.sq))
}

set.seed(123)
n.per.cell <- 1
# Create the locations of the random sample
selected.pts <- strat.samp(E, W, N, S, x.size, y.size, n.per.cell)
# Apply the function closest.point() to each row of the
# array sample.coords (i.e., each sample location)
samp.pts <- apply(selected.pts, 1, closest.point,
  grid.data = pop.data.sf)
plot(thsn.sf["cell_no"], axes = TRUE, main = "",
    col = "white", reset = FALSE) # Fig. 5.5
# Each element of samp.data.sf is the index of the population value
# closest to the corresponding location in sample.coords
samp.data.sf <- pop.data.sf[samp.pts,]
plot(samp.data.sf["Yield"], add = TRUE, col = "black", pch = 16,
   cex = 1)  
title(main = "Stratified Sampling", cex.main = 1)

# Monte Carlo simulation of stratified sampling
sim.strat.samp <- function (n.per.cell){
   selected.pts <- strat.samp(E, W, N, S, x.size, y.size, n.per.cell)
   samp.pts <- apply(selected.pts, 1, closest.point,
     grid.data = pop.data.sf)
   samp.data.sf <- pop.data.sf[samp.pts,]
   samp.mean <- mean(samp.data.sf$Yield)
   prct.err <- abs(samp.mean - pop.mean) / pop.mean
   return(c(samp.mean,prct.err))
}
n.per.cell <- 1
set.seed(123)
U <- replicate(1000,sim.strat.samp(n.per.cell))
mean(U[2,]) # Equation (5.1)
sd(U[1,])


