# Simulation of sampling the artificial population
library(sf)
pop.data.sf <- st_read("Created\\Set42pop.shp")
st_crs(pop.data.sf) <- "EPSG:32610"
sampbdry.sf <- st_read("created\\Set42sampbdry.shp")
st_crs(sampbdry.sf) <- "EPSG:32610"

pop.mean <- mean(pop.data.sf$Yield)
pop.sd <- sd(pop.data.sf$Yield)

# Determine the member of the population closest to a sample point
closest.point <- function(sample.pt, grid.data){
  dist.sq <- (st_coordinates(grid.data)[,1] - sample.pt[1])^2 +
    (st_coordinates(grid.data)[,2] - sample.pt[2])^2
  return(which.min(dist.sq))
}

# From Sec. 5.2.1
W <- st_bbox(pop.data.sf)[1]
E <- st_bbox(pop.data.sf)[3]
S <- st_bbox(pop.data.sf)[2]
N <- st_bbox(pop.data.sf)[4]
N <- N + 2.5
S <- S - 2.5
E <- E + 2.5
W <- W - 2.5

# Stratification of field by weeds
data.Set4.2 <- read.csv("set4\\set4.296sample.csv", header = TRUE)
# Extract the coords before converting to a spatial data frame
sample.coords <- cbind(data.Set4.2$Easting, data.Set4.2$Northing)
# Find the yield at the closest sample points
samp.pts <- apply(sample.coords, 1, closest.point,
   grid.data = pop.data.sf)
data.Set4.2$Yield <- pop.data.sf$Yield[samp.pts]
with(data.Set4.2, print(tapply(Yield, Weeds, mean), digits = 4))

library(terra)
data.4.2.May.ter <- rast("set4\\Set4.20596.tif")
crs(data.4.2.May.ter) <- "EPSG:32610"

# Build a plot of weed levels on top of the May IR image
data.Set4.2.sf <- st_as_sf(data.Set4.2, 
  coords = c("Easting", "Northing"), crs = "EPSG:32610")
greys <- grey(0:255 / 255)
par(mai = c(1,1,1,1))
image(data.4.2.May.ter, col = greys,  # Fig. 5.7
  xlim = c(592050,592900), ylim = c(4267500,4267860),
  axes = TRUE)
plot(sampbdry.sf$geom, add = TRUE)
data.Set4.2.sf$hiweeds <- 1
data.Set4.2.sf$hiweeds[data.Set4.2$Weeds == 5] <- 2
plot(data.Set4.2.sf["hiweeds"], add = TRUE, pch = 1,
   cex = data.Set4.2.sf$hiweeds, col = "black")
title(main = "High Weed Locations", xlab = "Easting",
  ylab = "Northing", cex.main = 2, cex.lab = 1.5,
  font.lab = 2)

#Create boundary file
x1 <- 592165
x2 <- 592240
x3 <- 592320
x4 <- 592490
x5 <- 592580
x6 <- 592620
x7 <- 592760
y1 <- 4267690
y2 <- 4267810
y3 <- 4267760

strat1 <- matrix(c(W,y1,x1,y2,x2,y2,x2,S,W,S,W,y1), ncol = 2,
   byrow = TRUE)
strat2 <- matrix(c(x3,S,x3,y3,x4,y3,x4,S,x3,S), ncol = 2,
   byrow = TRUE)
strat3 <- matrix(c(x5,N,x6,N,x6,y2,x5,y2,x5,N), ncol = 2,
   byrow = TRUE)
strat4 <- matrix(c(x7,N,E,N,E,y3,x7,y3,x7,N), ncol = 2, byrow = TRUE)
strat.pol1 <- st_sfc(st_polygon(list(strat1)))
strat.pol2 <- st_sfc(st_polygon(list(strat2)))
strat.pol3 <- st_sfc(st_polygon(list(strat3)))
strat.pol4 <- st_sfc(st_polygon(list(strat4)))
# Notice that this still works
strat1 <- matrix(c(W,y1,x1,y2,x2,y2,x2,S,W,S,W,y1), ncol = 2,
   byrow = TRUE)
strat2 <- matrix(c(x3,S,x3,y3,x4,y3,x4,S,x3,S), ncol = 2,
   byrow = TRUE)
strat3 <- matrix(c(x5,N,x6,N,x6,y2,x5,y2,x5,N), ncol = 2,
   byrow = TRUE)
strat4 <- matrix(c(x7,N,E,N,E,y3,x7,y3,x7,N), ncol = 2, byrow = TRUE)

image(data.4.2.May.ter, col = greys,  # Fig. 5.8
      xlim = c(592050,592900), ylim = c(4267500,4267860),
      axes = TRUE)
plot(sampbdry.sf$geom, add = TRUE)
plot(strat.pol1, add = TRUE)
plot(strat.pol2, add = TRUE)
plot(strat.pol3, add = TRUE)
plot(strat.pol4, add = TRUE)
title(main = "High Weed Strata", xlab = "Easting",
  ylab = "Northing", cex.main = 2, cex.lab = 1.5,
  font.lab = 2)


strat.pol <- st_sfc(st_polygon(list(strat1)),
   st_polygon((list(strat2))), st_polygon(list(strat3)),
   st_polygon(list(strat4)))
stratbdry.sf <- st_sf(z = c(1,1,1,1), strat.pol)
st_crs(stratbdry.sf) <- "EPSG:32610"
print(hi.area <- st_area(stratbdry.sf))
print(tot.area <- st_area(sampbdry.sf))
print(frac.hi <- as.numeric(sum(hi.area) / tot.area))

hiweeds <- st_intersects(pop.data.sf, stratbdry.sf)
n.hi <- as.numeric(lengths(hiweeds) > 0)
length(which(n.hi == 1)) / length(n.hi)
pop.data.sf$hiweeds <- n.hi
sample.size <- 32
hi.size <- round(frac.hi * sample.size, 0)
lo.size <- sample.size - hi.size
samp.size <- c(lo.size,hi.size)
# Generate a plot of s sampling pattern
set.seed(123)
for (i in 0:1){
   subpop <- which(pop.data.sf$hiweeds == i)
   {if (i == 0)
      samp <- sample(subpop,samp.size[i+1])
   else
      samp <- c(samp,sample(subpop,samp.size[i+1]))
}}
strat.samp <- pop.data.sf[samp,]
plot(strat.samp["hiweeds"], xlim = c(592100,592820),
   ylim = c(4267500,4267860), col = "black", main = "",
  axes = TRUE, pch = 16, reset = FALSE)   # Fig. 5.8
plot(sampbdry.sf$geom, add = TRUE)
plot(strat.pol1, add = TRUE)
plot(strat.pol2, add = TRUE)
plot(strat.pol3, add = TRUE)
plot(strat.pol4, add = TRUE)
title(main = "Stratification by Weed Level", cex.main = 1)

# Monte Carlo simulation of stratified sampling
stratified.samp <- function(samp.size){
# Low weed stratum
  subpop <- pop.data.sf$Yield[pop.data.sf$hiweeds == 0]
  samp <- sample(subpop,samp.size[1])
# High weed stratum
  subpop <- pop.data.sf$Yield[pop.data.sf$hiweeds == 1]
  samp <- c(samp,sample(subpop,samp.size[2]))
  samp.mean <- mean(samp)
  prct.error <- abs((samp.mean - pop.mean) / pop.mean)
  return(c(samp.mean, prct.error))
}
set.seed(123)
sample.size <- 32
hi.size <- round(frac.hi * sample.size, 0)
lo.size <- sample.size - hi.size
samp.size <- c(lo.size,hi.size)
U <- replicate(1000, stratified.samp(samp.size))
mean(U[2,])
sd(U[1,])

















