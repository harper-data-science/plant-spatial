# Simulation of sampling the artificial population
library(maptools)
library(sf)
pop.sf <- st_read("Created\\Set42pop.shp")
pop.data <- as(pop.sf, "Spatial")
sampbdry.sf <- st_read("created\\Set42sampbdry.shp")
sampbdry.sp <- as(sampbdry.sf, "Spatial")

pop.mean <- mean(pop.data$Yield)
pop.sd <- sd(pop.data$Yield)

# Determine the member of the population closest to a sample point
closest.point <- function(sample.pt, grid.data){
  dist.sq <- (coordinates(grid.data)[,1] - sample.pt[1])^2 +
    (coordinates(grid.data)[,2] - sample.pt[2])^2
  return(which.min(dist.sq))
}

# From Sec. 5.2.1
W <- bbox(pop.data)[1,1]
E <- bbox(pop.data)[1,2]
S <- bbox(pop.data)[2,1]
N <- bbox(pop.data)[2,2]
N <- N + 2.5
S <- S - 2.5
E <- E + 2.5
W <- W - 2.5

# Stratification of field by weeds
data.Set4.2 <- read.csv("set4\\set4.296sample.csv", header = TRUE)
# Extract the coords before converting to a spatial data frame
sample.coords <- cbind(data.Set4.2$Easting, data.Set4.2$Northing)
# Find the yield at the closest sample points
samp.pts <- apply(sample.coords, 1, closest.point, grid.data = pop.data)
data.Set4.2$Yield <- pop.data$Yield[samp.pts]
with(data.Set4.2, print(tapply(Yield, Weeds, mean), digits = 4))

library(raster)
data.4.2.May <- raster("set4\\Set4.20596.tif")
projection(data.4.2.May) <- CRS("+proj=utm +zone=10 +ellps=WGS84")

############################################################
# Don't run this if you use the function raster()
# If the raster package doesn't work use this
library(rgdal)
data.4.2.May <- readGDAL("set4\\Set4.20596.tif")
proj4string(data.4.2.May) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
###########################################################

# Build a plot of weed levels on top of the May IR image
coordinates(data.Set4.2) <- c("Easting", "Northing")
proj4string(data.Set4.2) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
greys <- grey(0:255 / 255)
par(mai = c(1,1,1,1))
# Use this version with raster
image(data.4.2.May, col = greys,  # Fig. 5.7
  xlim = c(592100,592900), ylim = c(4267500,4267860),
  axes = TRUE)

# Use this version with rgdal
image(data.4.2.May, "band1", col = greys,  # Fig. 5.7
      xlim = c(592100,592900), ylim = c(4267500,4267860),
      axes = TRUE)

plot(sampbdry.sp, add = TRUE)
data.Set4.2$hiweeds <- 1
data.Set4.2$hiweeds[data.Set4.2$Weeds == 5] <- 2
plot(data.Set4.2, add = TRUE, pch = 1, cex = data.Set4.2$hiweeds)
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
strat.pol <- st_sfc(st_polygon(list(strat1)),
   st_polygon((list(strat2))), st_polygon(list(strat3)),
   st_polygon(list(strat4)))
stratbdry.sf <- st_sf(z = c(1,1,1,1), strat.pol)
st_crs(stratbdry.sf) <- "+proj=utm +zone=10 +ellps=WGS84"
stratbdry.sp <- as(stratbdry.sf, "Spatial")

# Use with raster
image(data.4.2.May, col = greys,  # Fig. 5.7
      xlim = c(592100,592900), ylim = c(4267500,4267860),
      axes = TRUE)

# Use with rgdal
image(data.4.2.May, "band1", col = greys,  # Fig. 5.7
  xlim = c(592100,592900), ylim = c(4267500,4267860),
  axes = TRUE)

plot(sampbdry.sp, add = TRUE)
plot(stratbdry.sp, add = TRUE)
title(main = "High Weed Strata", xlab = "Easting",
  ylab = "Northing", cex.main = 2, cex.lab = 1.5,
  font.lab = 2)

print(hi.area <- st_area(stratbdry.sf))
print(tot.area <- st_area(sampbdry.sf))
print(frac.hi <- as.numeric(sum(hi.area) / tot.area))

# over(pts, polygon) produces a data frame with NA 
# for points outside the polygon
proj4string(pop.data) <- proj4string(stratbdry.sp)
hiweeds <- over(pop.data, stratbdry.sp)
head(hiweeds)
length(which(!is.na(hiweeds$z)))
pop.data$hiweeds <- 0
pop.data$hiweeds[which(!is.na(hiweeds$z))] <- 1

sample.size <- 32
hi.size <- round(frac.hi * sample.size, 0)
lo.size <- sample.size - hi.size
samp.size <- c(lo.size,hi.size)
# Generate a plot of s sampling pattern
set.seed(123)
for (i in 0:1){
   subpop <- which(pop.data$hiweeds == i)
   {if (i == 0)
      samp <- sample(subpop,samp.size[i+1])
   else
      samp <- c(samp,sample(subpop,samp.size[i+1]))
}}
strat.samp <- pop.data[samp,]
plot(strat.samp, xlim = c(592100,592820), ylim = c(4267500,4267860),
  axes = TRUE, pch = 16)   # Fig. 5.8
plot(stratbdry.sp, add = TRUE)
plot(sampbdry.sp, add = TRUE)
title(main = "Stratification by Weed Level", xlab = "Easting",
  ylab = "Northing", cex.main = 2, cex.lab = 1.5,
  font.lab = 2)

# Monte Carlo simulation of stratified sampling
stratified.samp <- function(samp.size){
# Low weed stratum
  subpop <- pop.data$Yield[pop.data$hiweeds == 0]
  samp <- sample(subpop,samp.size[1])
# High weed stratum
  subpop <- pop.data$Yield[pop.data$hiweeds == 1]
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

















