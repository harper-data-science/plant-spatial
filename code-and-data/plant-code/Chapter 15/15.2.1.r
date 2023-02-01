# Before executing this code you must create the files using the code in 
# the file 15 yield monitor data prep.r.

library(sf)
library(stars)
library(gstat)

#Creation of Fig. 15.1
# Execute these statements one at a time
yield.data <- read.csv("created\\set4.1yld96cleaned.csv", header = TRUE)
yield.data <- read.csv("Set4\\Set4.197tomatoyield.csv", header = TRUE)
yield.data <- read.csv("Set4\\Set4.198beanyield.csv", header = TRUE)
yield.data <- read.csv("Set4\\Set4.199sunfloweryield.csv", header = TRUE)

yield.data$ID <- 1:nrow(yield.data)
plot.data <- yield.data[which(yield.data$ID %% 10 == 0),]
plot.data.sf <- st_as_sf(plot.data, coords = c("Easting", "Northing"))
st_crs(plot.data.sf) <- "EPSG:32610" # UTM Zone 10N

# Execute the appropriate statement
# For 1996 and 1997
bdry.sf <- st_read("created\\Set419697bdry.shp")
# For 1998 and 1999
bdry.sf <- st_read("created\\Set419899bdry.shp")

#bdry.sp <- as(bdry.sf, "Spatial")


# Sample points
data.Set4.1 <- read.csv("Set4\\Set4.196sample.csv", header = TRUE)
data.Set4.1.sf <- st_as_sf(data.Set4.1, coords = c("Easting", "Northing")) 
st_crs(data.Set4.1.sf) <- "EPSG:32610" # UTM Zone 10N

# Pick the appropriate year
year = "1996" # Fig. 15.1a
year = "1997" # Fig. 15.1b
year = "1998" # Fig. 15.1c
year = "1999" # Fig. 15.1d

par(mai = c(1,1,1,1))
plot(st_geometry(plot.data.sf), xlim = c(592000, 592420), ylim = c(4270350,4271150),
     axes = TRUE, pch = 1, cex = 0.6)
title(xlab = "Easting", ylab = "Northing",
      main = year, cex.lab = 1.5, cex.main = 2)
plot(st_geometry(bdry.sf), add = TRUE)
plot(st_geometry(data.Set4.1.sf), add = TRUE, pch = 16)   # Fig. 15.1

# Read all the Field 4.1 data to create the spacetime object

data.Set4.1.96 <- read.csv("created\\set4.1yld96ptsidw.csv")
data.Set4.1.97 <- read.csv("created\\set4.1yld97ptsidw.csv")
data.Set4.1.98 <- read.csv("created\\set4.1yld98ptsidw.csv")
data.Set4.1.99 <- read.csv("created\\set4.1yld99ptsidw.csv")

# Eliminate the last 12 rows of each data set
Y96 <- data.Set4.1.96[(1:74),]
Y97 <- data.Set4.1.97[(1:74),]
Y98 <- data.Set4.1.98[(1:74),]
Y99 <- data.Set4.1.99[(1:74),]

# Create the attribute data
Y96.norm <- 100 * Y96[,2] / max(Y96[,2])
Y97.norm <- 100 * Y97[,2] / max(Y97[,2])
Y98.norm <- 100 * Y98[,2] / max(Y98[,2])
Y99.norm <- 100 * Y99[,2] / max(Y99[,2])
Yield.mat <- cbind(Y96.norm, Y97.norm, Y98.norm, Y99.norm)
Yield.norm <- data.frame(Y96 = Y96.norm, Y97 = Y97.norm,
   Y98 = Y98.norm, Y99 = Y99.norm) 

# Create location data
Yield.sites <- data.frame(Easting = Y96$Easting, Northing = Y96$Northing)
Yield.sites.sf <- st_as_sf(Yield.sites, coords = c("Easting", "Northing"))
st_crs(Yield.sites.sf) <- "EPSG:32610" # UTM Zone 10N

# Create the time objects
Yield.year <- c(1996, 1997, 1998, 1999)
Yield.month <- c(5, 9, 9, 9)
Yield.day <- rep(15, length(Yield.year))
print(Yield.time <- ISOdate(year = Yield.year, month =
   Yield.month, day = Yield.day))

thsn.sf <- st_read("auxiliary\\set4.1thiessen.shp")
st_crs(thsn.sf) <-"EPSG:32610" # UTM Zone 10N
thsn9899.sf <- thsn.sf[1:74,]
thsn.dim <- st_dimensions(poly = st_as_sfc(thsn9899.sf), time = Yield.time)
Yield.st <- st_as_stars(list(Yield.mat), dimensions = thsn.dim)
greys <- grey(seq(5, 19) / 22)  # Didn't work
plot(Yield.st, col = greys, main = "Normalized Yield")

# Color version
plot(Yield.st, main = "Normalized Yield")

Y.norm <- data.frame(Y96 = Y96.norm, Y97 = Y97.norm,
                      Y98 = Y98.norm, Y99 = Y99.norm)

#Plot the empirical variograms in perspective
Y.var <- function(Y.norm, sites){
  Y <- data.frame(Y.norm = Y.norm)
  Y$Easting <- sites$Easting
  Y$Northing <- sites$Northing
  coordinates(Y) <- c("Easting", "Northing")
  proj4string(Y) <- CRS("+proj=utm +zone=10 +ellps=WGS84")
  Y.var <- variogram(Y.norm ~ 1, Y, cutoff = 400)
  return(Y.var)
}

Y.norm.var <- Y.var(Y.norm[,1], Y96)
M.var <- matrix(0, nrow = 4, ncol = length(Y.norm.var$gamma))
M2.var <- matrix(0, nrow = 4, ncol = length(Y.norm.var$gamma))
M.var[1,] <- Y.norm.var$gamma
M2.var[1,] <- Y.norm.var$gamma[13:1]
var.dist <- Y.norm.var$dist
for (i in 2:4){
  Y.norm.var <- Y.var(Y.norm[,i], Y96)
  M.var[i,] <- Y.norm.var$gamma
  M2.var[i,] <- Y.norm.var$gamma[13:1]
}

Year <- c(1996, 1997, 1998, 1999)

persp(Year, var.dist, M.var, theta = 225, phi = 15, xlab = "Year",
      ylab = "dist", zlab = "gamma", ticktype = "detailed",
      nticks = 4) # Fig. 15.3a

persp(Year, var.dist, M2.var, theta = 225, phi = 15, xlab = "Year",
      ylab = "cut dist", zlab = "gamma", ticktype = "detailed",
      nticks = 4) # Fig. 15.3b



