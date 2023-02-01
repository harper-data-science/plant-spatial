## HEADER ####
## what: Plant Spatial ecology code
## what: Ch 02 modified
## who: Ed H
## when: last edited 2023-01-31

## CONTENTS ####
## Setup
## 2.2 R Basics
## 2.3 Programming concepts
## 2.4 data handling
## 2.5 Writing functions
## 2.8 Exercises

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Setup ####
## Unzip book data to folder /data in script working dir

library(gstat) 
library(sf)
library(terra)
library(stars)
library(starsExtra)
library(raster)
library(ggplot2)
library(lattice)
library(rasterVis)

setwd(r'(D:\Dropbox\git-harper-data-science\plant-spatial\code-and-data)')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2.2 R Basics ####

# Assign the value of 3 to w
w <- 3
w # Display the value of w
class(w) # Display the object class of w

# Find out about the function class()
?class

# See the source code
class

# Source code for sd()
sd

# Assign z a character value
z <- "a"
z
class(z)

z <- a

class(class)

# Example of the concatenate function
z <- c(1, 7, 6.2, 4.5, -27, 1.5e2, 7251360203, w, 2*w, w^2, -27/w)
z

z[6]
z[c(6,8)]

# Remove the 6th element of z
z[-6]
z[-(6:11)]

# This causes an error
v[1] <- 4
# This works
v <- numeric()
v[1] <- 4
v[2] <- 6.2
v

# Assign v to z and display the result
print(z <- v)
print(w <- 1:30)
print(z <- 30:1)

# Learn about the function seq()
?seq

print(a <- matrix(data = 1:9, nrow = 3))
a[1,]
2 * a[1,]

w <- c(1,3,6)
z <- 1:3
cbind(w, z)

library(maptools)
library(tools)
package_dependencies("maptools")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2.3 Programming concepts ####

# looping and branching
# initialize vectors
w <- numeric(30)
z <- numeric(30)
w
z

# classic 'for loop'
for(i in 1:30){
  w[i] <- i
  z[i] <- 30 - i + 1
}
w
z

# exploit variable address in for loop
w <- 1:10
for (i in 1:10){
  {if(w[i] > 5)
    w[i] <- 20
  else
    w[i] <- 0}
}
w           

# other index tricks
w <- 1:10
w > 5
w[w > 5]
w[w > 5] <- 20
w[w <= 5] <- 0
w

# Generate a sequence of 20 psuedorandom numbers
# explain pseudorandom!
set.seed(123) # Set random seed
print(w <- rnorm(20), digits = 2)

print(z <- rep(1:4, 5))

print(z <- sort(z))

# Apply the mean function to w indexed by z
tapply(w, z, mean)

set.seed(123)
tapply(rnorm(20), sort(rep(1:4, 5)), mean)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2.4 data handling ####

# Input and output are discussed in Section 2.4.2

# Surveys at 21 locations along the
# Sacramento River to identify the presence and estimate the number of individuals of a
# population of western yellow-billed cuckoos
data.Set1.obs <- read.csv("data/set1/obspts.csv", header = TRUE)
str(data.Set1.obs)

# data frame containing the point sample data from Field 1 of Data Set 4,
# one of the two fields in the precision agriculture experiment
data.Set4.1 <- read.csv("data/set4/set4.196sample.csv", header = TRUE)
class(data.Set4.1)
names(data.Set4.1)
data.Set4.1$EM38F425[20:30]
data.Set4.1[20:30,25]
data.Set4.1[25,25]
data.Set4.1$RowCol[1:5]

class(data.Set4.1$GrainProt)
class(data.Set4.1$RowCol)

print(w <- c("A", "A", "A", "B", "B", "B"))
class(w)
print(z <- as.factor(w))
class(z)

list.demo <- list(crop = "wheat", yield = "7900", fert.applied =
                    c("N", "P", "K"), fert.rate = c(150, 25, 15))
list.demo[[1]]
list.demo$crop
list.demo[[4]]
list.demo[[4]][1]

data.ex <- data.frame(Char = data.Set4.1$RowCol[1:3], Num = seq(1,5,2))
data.ex

### Spatial data ####

# Don't forget to run setwd() first!
library(gstat)
library(sf)

data.Set1.obs <- read.csv("data/set1/obspts.csv", header = TRUE)
class(data.Set1.obs)
str(data.Set1.obs)

# Read the data for Data Set 1 and create an sf point object
data.Set1.sf <- st_as_sf(data.Set1.obs, coords = c("Easting", "Northing"))

class(data.Set1.sf)
str(data.Set1.sf)

str(data.Set1.sf$geometry)

st_crs(data.Set1.sf) <- "EPSG:32610" # UTM Zone 10N
st_crs(data.Set1.sf)

# If you execute the step below directly you will get a warning message saying 
# crs already established.
# Instead, start over and skip the st_crs assignment step above
st_crs(data.Set1.sf) <- 32610
st_crs(data.Set1.sf)

### Field boundary ####

#Create a boundary for Field 4.2
N <- 4267873
S <- 4267483
E <- 592860
W <- 592080

N - S
E - W

print(coords.mat <- matrix(c(W,E,E,W,W,N,N,S,S,N), ncol = 2))
# Convert the coordinate matrix to a list object 
coords.lst <- list(coords.mat)

# Create the sf object by specifying the coordinates
coords.pol = st_sfc(st_polygon(coords.lst))

# Assign the value z = 1 to the cell of the polygon
Set42bdry.sf = st_sf(z = 1, coords.pol)
st_crs(Set42bdry.sf) <- 32601
plot(Set42bdry.sf)

st_write(Set42bdry.sf, "data/created/Set42bdry.shp")
data.Set1.landcover.sf <- st_read("data/Set1/landcover.shp")
str(data.Set1.landcover.sf)

plot(data.Set1.landcover.sf)
plot(st_geometry(data.Set1.landcover.sf))




### Raster objects ####

# Terra objects
# Read the three bands of a tiff image as a terra object
library(terra)
data.4.2.May <- rast("data/set4/Set4.20596.tif")
data.4.2.May

crs(data.4.2.May) <- "EPSG:32610" # UTM Zone 10N
data.4.2.May

plot(data.4.2.May)
# Only the first layger (IR)
plot(data.4.2.May, y = 1)
# Plot in grayscale
greys <- grey(0:255 / 255)
plot(data.4.2.May, y = 1, col = greys)

# Run this code to create an interpolated EC grid
data.Set4.2EC <- read.csv("data/Set4/Set4.2EC.csv", header = TRUE)
data.Set4.2EC$ID <- 1:nrow(data.Set4.2EC)
# Establish the coordinates of the rectangular boundary
N <- 4267868
S <- 4267513
E <- 592867
W <- 592082
cell.size <- 10
# Create the interpolation grid
library(stars)
library(starsExtra)

# Based on https://rdrr.io/cran/starsExtra/man/make_grid.html
SW <- st_point(c(W, S))
NE <- st_point(c(E, N))
xy.sfc <- st_sfc(SW, NE)
class(xy.sfc)
grid.xy <- make_grid(xy.sfc, res = cell.size, buffer = 0)
st_crs(grid.xy) <- "EPSG:32610" # UTM Zone 10N

# Select every (cell.size)th data value
# this is advanced stuff
data.vgm <- data.Set4.2EC[-which(data.Set4.2EC$ID %% cell.size != 0),]
data.vgm.sf <- st_as_sf(data.vgm, 
                        coords = c("Easting", "Northing"), crs = "EPSG:32610")
EC.vgm <- variogram(ECto30 ~ 1, data.vgm.sf)
EC.fit <- fit.variogram(EC.vgm, model = vgm(100000, "Sph", 700,10000))
plot(EC.vgm, EC.fit, col = "black") # Fig.1.2a

EC.krig <- krige(ECto30 ~ 1, data.vgm.sf, grid.xy, model = EC.fit)

library(ggplot2)
ggplot() + 
  geom_stars(data = EC.krig, aes(fill = var1.pred,
                                 x = x, y = y)) +
  scale_fill_gradient(low = "white", high = "black") + 
  geom_sf(data = xy.sfc, cex = 0.5)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2.5 Writing functions ####

z <- c(10,2,3,21,5,72,18,25,34,33)
print(ztrim <- sort(z))

trim <- 0.1
print(tl <- as.integer(trim * length(ztrim)))
# Trim from the low end
print(ztrim <- ztrim[-(1:tl)])
# Trim from the high end
th <- length(ztrim) - tl
print(ztrim <- ztrim[-((th + 1):length(ztrim))])

print(zsd <- sd(ztrim))


# Function to compute the trimmed standard deviation
sd.trim <- function(z, trim){
  # trim must be between 0 and 0.5 or this won't work
  # Sort the z values
  ztrim <- sort(z)
  # Calculate the number to remove
  tl <- as.integer(trim * length(ztrim))
  # Remove the lowest values
  ztrim <- ztrim[-(1:tl)]
  # Remove the highest values
  th <- length(ztrim) - tl
  ztrim <- ztrim[-((th + 1):length(ztrim))]
  # Compute the standard deviation
  zsd <- sd(ztrim)
  return(zsd)
}

sd.trim(z, 0.1)


a <- matrix(c(1,2,3,4), nrow = 2)
a
diag(a)

v <- c(1, 2)
v
diag(v)

to.the.n <- function(w) w^n
n <- 3
to.the.n(2)

better.to.the.n <- function(w, n) w^n
n <- 3
better.to.the.n(2,4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2.8 Exercises ####
# 2.1
# Venables and Ripley (2002) make the very cogent point that one of the first things
# to learn about a programming environment is how to stop it from running away.
# First type i <- 1:20 to create an index. Then type the statement while (i < 5) i <- 2. 
# What happens? Why does this happen? To stop the execution,
# either hit the <Esc> key, or, if you are running RStudio, click on the little Stop
# sign that appears in the upper right corner of the Console. This is how you “bail
# out” of any R execution in Windows.
i <- 1
while (i < 5) {
  i <- 2
}
  
# 2.2a

# Like all programming languages, mathematical operators in R have an order of
# precedence, which can be overridden by appropriate use of parentheses.

# Type the following expressions and observe the result:
3 * 4 ^ 2 + 2
-1:2
-(1:2)

#2.2b
#Type 1:3 – 1 and observe the result.
1:3 - 1

#2.2c
# Type the statement w <- 1:10 to create a vector w. Type the statement w > 5.
# The R symbol for equality used in a test is the double equal sign ==. Type w
# == 5. The R symbol for inequality used in a test is !=. Type w != 5.
w <- 1:10
w > 5
w == 5
w != 5

# 2.3a
# Using a single R statement, create a matrix A with three rows and four
# columns whose elements are the row number (i.e., every element of row 1 is 1,
# every element of row 2 is 2, and every element of row 3 is 3) (hint: make sure
# you read the Help file ?matrix). Display the matrix, then display the second
# row of the matrix, then display every row but the first.
A <- matrix(c(1,1,1,1,2,2,2,2,3,3,3,3), nrow = 3, byrow = TRUE)
A
A[2,]
A[-2,]

#2.3b
# In a single statement using the function cbind(), create a matrix B whose
# first column is equal to the first column of A and whose second column is five
# times the first column of A.
B <- cbind(A[,1],5*A[,1])

# 2.3c
# Create a data frame C from the matrix B and then give the fields of C the
# names “C1” and “C2.”
C <- data.frame(B)
names(C) <- c("C1","C2")

#2.3d
# Create a list L whose first element L1 is the vector w, whose second element
# L2 is the matrix B, and whose third element L3 is the data frame C. Display
# L$L3$C2. Now display the same object using the [[]] and []operators (note:
# in R nomenclature, these operators are denoted [[ and [. To look them up in
# Help, type ?"[" and ?"[[".
L <- list(L1 = w, L2 = B, L3 = C)
L$L3$C2
L[[3]][,2]

#2.4a
# Use the function c() to create a character vector w whose elements are the
# number 6, 2.4, and 3. Then create a vector z whose elements are the characters
# “a,” “b,” and “c.” Note that in R characters are always enclosed in quotes.
w <- c(6, 2.4, 3)
w
z <- c("a", "b", "c")
z

#2.4b
# Use the function c() to create a vector z that is the concatenation of the vectors
# w and z created in part (a). Evaluate the vector z. What has happened to
# the elements of the vector w?
z <- c(w, z)
z
class(z)

#2.4c
# Apply the function as.numeric() to the vector z to recover the numerical
# values of the vector w of part (a). What happens to the components of the vector
# z?
w <- as.numeric(z)
w

# 2.5
# In Section 2.3.1, we created a vector w whose elements were 1:10, and then we
# used two code sequences to convert the first five of these elements to 0 and the
# second five to 20. Repeat this code exactly, but instead of converting the second
# five elements to 20, attempt to convert them to 3. Do the two code sequences give
# the same result? If not, create a second code sequence that gives the same results
# as the first for and values of the two replacement quantities 20 and 3.
# These do not give the same results
w <- 1:10
for (i in 1:10){
  {if (w[i] > 5)
    w[i] <- 3
  else
    w[i] <- 0}
}
w           
w <- 1:10
w > 5
w[w > 5]
w[w > 5] <- 3
w[w <= 5] <- 0
w

# This does give the same result
w <- 1:10
i <- w[w > 5]
j <- w[w <= 5]
w[i] <- 3
w[j] <- 0
w

#2.6
# In Section 2.3.2, we used the functions rep() and sort() to create a sequence
# of five ones, five twos, five threes, and five fours. In this exercise we will use
# another approach. First, use the constructor function numeric() to create a
# vector z of 20 zeroes.
#a)
z <- numeric(20)
i <- 0:19

#b
# Use the modulo operator %% to assign a value of 1 to every fifth component of
# the vector z. The modulo operation u %% v evaluates to u mod v, which, for
# integer values of u and v is the remainder of the division of u by v. Thus, for
# example 6 mod 5 = 1. The only numbers i such that i mod 5 = 0 are 0, 5, 10, etc.,
# and therefore the components of the vector z with these indices are assigned
# the value 1. To read about an operator using special characters, you must
# place it in quotes, so you type ?"%%".
z[i %% 5 == 0] <- 1

#c)
# Next, use the R function cumsum() to compute the index vector
z <- cumsum(z)
z

#2.7a
# R has some built-in numbers, including pi. Read about the function print(),
# and then evaluate pi to 20 significant digits.
print(pi, digits = 20)

#2.7b
w <- seq(0, 6, 0.25)
z1 <- sin(0.5 * pi * w)
#2.7c
sin.npi <- function(w) sin(n * pi * w)
n <- 0.5
z2 <- sin.npi(w)
z <- cbind(z1,z2)
z
#2.7d
all.equal(z1, z2)
z1 <- exp(-z2)
#2.7e
exp.m.sin <- function(w) exp(-sin.npi(w))
z2 <- exp.m.sin(w)
z <- cbind(z1,z2)
z

#2.8a
data(UKgas)
#2.8b
UKg <- matrix(UKgas, ncol = 4, byrow = TRUE)
#2.8c
UKgm <- apply(UKg, 1, mean)
mean(UKg[1,])
#2.8d
UKg.df <- data.frame(UKg)
names(UKg.df) <- c( "Winter", "Spring", "Summer", "Fall")
UKg.df$Mean <- UKgm
UKg.df$Year <- 1960:1986
#2.8e
apply(UKg, 2, mean)

#2.9a
plot(UKg.df$Year, UKg.df$Mean)
# 2.9b
plot(UKg.df$Year, UKg.df$Mean, type = "l",
     main = "Mean UK Gas Consumption", xlab = "Year", ylab = "Consumption")
#2.9c
points(UKg.df$Year,UKg.df$Winter)
max(max(UKg.df$Spring), max(UKg.df$Summer), max(UKg.df$Fall),
    max(UKg.df$Winter))
#2.9d
plot(UKg.df$Year, UKg.df$Mean, type = "l", ylim = c(0,1200),
     main = "Mean UK Gas Consumption", xlab = "Year", ylab = "Consumption")
points(UKg.df$Year, UKg.df$Winter, pch = 1)
points(UKg.df$Year, UKg.df$Spring, pch = 2)
points(UKg.df$Year, UKg.df$Summer, pch = 3)
points(UKg.df$Year, UKg.df$Fall, pch = 4)
points(1965,1000, pch = 1)
#2.9e
text(1965, 1000, "Winter", pos = 4)
points(1965,925, pch = 2)
text(1965, 925, "Spring", pos = 4)
points(1965,850, pch = 3)
text(1965, 850, "Summer", pos = 4)
points(1965,775, pch = 4)
text(1965, 775, "Fall", pos = 4)
# 2.9f
plot(UKg.df$Year, UKg.df$Mean, type = "l", ylim = c(0,1200),
     main = "Mean UK Gas Consumption", xlab = "Year", ylab = "Consumption")
points(UKg.df$Year, UKg.df$Winter, pch = 1)
points(UKg.df$Year, UKg.df$Spring, pch = 2)
points(UKg.df$Year, UKg.df$Summer, pch = 3)
points(UKg.df$Year, UKg.df$Fall, pch = 4)
legend(1965, 1000, c("Winter", "Spring", "Summer",
                     "Fall"), pt.cex = 1, pch = 1:4, y.intersp = 1,
       title = "Season")

#2.10
library(raster)
getAnywhere("plot")
raster::plot
graphics::plot
library(sf)
?plot
?plot.sf
?plot.raster
#2.11

#Set 4-1, 1996 and 1997
N <- 4271132
W <- 592025
NE <- 592470
SE <- 592404
S <- 4270352
coords.mat <- matrix(c(W,NE,SE,W,W,N,N,S,S,N),
                     ncol = 2)
library(sf)
coords.lst <- list(coords.mat)
# Create the sf object by specifying the coordinates
coords.pol = st_sfc(st_polygon(coords.lst))
# Assign the value z = 1 to the cell of the polygon
Set41bdry = st_sf(z = 1, coords.pol)
st_crs(Set41bdry) <- 32601
plot(Set41bdry)
# Don't forget to rund setwd() first!
st_write(Set41bdry, "created\\Set419697bdry.shp")



