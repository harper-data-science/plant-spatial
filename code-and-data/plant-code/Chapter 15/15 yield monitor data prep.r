library(gstat)
library(spdep)
library(sf)
library(maptools)


# Yield monitor data prep for 1997-1999 Execute the appropriate line
data.Set4 <- read.csv("Set4\\Set4.197tomatoyield.csv", header = TRUE)
data.Set4 <- read.csv("Set4\\Set4.198beanyield.csv", header = TRUE)
data.Set4 <- read.csv("Set4\\Set4.199sunfloweryield.csv", header = TRUE)
data.Set4 <- read.csv("Set4\\Set4.297tomatoyield.csv", header = TRUE)
data.Set4 <- read.csv("Set4\\Set4.298sunfloweryield.csv", header = TRUE)
data.Set4 <- read.csv("Set4\\Set4.299cornyield.csv", header = TRUE)

# For Set4.299cornyield.csv only
data.Set4 <- data.Set4[-which(data.Set4$Yield < 1000),]

coordinates(data.Set4) <- c("Easting","Northing")
data.Set4$ID <- 1:nrow(data.Set4)

# Execute the appropriate lines
#Set 4.1
x.cells <- 3
y.cells <- 7
#Set 4.2
x.cells <- 7
y.cells <- 3

# Clean the data according to the procedure of Section 6.2.3
x.max <- max(coordinates(data.Set4)[,1]) + 1
x.min <- min(coordinates(data.Set4)[,1])
y.max <- max(coordinates(data.Set4)[,2]) + 1
y.min <- min(coordinates(data.Set4)[,2])
x.index <- trunc(x.cells*(coordinates(data.Set4)[,1] -
   x.min) / (x.max - x.min)) + 1
y.index <- trunc(y.cells*(coordinates(data.Set4)[,2] -
   y.min) / (y.max - y.min)) + 1
data.Set4$xyindex <- x.index + 10 * y.index
print(xyindex <- unique(data.Set4$xyindex))

started <- 0
for (i in 1:length(xyindex)){
# Separate out points in sub-region i
   data.Set4.i <-
      data.Set4[which(data.Set4$xyindex == xyindex[i]),]
   if (nrow(data.Set4.i) > 0){
# Compute the neighbor list and listw objects
      nlist.wd <- knn2nb(knearneigh(data.Set4.i, 5))
      W <- nb2listw(nlist.wd, style = "W")
# Compute the Moran scatterplot; don't print the results
      mp <- moran.plot(data.Set4.i$Yield, W, quiet = TRUE)
# Create a matrix with 6 columns,
# one for each outlier identification statistic
# Convert the values TRUE and FALSE to numbers and keep those that
# are all zero (outlier = FALSE)
      keep <- which(rowSums(matrix(as.numeric(mp$is.inf), ncol = 6)) == 0)
# Concatentate these values to keep into one array
      {if(started == 0)
         {
         keep.id <- data.Set4.i$ID[keep]
         started <- 1
         }
      else
        keep.id <- c(keep.id,data.Set4.i$ID[keep])}
   }
}
data.Set4.keep1 <- data.Set4[keep.id,]
data.Set4.keep <- data.Set4.keep1[order(data.Set4.keep1$ID),]

nrow(data.Set4)
nrow(data.Set4.keep)

# Execute the appropriate line
write.csv(data.Set4.keep,"created\\set4.1yld97cleaned.csv")
write.csv(data.Set4.keep,"created\\set4.1yld98cleaned.csv")
write.csv(data.Set4.keep,"created\\set4.1yld99cleaned.csv")
write.csv(data.Set4.keep,"created\\set4.2yld97cleaned.csv")
write.csv(data.Set4.keep,"created\\set4.2yld98cleaned.csv")
write.csv(data.Set4.keep,"created\\set4.2yld99cleaned.csv")

# Interpolate to sample points

# Pick the appropriate line
data.pts <- read.csv("set4\\set4.196sample.csv",header = TRUE)
data.pts <- read.csv("set4\\set4.296sample.csv",header = TRUE)

coordinates(data.pts) <- c("Easting","Northing")
grid.xy <- data.frame(coordinates(data.pts))
coordinates(grid.xy) <- c("Easting", "Northing")

yield.idw <- idw(Yield ~ 1, data.Set4.keep, grid.xy, idp = 2, nmax = 12)
yield.idw.df <- data.frame(Yield = yield.idw$var1.pred,
   Easting = coordinates(yield.idw)[,1],
   Northing = coordinates(yield.idw)[,2])

# Choose the appropriate line 
write.csv(yield.idw.df, "created\\set4.1yld97ptsidw.csv")
write.csv(yield.idw.df, "created\\set4.1yld98ptsidw.csv")
write.csv(yield.idw.df, "created\\set4.1yld99ptsidw.csv")
write.csv(yield.idw.df, "created\\set4.2yld97ptsidw.csv")
write.csv(yield.idw.df, "created\\set4.2yld98ptsidw.csv")
write.csv(yield.idw.df, "created\\set4.2yld99ptsidw.csv")

#Field 4.1, 1998 and 1999 boundary

N <- 4271132
W <- 592025
NE <- 592470
SE <- 592404
S <- 4270452

coords.mat <- matrix(c(W,NE,SE,W,W,N,N,S,S,N), ncol = 2)
# Convert the coordinate matrix to a list object 
coords.lst <- list(coords.mat)
# Create the sf object by specifying the coordinates
coords.pol = st_sfc(st_polygon(coords.lst))
# Assign the value z = 1 to the cell of the polygon
Set4.1.bdry = st_sf(z = 1, coords.pol)
st_crs(Set4.1.bdry) <- 32601
plot(Set4.1.bdry)
st_write(Set4.1.bdry, "created\\Set419899bdry.shp")



