library(spdep)
library(maptools)

# Here choose either 1 or 2
Field.no <- 1
Field.no <- 2
filename <- paste("Set4\\Set4.", as.character(Field.no),
   "96wheatyield.csv", sep = "")
data.Set4 <- read.csv(filename, header = TRUE)
# This is only for Field 4.2
par(mai = c(1,1,1,1))
plot(12001:15000, data.Set4$Yield[12001:15000],
   xlab = "Record Number", ylab = "Yield (kg/ha)",
   main = "Time Sequence of Field 4.2 Yield", cex.main = 1.5,
   cex.lab = 1.5, font.lab = 2) # Fig. 6.1a
   
coordinates(data.Set4) <- c("Easting","Northing")
data.Set4$ID <- 1:nrow(data.Set4)

# For Set 4.1, x.cells = 3 and y.cells = 7
# For Set 4.2, x.cells = 7 and y.cells = 3
add.factor <- 4 * Field.no - 6
x.cells <- 5 + add.factor
y.cells <- 5 - add.factor

x.max <- max(coordinates(data.Set4)[,1]) + 1
x.min <- min(coordinates(data.Set4)[,1])
y.max <- max(coordinates(data.Set4)[,2]) + 1
y.min <- min(coordinates(data.Set4)[,2])
# Geneerate the ID number for each sub-region
x.index <- trunc(x.cells*(coordinates(data.Set4)[,1] -
   x.min) / (x.max - x.min)) + 1
y.index <- trunc(y.cells*(coordinates(data.Set4)[,2] -
   y.min) / (y.max - y.min)) + 1
slot(data.Set4, "data")$xyindex <- x.index + 10 * y.index
print(xyindex <- unique(data.Set4$xyindex))
for (i in 1:length(xyindex)){
# Separate out points in sub-region i
   data.Set4.i <-
      data.Set4[which(slot(data.Set4, "data")$xyindex == xyindex[i]),]
# Compute the neighbor list and listw objects
   nlist <- dnearneigh(data.Set4.i, d1 = 0, d2 = 20)
   W <- nb2listw(nlist, style = "W")
   # Compute the Moran scatterplot; don't print the results
   mp <- moran.plot(data.Set4.i$Yield, W, quiet = TRUE)
# Create a matrix with 6 columns,
# one for each outlier identification statistic
# Convert the values TRUE and FALSE to numbers and keep those that
# are all zero (outlier = FALSE)
   keep <- which(rowSums(matrix(as.numeric(mp$is.inf),
      ncol = 6)) == 0)
# Concatentate these values to keep into one array
   {if(i == 1)
        keep.id <- data.Set4.i$ID[keep]
   else
        keep.id <- c(keep.id,data.Set4.i$ID[keep])}
}
data.Set4.keep1 <- data.Set4[keep.id,]
data.Set4.keep <- data.Set4.keep1[order(data.Set4.keep1$ID),]

data.Set4.plot <- data.Set4.keep[which((data.Set4.keep$ID > 12000)
   & (data.Set4.keep$ID <= 15000)),]
nrow(data.Set4)
nrow(data.Set4.keep)
plot(data.Set4.plot$ID, data.Set4.plot$Yield, # Fig. 6.2b
   xlab = "Record Number", ylab = "Yield (kg/ha)",
   main = "Cleaned Time Sequence of Field 4.2 Yield", cex.main = 1.5,
   cex.lab = 1.5)

filename <- paste("created\\set4.", as.character(Field.no),
   "yld96cleaned.csv", sep = "")
write.csv(data.Set4.keep,filename)

