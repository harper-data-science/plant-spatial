# These statements assume that a working directory has been set with setwd()
# as described in Section 2.4.2

# This is code to read raw yield monitor data for either Field 1 or
# Field 2 in order to produce a cleaned data set (Section 6.2.3)
# Here choose either 1 or 2
Field.no <- 1
Field.no <- 2
# Then enter the appropriate crop and year name
# Here it is for wheat in 1996
filename <- paste("Set4\\Set4.", as.character(Field.no),
   "96wheatyield.csv", sep = "")
data.Set4 <- read.csv(filename, header = TRUE)

#----------------------------------------------------------------------

#Input for Field 1
# Sample data
data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
# When it is desired to convert the data frame to a SpatialPointsDataFrame,
# use this statement
coordinates(data.Set4.1) <- c("Easting", "Northing")

# Raw yield monitor data, 1996
data.Set4.1Yield96raw <-
   read.csv("Set4\\Set4.196wheatyield.csv", header = TRUE)

# Cleaned yield monitor data, 1996
data.Yield4.1 <- read.csv("created\\set4.1yld96cleaned.csv", header = TRUE)

# Yield interpolated to 86 sample points
data.Yield4.1idw <-
   read.csv("created\\set4.1yld96ptsidw.csv", header = TRUE)
   
# To add a Yield data field to the sample data use this statement
data.Set4.1$Yield <- data.Yield4.1idw$Yield

# Read tiff image data
library(rgdal)
data.4.1.Dec <- readGDAL("set4\\set4.11295.tif")
data.4.1.Mar <- readGDAL("set4\\Set4.10396.tif")
data.4.1.May <- readGDAL("set4\\Set4.10596.tif")



#---------------------------------------------------------------

# Input for Field 2
# Sample data
data.Set4.2 <- read.csv("Set4\\set4.296sample.csv", header = TRUE)
# When it is desired to convert the data frame to a SpatialPointsDataFrame,
# use this statement
coordinates(data.Set4.2) <- c("Easting", "Northing")

# Automatically measured soil EC data
data.Set4.2EC <- read.csv("Set4\\Set4.2EC.csv", header = TRUE)

# Raw yield monitor data, 1996
data.Set4.2Yield96raw <-
   read.csv("Set4\\Set4.296wheatyield.csv", header = TRUE)

# Cleaned yield monitor data, 1996
data.Yield4.2 <- read.csv("created\\set4.2yld96cleaned.csv", header = TRUE)

# Read tiff image data
data.4.2.May <- readGDAL("set4\\Set4.20596.tif")
