# Input code for Data Set 2
# This statement assumes that a working directory has been set with setwd()
# as described in Section 2.4.2

data.Set2 <- read.csv("set2\\set2data.csv", header = TRUE)

# These files are created in the code for Section 7.3
library(maptools)
library(sf)
data.Set2S.sf <- st_read("created\\set2sierra.shp")
data.Set2S.sp <- as(data.Set2S.sf, "Spatial")
data.Set2C.sf <- st_read("created\\set2coast.shp")
data.Set2C.sp <- as(data.Set2C.sf, "Spatial")