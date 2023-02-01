# Input code for Data Set 1
# These statements assume that a working directory has been set with setwd()
# as described in Section 2.4.2

# Shapefile with explanatory variables
library(maptools)
library(sf)
data.Set1.sf <- st_read("set1\\set1data.shp")
data.Set1.sp <- as(data.Set1.sf, "Spatial")
proj4string(data.Set1.sp) <- CRS("+proj=utm +zone=10 +ellps=WGS84")

# Cuckoo obsertation sites
data.Set1.obs <- read.csv("set1\\obspts.csv", header = TRUE)

# Shapefile of habitat patches
library(maptools)
library(sf)
data.Set1.patches.sf <- st_read("set1\\habitatpatches.shp")
data.Set1.patches.sp <- as(data.Set1.patches.sf, "Spatial")
proj4string(data.Set1.patches) <-
  CRS("+proj=utm +zone=10 +ellps=WGS84")
  
# Shapefile of landcover data
library(maptools)
data.Set1.cover.sf <- st_read("set1\\landcover.shp")
data.Set1.cover.sp <- as(data.Set1.cover.sf, "Spatial")
proj4string(data.Set1.cover) <-
  CRS("+proj=utm +zone=10 +ellps=WGS84")
