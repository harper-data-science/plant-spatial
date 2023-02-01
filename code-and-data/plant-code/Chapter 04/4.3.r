# Join count statistics for high weeds
library(sf)
data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
# The file set4.1thiessen.shp was created using ArcGIS to make
# Fig. 4.2 pretty
# To just do the statistical test see the code in Section 3.6
thsn.sf <- st_read("auxiliary\\set4.1thiessen.shp")
HiWeeds <- unlist(lapply(data.Set4.1$Weeds,
   function(x)(as.numeric(x >= 4))))
thsn.sf["HiWeeds"] <- factor(HiWeeds, labels = c("High", "Low"))
library(terra)
thsn.ter <- vect(thsn.sf)
greys <- grey(c(50, 225) / 255)
plot(thsn.ter, y = 4, col = greys, main = "Field 4.1 Weed Levels") 

library(spdep)                                  
# spdep is needed for joincount.test
thsn.sp <- as(thsn.sf, "Spatial")
thsn.sp$ID <- 1:86
nlist <- poly2nb(thsn.sp,
  row.names = as.character(thsn.sp$ID), queen = FALSE)
W <- nb2listw(nlist, style = "B")
joincount.test(thsn.sp$HiWeeds, W)
set.seed(123)
joincount.mc(thsn.sp$HiWeeds,W,1000,
  alternative = "greater")
W <- nb2listw(nlist, style = "W")
joincount.test(thsn.sp$HiWeeds, W)
joincount.mc(thsn.sp$HiWeeds,W,1000,
  alternative = "greater")
nlist <- poly2nb(thsn.sp,
  row.names = as.character(thsn.sp$ID), queen = TRUE)
W <- nb2listw(nlist, style = "B")
joincount.test(thsn.sp$HiWeeds, W)
joincount.mc(thsn.sp$HiWeeds,W,1000,
  alternative = "greater")
W <- nb2listw(nlist, style = "W")
joincount.test(thsn.sp$HiWeeds, W)
joincount.mc(thsn.sp$HiWeeds,W,1000,
  alternative = "greater")

