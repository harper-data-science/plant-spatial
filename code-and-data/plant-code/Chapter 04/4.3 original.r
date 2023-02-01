# Join count statistics for high weeds
library(spdep)
library(sf)
data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
# The file set4.1thiessen.shp was created using ArcGIS
# to make Fig. 4.2 pretty
# To just do the statistical test see the code in Section 3.6
thsn.sf <- st_read("auxiliary\\set4.1thiessen.shp")
HiWeeds <- unlist(lapply(data.Set4.1$Weeds,
   function(x)(as.numeric(x >= 4))))
thsn.sf["HiWeeds"] <- factor(HiWeeds, labels = c("High", "Low"))
# Make a quick plot of Fig. 4.2 using the sf plotting function
plot(thsn.sf["HiWeeds"])
# Now make a pretty plot of Fig. 4.2 using the "classic" plotting function
thsn.sp <- as(thsn.sf, "Spatial")
thsn.sp@data$HiWeeds <- factor(HiWeeds, labels = c("High", "Low"))
greys <- grey(c(50, 225) / 255)
spplot(thsn.sp, "HiWeeds", col.regions = greys,
       scales = list(draw = TRUE), xlab = "Easting", ylab = "Northing",
       main = "Field 4.1 Weed Levels")

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

