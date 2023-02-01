library(lattice)
library(ggplot2)
library(terra)


data.Set3 <- read.table("Set3\\Set3data.csv", header = TRUE, sep = ",")
names(data.Set3)
data.Set3$SeasonFac <- factor(data.Set3$Season,
   labels = c("2002-03", "2003-04", "2004-05"))
ggplot(data = data.Set3) +
  geom_point(mapping = aes(x = ID, y = Northing, color = SeasonFac))

ggplot(data = data.Set3) +
  geom_boxplot(mapping = aes(x = Farmer, y = Yield, color = SeasonFac))

par(mai = c(1,1,1,1))
boxplot(Yield ~ Farmer, data = data.Set3,
  main = "Rice Yields by Farmer", xlab = "Farmer", cex.main = 2,
  ylab = "Yield (kg/ha)", cex.lab = 1.5) # Fig. 7.15
  
data.Set3$Location <- "Center"
data.Set3$Location[(data.Set3$Northing > 6340000)] <- "North"
data.Set3$Location[(data.Set3$Northing < 6280000)] <- "South"
trellis.par.set(par.main.text = list(cex = 2))
trellis.device(color = FALSE)
bwplot(Yield ~ Location | SeasonFac, data = data.Set3,
  main = "Rice Yields by Season",  # Fig. 7.16
  xlab = "Location", ylab = "Yield (kg/ha)", layout = c(3,1),
  aspect = 1)

data.Set3$LocN <- 2
data.Set3$LocN[(data.Set3$Northing > 6340000)] <- 1
data.Set3$LocN[(data.Set3$Northing < 6280000)] <- 3
data.Set3$LocSeason <- with(data.Set3, Season + 10 * LocN)
print(round(mean.yields <- tapply(data.Set3$Yield,
   data.Set3$LocSeason, mean)))
   
unique(data.Set3$RiceYear) 
sort(unique(data.Set3$Field[which(data.Set3$RiceYear == 1)]))
sort(unique(data.Set3$Field[which(data.Set3$RiceYear == 2)]))
t.test(data.Set3$Yield[which((data.Set3$Field == 3)
    & (data.Set3$RiceYear == 1))],
    data.Set3$Yield[which((data.Set3$Field == 3)
    & (data.Set3$RiceYear == 2))])
t.test(data.Set3$Yield[which((data.Set3$Field == 5) & (data.Set3$RiceYear == 1))],
    data.Set3$Yield[which((data.Set3$Field == 5) & (data.Set3$RiceYear == 2))])
t.test(data.Set3$Yield[which((data.Set3$Field == 12) & (data.Set3$RiceYear == 1))],
    data.Set3$Yield[which((data.Set3$Field == 12) & (data.Set3$RiceYear == 2))])
t.test(data.Set3$Yield[which((data.Set3$Field == 13) & (data.Set3$RiceYear == 1))],
    data.Set3$Yield[which((data.Set3$Field == 13) & (data.Set3$RiceYear == 2))])
t.test(data.Set3$Yield[which((data.Set3$Field == 14) & (data.Set3$RiceYear == 1))],
    data.Set3$Yield[which((data.Set3$Field == 14) & (data.Set3$RiceYear == 2))])
    
sort(unique(data.Set3$Field[which(data.Set3$Season == 1)]))
sort(unique(data.Set3$Field[which(data.Set3$Season == 2)]))
sort(unique(data.Set3$Field[which(data.Set3$Season == 3)]))

data.Set3$YearFarmerField <- with(data.Set3, paste(as.character(Season),Farmer,
   as.character(Field)))
print(tapply(data.Set3$Yield, data.Set3$YearFarmerField, mean),
   digits = 4)
   
# This file was downloaded from the CGIAR site
# http://srtm.csi.cgiar.org
dem.ter <- rast("auxiliary\\dem.asc")
crs(dem.ter) <- "EPSG:4326"
range(data.Set3$Latitude)
range(data.Set3$Longitude)
crop.extent <- c(-54.6,-53.7,-33.8,-32.7)
dem.Set3 <- crop(dem.ter, ext(crop.extent))
slope.Set3 <- terrain(dem.Set3, "slope")
Set3.WGS <- data.Set3
Set3.WGS.ter <- vect(data.Set3, geom = c("Longitude", "Latitude"),
   crs = "EPSG:4326")
slopes <- extract(slope.Set3, Set3.WGS.ter)
print(range(slopes), digits = 3)
sort(unique(data.Set3$Field[which(slopes > 0.03)]))
print(cor(data.Set3$Yield,slopes), digits = 2)

trellis.device(color = FALSE)  
bwplot(Yield ~ Farmer | Location + SeasonFac, data = data.Set3,
  main = "Rice Yields by Farmer and Season",
  xlab = "Farmer", ylab = "Yield (kg/ha)") # Fig. 7.17
  
# Check a few means (Table 7.4)
tapply(data.Set3$Fert, data.Set3$Location, mean)
tapply(data.Set3$Weeds, data.Set3$Location, mean)
tapply(data.Set3$Cont, data.Set3$Location, mean)
tapply(data.Set3$Irrig, data.Set3$Location, mean)
tapply(data.Set3$SoilP, data.Set3$Location, mean)                                      
tapply(data.Set3$SoilK, data.Set3$Location, mean)
tapply(data.Set3$N, data.Set3$Location, mean)
tapply(data.Set3$P, data.Set3$Location, mean)
tapply(data.Set3$K, data.Set3$Location, mean)

wthr.data <- read.csv("set3\\Set3Weather.csv", header = TRUE)
par(mai = c(1,1,1,1))
plot(wthr.data$Temp0203, type = "l", lty = 2, xaxt = "n",
   ylim = c(10,27),   main = "Regional Temperature Trends",
   ylab = expression(Temperature~"("*degree*C*")"),
   xlab = "Month", cex.main = 2, cex.lab = 1.5) # Fig. 7.18a
lines(wthr.data$Temp0304, lty = 3)
lines(wthr.data$Temp0405, lty = 4)
lines(wthr.data$TempAvg, lty = 1)
axis(side = 1, at = c(1,4,7,10,13,16,19),
   labels = c("Oct","Nov","Dec","Jan","Feb","Mar","Apr"))
legend(8, 15, c("2002-03", "2003-04", "2004-05",
  "Normal"), lty = c(2,3,4,1))

d.0203 <- wthr.data$Temp0203 - wthr.data$TempAvg
d.0304 <- wthr.data$Temp0304 - wthr.data$TempAvg
d.0405 <- wthr.data$Temp0405 - wthr.data$TempAvg
plot(d.0203, type = "l", lty = 2, xaxt = "n",
  main = "Regional Temperature Difference Trends", ylim = c(-5, 10),
  ylab = expression(Temperature~"("*degree*C*")"),
  xlab = "Month", cex.main = 2, cex.lab = 1.5)
lines(d.0304, lty = 3)
lines(d.0405, lty = 4)     # Fig. 7.18b
segments(1, 0, 21, 0)
axis(side = 1, at = c(1,4,7,10,13,16,19),
   labels = c("Oct","Nov","Dec","Jan","Feb","Mar","Apr"))
legend(4, 9, c("2002-03", "2003-04", "2004-05"), lty = c(2,3,4))


d.0203 <- wthr.data$Sun0203 - wthr.data$SunAvg
d.0304 <- wthr.data$Sun0304 - wthr.data$SunAvg
d.0405 <- wthr.data$Sun0405 - wthr.data$SunAvg
plot(d.0203, type = "l", lty = 2, xaxt = "n",
  main = "Regional Sun Hour Difference Trends", ylim = c(-5, 5),
  ylab = "Sun Hours Difference", xlab = "Month",
  cex.main = 2, cex.lab = 1.5)    # Fig. 7.18c
lines(d.0304, lty = 3)
lines(d.0405, lty = 4)
segments(1, 0, 21, 0)
axis(side = 1, at = c(1,4,7,10,13,16,19),
   labels = c("Oct","Nov","Dec","Jan","Feb","Mar","Apr"))
legend(5, 5, c("2002-03", "2003-04", "2004-05"), lty = c(2,3,4))

d.0203 <- wthr.data$Rain0203 - wthr.data$RainAvg
d.0304 <- wthr.data$Rain0304 - wthr.data$RainAvg
d.0405 <- wthr.data$Rain0405 - wthr.data$RainAvg
plot(d.0203, type = "l", lty = 2, xaxt = "n",
  main = "Regional Rain Difference Trends", ylim = c(-75,200),
  ylab = "Rain Difference (cm)", xlab = "Month",
  cex.main = 2, cex.lab = 1.5)  # Fig. 7.18d
lines(d.0304, lty = 3)
lines(d.0405, lty = 4)
axis(side = 1, at = c(1,4,7,10,13,16,19),
   labels = c("Oct","Nov","Dec","Jan","Feb","Mar","Apr"))
legend(3, 175, c("2002-03", "2003-04", "2004-05"), lty = c(2,3,4))
segments(1, 0, 21, 0)

Sand <- with(data.Set3, tapply(Sand, Location, mean))
Silt <- with(data.Set3, tapply(Silt, Location, mean))
Clay <- with(data.Set3, tapply(Clay, Location, mean))
print(cbind(Sand, Silt, Clay), digits = 3)

soil.data  <- with(data.Set3, data.frame(pH, Corg, SoilP, SoilK, Sand, Silt, Clay,
  Yield))
mgmt.data  <- with(data.Set3, data.frame(DPL, Emer, Weeds, Cont, Irrig, D50, Fert,
  N, P, K, Var, Yield))
splom(soil.data, par.settings = list(fontsize=list(text=9),
  plot.symbol = list(col = "black")), pscales = 0,
  main = "Soil Data and Yield, Data Set 3") # Fig. 7.19

trellis.device(color = FALSE)
data.Set3$FieldFac <- factor(data.Set3$Field,
   labels = c("Field 1","Field 2","Field 3",
   "Field 4","Field 5","Field 6","Field 7",
   "Field 8","Field 9","Field 10","Field 11",
   "Field 12","Field 13","Field 14","Field 15",
   "Field 16"))
xyplot(Yield ~ Silt | FieldFac, data = data.Set3,
   main = "Yield vs. Silt by Field") # Fig. 7.20

data.Set3$Variety <- "INIA Tacuarí"
data.Set3$Variety[which(data.Set3$Var == 2)] <- "El Pasol"
data.Set3$Variety[which(data.Set3$Var == 3)] <- "Perla"
data.Set3$Variety[which(data.Set3$Var == 4)] <- "INIA Olimar"
with(data.Set3, tapply(Yield, Variety, mean))

with(data.Set3, unique(Farmer[which(Variety == "INIA Tacuarí")]))
with(data.Set3, unique(Farmer[which(Variety == "El Pasol")]))
with(data.Set3, unique(Farmer[which(Variety == "INIA Olimar")]))
with(data.Set3, unique(Farmer[which(Variety == "Perla")]))

library(lattice)
trellis.device(color = FALSE)
bwplot(Yield ~ Farmer | Variety, data = data.Set3,
   xlab = "Farmer") # Fig. 7.21

   

  




