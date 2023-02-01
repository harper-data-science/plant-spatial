data.Set3 <- read.csv("Set3\\Set3data.csv", header = TRUE)
data.Set3$Location <- 1
data.Set3$Location[(data.Set3$Northing > 6340000)] <- 16
data.Set3$Location[(data.Set3$Northing < 6280000)] <- 3

library(plotrix)
data.Set3$NS <- as.numeric(data.Set3$Location != "Center") # Figure 17.7
data.Set3$ClayC <- 100 - with(data.Set3, Sand + Silt)
Set3.texture <- subset(data.Set3, select = c(Sand, Silt, ClayC))
soil.texture(as.matrix(Set3.texture), pch = data.Set3$Location,
   main = "Data Set 3 Soil Textures")
legend(0.8, 0.8, c("North", "Center", "South"), pch = c(16,1,3))

# Color version
data.Set3$NS <- as.numeric(data.Set3$Location != "Center")
data.Set3$ClayC <- 100 - with(data.Set3, Sand + Silt)
data.Set3$Color <- "red"
data.Set3$Color[which(data.Set3$Location == 16)] <- "blue"
data.Set3$Color[which(data.Set3$Location == 3)] <- "green"
Set3.texture <- subset(data.Set3, select = c(Sand, Silt, ClayC))
soil.texture(as.matrix(Set3.texture), pch = 16, col.symbols = data.Set3$Color, 
   main = "Data Set 3 Soil Textures")
legend(0.8, 0.8, c("North", "Center", "South"), pch = 16,
   col = c("red", "blue", "green"))

library(nlme)
library(lattice)
trellis.device(color = FALSE)
data.Set3$FieldFac <- factor(data.Set3$Field,
   labels = c("Field 1","Field 2","Field 3",
   "Field 4","Field 5","Field 6","Field 7",
   "Field 8","Field 9","Field 10","Field 11",
   "Field 12","Field 13","Field 14","Field 15",
   "Field 16"))
xyplot(Yield ~ N | FieldFac, data = data.Set3,
   main = "Yield vs. N by Field")

data.Set3N <- data.Set3[which(data.Set3$Field %in%
   c(5,11,12,13,16)),]
data.lis <- lmList(Yield ~ N | Field, data = data.Set3N)
print(coef(data.lis), digits = 3)
b0.N <- coef(data.lis)[,1]
b1.N <- coef(data.lis)[,2]

print(Nmax <- max(data.Set3$N))
print(Yield.predN <- b0.N + b1.N * Nmax, digits = 4)
print(Yield.N <- tapply(data.Set3N$Yield, data.Set3N$Field, mean),
   digits = 4)
print(delta.YN <- Yield.predN - Yield.N, digits = 3)


data.Set3I <- data.Set3[which(data.Set3$Field != 2),]
data.lis <- lmList(Yield ~ Irrig | Field, data = data.Set3I)
coef(data.lis)
b0.I <- coef(data.lis)[,1]
b1.I <- coef(data.lis)[,2]
print(Yield.predI <- b0.I + b1.I * 5, digits = 4)
print(Yield.I <- tapply(data.Set3I$Yield, data.Set3I$Field, mean),
    digits = 4)
print(delta.YI <- Yield.predI - Yield.I, digits = 3)


data.Set3D <- data.Set3[which(data.Set3$Field %in% c(3,4,5,10,11,12,16)),]
data.lis <- lmList(Yield ~ DPL | Field, data = data.Set3D)
coef(data.lis)
b0.D <- coef(data.lis)[,1]
b1.D <- coef(data.lis)[,2]
print(Yield.predD <- b0.D + b1.D)
print(Yield.D <- tapply(data.Set3D$Yield, data.Set3D$Field, mean), digits = 4)
print(delta.YD <- Yield.predD - Yield.D, digits = 3)
