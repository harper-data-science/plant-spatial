data.Set3 <- read.csv("Set3\\Set3data.csv", header = TRUE)
# Using the variable Irrig
library(lattice)
trellis.device(color = FALSE)
data.Set3$FieldFac <- factor(data.Set3$Field,
   labels = c("Field 1","Field 2","Field 3",
   "Field 4","Field 5","Field 6","Field 7",
   "Field 8","Field 9","Field 10","Field 11",
   "Field 12","Field 13","Field 14","Field 15",
   "Field 16"))

xyplot(Yield ~ Irrig | FieldFac, data = data.Set3, # Fig. 12.1
main = "Data Set 3 Yield vs. Irrigation by Field")
library(nlme)
data.lis <- lmList(Yield ~ Irrig | Field, data = data.Set3)
print(coef(data.lis), digits = 3)
tapply(data.Set3$Irrig, data.Set3$Field, unique)
data.Set3a <- data.Set3[-which(data.Set3$Field == 2),]

Yld.aov <- aov(Yield ~ factor(Field)+ Irrig , data = data.Set3a)
summary(Yld.aov)

