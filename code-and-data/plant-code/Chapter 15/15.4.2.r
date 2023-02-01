library(rpart)
library(randomForest)

weather.data <- read.csv("Set4\\dailyweatherdata.csv", header = TRUE)
names(weather.data)

season.1 <- 62:225
season.2 <- 548:701
season.3 <- 913:1066
season.4 <- 1278:1431

which(is.na(weather.data$CIMIS_Eto_mm))
ETo1997 <- sum(weather.data$CIMIS_Eto_mm[season.2])
ETo1998 <- sum(weather.data$CIMIS_Eto_mm[season.3])
ETo1999 <- sum(weather.data$CIMIS_Eto_mm[season.4])
c(ETo1997, ETo1998, ETo1999)

which(is.na(weather.data$MinAirTemp_C))

######### Imputation is done in Exercise 15.5

DD.1997 <- DD.per.day(imputed.max1997, imputed.min1997)
DD.1998 <- DD.per.day(imputed.max1998, imputed.min1998)
DD.1999 <- DD.per.day(imputed.max1999, imputed.min1999)

season.length <- 1431 - 1278 + 1
par(mai = c(1,1,1,1))
plot(1:season.length, cumsum(DD.1997), type = "l",  # Fig. 15.14a
   xlab = "Days after 1 April", cex.lab = 1.5,
   ylab = "DegreeDays", main = expression(paste("Degree Days Above 10",
   degree,"C"), cex.main = 2))
lines(1:season.length, cumsum(DD.1998), lty = 2)
lines(1:season.length, cumsum(DD.1999), lty = 3)
legend(10, 1500, c("1997", "1998", "1999"), lty = 1:3)

data.Set4.1samp <- read.csv("Set4\\Set4.196sample.csv",
   header = TRUE)[1:74,]

# Pick one successively
clusters.Set4.1 <- read.csv("created\\clusters2.csv", header = TRUE)
n.clus <- 2
clusters.Set4.1 <- read.csv("created\\clusters3.csv", header = TRUE)
n.clus <- 3
clusters.Set4.1 <- read.csv("created\\clusters4.csv", header = TRUE)
n.clus <- 4

# Choose explanatory variables
data.Set4.1clus <- with(data.Set4.1samp, data.frame(Sand, Clay, Silt,
   SoilpH, SoilTN, SoilP, SoilK, FLN, Disease, Weeds))
data.Set4.1clus$Cluster <- clusters.Set4.1$x
cont.parms <- rpart.control(minsplit = 2,cp = 0.06)
model.1 <- as.formula("Cluster ~ .")
y41.rp1 <- rpart(model.1, data = data.Set4.1clus, method = "class")
plotcp(y41.rp1)
printcp(y41.rp1)

plot(y41.rp1, branch = 0.4,uniform = T,margin = 0.1, # Figs. 15.14a,b,c
   main = paste("Field 4.1,", as.character(n.clus),
   "Clusters"), cex.main = 2)
text(y41.rp1,use.n = T,all = T)
summary(y41.rp1)
                                             
library(sf)
data.disp.sf <- st_as_sf(data.Set4.1samp, coords = c("Easting", "Northing"))

# k = 2
data.disp.sf$node <- 1
data.disp.sf$node[which(data.disp.sf$Clay < 35.52)] <- 17
data.disp.sf$node[which(data.disp.sf$Clay >= 35.52 & 
   data.disp.sf$SoilTN >= 0.1065)] <- 2  
plot(st_geometry(data.disp.sf), pch = data.disp.sf$node, axes = FALSE, # Fig. 15.16
   ylim = c(4270300,4271100))
legend(591950, 4270450, c("Clay < 35.52",
   "Clay >= 35.52 & Soil TN < 0.1065",
   "Clay >= 35.52 & Soil TN >= 0.1065"),
    pch = c(17,1,2))
title(main = "Two Clusters", cex.main = 2)


data.Set4.1clus$Cluster <- clusters.Set4.1$x
y41.rf <- randomForest(factor(Cluster) ~ Sand + Clay + Silt +
   SoilpH + SoilTN + SoilP + SoilK + FLN + Weeds + Disease,
   data = data.Set4.1clus, importance = TRUE, proximity = TRUE)
varImpPlot(y41.rf, main = "Cluster Variable Importance")

# Random perturbations of the tree 
clusters.Set4.1 <- read.csv("created\\clusters3.csv", header = TRUE)
n.clus <- 3
data.Perturb <- data.Set4.1clus
p <- 0.1
set.seed(123)
for (i in 1:10){
   data.Perturb$Cluster <- clusters.Set4.1$x +
      rbinom(74, 1, p) - rbinom(74, 1, p)
   data.Perturb$Cluster[which(data.Perturb$Cluster < 0)] <- 0
   data.Perturb$Cluster[which(data.Perturb$Cluster > 4)] <- 1
   data.Perturb$Cluster
   Perturb.rp <-  rpart(model.1, data = data.Perturb, method = "class")
   print(as.character(Perturb.rp$frame[1:6,1]))
}
