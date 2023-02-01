library(rpart)

data.Set3 <- read.csv("Set3\\Set3data.csv", header = TRUE)
names(data.Set3)

data.Set3$Location <- 2
data.Set3$Location[(data.Set3$Northing > 6340000)] <- 1
data.Set3$Location[(data.Set3$Northing < 6280000)] <- 3
data.Set3rp <- with(data.Set3, data.frame(Farmer,pH,Corg,SoilP,SoilK,
   Sand,Silt,Clay,DPL,Emer,Weeds,Cont,Irrig,D50,Fert,N,P,K,Var,Yield, Location))
   

cont.parms <- rpart.control(minsplit = 10,cp = 0.002)
Set3.rp1T <-  rpart(Yield ~ pH+Corg+SoilP+SoilK+Sand+Silt+Clay, data = data.Set3,
  control = cont.parms)
plotcp(Set3.rp1T)
printcp(Set3.rp1T)
cont.parms <- rpart.control(minsplit = 10,cp = 0.023)
Set3.rp1 <-  rpart(Yield ~ pH+Corg+SoilP+SoilK+Sand+Silt+Clay, data = data.Set3,
  control = cont.parms)
plot(Set3.rp1,branch = 0.4,uniform = T,margin = 0.1, # Fig. 9.11
   main = "Data Set 3 Yield vs. Exogenous Variables", cex.main = 2)
text(Set3.rp1,use.n = T,all = T, cex = 0.65)


print(sort(with(data.Set3, tapply(Yield, Farmer, mean))), digits = 4)
print(sort(with(data.Set3, tapply(Silt, Farmer, mean))), digits = 3)
print(sort(with(data.Set3, tapply(pH, Farmer, mean))), digits = 3)

data.Set3rp$Variety <- "INIA Tacuarí"
data.Set3rp$Variety[which(data.Set3$Var == 2)] <- "El Pasol"
data.Set3rp$Variety[which(data.Set3$Var == 3)] <- "Perla"
data.Set3rp$Variety[which(data.Set3$Var == 4)] <- "INIA Olimar"
  
cont.parms <- rpart.control(minsplit = 10,cp = 0.002)
Set3.rp2T <-  rpart(Yield ~ DPL + Cont + Irrig +
    N + P + K + Variety + pH + Corg + SoilP + SoilK + Sand +
   Silt + Clay + Farmer, data = data.Set3rp,
  control = cont.parms)
plotcp(Set3.rp2T)
printcp(Set3.rp2T)
cont.parms <- rpart.control(minsplit = 20,cp = 0.003)
Set3.rp2 <- rpart(Yield ~ DPL + Cont + Irrig +
    N + P + K + Variety + pH + Corg + SoilP + SoilK + Sand +
   Silt + Clay + Farmer, data = data.Set3rp, control = cont.parms)
summary(Set3.rp2)
plot(Set3.rp2,branch = 0.4,uniform = T,margin = 0.1, # Fig. 9.12
   main = "Data Set 3 Yield Regression Tree", cex.main = 2)
text(Set3.rp2,use.n = T,all = T, cex = 0.6)
levels(data.Set3$Farmer)

cont.parms <- rpart.control(minsplit = 10,cp = 0.001)
Set3.rp3 <-  rpart(Farmer ~ pH + Corg + SoilP + SoilK + Sand +
   Silt + Clay, data = data.Set3rp, control = cont.parms)
plot(Set3.rp3,branch = 0.4,uniform = T, # Fig. 9.13
   main = "Farmers' Environmental Conditions")
text(Set3.rp3,all = FALSE, cex = 0.65)

Set3.rp4 <- rpart(Farmer ~ DPL + Cont + Irrig +
    N + P + K,data = data.Set3rp, control = cont.parms)
plot(Set3.rp4,branch = 0.4,uniform = T, # Fig. 9.14
   main = "Farmers' Management Variables")
text(Set3.rp4,all = FALSE, cex = 0.8)
summary(Set3.rp4)

Yield <- with(data.Set3, ave(Yield, Farmer, Season, RiceYear, Field))
N <- with(data.Set3, ave(N, Farmer, Season, RiceYear, Field))
K <- with(data.Set3, ave(K, Farmer, Season, RiceYear, Field))
P <- with(data.Set3, ave(P, Farmer, Season, RiceYear, Field))
Irrig <- with(data.Set3, ave(Irrig, Farmer, Season, RiceYear, Field))
Cont <- with(data.Set3, ave(Cont, Farmer, Season, RiceYear, Field))
DPL <- with(data.Set3, ave(DPL, Farmer, Season, RiceYear, Field))
Var <- with(data.Set3, ave(Var, Farmer, Season, RiceYear, Field))
df.C <- data.frame(N, P, K, Irrig, Cont, DPL, Var, Yield)
library(cwhmisc)
Table.C <- remove.dup.rows(df.C)
Comb <- with(data.Set3, paste(as.character(Farmer),
  as.character(Season), as.character(RiceYear), as.character(Field)))
Table.C2 <- data.frame(FrmRySnFld = unique(Comb), Table.C)
print(Table.C2[order(Table.C2$Yield, decreasing = TRUE),],
   digits = 3, right = TRUE)


