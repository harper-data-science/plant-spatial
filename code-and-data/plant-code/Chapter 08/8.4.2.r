library(sf)
data.Set2S.sf <- st_read("created\\set2Sierra.shp")
data.Set2S.glm <- with(data.Set2S.sf, data.frame(MAT, TempR,
   Precip, PE, ET, Texture, AWCAvg, Permeab, SolRad6, SolRad12,
   SolRad, CoastDist, QUDO))
data.Set2S.glm$PM100 <- as.numeric(data.Set2S.sf$PM100 > 0)
data.Set2S.glm$PM200 <- as.numeric(data.Set2S.sf$PM200 > 0)
data.Set2S.glm$PM300 <- as.numeric(data.Set2S.sf$PM300 > 0)
data.Set2S.glm$PM400 <- as.numeric(data.Set2S.sf$PM400 > 0)
data.Set2S.glm$PM500 <- as.numeric(data.Set2S.sf$PM500 > 0)
data.Set2S.glm$PM600 <- as.numeric(data.Set2S.sf$PM600 > 0)

model.glmSfull <- glm(QUDO ~ ., data = data.Set2S.glm,
  family = binomial)
AIC(model.glmSfull, k = log(nrow(data.Set2S.glm)))
# Take a peak (this is not in the text)
d1 <- drop1(model.glmSfull)
d1[order(d1[,3], decreasing = TRUE),]

model.formula <- as.formula("QUDO ~ MAT + TempR + Precip + PE + ET +
  Texture + AWCAvg + Permeab + SolRad6 + SolRad12 + SolRad +
  PM100 + PM200 + PM300 + PM400 + PM500 + PM600 + CoastDist")
model.glmS0 <- glm(QUDO ~ 1, data = data.Set2S.glm, family = binomial)
a1 <- add1(model.glmS0, model.formula)
a1[order(a1[,3]),]

model.glmS1 <- update(model.glmS0,
   formula = as.formula("QUDO ~ Precip"))
model.formula2 <- as.formula("QUDO ~ MAT + Precip + PE + ET +
  Texture + AWCAvg + Permeab + SolRad6 + SolRad12 + SolRad + CoastDist")
a1 <- add1(model.glmS1, model.formula)
a1[order(a1[,3]),]

with(data.Set2S.glm, cor(Precip, MAT))

##########
# Not in text. Again, try a parallel anaysis with drop1
model.glmSfull2 <- glm(QUDO ~ MAT + PE + ET + Texture + AWCAvg +
  Permeab + SolRad6 + SolRad12 + SolRad + CoastDist, data = data.Set2S.glm,
  family = binomial)
d1 <- drop1(model.glmSfull2)
d1[order(d1[,3], decreasing = TRUE),]
########

model.glmS2 <- update(model.glmS0,
   formula = as.formula("QUDO ~ Precip + MAT"))
summary(model.glmS1)
summary(model.glmS2)

library(car)
# Discussed but not shown in text
avPlots(model.glmS2,
   main = "Added Variable Plot for QUDO ~ Precip + MAT")
crPlots(model.glmS2,
   main = "Partial Residual Plot for QUDO ~ Precip + MAT")
   
model.glmS1sq <- update(model.glmS0,
   formula = as.formula("QUDO ~ Precip + I(Precip^2)"))
summary(model.glmS1sq)

model.glmS1sq <- update(model.glmS0,
   formula = as.formula("QUDO ~ Precip + I(Precip^2)"))
anova(model.glmS1, model.glmS1sq, test = "Chisq")

model.glmS2sq <- update(model.glmS0,
   formula = as.formula("QUDO ~ Precip + I(Precip^2) + MAT"))
coef(model.glmS2sq)
anova(model.glmS2, model.glmS2sq, test = "Chisq")

model.glmS2int <- update(model.glmS0,
   formula = as.formula("QUDO ~ Precip + I(Precip*MAT) +MAT"))
anova(model.glmS2, model.glmS2int, test = "Chisq")

AIC(model.glmS1, k = log(nrow(data.Set2S.glm)))
AIC(model.glmS2, k = log(nrow(data.Set2S.glm)))

 
library(maps)
data(stateMapEnv)
library(ggplot2)
cal.map <- map("state", "california",
   fill=TRUE, col="transparent",  plot = FALSE)
cal.poly.sf <- st_as_sf(cal.map, "California")
st_crs(cal.poly.sf) <- "EPSG:4326" # WGS 84
st_crs(data.Set2S.sf) <- "EPSG:4326"
# ref https://geanders.github.io/navy_public_health/3-2-basic-mapping.html
data.Set2S.sf$QUDO_Pres <- factor(data.Set2S.sf$QUDO, labels =
    c("Absent", "Present"))
arrow.df <- data.frame(Longitude = -116, Latitude = 38,
   x2 = -116, y2 = 40)
lat <- 34
deg.per.km <- 360 / 40075
deg.per.km <- deg.per.km * cos(pi*lat / 180)
scalebar.df <- data.frame(x1 = -123, y1 = lat,
   x2 = -123 + 200 * deg.per.km, y2 = lat)

ggplot() +    
     geom_sf(data = cal.poly.sf, fill = "white" ) + 
     geom_sf(data = data.Set2S.sf, aes(color = QUDO_Pres)) +
       theme_bw() +
       theme(panel.background = element_rect(fill = "white")) +
     ggtitle("Blue Oak Presence/Absence: Precip Only") +
       geom_segment(aes(x = Longitude, y = Latitude, xend = x2,
          yend = y2),
          data = arrow.df, lwd = 1, 
          arrow = arrow(length = unit(0.30, "cm"), ends="last",
             type = "closed")) + 
       geom_text() + annotate("text", label = "N",
         x = arrow.df$Longitude, 
         y = (arrow.df$Latitude + arrow.df$y2) / 2,
         size = 7, color = "black") +
       geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
          data = scalebar.df, lwd = 1, color = "black") + 
       geom_text() + annotate("text", label = "0",
         x = scalebar.df$x1 - 0.2, y = scalebar.df$y1,
         size = 4, color = "black") +
       geom_text() + annotate("text", label = "200 km",
         x = scalebar.df$x2 + 0.7, y = scalebar.df$y1,
         size = 4, color = "black") 


data.Set2S.sf$MAT_Effect <- factor(predict(model.glmS2, type = "response") -
   predict(model.glmS1, type = "response") >= 0,
   labels = c("Decrease", "Increase"))

ggplot() +    
     geom_sf(data = cal.poly.sf, fill = "white" ) + 
     geom_sf(data = data.Set2S.sf, aes(color = MAT_Effect)) +
       theme_bw() +
       theme(panel.background = element_rect(fill = "white")) +
     ggtitle("Effect of Adding MAT") +
       geom_segment(aes(x = Longitude, y = Latitude, xend = x2,
          yend = y2),
          data = arrow.df, lwd = 1, 
          arrow = arrow(length = unit(0.30, "cm"), ends="last",
             type = "closed")) + 
       geom_text() + annotate("text", label = "N",
         x = arrow.df$Longitude, 
         y = (arrow.df$Latitude + arrow.df$y2) / 2,
         size = 7, color = "black") +
       geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
          data = scalebar.df, lwd = 1, color = "black") + 
       geom_text() + annotate("text", label = "0",
         x = scalebar.df$x1 - 0.2, y = scalebar.df$y1,
         size = 4, color = "black") +
       geom_text() + annotate("text", label = "200 km",
         x = scalebar.df$x2 + 0.7, y = scalebar.df$y1,
         size = 4, color = "black") 


model.formula2 <- as.formula("QUDO ~ MAT + Precip + PE + ET +
  Texture + AWCAvg + Permeab + SolRad6 + SolRad12 + SolRad")
a1 <- add1(model.glmS2, model.formula2)
a1[order(a1[,3]),]

#######
#Not in text. Continue with parallel dropping
model.glmSfull3 <- glm(QUDO ~ PE + ET + Texture + AWCAvg +
  Permeab + SolRad6 + SolRad12 + SolRad, data = data.Set2S.glm,
  family = binomial)
d1 <- drop1(model.glmSfull3)
d1[order(d1[,3], decreasing = TRUE),]
############

model.glmS5 <- update(model.glmS0,
   formula = as.formula("QUDO ~ Precip + MAT + SolRad +
   AWCAvg + Permeab"))

# Consider the effect of including Elevation
data.Set2SglmE <- data.frame(cbind(data.Set2S.glm,
   Elevation = data.Set2S.sf$Elevation))
model.glmS5E <- glm(QUDO ~ Precip + MAT + SolRad +
   AWCAvg + Permeab + Elevation, data = data.Set2SglmE,
   family = binomial)
AIC(model.glmS5, k = log(nrow(data.Set2S.glm)))
AIC(model.glmS5E, k = log(nrow(data.Set2S.glm)))
summary(model.glmS5E)

print(d1 <- drop1(model.glmS5E))


