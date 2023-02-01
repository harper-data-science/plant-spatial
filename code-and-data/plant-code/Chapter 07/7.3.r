# Preliminary exploration of Data Set 2 (California oaks)
library(sf)
library(ggplot2)
library(maps) # For map()
library(lattice) # For splom()
library(hexbin) # For hexbin()
library(spatstat)

data.Set2 <- read.csv("set2\\set2data.csv", header = TRUE)
# Quick plot to look at the data
x <- with(data.Set2, cbind(Longitude, Latitude, QUDO, Elevation, MAT, ET,
  Precip, Texture))
data.plot <- data.frame(x) 
data.plot.sf <- st_as_sf(data.plot, coords = c("Longitude", "Latitude"))  
st_crs(data.plot.sf) <- "+proj=longlat +datum=WGS84"
plot(data.plot.sf)

# Examples of the use of ggplot() for data exploration

ggplot(data = data.Set2) +
  geom_point(aes(x = Elevation, y = QUDO)) +
  geom_smooth((aes(x = Elevation, y = QUDO)))

data.Set2$QF <- as.character(data.Set2$QUDO)
ggplot(data = data.Set2) +
  geom_point(aes(x = Elevation, y = MAT, color = QF)) + 
  geom_smooth(aes(x = Elevation, y = MAT)) 

ggplot(data = data.Set2) +
  geom_point(aes(x = Elevation, y = MAT, color = Precip)) + 
  geom_smooth(aes(x = Elevation, y = MAT)) 

ggplot(data = data.Set2) +
   geom_histogram(aes(data.Set2$Elevation))

# Compare ggplot version with base version
hist(data.Set2$Elevation)  
max(data.Set2$Elevation)

n.oaks <- function(var.name, oak, low, high, PresAbs){
    length(which(var.name >= low &
       var.name < high & oak == PresAbs))}
pres <- numeric(22)
absent <- numeric(22)
for(i in 0:22) pres[i] <- with(data.Set2,
    n.oaks(Elevation, QUDO, i*100,(i+1)*100, 1))
for(i in 0:22) absent[i] <- with(data.Set2,
    n.oaks(Elevation, QUDO, i*100,(i+1)*100, 0))
pa <- pres / (pres + absent)
x <- seq(100,2200,100)
par(mai = c(1,1,1,1))
plot(x, pa, type = "o", xlab = "Elevation (m)",   # Fig. 7.7
   ylab = "Portion of Sites with a Blue Oak", cex.lab = 1.5)
title(main = "Blue Oak Presence vs. Elevation", cex.main = 2)

data(stateMapEnv)
cal.map <- map("state", "california",
               fill=TRUE, col="transparent",  plot = FALSE)
cal.poly.sf <-st_as_sf(cal.map, "California")
st_crs(cal.poly.sf) <- "EPSG:4326" # WGS 84
data.Set2.sf <- st_as_sf(data.Set2, coords = c("Longitude", "Latitude")) 
st_crs(data.Set2.sf) <- "EPSG:4326" # WGS 84
plot(st_geometry(cal.poly.sf), axes = TRUE, xlim = c(-120, -119))


# Plot the figures shown in the text.
data.TextPlot <- data.Set2.sf[which(data.Set2.sf$Texture < 5.5),]
data.TextPlot$QUDOFac <- factor(data.TextPlot$QUDO, labels =
                                  c("Absent", "Present"))

ggplot() +    
     geom_sf(data = cal.poly.sf, fill = "white" ) + 
     geom_sf(data = data.TextPlot, aes(color = QUDOFac)) +
       theme_bw() +
       theme(panel.background = element_rect(fill = "white")) +
     ggtitle("Blue Oak Presence/Absence") +
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


ggplot() +    
     geom_sf(data = cal.poly.sf, fill = "white" ) + 
     geom_sf(data = data.TextPlot, aes(color = Elevation)) +
       theme_bw() +
       theme(panel.background = element_rect(fill = "white")) +
     ggtitle("Blue Oak Elevation") +
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

ggplot() +    
     geom_sf(data = cal.poly.sf, fill = "white" ) + 
     geom_sf(data = data.TextPlot, aes(color = Precip)) +
       theme_bw() +
       theme(panel.background = element_rect(fill = "white")) +
     ggtitle("Blue Oak Precipitation") +
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

ggplot() +    
     geom_sf(data = cal.poly.sf, fill = "white" ) + 
     geom_sf(data = data.TextPlot, aes(color = MAT)) +
       theme_bw() +
       theme(panel.background = element_rect(fill = "white")) +
     ggtitle("Blue Oak MAT") +
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

ggplot() +    
     geom_sf(data = cal.poly.sf, fill = "white" ) + 
     geom_sf(data = data.TextPlot, aes(color = Texture)) +
       theme_bw() +
       theme(panel.background = element_rect(fill = "white")) +
     ggtitle("Blue Oak Soil Texture") +
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

climate.data <- with(data.Set2.sf, data.frame(Precip, MAT, JaMin,
    JaMax, JaMean, JuMin, JuMax, JuMean, GS32, CoastDist))

trellis.par.get()
trellis.par.set(par.main.text = list(cex = 2))
splom(climate.data, par.settings = list(fontsize=list(text=9),
    plot.symbol = list(col = "black")), pscales = 0, main = "Climate Data")   # Fig. 7.9a


# Use locator() to isolate individual mountain ranges
par(mai = c(1,1,1,1))
plot(cal.poly, axes = TRUE)  # Fig. 7.10a
title(main = "Set 2 Sample Locations",
      xlab = "Longitude", ylab = "Latitude",
      cex.lab = 1.5, cex.main = 2)
plot(st_geometry(data.Set2.sf), pch = 1, cex = 0.4, add = TRUE)

# Identify the regions
# This code is executed twice separately for each mountain range
bdry.pts <- locator(type = "o")
coords.mat <- matrix(unlist(bdry.pts), ncol = 2)
coords.mat <- rbind(coords.mat,coords.mat[1,])
coords.lst <- list(coords.mat)
coords.pol = st_sfc(st_polygon(coords.lst))
bdry.sf = st_sf(z = 1, coords.pol)
st_crs(bdry.sf) =  "EPSG:4326"
data.int <- st_intersects(data.Set2.sf, bdry.sf)
region.spdf <- data.Set2[which(is.na(data.ol$z) == FALSE),]
region.sf <- as(region.spdf, "sf")
st_crs(data.plot.sf) <- "+proj=longlat +datum=WGS84"

# Quick check to see that we have done it right
plot(region.sf)
st_write(region.sf, "created\\set2sierra.shp")
#st_write(region.sf, "created\\set2coast.shp")

# Use these if you have already created and saved the shapefiles
sierra.sf <- st_read("created\\set2sierra.shp")
coast.sf <- st_read("created\\set2coast.shp")

region.sf <- sierra.sf
#region.sf <- coast.sf
climate.data <- with(region.sf, cbind(Precip, MAT, JaMin, JaMax,
   JaMean, JuMin, JuMax, JuMean, GS32, CoastDist))
splom(climate.data, par.settings = list(fontsize=list(text=9),
  plot.symbol = list(col = "black")), pscales = 0,
#  main = "Sierra Nevada Data Set")  # Fig. 7.9b
  main = "Coast Range Data Set")    # Fig. 7.9c

pres <- numeric(22)
absent <- numeric(22)
for(i in 0:22) pres[i] <- with(coast.sf, n.oaks(Elevation, QUDO, i*100,(i+1)*100, 1))
for(i in 0:22) absent[i] <- with(coast.sf,
    n.oaks(Elevation, QUDO, i*100,(i+1)*100, 0))
pa.coast <- pres / (pres + absent + 0.00001)
x <- seq(100,2200,100)
par(mai = c(1,1,1,1))
plot(x, pa.coast, type = "o", xlab = "Elevation (m)",
   ylab = "Portion with Blue Oak", cex.lab = 1.5,
   main = "Blue Oak Presence vs. Elevation",
   cex.main = 2, lty = 1) # Fig. 7.11
for(i in 0:22) pres[i] <- with(sierra.sf, n.oaks(Elevation, QUDO, i*100,(i+1)*100, 1))
for(i in 0:22) absent[i] <- with(sierra.sf,
    n.oaks(Elevation, QUDO, i*100,(i+1)*100, 0))
pa.sierra <- pres / (pres + absent + 0.00001)
x <- seq(100,2200,100)
lines(x, pa.sierra, type = "o", lty = 2)
legend(1000, 0.8, c("Coast Range", "Sierra Nevada"), lty = 1:2,
  title = "Mountain Range")

pres <- numeric(22)
absent <- numeric(22)
for(i in 0:22) pres[i] <- with(coast.sf, n.oaks(Precip, QUDO, i*100,(i+1)*100, 1))
for(i in 0:22) absent[i] <- with(coast.sf, n.oaks(Precip, QUDO, i*100,(i+1)*100, 0))
pa <- pres / (pres + absent + 0.00001)
x <- seq(100,2200,100)
plot(x, pa, type = "o", xlab = "Precipitation (mm)",
   ylab = "Portion with Blue Oak", cex.lab = 1.5,
   main = "Blue Oak Presence vs. Precipitation",
   cex.main = 2, lty = 1, ylim = c(0,1))   # Fig. 7.12a
for(i in 0:22) pres[i] <- with(sierra.sf, n.oaks(Precip, QUDO, i*100,(i+1)*100, 1))
for(i in 0:22) absent[i] <- with(sierra.sf, n.oaks(Precip, QUDO, i*100,(i+1)*100, 0))
pa <- pres / (pres + absent + 0.00001)
x <- seq(100,2200,100)
lines(x, pa, type = "o", lty = 2)
legend(1000, 0.8, c("Coast Range", "Sierra Nevada"), lty = 1:2,
  title = "Mountain Range") 

pres.coast <- numeric(6)
absent.coast <- numeric(6)
for(i in 0:5) pres.coast[i+1] <- with(coast.sf, n.oaks(Texture, QUDO, i-0.1,i+0.1, 1))
for(i in 0:5) absent.coast[i+1] <- with(coast.sf, n.oaks(Texture, QUDO, i-0.1,i+0.1, 0))
pa.coast <- pres.coast / (pres.coast + absent.coast + 0.00001)
pres.sierra <- numeric(6)
absent.sierra <- numeric(6)
for(i in 0:5) pres.sierra[i+1] <- with(sierra.sf, n.oaks(Texture, QUDO, i-0.1,i+0.1, 1))
for(i in 0:5) absent.sierra[i+1] <- with(sierra.sf,
  n.oaks(Texture, QUDO, i-0.1,i+0.1, 0))
pa.sierra <- pres.sierra / (pres.sierra + absent.sierra + 0.00001)
pa <- rbind(pa.coast, pa.sierra)
barplot(pa, beside = TRUE,
  names = c("0", "1", "2", "3", "4", "5"),
  ylim = c(0,1), xlab = "Texture Class",
  ylab = "Percent Occupied Sites", cex.lab = 1.5,
  legend.text = c("Coast", "Sierra"),
  main = "Percent Blue Oak Occupation vs. Texture",
  cex.main = 1.5)   # Fig. 7.12b
  
pres.coast <- numeric(6)
absent.coast <- numeric(6)
for(i in 0:5) pres.coast[i+1] <- with(coast.sf, n.oaks(Permeab, QUDO, i-0.1,i+0.1, 1))
for(i in 0:5) absent.coast[i+1] <- with(coast.sf,
    n.oaks(Permeab, QUDO, i-0.1,i+0.1, 0))
pa.coast <- pres.coast / (pres.coast + absent.coast + 0.00001)
pres.sierra <- numeric(6)
absent.sierra <- numeric(6)
for(i in 0:5) pres.sierra[i+1] <- with(sierra.sf, n.oaks(Permeab, QUDO, i-0.1,i+0.1, 1))
for(i in 0:5) absent.sierra[i+1] <- with(sierra.sf,
    n.oaks(Permeab, QUDO, i-0.1,i+0.1, 0))
pa.sierra <- pres.sierra / (pres.sierra + absent.sierra + 0.00001)
pa <- rbind(pa.coast, pa.sierra)
barplot(pa, beside = TRUE,
  names = c("0", "1", "2", "3", "4", "5"),
  ylim = c(0,1), xlab = "Permeability Class",
  ylab = "Percent Occupied Sites", cex.lab = 1.5,
  legend.text = c("Coast", "Sierra"),
  main = "Percent Blue Oak Occupation vs. Permeability Class",
  cex.main = 1.5)   # Fig. 7.12c

plot(hexbin(coast.sf$Precip, coast.sf$MAT),
   xlab = "Precipitation",  # Fig. 7.13
   ylab = "Mean Annual Temperature",
   main = "Coast Range")

N <- 38.54357
W <- -120.8994
S <- 38.0425
E <- -120.1537

samp.pts <- which(coordinates(data.Set2)[,1] <= E & coordinates(data.Set2)[,1] >= W &
   coordinates(data.Set2)[,2] >= S & coordinates(data.Set2)[,2] <= N)
longitude <- coordinates(data.Set2)[samp.pts,1]
latitude <- coordinates(data.Set2)[samp.pts,2]
samp.ppp <- ppp(longitude,latitude, window = owin(c(W,E),c(S,N)))
plot.ppp(samp.ppp, main = "Sample points")  # Fig. 17.14a 
plot(envelope(samp.ppp, Kest), main = "Ripley's K")  # Fig. 17.14b
