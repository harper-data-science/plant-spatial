library(sf)

data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
data.Set4.1$x <- data.Set4.1$Easting - min(data.Set4.1$Easting)
data.Set4.1$y <- data.Set4.1$Northing - min(data.Set4.1$Northing)
trend.lm <- lm(Sand ~ x + y + I(x^2) +
  I(y^2) + I(x*y), data = data.Set4.1)
data.Set4.1$SandDT <- data.Set4.1$Sand - predict(trend.lm)

# construct Fig. 4.6
bdry.sf <- st_read("created\\set419697bdry.shp")
data.Set4.1.sf <- st_as_sf(data.Set4.1, 
   coords = c("Easting", "Northing"))
plot(bdry.sf["ID"],  main = "", col = "white", axes = TRUE,
   reset = FALSE) # Fig. 4.6
plot(st_geometry(data.Set4.1.sf), add = TRUE, pch = 1, col = "black")
title(main = "Field 4.1", cex.main = 1)
x0 <- 592173
y0 <- 4270677
arrows(x0, y0, x0 + 61, y0, lwd = 2, length = 0.1)
text(x0 + 30, y0 - 20, expression(bold(h[1])))
arrows(x0, y0, x0 + 61, y0 + 61, lwd = 2, length = 0.1)
text(x0 + 60, y0 + 30, expression(bold(h[2])))
arrows(x0, y0, x0, y0+183, lwd = 2, length = 0.1)
text(x0 - 20, y0 + 90, expression(bold(h[3])))

# Silt content variogram
library(gstat)
Silt.vgm <- variogram(Silt ~ 1, data.Set4.1.sf, cutoff = 600)
Silt.fit <- fit.variogram(Silt.vgm, model = vgm(1, "Sph", 700, 1))
plot(Silt.vgm$dist, Silt.vgm$gamma, ylim = c(0,7),  # Fig. 4.7a
    xlim = c(0,600), main = "Silt Content Variogram",
   xlab = expression(bold("lag h")),
   ylab = (expression(bold(hat(gamma)*"(h)"))),
   xaxs = "i", yaxs  = "i")  
arrows(0,0.5,386.0651,0.5, length = 0.15)
r1 <- seq(0,375,25)
a <- Silt.fit$range[2]
g.sph <- 1.5 * r1 / a - 0.5 * (r1 / a)^3
b <- Silt.fit$psill[1]
cc <- Silt.fit$psill[2]
g.hat <- b + cc * g.sph
lines(r1,g.hat)
lines(c(max(r1),600),c(b+cc,b+cc))
lines(c(a,a),c(0.2,0.8))
text(a+5,0.5, "Range", pos = 4)
arrows(50,0,50,b, length = 0.15)
lines(c(10,90), c(b,b))
text(90, b, "Nugget", pos = 4)
arrows(550, 0, 550, b+cc, length = 0.15)
text(550, 4, pos = 2, "Sill")
text(Silt.vgm$dist, Silt.vgm$gamma, as.character(Silt.vgm$np), pos = 4)

#Fig. 4.7b
plot(bdry.sf["ID"],  main = "", col = "white", axes = TRUE,
   reset = FALSE) # Fig. 4.6
plot(data.Set4.1.sf["Silt"], add = TRUE, pch = 1, col = "black",
   cex = (data.Set4.1$Silt - 30)/3)
title(main = "Field 4.1 Silt Content", cex.main = 1)
arrows(592200,4270433, 592200, 4270921, length = 0.15, code = 3)
text(592200, 4270710, "488 m", pos = 4)

# Sand content variogram
# Without detrending  (Fig. 4.8a)
Sand.vgm <- variogram(Sand ~ 1, data.Set4.1.sf, cutoff = 600)
plot(Sand.vgm, col = "black", main = "Sand Content Variogram",
   xlab = expression(bold("lag h")),
   ylab = expression(bold(hat(gamma)*"(h)")))

# With detrending  (Fig. 4.8b)
SandDT.vgm <- variogram(SandDT ~ 1, data.Set4.1.sf, cutoff = 600)
plot(SandDT.vgm, col = "black", main = "Detrended Sand Content Variogram",
   xlab = expression(bold("lag h")),
   ylab = (expression(bold(hat(gamma)*"(h)"))))

