library(sf)
library(gstat)

# From 12.6.1
data.Set2C.sf <- st_read("created\\set2coast.shp")
Set2.kmeans <- with(data.Set2C.sf, data.frame(scale(Longitude),
  scale(Latitude), 0.5 * scale(Elevation)))
cluster.k <- 50
set.seed(123)
cl <- kmeans(Set2.kmeans, cluster.k, iter.max = 100)
data.Set2C.sf$clusID <- cl$cluster
data.Set2Cpts <- with(data.Set2C.sf, data.frame(MAT, TempR, GS32,
   Precip, PE, ET, Texture, AWCAvg, Permeab, SolRad6, SolRad12,
   SolRad, QUDO, Longitude, Latitude, clusID))
data.Set2Cpts.sf <- st_as_sf(data.Set2Cpts, coords = c("Longitude",
   "Latitude"))   
st_crs(data.Set2Cpts.sf) <- "+proj=longlat +datum=WGS84"
data.Set2Cutm.sf <- st_transform(data.Set2Cpts.sf,
  crs = ("+proj=utm +zone=10 +ellps=WGS84"))
data.Set2Cpts$Easting <- st_coordinates(data.Set2Cutm.sf)[,1]
data.Set2Cpts$Northing <- st_coordinates(data.Set2Cutm.sf)[,2]

data.Set2C.mat <- matrix(nrow = length(unique(data.Set2Cpts$clusID)),
   ncol = ncol(data.Set2Cpts))
for (i in 1:ncol(data.Set2Cpts)){
   data.Set2C.mat[,i] <-  tapply(data.Set2Cpts[,i],
      data.Set2Cpts$clusID, mean)
}
data.Set2Cclus <- data.frame(data.Set2C.mat)
names(data.Set2Cclus) <- names(data.Set2Cpts)

### Gotway and Stroup and Waller and Gotway
# Waller and Gotway p. 337 and 388

# Step 1: obtain a starting estimate of the betas
# This is used for the full model
oaks.irwgls <- oaks.glm <- glm(QUDO ~ Permeab + GS32 + PE,
   data = data.Set2Cclus, family = quasibinomial)
print(b.irwgls <- coef(oaks.irwgls), digits = 3)
data.Set2Cclus$p.irwgls <- p.irwgls <- predict(oaks.irwgls,
   type = "response")
   
# Step 2: construct a variogram of the residuals
data.Set2Cgeo <- data.Set2Cclus
data.Set2Cgeo.sf <- st_as_sf(data.Set2Cgeo, coords = c("Easting", "Northing"))
data.Set2Cgeo.sf$Easting <- st_coordinates(data.Set2Cgeo.sf)[,1]
data.Set2Cgeo.sf$Northing <- st_coordinates(data.Set2Cgeo.sf)[,2]
oaks.vgm <- variogram(log(p.irwgls / (1 - p.irwgls)) ~
   Easting + Northing, data = data.Set2Cgeo.sf)
   
# Step 3: Estimate semivariogram of the residuals
par(mai = c(1,1,1,1))
plot(oaks.vgm$dist, oaks.vgm$gamma,
   xlim = c(0,140000), ylim = c(0, 1.4),
   cex.main = 2, cex.lab = 1.5,
     main = "Variogram of Residuals of ln(p/(1-p))",
   xlab = expression(bold(lag~h)),
   ylab = (expression(bold(hat(gamma)*"("*h*")"))),
   xaxs = "i", yaxs  = "i")

h2 <- seq(0, 140000, 1000)
b.exp <- 1.0
alpha.exp <- 20000
g.exp <- b.exp * (1 - exp(-h2 / alpha.exp))
lines(h2, g.exp)

# Step 4
n.sites <- as.numeric(table(data.Set2C.sf$clusID))
v.half <- diag(as.vector(p.irwgls * (1 - p.irwgls) / n.sites)) # Eq. (4.2) G&S
x <- data.Set2Cclus$Easting
y <- data.Set2Cclus$Northing
lag.mat <- as.matrix(dist(cbind(x, y)))
R <- exp(-lag.mat / alpha.exp)
V <- sqrt(v.half) %*% R %*% sqrt(v.half) # Eq. (2.13) G&S
D <- diag(as.vector(p.irwgls * (1 - p.irwgls))) # p. 165 G&S
V.inv <- solve(V)
W <- t(D) %*% V.inv %*% D
eta <- log(p.irwgls / (1 - p.irwgls))
mu <- as.vector(p.irwgls)
z <- data.Set2Cclus$QUDO
D.inv <- solve(D)
z.star <- eta + D.inv %*% (z - mu)
X <- with(data.Set2Cclus, cbind(rep(1,length(p.irwgls)),
   Permeab, GS32, PE))
XpWX.inv <- solve(t(X) %*% W %*% X)
b.old <- b.irwgls
b.irwgls <- XpWX.inv %*% t(X) %*% W %*% z.star
t(b.irwgls)
t(b.old)
t(abs((b.irwgls - b.old) / b.old))

# Step 5
data.Set2Cclus$p.irwgls <- p.irwgls <-
    1 / (1 + exp(-X %*% b.irwgls))

# Go to Step 2

# Final result

print(t(b.glm <- coef(oaks.glm)), digits = 3)
print(t(b.irwgls), digits = 3)
print(t((b.irwgls - b.glm) / b.glm), digits = 3)








