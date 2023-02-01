library(spacetime)
library(gstat)
library(date) # For date()
library(maptools) # For CRS()

# Read all the Field 4.1 data to create the spacetime object

data.Set4.1.96 <- read.csv("created\\set4.1yld96ptsidw.csv")
data.Set4.1.97 <- read.csv("created\\set4.1yld97ptsidw.csv")
data.Set4.1.98 <- read.csv("created\\set4.1yld98ptsidw.csv")
data.Set4.1.99 <- read.csv("created\\set4.1yld99ptsidw.csv")

# Eliminate the last 12 rows of each data set
Y96 <- data.Set4.1.96[(1:74),]
Y97 <- data.Set4.1.97[(1:74),]
Y98 <- data.Set4.1.98[(1:74),]
Y99 <- data.Set4.1.99[(1:74),]

# Normalize the data
Y96.norm <- 100 * Y96[,2] / max(Y96[,2])
Y97.norm <- 100 * Y97[,2] / max(Y97[,2])
Y98.norm <- 100 * Y98[,2] / max(Y98[,2])
Y99.norm <- 100 * Y99[,2] / max(Y99[,2])

# Create the attribute data
eps <- 5
lambda <- 0.4
nt <- 100
eta <- numeric(nt)

# Create and plot the first set of attribute values
set.seed(123)
v <- c(Y96.norm[1], Y97.norm[1], Y98.norm[1],
  Y99.norm[1])
t <- c(0, 1, 2, 3)
s <- spline(t, v, n = nt, method = "natural")
for (i in 2:nt) eta[i] <- lambda * eta[i - 1] + eps * rnorm(1)
v2 <- s$y + eta
plot(t, v, xlab = "time", ylab = "Y", ylim = c(0,100), pch = 16)
lines(s$x, v2, lty = 1)
lines(s$x, s$y, lty = 2)
points(s$x, v2, cex = 0.8)  # Fig. 15.4

# Color version
plot(t, v, xlab = "time", ylab = "Y", ylim = c(0,100), pch = 16, col = "red")
lines(s$x, v2, lty = 1, col = "blue")
lines(s$x, s$y, lty = 2, col = "red", lwd = 2)
points(s$x, v2, cex = 0.8, col = "blue")  # Fig. 15.4


# Add the remaining attribute data
Y <- v2
for (i in 2:74){
  v <- c(Y96.norm[i], Y97.norm[i], Y98.norm[i],
    Y99.norm[i])
  t <- c(96,97,98,99)
  s <- spline(t, v, n = nt, method = "natural")
  eta[1] <- 0
  for (i in 2:nt) eta[i] <- lambda * eta[i - 1] + eps * rnorm(1)
  v2 <- s$y + eta
  Y <-c(Y, v2)
}

Y.data <- data.frame(Y = Y)
nrow(Y.data)

Y.sites <- data.frame(Easting = Y96$Easting, Northing = Y96$Northing)
coordinates(Y.sites) <- c("Easting", "Northing")
proj4string(Y.sites) <- CRS("+proj=utm +zone=10 +ellps=WGS84")

as.date(0:3)
class(as.date(0:3))
as.POSIXct(as.date(0:3))

Y.time <- as.POSIXct(as.date(0:(nt - 1)))

# Create the STFDF object
Y.stfdf <- STFDF(Y.sites, Y.time, data = Y.data)

#Simple empirical variogram
Y.empvgm <- variogramST(Y ~ 1, data = Y.stfdf, tlags = 0:7, cutoff = 400,
    width = 100)
greys <- grey(0:200 / 255)
plot(Y.empvgm, wireframe = TRUE, col = greys) # Fig. 15.5.

#Color version
Color <- terrain.colors(50)
plot(Y.empvgm, wireframe = TRUE, col = Color) 

print(st.ani <- estiStAni(Y.empvgm, interval = c(0,500)))

# Separable variogram
Y.sepvgm <- vgmST("separable", space = vgm(0,"Exp", 200, 0.1),
     time = vgm(1,"Sph", 3, 0.1), sill=1)

Y.sep.fit <- fit.StVariogram(Y.empvgm, Y.sepvgm, fit.method = 6,
                             stAni = st.ani,
                             lower = c(1,0,1,0,1), 
                             upper = c(2000,10,12,1,200)
)

plot(Y.empvgm, Y.sep.fit, wireframe=T, all=T, col = greys) # Fig. 15.6

attr(Y.sep.fit, "MSE")

# Sum-metric fit
Y.smm <- vgmST("sumMetric",  space = vgm(20, "Sph", 150, 1),
               time = vgm(10, "Exp", 2, 0.5), joint = vgm(80, "Sph", 1500, 2.5),
               stAni = st.ani)

Y.smm.fit <- fitSumMetricModel <- fit.StVariogram(Y.empvgm, Y.smm, fit.method = 7, stAni=st.ani,
    method = "L-BFGS-B", lower = c(sill.s = 0,  range.s = 10,  nugget.s = 0,
    sill.t = 0,  range.t = 0.1,   nugget.t = 0,
    sill.st= 0, range.st = 10, nugget.st = 0, anis = 40),
    upper = c(sill.s = 200,  range.s = 1E3,  nugget.s = 20,
    sill.t = 200,  range.t = 75,   nugget.t = 20,
    sill.st= 200, range.st = 5E3, nugget.st = 20, anis = 500),
    control = list(parscale = c(1,100,1,1,0.5,1,1,100,1,100), maxit=1e4))

plot(Y.empvgm, Y.smm.fit, wireframe=T, col = greys) # Fig. 15.7

attr(Y.smm.fit, "MSE")

