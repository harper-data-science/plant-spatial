#Simulation of the 3 component model (deterministic+correlated+random)

not.cell2nb = function(n.x, n.y, rook = TRUE){
# Create a neighbor list for a rectangular lattice
   Q <- FALSE
   if (!rook) Q <- TRUE
   xy.coords <- data.frame(x = sort(rep(1:n.x, n.y)),
      y = rep(1:n.y, n.x))
   xy.sf <- st_as_sf(xy.coords, coords = c("x", "y")) 
   nlist <- st_nn(xy.sf, xy.sf, sparse = TRUE,
      k = 8, returnDist = TRUE)
   nbr <- nlist[[1]]
   dst <- nlist[[2]]
   r.list <- list()
   d.max <- 1.4
   if (Q) d.max <- 1.5
   for (n in 1:(lengths(nlist)[1])){
      nrange <- which(dst[[n]] > 0 & dst[[n]] < d.max)
      r.list[[n]] <- nbr[[n]][nrange]
      }
   return(r.list) 
   }

not.invIrM <- function(n.list, lambda){
   W <- matrix(0, nrow = length(n.list), ncol = length(n.list))
   lnl <- length(n.list)
   for (n in 1:lnl){
      z <- as.numeric(n.list[[n]])
      W[n, z] <- 1 / length(z)
      }
   ImlW <- diag(lnl) - lambda * W 
   ImlWinv <- solve(ImlW, diag(lnl))
   }
     
#Correlated data
# Using homemade function
library(sf)
library(nngeo)
set.seed(123)
sigma.eta <- 1.0  # Variance of autocorrelated component
lambda <- 0.4     # Autocorrelation coefficient
nlist <- not.cell2nb(20, 20)
e <- rnorm(20^2)
eta <- sigma.eta * not.invIrM(nlist, lambda) %*% e

# Using spatreg functions
library(spdep)
library(spatialreg)
set.seed(123)
sigma.eta <- 1.0  # Variance of autocorrelated component
lambda <- 0.4     # Autocorrelation coefficient
nlist <- cell2nb(20, 20)
e <- rnorm(20^2)
eta <- sigma.eta * invIrM(nlist, lambda) %*% e

#Random data
sigma.eps <- 0.4 # Magnitude of random component
eps <- sigma.eps * rnorm(20^2)

# Create the correlated component plus uncorrelated component
Y.df <- data.frame(eta)  # Start with autocorr. comp.
Y.df$eps <- eps  # Add uncorrelated component

#Add deterministic logistic trend
a <- 2
b.eps <- 0.5  # Variance factor for uncorr. component
b.eta <- 0.15 # Variance factor for autocorr. component
c <- 1
f2 <- 20 / 2
xymax <- 20 - 0.5
coords.xy <- expand.grid(x = 0.5:xymax, y = xymax:0.5)
x <- coords.xy[,1]
y <- coords.xy[,2]
Y.trend <- a * exp(c * (x - f2)) / (1 + exp(c * (x - f2)))
Y <- Y.trend + b.eps * eps + b.eta* eta
Y.df$trend <- Y.trend
Y.df$Y <- Y
Y.df$x <- x
Y.df$y <- y

# Create Fig. 3.1
Y.plot <- Y.df[(Y.df$y == 20 / 2 + 0.5),]
Yt <- Y.plot$trend
Yeps <- b.eps * Y.plot$eps
Yeta <- b.eta * Y.plot$eta
Y <- Yt + Yeps + Yeta
x <- Y.plot$x
par(mai = c(1,1,1,1))
plot(x, Y, type = "l", lwd = 1, cex.lab = 1.5,  #Fig. 3.1
   ylim = c(-0.5,2.5), xlab = expression(italic(x)),
   ylab = expression(italic(Y)))
lines(x, Yt, lwd = 3)
lines(x, Yt + Yeta, lwd = 2)
text(x = 12.5, y = 0.25, "T(x,y)", pos = 4)
lines(c(10,12.5),c(0.25,0.25),lwd = 3)
text(x = 12.5, y = 0.0, expression("T(x,y)"~+~eta*"(x,y)"),
   pos = 4)
lines(c(10,12.5),c(0,0),lwd = 2)
text(x = 12.5, y = -0.25, pos = 4,
   expression("T(x,y)"~+~eta*"(x,y)"~+~epsilon*"(x,y)"))
lines(c(10,12.5),c(-0.25,-0.25),lwd = 1)

# Color version
plot(x, Y, type = "l", lwd = 2, cex.lab = 1.5,  #Fig. 3.1
   ylim = c(-0.5,2.5), xlab = expression(italic(x)),
   ylab = expression(italic(Y)), col = "red")
lines(x, Yt, lwd = 2, col = "blue")
lines(x, Yt + Yeta, lwd = 2, col = "dark green")
text(x = 12.5, y = 0.25, "T(x,y)", pos = 4)
lines(c(10,12.5),c(0.25,0.25),lwd = 2, col = "blue")
text(x = 12.5, y = 0.0, expression("T(x,y)"~+~eta*"(x,y)"),
   pos = 4)
lines(c(10,12.5),c(0,0),lwd = 2, col = "dark green")
text(x = 12.5, y = -0.25, pos = 4,
   expression("T(x,y)"~+~eta*"(x,y)"~+~epsilon*"(x,y)"))
lines(c(10,12.5),c(-0.25,-0.25),lwd = 2, col = "red")


# Example of a plot with ggplot()
library(ggplot2)
Y.ggplot <- data.frame(x)
Y.ggplot$Yt <- Y.plot$trend
Y.ggplot$eps <- b.eps * Y.plot$eps
Y.ggplot$eta <- b.eta * Y.plot$eta
Y.ggplot$Yeta <- with(Y.ggplot, Yt + eta)
Y.ggplot$Y <- with(Y.ggplot, Yt + eps + eta)
ggplot(data = Y.ggplot) +
  geom_line(aes(x = x, y = Yt, color = "T(x,y)"), size = 1) +
  geom_line(aes(x = x, y = Yeta, color = "T(x,y)+n(x,y)"), size = 1) +
  geom_line(aes(x = x, y = Y, color = "T(x,y)+n(x,y)+e(x,y)"), size = 1) +
  theme(legend.position = c(0.75, 0.25)) +
  theme(legend.title = element_blank())

# --------------------------------------

# Perspective plots (Fig. 3.2)
# Plot using the function persp

x <- (1:20) + 1.5
y <- x

# Original data (Fig. 3.2a)
Y.persp <- matrix(4*Y.df$Y, nrow = 20)
persp(x, y, Y.persp, theta = 225, phi = 15,
  scale = FALSE, zlab = "Y")  #Fig. 3.2a

# Trend T(x,y) (Fig. 3.2b)
Trend <- matrix(4*Y.df$trend, nrow = 20)
persp(x, y, Trend, theta = 225, phi = 15, scale = FALSE,
   zlab = "T(x,y)")  #Fig. 3.2b

# Fit linear trend to logistic model (Fig. 3.2c)
# Create the data with threecomponent.r
model.lin <- lm(Y ~ x + y, data = Y.df)
coef(model.lin)
trend.lin <- predict(model.lin)
Y.lin <- matrix(trend.lin, nrow = 20)
Fit <- 4 * Y.lin
persp(x, y, Fit, theta = 225, phi = 15, scale = FALSE)

# Median polish trend  (Fig. 3.2d)
Y.trend <- matrix(Y.df$Y, nrow = 20)
model.mp <- medpolish(Y.trend)
Y.mp <- Y.trend - model.mp$residuals
Fit <- 4 * Y.mp
persp(x, y, Fit, theta = 225, phi = 15, scale = FALSE)

# ---------------------------------------------

#Trend plus random component for Field 4.1

# Read data using code in Appendix B.4
data.Set4.1 <- read.csv("Set4\\set4.196sample.csv", header = TRUE)
Sand <- matrix(nrow = 13, ncol = 7)
for (i in 1:86){
  Sand[data.Set4.1$Row[i],data.Set4.1$Column[i]] <-
    data.Set4.1$Sand[i]
}
North <- 3 * 1:13
West <- 3 * 1:7
persp(North, West, Sand, theta = 30, phi = 20,
  zlim = c(0,45), scale = FALSE)  # Fig. 3.3a
  
# Linear model (quadratic)
trend.lm <- lm(Sand ~ Row + Column + I(Row^2) +
  I(Column^2) + I(Row*Column), data = data.Set4.1)
b <- coef(trend.lm)
Trend <- matrix(nrow = 13, ncol = 7)
for (i in 1:8){
   for (j in 1:7){
      Trend[i,j] <- b[1] + b[2]*i + b[3]*j + b[4]*i^2 + b[5]*j^2 + b[6]*i*j
}}
for (i in 9:13){
   for (j in 1:6){
      Trend[i,j] <- b[1] + b[2]*i + b[3]*j + b[4]*i^2 + b[5]*j^2 + b[6]*i*j
}}
persp(North, West, Trend, theta = 30, phi = 20,
  zlim = c(0,45), scale = FALSE) # Fig. 3.3b


#Median polish for Field 4.1
#Break into 2 parts, one with 7 columns and one with 6
Sand1 <- Sand[1:8,]
Sand2 <- Sand[9:13,1:6]
trend1 <- medpolish(Sand1)
trend2 <- medpolish(Sand2)
T1.mp <- matrix(nrow = 8, ncol = 7)
for (i in 1:8){
   for (j in 1:7){
      T1.mp[i,j] <- trend1$row[i] + trend1$col[j] + trend1$overall
}}
T2.mp <- matrix(nrow = 5, ncol = 7)
for (i in 1:5){
   for (j in 1:6){
      T2.mp[i,j] <- trend2$row[i] + trend2$col[j] + trend2$overall
}}
MedPolish <- rbind(T1.mp,T2.mp)
persp(North, West, MedPolish, theta = 30, phi = 20, zlim = c(0,45),
  scale = FALSE)   # Fig. 3.3c
  
# --------------------------------------------

# Bubble plots of Field 4.1 sand content
#Fig. 3.4a
library(sf)
# The shapefile set419697bdry.shp is created in Exercise 2.11
Set4.1.bdry.sf <- st_read("created\\set419697bdry.shp")
st_crs(Set4.1.bdry.sf) <- "EPSG:32610"
data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
data.Set4.1.sf <- st_as_sf(data.Set4.1, 
  coords = c("Easting", "Northing"), crs = "EPSG:32610")
  
library(tmap)
par(mai = c(1,1,1,1))
tm_shape(Set4.1.bdry.sf) +    #Fig. 3.4a
   tm_layout(main.title = "Field 4.1 Sand Content") +
   tm_borders("black") +
   tm_shape(data.Set4.1.sf) +
      tm_symbols(col = "white", size = "Sand", scale = 1,
         perceptual = TRUE) +
   tm_legend(outside = TRUE, outside.position = "right",
      legend.stack = "vertical", title = "Percent Sand") +
      tm_grid(lwd = 0.1, x = seq(592100, 592450, by=150),
          y=seq(4270400, 4271100, by=200), col = "white") +
      tm_xlab("Easting") + tm_ylab("Northing")

i <- data.Set4.1$Row
j <- data.Set4.1$Column
data.Set4.1.sf$lin <- data.Set4.1$Sand - (b[1] + b[2]*i + b[3]*j +
  b[4]*i^2 + b[5]*j^2 + b[6]*i*j)
data.Set4.1.sf$Linear <- data.Set4.1.sf$lin / 10 + 1
tm_shape(Set4.1.bdry.sf) +    #Fig. 3.4b
   tm_layout(main.title = "Linear Detrending") +
   tm_borders("black") +
   tm_shape(data.Set4.1.sf) +
      tm_symbols(col = "white", size = "Linear", scale = 1,
         perceptual = TRUE) +
   tm_legend(outside = TRUE, outside.position = "right",
      legend.stack = "vertical", title = "Percent Sand") +
      tm_grid(lwd = 0.1, x = seq(592100, 592450, by=150),
          y=seq(4270400, 4271100, by=200), col = "white") +
      tm_xlab("Easting") + tm_ylab("Northing")

 
# Concatenate the columns of T1.mp into a vector
stack(data.frame(T1.mp))[,1] # Check that the concat. is correct
data.Set4.1.sf$mp <- data.Set4.1.sf$Sand -
   c(stack(data.frame(T1.mp))[,1],stack(data.frame(T2.mp))[1:30,1])
data.Set4.1.sf$MedianPolish <- data.Set4.1.sf$mp / 10 + 1
tm_shape(Set4.1.bdry.sf) +    #Fig. 3.4a
   tm_layout(main.title = "Median Polish") +
   tm_borders("black") +
   tm_shape(data.Set4.1.sf) +
      tm_symbols(col = "white", size = "MedianPolish", scale = 1,
         perceptual = TRUE) +
   tm_legend(outside = TRUE, outside.position = "right",
      legend.stack = "vertical", title = "Percent Sand") +
      tm_grid(lwd = 0.1, x = seq(592100, 592450, by=150),
          y=seq(4270400, 4271100, by=200), col = "white") +
      tm_xlab("Easting") + tm_ylab("Northing")

