# Preliminary exploration of Data Set 2 (California oaks)
#library(maptools)
library(sf)
library(car) # For crplots()

data.Set2S <- st_read("created\\set2sierra.shp")

glm.demo <- glm(QUDO ~ Elevation, data = data.Set2S,
   family = binomial)
summary(glm.demo)

par(mai = c(1,1,1,1))
plot(data.Set2S$Elevation, data.Set2S$QUDO, # Fig. 8.8
   main = "Sierra Nevada Blue Oaks vs. Elevation",
   cex.main = 1.5, xlab = "Elevation (m)",
   ylab = "Blue Oak Presence/Absence", cex.lab = 1.5)
elev.seq <- data.frame(Elevation = seq(0,2000,50))
lines(seq(0,2000,50), predict(glm.demo, elev.seq,
   type = "response"))


# Artificial data set
set.seed(123)
X <- cbind(rnorm(400), rnorm(400))
p <- 1 / (1 + exp(-rowSums(X)))
Y <- rbinom(numeric(length(p)), 1, p)
model.lin <- glm(Y ~ X[,1] + X[,2], family = binomial)
crPlots(model.lin, col.lines = c("black", "black"), # Fig. 8.9a
   main = expression(Partial~Residual~Plots~"for"~italic(X)[2]))

set.seed(123)
X <- cbind(rnorm(400), rnorm(400))
X <- cbind(X, X[,2]^2)
p <- 1 / (1 + exp(-rowSums(X)))
Y <- rbinom(numeric(length(p)), 1, p)
model.lin <- glm(Y ~ X[,1] + X[,2], family = binomial)
crPlots(model.lin, col.lines = c("black", "black"), # Fig. 8.9b
   main = expression(Partial~Residual~Plots~"for"~italic(X)[2]))



