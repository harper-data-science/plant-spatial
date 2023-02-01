data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
par(mai = c(1,1,1,1))
with(data.Set4.1, plot(SPAD,LeafN, cex.main = 1.5,
   main = "Leaf Nitrogen Content vs. SPAD Meter Reading",
   xlab = "SPAD Meter Reading", cex.lab = 1.5,
   ylab = "Leaf Nitrogen Content (Percent)")) # Fig. 11.1a
   
with(data.Set4.1, plot(SPAD,GrainProt, cex.main = 1.5,
   main = "Grain Protein Content vs. SPAD Meter Reading",
   xlab = "SPAD Meter Reading", cex.lab = 1.5,
   ylab = "Grain Protein Content (Percent)")) # Fig. 11.1b

data.Set2 <- read.csv("set2\\set2data.csv", header = TRUE)
with(data.Set2,
   matrix(c(sum(Elevation <= 1100 & QUDO == 1),
   sum(Elevation <= 1100 & QUDO == 0),
   sum(Elevation > 1100 & QUDO == 1),
   sum(Elevation > 1100 & QUDO == 0)), nrow = 2, byrow = TRUE,
   dimnames = list(c("Low", "High"), c("Pres", "Abs"))))

