data.Set3 <- read.table("Set3\\Set3data.csv", header = TRUE, sep = ",")
with(data.Set3, cor(Yield, Silt))
Data <- with(data.Set3, data.frame(Yield, Silt))
Field <- data.Set3$Field
print(sort(by(Data, Field, function(x) cor(x)[1,2])[1:16]),
  digits = 2)
