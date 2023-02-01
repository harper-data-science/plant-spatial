# Input and output are discussed in Section 2.4.2
data.Set1.obs <- read.csv("set1\\obspts.csv", header = TRUE)
str(data.Set1.obs)

data.Set4.1 <- read.csv("set4\\set4.196sample.csv", header = TRUE)
class(data.Set4.1)
names(data.Set4.1)
data.Set4.1$EM38F425[20:30]
data.Set4.1[20:30,25]
data.Set4.1[25,25]
data.Set4.1$RowCol[1:5]

class(data.Set4.1$GrainProt)
class(data.Set4.1$RowCol)

print(w <- c("A", "A", "A", "B", "B", "B"))
class(w)
print(z <- as.factor(w))
class(z)

list.demo <- list(crop = "wheat", yield = "7900", fert.applied =
  c("N", "P", "K"), fert.rate = c(150, 25, 15))
list.demo[[1]]
list.demo$crop
list.demo[[4]]
list.demo[[4]][1]

data.ex <- data.frame(Char = data.Set4.1$RowCol[1:3], Num = seq(1,5,2))
data.ex