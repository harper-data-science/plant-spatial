w <- numeric(30)
z <- numeric(30)
for(i in 1:30){
   w[i] <- i
   z[i] <- 30 - i + 1
}
w

w <- 1:10
for (i in 1:10){
   {if(w[i] > 5)
      w[i] <- 20
   else
      w[i] <- 0}
}
w           


w <- 1:10
w > 5
w[w > 5]
w[w > 5] <- 20
w[w <= 5] <- 0
w
