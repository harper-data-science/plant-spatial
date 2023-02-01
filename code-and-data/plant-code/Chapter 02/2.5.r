z <- c(10,2,3,21,5,72,18,25,34,33)
print(ztrim <- sort(z))

trim <- 0.1
print(tl <- as.integer(trim * length(ztrim)))
# Trim from the low end
print(ztrim <- ztrim[-(1:tl)])
# Trim from the high end
th <- length(ztrim) - tl
print(ztrim <- ztrim[-((th + 1):length(ztrim))])

print(zsd <- sd(ztrim))


# Function to compute the trimmed standard deviation
sd.trim <- function(z, trim){
# trim must be between 0 and 0.5 or this won't work
# Sort the z values
  ztrim <- sort(z)
# Calculate the number to remove
  tl <- as.integer(trim * length(ztrim))
# Remove the lowest values
  ztrim <- ztrim[-(1:tl)]
# Remove the highest values
  th <- length(ztrim) - tl
  ztrim <- ztrim[-((th + 1):length(ztrim))]
# Compute the standard deviation
  zsd <- sd(ztrim)
  return(zsd)
}

sd.trim(z, 0.1)


a <- matrix(c(1,2,3,4), nrow = 2)
a
diag(a)

v <- c(1, 2)
v
diag(v)

to.the.n <- function(w) w^n
n <- 3
to.the.n(2)

better.to.the.n <- function(w, n) w^n
n <- 3
better.to.the.n(2,4)

