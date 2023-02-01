# Generate a sequence of 20 psuedorandom numbers
set.seed(123) # Set random seed
print(w <- rnorm(20), digits = 2)

print(z <- rep(1:4, 5))

print(z <- sort(z))

# Apply the mean function to w indexed by z
tapply(w, z, mean)

set.seed(123)
tapply(rnorm(20), sort(rep(1:4, 5)), mean)
