# This continues from the file 3.6.2.r

# Estimate lambda
nlist <- poly2nb(thsn.spdf,
                 row.names = as.character(thsn.spdf$ID), queen = FALSE)

W <- nb2listw(nlist, style = "W")
Y.mod <- errorsarlm(SandDT ~ 1, data = thsn.spdf, listw = W)
print(Y.mod$lambda, digits = 4)

W <- nb2listw(nlist, style = "B")
Y.mod <- errorsarlm(SandDT ~ 1, data = thsn.spdf, listw = W)
print(Y.mod$lambda, digits = 4)

nlist <- poly2nb(thsn.spdf,
                 row.names = as.character(thsn.spdf$ID), queen = TRUE)

W <- nb2listw(nlist, style = "W")
Y.mod <- errorsarlm(SandDT ~ 1, data = thsn.spdf, listw = W)
print(Y.mod$lambda, digits = 4)

W <- nb2listw(nlist, style = "B")
Y.mod <- errorsarlm(SandDT ~ 1, data = thsn.spdf, listw = W)
print(Y.mod$lambda, digits = 4)

