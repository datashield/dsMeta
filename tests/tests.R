
# tests for the function est4comparison
a <- rbinom(1000, 1, 0.3)
b <- rbinom(1000, 1, 0.8)
est4comparisonDS(formula=a~b,data=NULL,vartype='binary')

c <- rnorm(1000, 0, 1.5)
d <- rnorm(1000, 0, 2)
est4comparisonDS(formula=c~d,data=NULL,vartype='qtl')