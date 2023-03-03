m <- matrix(nrow = 0, ncol = 3, byrow = TRUE) # Build an empty matrix with 3 columns and no rows. 
set.seed(123)
for (i in 1:5) m <- rbind(m, rnorm(n = 3, mean = i))
colnames(m) <- c("Alpha", "Beta", "Gamma")
rownames(m) <- c("Alpha", "Beta", "Gamma", "Delta", "Epsilon") ; m

m[c(1, 3, 5), 1:2]
m[m > 5]
m
m[c(TRUE, FALSE, FALSE, FALSE, TRUE), 2]

