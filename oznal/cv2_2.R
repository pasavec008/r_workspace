m <- matrix(nrow = 0, ncol = 3, byrow = TRUE) # Build an empty matrix with 3 columns and no rows. 
set.seed(123)
for (i in 1:5) m <- rbind(m, rnorm(n = 3, mean = i))
colnames(m) <- c("Alpha", "Beta", "Gamma")
rownames(m) <- c("Alpha", "Beta", "Gamma", "Delta", "Epsilon") ; m

m[c(1, 3, 5), 1:2]
m[m > 5]
m
m[c(TRUE, FALSE, FALSE, FALSE, TRUE), 2]


k <- data.frame(alpha = 1:7, beta = 2:8, gamma = 3:9)
k
k$alpha <- NULL
k

kk <- data.frame(x = 20:30, y = 30:40, z = 40:50, m = 10:20)
kk[c('x', 'z')] + 1

# to exclue one column
kk[setdiff(names(kk), 'z')]

t <- c('a', 'a', 'b', 'c')
lookupTable <- c(a = 'alpha', c = NA)
lookupTable[t]


x <- sample(1:5) ; x

9:1
mtcars
mtcars[11:1]
mtcars[2:5, 11:1]

#random sample from mtcars, but first one and the last one is the same
mtcars[c(1, sample(1:(nrow(mtcars)), 5), nrow(mtcars)),]

mtcars[order(row.names(mtcars)),]
