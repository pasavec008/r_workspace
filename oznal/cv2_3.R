k <- c(1, 5, 7, 9)
k
class(k)
as.list(k)
c(k, 'c')
c(as.list(k), 'c')

o <- list(1, 2, 3)
oo <- o
tracemem(o); tracemem(oo)
tracemem(o[1]); tracemem(oo[1])
tracemem(o[2]); tracemem(oo[2])
tracemem(o[3]); tracemem(oo[3])

oo[3] <- 4

tracemem(o); tracemem(oo)
tracemem(o[1]); tracemem(oo[1])
tracemem(o[2]); tracemem(oo[2])
tracemem(o[3]); tracemem(oo[3])

o[1]
o[2]
o[3]

# dataframes are rectangular (vectors in them must be the same length) .. that is
# the difference between them and lists

# A data frame has rownames(), colnames(). When names() are used on a data frame then the column names are returned.
# A data frame has nrow() rows and ncol() columns. dim() returns a vector containing the number of rows and columns.

install.packages('tidyverse')
library(tidyverse)

head(mtcars)
as_tibble(mtcars)
rownames(as_tibble(mtcars))

# [ ] for subseting list always returns smaller list .. that s a problem when we
# need simple vector .. so we use [[ ]] instead
# If list x is a train carrying objects, then x[[5]] is the object in car 5; x[4:6] is a train of cars 4-6.

x <- list(10, '20', 30)
class(x[1])
class(x[[1]])
class(x[2])
class(x[[2]])
x[1]
x[[1]]

mtcars
mtcars[1:20,]

# fix these
mtcars[mtcars$cyl == 4,]
mtcars[1:4, ]
mtcars[mtcars$cyl <= 5,]
mtcars[mtcars$cyl %in% c(4, 6), ]


#Why does x <- 1:5; x[NA] yield five missing values?
x <- 1:5; x[NA]
x
x[NA_real_]
#because if we try to subset NA, it is always NA


l1 <- list(1,2,3)
l2 <- l1
l2[3] <- 4
l2

class(l1[1])
l1[1]
class[l1[[1]]]

l2[2][2]
l2[2][2][2]
l2[[2]][2]
class(l2[2][2])
class(l2[2][[2]])


# Create a 3x3 matrix
m <- matrix(1:9, nrow = 3)
m
# Extract the upper triangular part of the matrix
upper_tri <- m[upper.tri(m)]

# Print the upper triangular part
upper_tri

m[lower.tri(m)]