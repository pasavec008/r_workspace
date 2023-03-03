x <- 1:10
x
attributes(x)
rownames(x)
colnames(x)

dim(x) <- c(5, 2)
x
dim(x) <- c(2, 5)
x

dimnames(x) <- list(c('a', 'b'), c('c', 'd', 'e', 'f', 'g'))
x
attributes(x)

dim(x) <- c(5, 2)
x

dimnames(x) <- list(c('c', 'd', 'e', 'f', 'g'), c('a', 'b'))
x

# byrow specifies how should be matrix filled
m <- matrix(ncol = 5, byrow = F)
m

set.seed(123)
for(i in 1:7)
  m <- rbind(m, rnorm(n = 5, mean = i));
m

n <- matrix(nrow = 7)
n

for(i in 1:10)
  n <- cbind(n, i:(i+6));
n

n[5,]

typeof(n)
class(n)

m
dim(m) <- NULL
mmm <- matrix(m, c(8, 5))
mmm[2,]

attributes(m)

help('array')
mm <- array(m, dim = c(8, 5))
mm

class(mm)

help('for')
help('rbind')

m_clean <- m[!is.na(m)]
m_clean
for(i in 1:5){
  if(i == 1)
    m_new <- m_clean[1:3]
  else
    m_new <- rbind(m_new, m_clean[(i*3-2):(i*3)])
}

m_new
