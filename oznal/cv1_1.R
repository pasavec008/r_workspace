a <- 1:10 # 1.
b <- a # 2.
a[2] <- 4 # 3.
c <- b
d <- 1:10

tracemem(a)
tracemem(b)
tracemem(c)
tracemem(d)
