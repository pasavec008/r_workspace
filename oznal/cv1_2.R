x <- 1:4
y <- 10:20
y*2



y[y>15]
subset(y, y>15)

ticker <- c('AVAX', 'TSLA', 'MCSFT')
price <- c('120.0', '69', '70')
stocks <- data.frame(ticker, price)
stocks
stocks$buy <- c('yes', 'no', 'no')
stocks

27 -> z
z

xx <- c(1, TRUE, 'Ahoj')
class(xx)

x <- 1:4
y <- 10:20
x*2
z <- x * y
z
sum(z)

integer(length(x))
rep(1:5, 1:5)

str(z)
z
z[z>40]
stocks <- c('AVAX', 'BTC', 'ETH', 'AVAX')

stocks[2] <- NA
stocks
stocks[!is.na(stocks)]
