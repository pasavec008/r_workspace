library('readxl')
getwd()
setwd('C:/R/workspace')
data <- read_excel('statistika/dataE.xlsx')

# Prvotna statisticka analyza dat
# Uzitocne prikazy pri predpriprave dat
View(data)
dim(data) #rozmer dat
str(data) #struktura dat
head(data, 3) # ina struktura dat, prvych x
#install.packages('Amelia')
library(Amelia)
missmap(data) # aby sme pozreli, ktore data chybaju
missmap(airquality)
airquality$Ozone
aa <- na.omit(airquality)
missmap(aa)

# vhodne kniznice na analyzu
#install.packages('psych')
#install.packages('Hmisc')
#install.packages('FSA')
#install.packages('pastecs')
#install.packages('moments')
library(psych)
library(Hmisc)
library(FSA)
library(pastecs)
library(moments)

# Charakteristiky polohy pre stlpec mprij
plat <- data$mprij
min(plat)
max(plat)
mean(plat)
median(plat)
find_mode <- function(x){
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
find_mode(plat) # modus, najcastejsia hodnota
quantile(plat) # kvartily
quantile(plat, 0.5) # median
quantile(plat, 0.1) # 10% je do tohto cisla
# prikazy pre harmonicky a geometricky priemer
harmonic.mean(plat)
geometric.mean(plat)
# ine uzitocne prikazy
sort(plat, decreasing = T) # usporiadanie
(sort(plat)[25] + sort(plat)[26])/2 #median
# sucet ludi s danou vlastnostou
sum(data$pohlavie == 'z')
sum(data$vek <= 40)
sum(data$mprij > 1000)
# Tabulka pocetnosti a relativnych pocetnosti
table(plat)
histogram(plat, col = 'red')
prop.table(plat)
cumsum(plat) #kumulativne pocetnosti

# Charakteristiky rozptylu
max(plat) - min(plat) # variacne rozpatie
range(plat)
var(plat) # odhad disperzie
sd(plat) # smerodajna odchylka
IQR(plat) # treti kvartil - prvy kvartil
IQR(plat) / 2 # kvartilova odchylka
# mame charakteristiky take, ze mozeme zistit, ci su v datovom subore
# vybocujuce hodnoty -> outliers
mean(plat) + 3 * c(-sd(plat), sd(plat)) # pravidlo 3 sigma
quantile(plat, 0.25) - 1.5 * IQR(plat) # dolna hranica
quantile(plat, 0.75) + 1.5 * IQR(plat) # horna hranica
# hodnotu 1950 mozno pri tejto metode povazovat za vybocujucu
# Sikmost a spicatost
# prikazy su vo viacerych knizniciach, pouzit moments
skewness(plat) # sprava mierne zosikmeny
skew(plat)
hist(plat) # vizualne overim
plot(density(plat))
kurtosis(plat)
kurtosi(plat) #centrovana hodnota -3
# nasimulujeme data symetricke a sice N(0, 1) a data nesymeytricke
# z rozdelenia Chi kvadrat, spocitame sikmost a spicatost
s1 <- rnorm(100) # symetricke
s2 <- rchisq(100, 5)
skewness(s1)
skewness(s2)
kurtosis(s1)
kurtosis(s2)
par(mfrow=c(1, 2)) # na rozdelenie grafov
plot(density(s1))
plot(density(s2))
# sumarne statistiky
describe(plat)
stat.desc(plat)
Summarize(plat)
# vypocet charakteristik na podmnozinach danej mnoziny,
# podla deliaceho faktoru (nejakeho zvoleneho)
tapply(data$mprij, data$pohlavie, mean)
tapply(data$mprij, data$pohlavie, describe)
tapply(data$mprij, data$vzdelanie, mean)
# grafy statistickej analyzy
library(ggplot2)
library(car)
library(carData)
library(MASS)
library(RColorBrewer)
#install.packages('vioplot')
library(vioplot)

# histogramy, krabicove boxplot grafy, huslove grafy
# grafy
par(mfrow = c(1, 1)) # rozdelit okno grafov na 1 cast
hist(plat)
histogram(plat) # rozne obrazky histogramov, aj ked su to tie iste data,
# to kvoli roznej dlzke tried
hist(plat, breaks = 'Sturges', main = 'Histogram', ylab = 'pocetnosti')
par(mfrow = c(1, 2))
muzi <- subset(data$mprij, data$pohlavie == 'm')
zeny <- subset(data$mprij, data$pohlavie == 'z')
hist(muzi, ylim = c(0, 12))
hist(zeny, ylim = c(0, 12))
# alebo inak, druha moznost
hist(data$mprij~data$pohlavie)
# Krabicove grafy - boxploty
boxplot(plat)
boxplot(data$mprij ~ data$pohlavie, col = c('blue', 'red'))
boxplot(data$mprij ~ data$vzdelanie, col = brewer.pal(3, 'Greens'))
boxplot(data$mprij~data$pohlavie + data$vzdelanie, col = c('blue', 'red'))

# Huslove grafy - vioplot
vioplot(plat)
vioplot(data$mprij ~ data$pohlavie, col = c('blue', 'red'))
vioplot(data$mprij ~ data$vzdelanie, col = brewer.pal(3, 'Greens'))
vioplot(data$mprij~data$pohlavie + data$vzdelanie, col = c('blue', 'red'))
