library(latexpdf)
#'# Dvojrozmerny datovy subor, korelacna analyza
#' Analyzujeme dvojrozmerny datovy subor. Ratame charakteristiky pre
#' jednorozmerne subory, pribudnu charakteristiky pre dvojrozmerny subor.
#' Priklad. Tabulka uvadza cenu pevnych diskov (y) ich kapacitu (x)
#' od vyrobcu ABC. Zratajte vektor strednych hodnot a disperzii.
x <- c(160, 250, 320, 500, 750, 900, 1000, 1500, 2000)
y <- c(789, 800, 851, 874, 1193, 1200, 1335, 1704, 2073)
c(mean(x), mean(y))
c(var(x), var(y))
#' Dalej ratame charakteristiku dvojrozmerneho datoveho suboru kovarianciu
#' (Personova, Kendallova, Spearmanova). Je to cislo, ktore nam charakterizuje,
#' ci je medzi zlozkami linearna zavislost alebo nie, ci koreluju. Kym
#' neznormujeme toto cislo, tak mozeme povedat iba jedine, cov = 0 zlozky su
#' nekorelovanea ako cov != 0 su korelovane.
plot(x, y, type = 'b')
cov(x, y) #korelovane
cov(x, y, method = 'kendall')
cov(x, y, method = 'spearman')
#' Aby sme zistili, ako silno sa ovplyvnuju, je potrebne nromovat kovarianciu,
#' dostaneme korelacny koeficient, co je cislo medzi -1 a 1. Blizke -1, 1 silna,
#' silna korelacia, silna linearna zavislost. Blizke 0, nekorelovane a linearne
#' nezavisle iba pre normalne rozdelene data. Zlozitejsie zavislosti sa
#' nemusia odhalit. Pearsonov korelacny koeficient je citlivy na vybocujuce
#' hodnoty, vsetky naraz ratat.Pripravime funkciu korelacie.
cor(x, y)
korelacie <- function(x, y){c('Pearson' = cor(x, y),
                              'Kendall' = cor(x, y, method = 'kendall'),
                              'Spearman' = cor(x, y, method = 'spearman'))}
korelacie(x, y)
#' Testovat korelacny koeficient mozeme, dvojrozmerne data ale musia byt
#' normalne rozdelene, testujeme hypotezu o nulovosti korelacneho koeficientu.
#' $$H_0\quad \rho=0\quad H_1\quad \rho \neq 0$$
#' kniznice pre test normality
library(nortest)
library(mvnormtest)
#' uprava dat, dvojrozmerny subor
data <- rbind(x, y)
mshapiro.test(data)
#' P hodnota > 0.05 nezamietam hypotezu o normalite dat mozeme testovat
#' nulovost korelacneho koeficienta
cor.test(x, y) #zamietam H0
cor.test(x, y, method = 'kendall') #zamietam H0
cor.test(x, y, method = 'spearman') #zamietam H0
#' graf s viac informaciami, nacitame kniznice
library(ggplot2)
library(ggpubr)
data1 <- data.frame(x, y)
ggscatter(data1, 'x', 'y',
          add = 'reg.line',
          conf.int = T,
          cor.coef = T)
#' Nasiumulujeme linearne nezavisle, linearne zavisle a data so zlozitejsou
#' zavislostou, grafy a korelacne koeficienty
xx <- rnorm(100, 2, 4)
yy <- rnorm(100, 1, 1)
plot(xx, yy, main = 'Nezavisle data')
korelacie(xx, yy)
xxx <- 1:100+xx
yyy <- 2+3*xxx+rnorm(100, 2, 6)
plot(xxx, yyy, main = 'Zavisle data')
korelacie(xxx, yyy)
x1 <- 1:100
y1 <- sin(x1) + rnorm(100, 0, 0.05)
plot(x1, y1, main='Sin(x)', type = 'b')
korelacie(x1, y1)
#'# Korelacna analyza pre viacrozmerny vektor
#'Castejsie mame viacrozmerne merania, vtedy ratame korelacne koeficienty
#'vzdy medzi dvomi zlozkami, zapisujeme do matice, ktora sa nazyva korelacna
#'matica. Symetricka matica, na diagonale su jednotky. Vyukovy dataset
#'mtcars, carData, car
library(carData)
library(car)
head(mtcars)
View(cor(mtcars))
#' lepsie je to aj vizualizovat, kniznica corrplot
library(corrplot)
km <- cor(mtcars)
corrplot(km)
corrplot.mixed(km)
#' Testovat korelacne koeficienty mozno aj naraz Hmisc
library(Hmisc)
rcorr(as.matrix(mtcars))
#' Este jedna vhodna kniznica
library(correlation)
correlation(data1)
#' Grafy, ktore nam umoznia zistit strukturu dat, zavislosti v datach
#' Nase data dataE.xlsx
library(readxl)
data <- read_xlsx('dataE.xlsx')
head(data)
pairs(data[,c('vek', 'mprij', 'mstrava')])
korelacie(data$mprij, data$mstrava)
pairs(data[,c('vek', 'mprij', 'mstrava')], col='orange')
library(GGally)
ggpairs(data[,c('vek', 'mprij', 'mstrava')])
#' toto boli graficke analyzy pre cely datovy subor
#' Mozeme analyzovat aj podmnoziny datoveho suboru
ggpairs(data[,c('pohlavie', 'vek', 'mprij', 'mstrava')],
        aes(colour=pohlavie, alpha = 0.3))
#'# Regresna analyza
#'Ked korelacne koeficienty su vysoke, teda data su linearne zavisle, tak
#'nasleduje regresna analyza. Snazime sa zavislost popisat vhodnou
#'funkciou - regresna funkcia, priklad zo zaciatku, prelozime priamku y=a+bx
plot(x, y, type = 'b')
lr1 <- lm(y~x)
lr1
summary(lr1)
#' Multiple R-squared -> cim blizsie k 1, tym lepsi model -> 98% dat je 
#' vysvetlenych nasou priamkou
#abline(lr1, col = 'red')
#' Ak konstantu neuvazujeme
lr2 <- lm(y~0+x)
summary(lr2)
#abline(lr2, col = 'green')
#' prelozime kvadraticku funckiu
lr3 <- lm(y~poly(x, 2))
summary(lr3)
#lines(x, fitted(lr3), col = 'blue')
