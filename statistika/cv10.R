library(latexpdf)
#'# Regresna analyza pokracovanie
#' zacneme prikladom, ktory bol minule o kapacite a cien diskov.
#' Prelozime regresnu priamku, vysvetlime charakteristiky, ktore
#' sa roluju prikazom summary, funkcia y = a + bx

x <- c(160, 250, 320, 500, 750, 900, 1000, 1500, 2000)
y <- c(789, 800, 851, 874, 1193, 1200, 1335, 1704, 2073)
korelacie <- function(x, y){c('Pearson' = cor(x, y),
                              'Kendall' = cor(x, y, method = 'kendall'),
                              'Spearman' = cor(x, y, method = 'spearman'))}
korelacie(x, y)
plot(x, y, type = 'b', xlab = 'kapacita', ylab = 'cena', col = 'blue')
lm1 <- lm(y~x)
summary(lm1)
#abline(lm1)

#' Prva cast analyza rezidualov, iba charakteristiky polohy. Analyza
#' rezidualov je dolezita v regresii. Regresnu funkciu mozeme prelozit
#' bez obmedzeni (bez podmienok kladenych na data), ale ak chceme
#' testovat a vyhodnotit testy, tak ziadame, aby rezidualy boli normalne
#' rozdelene, s nulovou strednou hodnotou, nekorelovane a s konstantnou
#' disperziou (homoskedasticita)
#' Druha cast: tabulka koeficientov obsahuje odhady parametrov, statistiky,
#' ktore vstupuju do nasledujucich testov a P hodnoty,  ktore sluzia na
#' vyhodnotenie testov.
#' Pre prvy riadok (a ostatne) hypoteza je, ze odhadnuty koeficient sa
#' rovna nule $H_)\quad a=0$. Ak koeficient sa rovna nule, nezamietam H0,
#' tak funkcia pri koeficiente do modelu nepatri, nepatri medzi bazove
#' funkcie (tak sa to hovori). Vynechame z bazovych funkcii, prepocitame
#' regresiu, P hodnota < 0.05, zamietame H0 pre prvy aj druhy riadok.
#' Konstanta aj linearny clen patria medzi bazove funkcie
#' Residual standard error - odhad smerodajnej odchylky chyb, cim mensie
#' cislo, tym lepsie, hovori o tom ako blizko alebo ci rozptylene hodnoty
#' daleko od regresnej funkcie
#' R Squared - koeficienty determinacie, cim blizsie k 1, tym lepsie,
#' ak ho prenosbime 100%, tak vieme na kolko % vysvetluje nas model
#' spravanie sa dat.
#' Posledny riadok F statistika pre ANOVA test, nulova hypoteza je, ze
#' Y (zavisla premenna) staci popisat nejakou konstantou, teda nie nasim
#' modelom. V poslednom riadku je aj P hodnota, ak P hodnota je mensia
#' ako 0.05, zamietam hypotezu H0 a teda regresny model je vhodny.
#' Ak mame k dispozicii viac modelov, tak na vyber najlepsieho sluzia
#' hodnoty informacnych kriterii AIC, BIC. Pomocou nich sa vyberie
#' optimalny model, ktory nie je preparametrizovany (penale za velky
#' pocet parametrov) a ani nedoparametrizovany (penale za velku disperziu)
AIC(lm1)
BIC(lm1)
#' porovnajme s modelom y = bx
lm2 <- lm(y~0+x)
AIC(lm1, lm2) #nizsie cislo, lepsi model
#' Dalsie charakteristiky
lm1$coefficients #odhady parametrov
lm1$fitted.values #odhady y pre namerane xi
lm1$residuals
plot(lm1$residuals, type = 'b')
##################################
#' Prikald v 10 vzdialenostiach xi sa merala velkost priehybu zatazenia
#' dosky yi, vysledky su dane, prelozte vhodnu regresnu krivku
xx <- 1:10
yy <- c(2.1299,2.1532,2.1611,2.151,2.1282,2.0807,
        2.0266,1.9594,1.8759,1.7723 )
plot(xx, yy, type = 'b', xlab = 'vzdialenost', ylab = 'priehyb', col = 'purple')
#' prelozime kvadraticku funkciu, vyhodnotime kvalitu modelu, mozeme
#' vykreslit aj rezidualy
lm3 <- lm(yy~poly(xx, 2))
#lines(lm3$fitted.values, type = 'b', col = 'red')
summary(lm3)
#' Kazda z bazovych funkcii patri do mnoziny bazovych funkcii, zamietame
#' hypotezu, ze koeficienty pri funkciach su nulove, odhad smerodajnej
#' odchylky je minimalny, koeficienty determinacie su skoro jedna - kvalitny
#' model.
#' ANOVA test o vhodnosti modelu ako celku, zamietame hypotezu, ze model
#' nema zmysel
#' Vykreslime rezidualy
lm3$residuals
plot(lm3$residuals, type = 'b')
library(randtests)
turning.point.test(lm3$residuals)
##############################################
#' Transformacia na linearnu regresiu
#' Po jednoduchej transformacii mozno pouzit metody linearnej regresie,
#' najst odhady parametrov, spatne transformovat.
#' Priklad barometricky tlak p (v Pascaloch) zavisi od nadmorskej vysky
#' h (m). $y=ae^{bx}$. Namerali sme 6 hodnot tlaku v roznych nadmorskych vyskach.
#' Prelozte danu regresnu krivku.

h <- c(0,270,840,1452,2116,3203)
p <- c(100000,96974,90263,83553,76842,66842)

korelacie(h, p)
plot(h, p, type = 'b', xlab = 'vyska', ylab = 'tlak')
#' regresia po transformacii dat
lm4 <- lm(log(p)~h)
summary(lm4)
#' Spatna transformacia pre parametre
a <- exp(lm4$coefficients[1])
a
b <- lm4$coefficients[2]
b
pp <- a * exp(b*h)
pp
#lines(h, pp, type = 'b', col = 'red')
#########################################
#' Regresna analyza s viac ako jednou nezavislou premennou
#' Vysvetlujucich premennych moze byt viac
#' Pokusime sa namodelovat umrtia zz v prvej polovici decembra 52
#' v Londyne pomocou dvoch premennych xx, yy znecistenie vzduchu

zz <- c(112,140,143,120,196,294,513,518,430,274,
        255,236,256,222,213)
xx <- c(0.3,0.49,0.61,0.49,2.64,3.45,4.46,4.46,1.22,
        1.22,0.32,0.29,0.5,0.32,0.32)
yy <- c(0.09,0.16,0.22,0.14,0.75,0.86,1.34,1.34,0.47,0.47,
        0.22,0.23,0.26,0.16,0.16)
#' najprv spojnicove grafy
plot(zz, type = 'b', xlab = 'cas', ylab = 'hodnota', col = 'blue',
     ylim = c(0, 600))
#lines(xx*100, type = 'b', col = 'red')
#lines(yy*100, type = 'b', col = 'green')
lm5 <- lm(zz~xx+yy)
summary(lm5)
#' Nezavislych premennych moze byt viac a mame vybrat len tie, ktore
#' naozaj vysvetluju zavislu premennu, navyse sa nezavisle mozu aj
#' ovplyvnovat (kolinearita, setrit). Zakladny postup na vyukovych
#' datach z kniznice datarium, marketing, su to vydaje na reklamu
#' v mediach youtube, facebook, newspaper a k tomu objem predaja sales
library(datarium)
head(marketing)
#' Najprv grafy, aby sme odhalili strukturu dat, teraz nas zaujimaju
#' iba dvojice sales+ vzdy kazda nezavisla
pairs(marketing)
#' korelacie a grafy
library('corrplot')
k <- cor(marketing)
corrplot.mixed(k)
#' regresna funkcia, vyhodnotenie kvality
lm6 <- lm(sales~., data=marketing)
summary(lm6)
#' Vidime, ze premennu newspaper mozno vylucit z mnoziny bazovych
#' premennych, prepocitame regresiu
lm7 <- lm(sales~youtube + facebook, data = marketing)
summary(lm7)
AIC(lm6, lm7)
#' Kedze nezavislych premennych moze byt viac a nie vsetky patria
#' do regresie, preto je vhodne pouzit prikaz standardneho balika step,
#' predtym regresia pre vsetky premenne
step(lm6)
#' este mame k dispozicii aj napr. kniznicu leaps
library(leaps)
vyber <- regsubsets(sales~., marketing)
summary(vyber)
vyber2 <- regsubsets(sales~., marketing, nbest = 2)
summary(vyber2)$bic
#' Najlepsi je opat treti (najmensie cislo v bic), teda youtube + facebook