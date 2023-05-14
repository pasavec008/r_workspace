library(latexpdf)
#'# Analyza reziduj a vplyvnych bodov regresie
#'reziduu je vlastne chyba, ktorej sa dopustime ak skutocne
#'nameranu hodnotu nahradime odhadnutou z modelu. Toto nazyvame
#'klasicke reziidum. Zacneme prikladom
#'Certifikacny urad vykonal 11 nahodnych merani na rovnakych obrabacich
#'strojoch s cielom zistit interval pravidelnej kalibracie.
#'Merala sa odchylka od optimalneho nastavenia (%) a doba prevadzky
#'(v mesiacoch) od poslednej kalibracie. Spocitajte korelacne koeficienty,
#'prelozte vhodny regresny model, analyzujte rezidua, zistite vplyvne body
#'regresie
doba <- c(8, 3, 6, 7, 10, 14, 8, 2, 16, 5, 6)
odchylka <- c(0.15, 0.08, 0.3, 0.25, 0.6, 1.05, 0.9, 0.09, 1.15, 0.35, 0.15)

korelacie <- function(x, y){c('Pearson' = cor(x, y),
                              'Kendall' = cor(x, y, method = 'kendall'),
                              'Spearman' = cor(x, y, method = 'spearman'))}
korelacie(doba, odchylka)
plot(doba, odchylka, type = 'b')
m1 <- lm(odchylka~doba)
summary(m1)
#' lepsi model bude, ktory respektuje ze pre dobu cinnosti 0 je aj odchylka 0
m2 <- lm(odchylka~0+doba)
summary(m2)
#abline(m1, col='red')
#abline(m2, col='blue')
#' Este pozrime informacne kriteria, lepsi model, nizsia hodnota
AIC(m1, m2) # lepsi je druhy
#' Vypiseme si rezidua
m2$residuals
#' Analyzujeme najprv graficky, pomocou 4 grafov, ktore vysvetlime
par(mfrow = c(2, 2))
plot(m2)
#' prvy graf, nahodnost rezidui, mali by byt nahodne rozdelene okolo
#' priamky y = 0
#' Druhy graf je o normalite rezidui, idealne by mali lezat na naznacenej
#' priamke, 1, 7, 11
#' Treti graf - konstantnost rozptylu, homoskedasticita, oznacene body 1, 7, 11
#' tiez budeme testovat vhodnym testom
#' Detekovanie vplyvnych bodov regresie (1, 7, 9)
#' Rataju sa aj ine rezidua - standardizovane, predikovane, studentizovane
residuals(m2)
rstandard(m2)
rstudent(m2)
#' ideme overovat postupne vlastnosti rezidui
#' Nahodnost
library(randtests)
turning.point.test(m2$residuals)
#' Su nahodne, lebo p hodnota > 0.05
#' test autokorelovanosti
library(car)
durbinWatsonTest(m2)
#' Nulova hypoteza je, ze su nekorelovane, Phodnota > 0.05 takze
#' nezamietame nulovu hypotezu.
#' Ideme overit ci su data normalne rozdelene s nulovou strednou
#' hodnotou
library(nortest)
shapiro.test(m2$residuals)
#' Nezamietam hypotezu o normalite rezidui
t.test(m2$residuals)
#'p hodnota > 0.05, teda nezamietame hypotezu o nulovosti strednej hodnoty.
#'Ostava este overit konstantnost rozptylu, homoskedasticitu
library(lmtest)
gqtest(odchylka~0+doba)
#' P-hodnota > 0.05, nezamietame hypotezu o konstantnosti rozptylu
#' rezidui
#'# Vplyvne body regresie
#'napr podla Cook distancies, vacsie ako 1 vplyvny bod
cooks.distance(m2) # nie su vplyvne body
library(olsrr)
ols_plot_cooksd_chart(m2) # nie su vplyvne body
ols_plot_resid_stand(m2) # nie su vybocujuce
ols_plot_resid_stud(m2) # nie su vybocujuce
ols_plot_resid_lev(m2) # outliers 1, 7, lever. 6, 9
#' meranie 7 sa casto vyskytuje ako problemove, je tam najvacsia hodnota
#' rezidua, skusime ju vyhodit, prepocitat regresiu, vyhodnotit kvalitu
#' s tym ale opatrne, ak mame maly pocet merani
odchylka1 <- odchylka[-c(7)]
doba1 <- doba[-c(7)]
m3 <- lm(odchylka1~0+doba1)
AIC(m1, m2, m3)
summary(m3)
plot(doba1, odchylka1, xlim=c(0, 17))
#abline(m3, col='red')
ols_plot_resid_lev(m3)
