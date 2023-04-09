library('latexpdf')
#'# Dvojvyberove testy pre parametre normalneho rozdelenia
#' Dvojvyberove testy su
#' - parove pre zavisle merania, bolo
#' - neparove pre nezavisle merania
#' Testujeme hypotezu o rovnosti strednych hodnot, testy
#' treba vybrat podla toho, ci sa disperzie rovnaju alebo nerovnaju.
#' - test, ak disperzie sa rovnaju
#' - test, ak sa disperzie nerovnaju
#' Priklad 1
#' Dva druhy plastov (plastove okna) sme vystavili slnecnemu ziareniu
#' po dobu pol roka. Vysledky (odolnosti voci ziareniu v dnoch) sme
#' zaznamenali. Na hladine vyznamnosti $\alpha=0.05$. Testujte hypotezu o
#' rovnakej odolnosti voci slnecnemu ziareniu.

prvy <- c(85, 87, 92, 80, 84, 86)
druhy <- c(89, 89, 90, 84, 92)
#' Najprv grafy
boxplot(prvy, druhy, col = c('red', 'green'))
#' Test o rovnosti disperzii, pouzijeme Fisher Ratio test
var.test(prvy, druhy)
var.test(prvy, druhy)$p.value
#' Kedze P hodnota = 0.59 > 0.05 nezamietam hypotezu o rovnosti
#' disperzii. Vyberieme spravny test rovnosti strednych hodnot
t.test(prvy, druhy, paired = F, var.equal = T)
t.test(prvy, druhy, paired = F, var.equal = T)$p.value
#' P hodnota = 0.17 > 0.05, nezamietam hypotezu o rovnosti strednych hodnot,
#' odolnost voci ziareniu je rovnaka.
#####################################################
#' Priklad 2 - ked sa disperzie nebudu rovnat
#' Firma odobera ochranne pracovne prostriedky od dvoch dodavatelov
#' AA, BB. Zaznamenali sme dlzky dodavok na hladine vyznamnosti
#' $\alpha=0.05$ Testujte hypotezu o tom, ze dlzky dodavok su rovnake
aa <- c(10, 12, 15, 25, 18, 20, 15, 25, 30)
bb <- c(15, 15, 18, 10, 16, 12, 15)
boxplot(aa, bb, col = c('blue', 'purple'))
var.test(aa, bb)$p.value
#' Phodnota - 0.03 < 0.05, zamietam hypotezu o rovnosti disperzii
#' Teraz test pre rovnost strednych hodnot
t.test(aa, bb, paired = F, var.equal = F)$p.value
#' phodnota = 0.09 > 0.05, nezamietam hypotezu o rovnosti strednych
#' hodnot, dlzky dodavok su rovnake
#'# ANOVA Analysis of Variance
#' Testujeme hypotezu o rovnosti strednych hodnot viac ako dvoch
#' vyberov.
#' $$H_0\quad \mu_1 = \mu_2 = .... = \mu_I$$
#' alternativa je aspon jedna dvojica sa nerovna
#' Jednofaktorova analyza rozptylu (One Way ANOVA)
#' Datovy subor sa rozlozi do tried podla nejakeho faktora,
#' triedny faktor $\alpha_i$, mame napr. datovy subor mesacnych prijmov
#' 50 respondentov, rozdelime mesacne prijmy podla faktora typ vzdelania,
#' 1-svs, 2-sos, 3-vs
#' Testujeme potom hypotezu o nulovosti triedneho faktora, cize triedny
#' faktor nema vplyv na merania
#' ANOVA ma silne predpoklady
#' - normalita dat v triedach (Shapiro Wilk test)
#' - rovnost disperzii v triedach (Bartlettov test)
#' Ak podmienky nie su splnene, tak musime testovat neparametrickym
#' testom (Kruskall Wallis)
library(readxl)
library(RColorBrewer)
library(vioplot)
library(ggplot2)
library(ggpubr)
#' datovy subor dataE obsahuje, okrem ineho, data mesacny prijem a
#' informaciu o vzdelani respondenta (triedny faktor-vzdelanie, tri urovne).
#' Na hladine vyznamnosti $\alpha = 0.05$ testujte hypotezu, ze faktor
#' vzdelanie je nulovy a teda nema vplyv na vysku prijmu
data <- read_xlsx('..\\data\\dataE.xlsx')
data
#' najprv graficka analyza
boxplot(data$mprij~data$vzdelanie, col = brewer.pal(3, 'Pastel1'),
        main = 'Prijem podla vzdelania', xlab = 'vzdelanie',
        ylab = 'prijem')
pomoc <- data.frame(data$vzdelanie, data$mprij)
ggline(pomoc, x = 'data.vzdelanie', y = 'data.mprij',
       add = c('mean_ci', 'jitter', 'violin'))
#' Overime podmienky ANOVA
#' Normalita dat v triedach
tapply(data$mprij, data$vzdelanie, shapiro.test)
#' Nulova hypoteza je, ze data su normalne rozdelene
#' P hodnoty pre kazdu triedu su > 0.05, teda nezamietam H0, data v triedach
#' su normalne rozdelene
#' Rovnost disperzii v triedach
bartlett.test(data$mprij~data$vzdelanie)
#' H0 je, ze rovnost disperzii
#' P hodnota > 0.05, nezamietam H0, disperzie v triedach su rovnake
#' Pred vypoctom musime este faktorizovat vzdelanie
vzdelanie <- factor(data$vzdelanie)
an1 <- aov(data$mprij~vzdelanie)
an1
summary(an1)
#' P hodnota < 0.05 zamietam hypotezu o nulovosti faktora vzdelanie,
#' faktor je statisticky vyznamny a ma vplyv na mesacny prijem..
#' Nasleduju post testy, ktorymi urcime odlisnosti pre kazde dve triedy,
#' Tukey, Scheffe post test
TukeyHSD(an1)
#' trieda 1, 2 - phodnota > 0.05, nezamietam hypotezu o rovnosti strednych
#' hodnot, nie su odlisne
#' trueda 1, 3 a 2, 3 Phodnota < 0.05, zamietam hypotezu o rovnosti
#' strednych hodnot, triedy su odlisne
plot(TukeyHSD(an1))
library(DescTools)
ScheffeTest(an1)
plot(ScheffeTest(an1))
#############################################
#'# Dvojfaktorova analyza rozptylu
#'Data  sa rozdeluju do tried podla dvoch faktorov, riadkovy je
#'oznaceny $\alpha_i$ a stlpcovy faktor $\beta_j$, napr. vysledky
#'liecby-vek, liek, merania-laborant, pristroj
#'Kazda trieda musi byt normalne rozdelena a s rovnakou disperziou,
#'nebudeme overovat, testuju sa teda dve hypotezy
#'H0 Riadkovy faktor je nulovy, nema vplyv na merania
#'H0 Stlpcovy faktor je nulovy, nema vplyv na merania
#'Priklad
#'Traja laboranti urobili dve opakovane merania latky deg na 4 chromatografoch.
#'Ma na vysledok vplyv laborant, pristroj?
anova1 <- read_xlsx('..//data//anova1.xlsx')
anova1
#' grafy rozdelit vzhladom na dva faktory
par(mfrow = c(1, 2))
boxplot(anova1$deg~anova1$laborant, col = brewer.pal(3, 'Pastel1'))
boxplot(anova1$deg~anova1$pristroj, col = brewer.pal(4, 'Pastel1'))
pomoc1 <- data.frame(anova1$laborant, anova1$deg)
ggline(pomoc1, x = 'anova1.laborant', y = 'anova1.deg',
       add = c('mean_ci', 'jitter', 'violin'))
pomoc2 <- data.frame(anova1$pristroj, anova1$deg)
ggline(pomoc2, x = 'anova1.pristroj', y = 'anova1.deg',
       add = c('mean_ci', 'jitter', 'violin'))
#' opat faktorizujeme
laborant <- factor(anova1$laborant)
pristroj <- factor(anova1$pristroj)
an2 <- aov(anova1$deg~laborant + pristroj)
summary(an2)
#' Obe P hodnoty < 0.05, zamietam hypotezu o nulovosti faktora
#' laborant, laborant ma vplyv na merania. Zamietam tiez
#' hypotezu o nulovosti faktora pristroj, pristroj ma vplyv na
#' vysledky merania. Nasleduju post testy
TukeyHSD(an2)
plot(TukeyHSD(an2))
#' Odlisnosti su vyznamne pre laborantov a, b a pre pristroje
#' A, D a C, D
#' ANOVA s interakciami, pribudne hypoteza H0 ze kombinacie faktorov
#' su nulove, nemaju vplyv na vysledky merania
an3 <- aov(anova1$deg~laborant + pristroj + laborant * pristroj)
summary(an3)
#' P hodnota pre interakcie > 0.05, interakcie nemaju vplyv na vysledky merani,
#' su nulove.
TukeyHSD(an3)
plot(TukeyHSD(an3))