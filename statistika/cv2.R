#Diskretne a spojite rozdelenia pravdepodobnosti
#Kazde rozdelenie ma svoj zakladny prikaz a k nemu dane
#pismeno, podla toho, co ratame
# d ratame P(x=k)
# p ratame P(X\leq k)
# q ratame kvantily
# r generujeme nahodne cislo z daneho rozdelenia
# Binomicke rozdelenie
# V statistickej kontrole akosti, n/krat nezavisle na sebe opakujeme
# pokus, sledujeme vyskyt danej udalosti
# Pravdepodobnost danes udalosti v jedinom pokuse je p,
# parametre su n,p, binom

# Pr.1
# Ucinost antibiotika je  80%. Podavame ho 10 pacientom na oddeleni.
# Zratajte tieto pravdepodobnosti:
# Vsetci pacienti sa vyliecia P(x=10)
# Prave 7 sa vylieci P(x=7)
# Najviac 8 sa vylieci P(x<=8).
# Aspon 5 sa vylieci P(x>=5) = P(x>4)  ostra nerovnost je lepsia

n <- 10
p <- 0.8
dbinom(10, n, p) #prva uloha
dbinom(7, n, p) #druha uloha
pbinom(8, n, p) #tretia uloha
pbinom(4, n, p, lower.tail = F) #stvrta uloha - prva moznost
1 - pbinom(4, n, p) #stvrta uloha - druha moznost

# DO tej hodnoty je neostra, OD tej hodnoty je ostra
# pbinom ked kumulujem

#Zostrojte tabulku a graf rozdelenia pravdepodobnosti
xB <- 0:10
hustotaB <- dbinom(xB, n, p)
hustotaB
(tabulkaB <- data.frame(hodnota = xB, pravdepodobnost = hustotaB))
tabulkaB
View(tabulkaB)
barplot(tabulkaB$pravdepodobnost, main = 'Binomicke rozdelenie', names.arg = xB,
        xlab = 'hodnota', ylab = 'pravdepodobnost', col = 'green')

# hodnoty distribucnej funkcie
(distB <- pbinom(xB, n, p))

###############################################
# Hypergeometricke rozdelenie
# Pouzitie v statistickej kontrole akosti. Mnozina obsahuje
# m-prvkov so sledovanou vlastnostou a
# n-prvkov bez tejto vlastnosti,
# nahodne vyberieme k-prvkov.
# Nahodna premenna je pocet prvkov so sledovanou vlastnostou v nasom vybere.
# Prikaz je hyper, pismenka platia, parametre v poradi m, n, k

# Priklad
# Student sa nauci na skusku 12 z 20 otazok. Test obsahuje 5 otazok.
# Vypocitajte nasledujuce pravdepodobnost:
# Student dostane Acko, zodpovie vsetky otazky P(x=5)
# Student neurobi skusku, zodpovie menej ako 3 P(x<=2)  pre dolnu hranicu neostra
# Student urobi skusku (doplnok k predchadzajucej), zodpovie 3 a viac P(x>2)
# Parametre su m = 12, n = 8, k = 5
m <- 12
n <- 8
k <- 5
# prva uloha
dhyper(5, m, n, k)
# druha uloha
phyper(2, m, n, k)
# tretia uloha
1 - phyper(2, m, n, k) # ako doplnok
phyper(2, m, n, k, lower.tail = F)

# tabulka a graf rozdelenia pravdepodobnosti
xH <- 0:5 # viem zodpovedat ani jednu az vsetkych 5 otazok
hustotaH <- dhyper(xH, m, n, k)
tabulkaH <- data.frame(hodnota = xH, pravdepodobnost = hustotaH)
tabulkaH
View(tabulkaH)
barplot(
  tabulkaH$pravdepodobnost,
  main = 'Hypergeometricke rozdelenie',
  names.arg = xH,
  xlab = 'hodnota',
  ylab = 'pravdepodobnost',
  col = 'blue',
  ylim = c(0,0.4)
)

# nakreslite empiricku distribucnu funkciu pomocou nasimulovanych dat 10 000
data <- rhyper(10000, m, n, k)
plot(ecdf(data), main = 'Empiricka distribucna funkcia')

##########################################
# Poissonovo rozdelenie
# pouziva sa v teorii hromadnej obsluhy, pravdepodobnosti zriedkavych javov
# v casovom intervale, na nejakom objeme. Ma jediny parameter lambda
# Pri zmene casoveho intervalu treba parameter tiez prepocitat.
# Lambda je ocakavana hodnota v zadani.
# Prikaz je pois
# Na samoobsluznu linku pride 20 ludi za hodinu. Vypocitajte tieto pravdepodobnosti:
# V priebehu 15 min. pride 1 clovek P(x = 1), prepocet lambda = 20/60 je jedna minuta
# 15 = 5
# V priebehu 5 min. nikto nepride P(x = 0), lmbda = 20/60, 5 = 5 / 3
# V priebehu 10 min. pride aspon 10 ludi, P(x >= 10) = P(x > 9),
# lambda = 20/60, 10 = 510/3

#prva uloha
dpois(1, 5)
#druha uloha
dpois(0, 5/3)
#tretia uloha
ppois(9, 10/3, lower.tail = F)

# Uvazujme casovy okamih 1 hodinu, urcte maximalny pocet ludi, ktori navstivia
# linku s pravdepodobnostou 90% (na linku pride max. tolko ludi) .. zarucujeme 
# sa, ze na 90% tolko pride max
qpois(0.9, 20)
# zostrojte tabulku a graf rozdelenia pravdepodobnosti
# pre interval 1 hodina a prvych 40 hodnot
xP <- 0:40 # viem zodpovedat ani jednu az vsetkych 5 otazok
hustotaP <- dpois(xP, 20)
tabulkaP <- data.frame(hodnota = xP, pravdepodobnost = hustotaP)
tabulkaP
View(tabulkaP)
barplot(
  tabulkaP$pravdepodobnost,
  main = 'Poissonovo rozdelenie',
  names.arg = xP,
  xlab = 'hodnota',
  ylab = 'pravdepodobnost',
  col = 'red',
  ylim = c(0,0.15)
)

################################
# spojite rozdelenia
# normalne rozdelenie pravdepodobnosti ma dva parametre, strednu hodnotu
# mu (to je asi nejakej pismenko) a smerodajnu odchylku sigma
# prikaz je norm
# Zivotnost bateriek do mobilnych telefonov sa riadi normalnym rozdelenim
# so strednou hodnotou 8 a smerodajnou odchylkou 2
# Ulohy
# Kolko % bateriek treba vymenit do 7.5 roka P(x <= 7.5)
# Kolko % bateriek vydrzi v rozpati 7-9 rokov P(7 <= x <= 9)
# Kolko vydrzi viac ako 10 P(x >= 10)
# Za aku dobu zivotnosi sa mozno zarucit na 90% (tolko a viac)

# prva uloha
pnorm(7.5, mean = 8, sd = 2)
# druha uloha
# na dva kroky, najskor spocitam po a potom odcitam
pnorm(9, mean = 8, sd = 2) - pnorm(7, mean = 8, sd = 2)
# tretia uloha
pnorm(10, mean = 8, sd = 2, lower.tail = F)
# stvrta uloha
qnorm(0.9, mean = 8, sd = 2, lower.tail = F) #zarucujem sa na tolko a hornu hranicu

# nakreslime histogram nasimulovanych dat, N(0, 1), prelozime hustotu cez histogram
xx <- rnorm(500, mean = 0, sd = 1) # pre histogram
xxx <- seq(-3, 3, 0.01) # pre kreslenie hustoty
hist(xx, freq = F) # freq F na zmenu mierky na pravdepodobnostnu
lines(xxx, dnorm(xxx, 0, 1), col = 'blue')

##########################################
# Exponencialne rozdelenie
# Zivotnost zariadenia, doba do prvej poruchy, doby medzi poruchami,
# jediny parameter lambda, je to prevratena hodnota strednej hodnoty,
# pozor ako to bude v zadani. E(x) = 1 / lambda
# Priklad
# Dlzka zivotnosti pouzivaneho PC v pocitacovej ucebni je 2 roky
# Vypocitajte PC ma zivotnost aspon 1 rok P(X >= 1)
# PC ma zivotnost najviac % rokov P(X <= 5)
# Za aku zivotnost by ste sa zarucili s pravdepodobnostou 5%, p = 0.05
# parameter je 1/2
# Uloha 1
pexp(1, rate = 1/2, lower.tail = F)
# Uloha 2
pexp(5, rate = 1/2)
# Uloha 3
qexp(0.05, rate = 1/2, lower.tail = F)

# Nakreslite histogram, prelozte hustotu
xx <- rexp(500, rate = 1/2) # pre histogram
xxx <- seq(0, 5, 0.01) # pre kreslenie hustoty
hist(xx, freq = F) # freq F na zmenu mierky na pravdepodobnostnu
lines(xxx, dexp(xxx, rate = 1/2), col = 'purple')
