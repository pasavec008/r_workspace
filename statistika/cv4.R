library(latexpdf)
#'# Intervaly spolahlivosti pre parametre normalneho rozdelenia
#'pre strednu hodnotu $\mu$ a disperziu $\sigma^2$. Bodove
#'odhady pozname, maju dobre statisticke vlastnosti (nevychylene, efektivne)
#'Pri nahodnom pokuse je vzdy nejaka chyba, preto si urcujeme,
#'akej chyby sa mozem dopustit. IS konstruujeme s vopred danou, zvolenou
#'chybou, ktorej sa mozem dopustit. Ak robim 95% IS, tak sa dopustam 5% (nepokryje),
#'$\alpha=0.05$
#'Nahodny vyber musi byt normalne rozdeleny, da otestovat, my to
#'predokladame.
#'# IS pre parameter alternativneho rozdelenia, $p$.
#'Zo 100 opytanych respondentov, 15 vyjadrilo ochotu dat sa zaockovat prave
#'vakcinou ABC. Bodovy odhad najdeme ako pomer
#' $\hat{p}=\frac{15}{100}$. (1-\alpha)100%$ IS pre tento parameter
#' $$\hat{p} \pm u(1-\alpha/2)\sqrt{\frac{p(1-p)}{n}}$$
#' Najdite 95% IS pre $p$
alfa <- 0.05
p <- 0.15
n <- 100
IS <- p + c(-1, 1) * qnorm(1 - alfa / 2, 0, 1) * sqrt(p * (1 - p) / n)
IS

#' Teraz vstavana funkcia
binom.test(15, 100, p = 0.15)
binom.test(15, 100, p = 0.15)$conf.int

#' Zostrojte 90% IS
binom.test(15, 100, p = 0.15, conf.level = 0.9)$conf.int

#' Lavostranny, pravostranny
binom.test(15, 100, p = 0.15, alternative = 'greater')$conf.int
binom.test(15, 100, p = 0.15, alternative = 'less')$conf.int

#####################################################################
#' IS pre $\mu$, ak $\sigma$ pozname
#' Priklad prednaska - kopirka
#' Najdite 95% IS, 90% IS obojstranne, 95% jednostranne podla vzorca
x <- c(2445, 2450, 2453, 2462, 2463, 2463, 2466, 2471, 2474, 2475, 2475,
       2484, 2485, 2486, 2487, 2490, 2491, 2493, 2499, 2501, 2501, 2503,
       2504, 2505, 2505, 2506, 2506, 2507, 2509, 2511, 2511, 2513, 2514,
       2515, 2518, 2523, 2523, 2524, 2525, 2527, 2529, 2530, 2530, 2533,
       2535, 2536, 2537, 2539, 2560, 2571)
alfa <- 0.05
sigma <- 30
n <- 50
IS <- mean(x) + c(-1, 1) * qnorm(1- alfa / 2) * sigma / sqrt(n)
IS
alfa <- 0.1
IS <- mean(x) + c(-1, 1) * qnorm(1- alfa / 2) * sigma / sqrt(n)
IS
alfa <- 0.05
ISl <- c(mean(x) - qnorm(1 - alfa) * sigma/ sqrt(n), Inf)
ISl

ISp <- c(-Inf, mean(x) + qnorm(1 - alfa) * sigma/ sqrt(n))
ISp

#' IS pre strednu hodnotu, ak sigma nie je zname. Najprv vzorcom,
#' a potom vstavanymi funkciami. Rieste rovnake ulohy,
#' ale za predokladu, ze sigma nepozname
#' $\sigma$ nahradzame odhadom, kvantil je kvanitol studentovho
#' t rozdelenia s parametrom stupne volnosti tu je to n - 1
alfa <- 0.05
IS <- mean(x) + c(-1, 1) * qt(1 - alfa / 2, n - 1) * sd(x) / sqrt(n)
IS
alfa <- 0.1
IS <- mean(x) + c(-1, 1) * qt(1 - alfa / 2, n - 1) * sd(x) / sqrt(n)
IS
alfa <- 0.05
ISl <- c(mean(x) - qt(1 - alfa, n - 1) * sd(x) / sqrt(n), Inf)
ISl

ISp <- c(-Inf, mean(x) + qt(1 - alfa, n - 1) * sd(x) / sqrt(n))
ISp
#' pomocou vstavanych funkcii a roznych kniznic
t.test(x)$conf.int #default je 95% obojstranny
t.test(x, conf.level = 0.9)$conf.int # 90%, ale stale obojstranny
t.test(x, alternative = 'greater')$conf.int # 95%, lavostranny
t.test(x, alternative = 'less')$conf.int # 95%, pravostranny

#install.packages('DescTools')
library('DescTools')
MeanCI(x) # 95% obojstranny
MeanCI(x, conf.level = 0.9) # 90%, ale stale obojstranny
MeanCI(x, sides = 'left') # 95% lavostranny
MeanCI(x, sides = 'right') # 95% pravostranny
#' 
library(Rmisc)
#install.packages('Rmisc')
CI(x) #95%
CI(x, ci = 0.9) #90%
#'# IS pre strednu hodnotu a podmnoziny dat, Rmisc
#' najdeme IS pre priemerny prijem vzhladom na pohlavie,
#' vzhladom na vzdelanie
library(readxl)
data <- read_xlsx('C:/R/r_workspace/statistika/dataE.xlsx')
group.CI(data$mprij ~ data$pohlavie, data = data)
group.CI(mprij ~ vzdelanie, data = data)
#'# IS pre disperziu
#' Vypocitajte 95% obojstranny, lavostranny a pravostranny
#' pre nas priklad hore podla vzorca
alfa <- 0.05
c((n - 1) * var(x) / qchisq(1 - alfa / 2, n - 1), (n - 1) * var(x) / qchisq(alfa / 2, n - 1))
c((n - 1) * var(x) / qchisq(1 - alfa, n - 1), Inf)
c(0, (n - 1) * var(x) / qchisq(alfa, n - 1))
#' Kniznica EnvStats
#install.packages('EnvStats')
library('EnvStats')
varTest(x)$conf.int
varTest(x, conf.level = 0.9)$conf.int
varTest(x, alternative = 'greater')$conf.int
varTest(x, alternative = 'less')$conf.int
VarCI(x)
##########################################################
#' Simulujeme 100 krat nahodny vyber z normalneho rozdelenia
#' dlzky 30, $\mu=3$ a $\sigma=1$, zratame 95% IS pre kazdu simulaciu,
#' kolko je takych intervalov, ktore nepokryju $\mu$

k <- 0
for(i in 1:100){
  a <- rnorm(30, mean = 3, sd = 1);
  is <- t.test(a)$conf.int;
  if(3 > is[2])
    k <- k + 1;
  if(3 < is[1])
    k <- k + 1;
}
k

#' Nakreslime hustotu N(0, 1) a zopar hustot studentovho t rozdelenia
#' s roznymi stupnami volnosti, porovname tvary kriviek
plot(function(x) dnorm(x, 0, 1), xlim = c(-5, 5))
plot(function(x) dt(x, df = 2), col = 'green', xlim = c(-5, 5), add = T)
plot(function(x) dt(x, df = 4), col = 'red', xlim = c(-5, 5), add = T)
plot(function(x) dt(x, df = 10), col = 'purple', xlim = c(-5, 5), add = T)