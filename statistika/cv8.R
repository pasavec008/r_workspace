library('latexpdf')
#'# Neparametricke metody
#'neparametricke metody pouzivame tam, kde data nie su normalne
#'rozdelene, je ich malo, su nestandardne. Neparametricke testy
#'vyzaduju podmienku, ze data su z nejakeho spojiteho rozdelenia.
#'Su menej presne a menej citlive.
#'
#'Hypoteza H0 je v tvare
#'$H_0\quad \text{data su nahodne}\quad H_1\quad \text{nie su nahodne}$
#'Test serii (Wald Wofovitzov test), testujeme nahodnost dat.
#'Test je citlivy na trend, kniznica randtests
library('randtests')
#' Linka MHD v rannej spicke prejde trasu priemernou rychlostou 8 km/h.
#' Bola navrhnuta mala zmena trasy s cielom zrychlit dopravu. Pocas
#' 10 dni sme namerali udaje o priemernej rychlosti datovy subor-v.
#' Na hladine vyznamnosti $\alpha=0.05$ testujte hypotezu o nahodnosti dat.
#' Testujte nahodnost merani.

v<- c(7.7, 7.8, 8.5, 7.8, 7.9, 9, 7.5, 8.2, 9.3, 8.1)
runs.test(v, plot=T)
#' Phodnota > 0.05, data su nahodne
runs.test(v, alternative = 'left.sided')
runs.test(v, alternative = 'right.sided')
#' Test kritickych bodov, bodov obratu (turning point test)
#' odhaluje periodicitu v datach
turning.point.test(v)
#' P hodnota > 0.05, nezamietam hypotezu o nahodnosti dat
turning.point.test(v, alternative = 'left.sided')

#'# Neparametricke testy o polohe (mediane). Parametrickymi testami
#'sme testovali tvrdenia o strednej hodnote, boli jednovyberove t.testy
#'a dvojvyberove t.testy - parove testy a neparove
#'# Znamienkovy test (sign test)
#'testujeme $H_0\quad \text{median} = x_0\quad H_1\quad
#'\text{median}\neq x_0$
#'Jedina podmienka kladena na data je, aby boli vyberom zo
#'spojiteho rozdelenia. Test ma malu silu, t.j. chyba druheho druhu
#'je velka (nezamietame H0 a pritom H0 neplati).
#'Testujte, ze data z predosleho prikladu maju median = 8, teda ze
#'priemerna rychlost ani po uprave sa nezmenila.
boxplot(v, horizontal = T)
hist(v)
#' pre symetricke data je sikmost nulova, spocitame este sikmost
library('moments')
skewness(v)
library('BSDA')
SIGN.test(v, md=8)
#' P hodnota > 0.05 nezamietame hypotezu o tom, ze priemerna
#' rychlost sa nezmenila. Tento test mozeme pouzit aj
#' ako dvojvyberovy parovy. Testujeme, ze median rozdielov je rovny
#' nejakemu cislu, pouzijeme ho tam, kde je asymetria dat.
#' Ak pridame predpoklad, ze data su symetricke okolo medianu, tak
#' radsej pouzijem jednovyberovy Wilcoxonov test
#' -signed rank test. Otestujeme nase data, ci su symetricke a ak ano
#' testujeme tymto testom.
library('lawstat')
symmetry.test(v)
#' p hodnota >0.05, radsej Wilcoxonov test
wilcox.test(v, mu=8)
#' P hodnota >0.05, nezamietam H0, priemerna rychlost sa nezmenila
#' Pri tradicnom opracovani suciastok sa dosahovali priemerne hodnoty
#' kvalitativnej vlastnosti 4.4, pokusne sa zavadza nova jednoduchsia
#' metoda opracovania suciastok. Hodnoty kvalitativnej vlastnosti su
#' v datovom subore x. Testujte hypotezu, ze kvalitativna vlastnost aj pri
#' novej metode zostala rovnaka.
x<-c(4.5, 4.3, 4.1, 4.9, 4.6, 3.6, 4.7, 5.1, 4.8, 4, 3.7, 4.4,
     4.9, 4.9, 5.2, 5.1, 4.7, 4.9, 4.6, 4.8)
boxplot(x)
symmetry.test(x)
#' Data su symetricke, teda jednoznacne WT.
wilcox.test(x, mu=4.4)
#' P hodnota>0.05, nezamietam hypotezu, ze novou metodou opracovane
#' suciastky maju rovnaku kvalitativnu vlastnost
#' Prakticky by sme mali na zaciatku overit normalitu dat a ak
#' su normalne rozdelene, tak t.test
library('nortest')
shapiro.test(x) #normalita
t.test(x, mu=4.4)
#' Aj parametrickym testom nam vyslo, ze kvalitativna vlastnost ostala rovnaka.
#' Priklad pouzitia, ako parovy test.
#' Su dane casy v sekundach, pocas ktorych vyriesili kontrolne ulohy
#' ziaci pred a po specialnych cviceniach z pamatoveho pocitania. Zlepsili
#' cvicenia schopnost ziakov rychlejsie riesit ulohy?
#' Ak sa pytame, ci zlepsili, pred - po >= 0, teda negacia je <0 alternativa
#' bude less. Testy vyberam podla toho, ci rozdiely su alebo nie su
#' symetricke.
pred <- c(87,61,98,90,93,74,83,72,81,75,83)
po <- c(50,45,79,90,88,65,52,79,84,61,52)
rozdiel = pred - po
rozdiel
boxplot(rozdiel, horizontal = T)
skewness(rozdiel)
symmetry.test(rozdiel) #symetria dat
wilcox.test(rozdiel, alternative = 'less')
#' p hodnota>0.05, nezamietame H0, ziaci sa zlepsili.
#' Dvojvyberovy neparovy test, neparametricky, dvojvyberovy
#' Wilcoxonov test (Mann Whitney U test). Nulova hypoteza je
#' $H_0\quad F_X=F_Y$. Pred testom treba overit, ci sa rozdelenia aspon
#' priblizne rovnaju a tiez ci maju rovnaku disperziu. Ak su velke rozdiely,
#' tak dvojvyberovy Kolmogorov Smirnov test. Neparametricky test pre
#' rovnost disperzii Levene test.
#' Z produkcie dvoch firiem bolo nahodne vybratych n=10 a m=8 vyrobkov.
#' Nezavisli experti hodnotili ich kvalitu pridelenim bodov.
#' Datovy subor x, y. Testujte hypotezu, ze kvalita vyrobkov dvoch
#' firiem je rovnaka.
x<-c(420,560,600,490,550,570,340,480,510,460)
y<-c(400,420,580,470,470,500,520,530)
boxplot(x, y, col = c('blue', 'red'))
par(mfrow=c(1,2))
hist(x, breaks = 5)
hist(y, breaks = 5)
par(mfrow = c(1,1))
#' Uprava dat pre test
data <- data.frame('body' = c(x, y), 'firma' = rep(c(1,2), times = c(10,8)))
data
levene.test(data$body, data$firma)
#' Mozeme pouzit dvojvyberovy WT
wilcox.test(x,y)
#' P hodnota > 0.05, nezamietame hypotezu o rovnosti DF, teda aj
#' medianov, kvalita je rovnaka, este KS test
ks.test(x, y)
#' plati tvrdenie hore
#'# Kruskal Wallisov test, neparametricky ekvivalent k ANOVA. ANOVA
#'mala silne podmienky, normalita dat v triedach, rovnost disperzii.
#'Ak to nie je splnene, tak KW test. Pri ANOVE sme testovali,
#'ze stredne hodnoty sa rovnaju, tu testujeme, ze distribucne funkcie sa
#'rovnaju (a teda aj mediany). Zaznamenali sme vykony strojov troch znaciek.
#'Na hladine vyznamnosti $\alpha=0.05$ testujte hypotezu vykony strojov
#'su rovnake.
data<-data.frame("vykon"=c(53,47,46,61,55,52,58,54,51,51,49,54),
                 "stroj"=c(rep(1,3),rep(2,5),rep(3,4)))
boxplot(data$vykon~data$stroj, col=c('red', 'green', 'blue'))
#' Faktorizujeme
data$stroj <- factor(data$stroj)
data$stroj
kruskal.test(data$vykon, data$stroj)
#' P hodnota <0.05, zamietame hypotezu o rovnakej vykonnosti
#' Nasleduju post testy. ANOVA (Tukey, Scheffe), tu pouzijeme dunn.test
#' (aj kniznica sa tak vola)
library('dunn.test')
dunn.test(data$vykon, data$stroj)
dunn.test(data$vykon, data$stroj, altp = T, list = T)
#' lisi sa prva druha trieda