#'# Logisticka regresia
#' Objekty klasifikujeme do dvoch skupin, ma danu vlastnost Y=1,
#' nema danu vlastnost Y=0. Modelujeme P(Y=1). Vezmime priklad z prednasky.
#' Priklad Na klientovi banky sledujeme dva znaky, vzdelanie X=0, nema vs,
#' X=1 ma vs a ci ma problem splatit uver, Y=1 - ma problem splatit, nema
#' problem Y = 0. Model bude vyzerat
library(latexpdf)
#' $$P(Y=1)=\frac{1}{1+e^{-b_0-b_1 x}}$$
banka <- readxl::read_xlsx(path = '../data/banka.xlsx')
head(banka)
#' kontingencna tabulka znakov
table(banka$vzdelanie, banka$problem)
logit <- glm(problem~., data=banka, family=binomial)
summary(logit)
summary(logit)$coef
#' Nakreslime data aj logisticku krivku, nie je velmi informacny graf
b0 <- logit$coefficients[1]
b1 <- logit$coefficients[2]
xx <- seq(0, 1, 0.1)
yy <- 1/(1+exp(-b0-b1*xx))
plot(problem~vzdelanie, data = banka)
lines(xx, yy)
library(mlbench)
library(caret)
library(ggplot2)
library(lattice)
library(corrplot)
library(Amelia)
data('PimaIndiansDiabetes2')
head(PimaIndiansDiabetes2)
#' Vyuzijeme vykove data, ktore pochadzaju z narodneho ustavu diabetu .. ma
#' 8 vysvetlujucich premennych a jednu zavislu vsvetlovanu premennu cukrovka
#' pos, neg, vyhodime vektory s chybajucimi datami
missmap(PimaIndiansDiabetes2)
diab <- na.omit(PimaIndiansDiabetes2)
nrow(diab)
#' pozrieme na strukturu dat, najprv krabicove grafy pre nezavisle premenne
par(mfrow=c(2,4))
for(i in 1:8){boxplot(diab[,i], main=names(diab)[i])}
#' lepsie su krabicove grafy rozdelene na cukrovku-ano, nie
invisible(lapply(1:8, function(i)
  boxplot(diab[,i]~diab$diabetes, main=names(diab)[i])))
par(mfrow=c(1,1))
pairs(diab, col=diab$diabetes)
#' najprv vezmime iba jednu premennu glucose, prelozme logisticku krivku
logit1 <- glm(diabetes~glucose, data=diab, family=binomial)
logit1$coefficients
b0 <- logit1$coefficients[1]
b1 <- logit1$coefficients[2]
plot(function(x) 1/(1+exp(-b0-b1*x)), xlim=c(50, 200))
min(diab$glucose)
max(diab$glucose)
#' este sme zabudli na korelacie medzi nezavislymi premennymi, moze problem,
#' ze navzajom koreluju, kolinearita
kk <- cor(diab[,1:8])
corrplot.mixed(kk)
#' urobime logisticku regresiu so vsetkymi nezavislymi premennymi
pimamodel <- glm(diabetes~., data = diab, family = binomial)
summary(pimamodel)
#' Niektore premenne nie je treba uvazovat v regresii, teraz to nebudeme
#' riesit, ideme vyhodnotit kvalitu regresie. Hosmer-Lemeshow test (test
#' dobrej zhody, ci ked rozdelime data do tried podla nejakeho pravidla
#' su empiricke a napocitane pocetnosti v zhode). H0 su rovnake, model je ok
library(glmtoolbox)
hltest(pimamodel)
#' p hodnota < 0.05, zamietame H0, z tohto hladiska model nie je vhodny
#' likelihood Ratio test, porovnanie dvoch a viac modelov,
#' ak uvedieme len jeden model, tak porovname s konstanym modelom.
#' H0 je, ze medzi dvomi modelmi nie je rozdielu. Ak nie su rozdiely,
#' vyberiem jednoduchsi, ak su rozdiely, tak zostava vp latnosti zlozitejsi.
library(lmtest)
lrtest(pimamodel)
#' Zamietame H0, vyberame model 1
lrtest(pimamodel, logit1)
#' Aj tu zamietame H0, vyberame model 1
#' Kontingencne tabulky, aby sme zistili uspesnost predikcie cukrovky
#' podla modelu pimamodel
#' Najprv vypocitame pravdep. nastatia daneho javu
probs <- predict(pimamodel, type = 'response')
#' nasa medzna hodnota nech je 0.5 (cut point), vsetko nad je pozitivny,
#' vsetko pod je negativny
pred <- ifelse(probs > 0.5, 'pos', 'neg')
pred
table(diab$diabetes, pred)
mean(pred==diab$diabetes)
#' Uspesnost predpovede je 78%
#' ROC krivka, hodnota AUC, kniznica ROCR, ROCit, pROC
library(ROCR)
#' Napocitame pravdepodobnosti uz mame - probs, vytvorime objekt prediction
pr <- prediction(probs, diab$diabetes)
pr
prf <- performance(pr, 'tpr', 'fpr')
plot(prf, colorize=T)
plot(prf, colorize=T, print.cutoffs.at=seq(0.1, by=0.2))
#' Este zratame obsah plochy pod krivkou
auc <- performance(pr, 'auc')
auc <- auc@y.values[[1]]
auc <- round(auc, 2)
#legend(0.6, 0.4, auc, title='AUC', cex=1)
#' teraz ideme riesit otazku, kolko premennych je vysvetlujucich, aby
#' sme dostali co najoptimalnejsi model, pouzijeme proceduru step, MASS
library(MASS)
model.step <- step(pimamodel)
model.step1 <- stepAIC(pimamodel)
#' Likelihood Ratio test na porovnanie modelov
lrtest(pimamodel, model.step)
#' p hodnota >0.05, modely su porovnatelne, vyberame jednoduchsi
#' % uspesnosti, ROC, AUC
probs <- predict(model.step, type = 'response')
pred <- ifelse(probs > 0.5, 'pos', 'neg')
table(diab$diabetes, pred)
mean(pred==diab$diabetes)
pr <- prediction(probs, diab$diabetes)
prf <- performance(pr, 'tpr', 'fpr')
plot(prf, colorize=T)
plot(prf, colorize=T, print.cutoffs.at=seq(0.1, by=0.2))
#' Este zratame obsah plochy pod krivkou
auc <- performance(pr, 'auc')
auc <- auc@y.values[[1]]
auc <- round(auc, 2)
#legend(0.6, 0.4, auc, title='AUC', cex=1)
