# Uvod do R, uzitocne prikazy
getwd()
setwd('D:\\skola2022_2023\\1letny\\r_workspace')
dir()
list.files()

2**3
2-3
2+3
2*3
2/3
abs(-5)
pi
exp(1) #Eulerovo cislo
log(10)
log(exp(1))
log(100, 10)

a <- Inf
a

factorial(6)
choose(6, 2)
log(0)

b <- 5L
class(b)
is.integer(27L)
b

# komplexne cisla
d <- 2+3i
class(d)
class(abs(d))
d

# logicke
!(4 < 5)
3 != 5

# textove
f <- 'ahoj nazdar cau'
f

# vektory a matice
v1 <- c(4, 8, 7)
v2 <- c(6, 7, 9)

v1 * v2
v1 / v2

v1 * c(2, 3)

length(v1)
sum(v1)
prod(v1)
cumsum(v1)

# postupnost cisel
v3 <- 1:7
v3

v4 <- seq(1, 26, 2)
v4

v5 <- seq(1, 20, length=30)
v5

# z vektora opakovanim prvkov
v6 <- rep(v1, times=4)
v6

# prvky vektora
v7 <- 1:20
v7[5]
v7[4:7]

# vymazanie objektov
rm(v7)

# matice
v1 <- 1:21
matrix(v1, 7, 3)
matrix(v1, 7, 3, byrow=T)

############################################
# datove tabulky a zoznamy
# vytorime databazu studentov s tymito stlpcami - meno, vek, studium, pocet bodov z pisomky
name <- c('Jan', 'Ivo', 'Oto', 'Eva', 'Ida')
age <- c(19, 20, 21, 22, 18)
students <- data.frame(name, age)
students
students$gender <- c('man', 'man', 'man', 'woman☕' ,'woman☕')
students$points <- c(5, 9, 7, 1, 6)
students$studium <- factor(c(1, 0, 1, 0, 1), labels=c('it', 'ib'))
students$home <- factor(c(1, 2, 3, 1, 2), labels=c('h', 'k', 'i'))

women <- subset(students, students$gender=='woman☕')
women

points <- subset(students, students$points > 5)
points

#kolacovy graf
pie(table(students$gender))
pie(table(students$home))
#rozdelenie graf. okna na 2
par(mfrom=c(1, 2))
barplot(table(students$gender))
barplot(table(students$home))

# kreslenie funkcii
plot(cos, from=-2*pi, to=2*pi)

#vykreslit len v bodoch
xx <- seq(from=-2*pi, to=2*pi, length=50)
plot(xx, cos(xx))

#nacitanie dat
dataset <- read.csv('diabetes.csv')
dataset
View(dataset)

x <- subset(dataset, dataset$BMI > 35 & dataset$BloodPressure > 90)
x

# nainstalovanie packageov
#install.packages('readxl')
library('readxl')

data <- read_excel('data_cviko.xlsx')
View(data)
View(subset(data, data$vzdelanie == 3))
