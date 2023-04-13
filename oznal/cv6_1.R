library(tidymodels)
library(tidyverse)
library(magrittr)

data <- read.csv('data/Default.csv')
data

# Task 1
# binary encode
data[data$default == 'Yes',]$default <- 1
data[data$default == 'No',]$default <- 0
data[data$student == 'Yes',]$student <- 1
data[data$student == 'No',]$student <- 0
data
# add IDs
data$ID <- 1:nrow(data)
data
# converting to binary variables to factors
data %<>% mutate(default = as_factor(default))
data %<>% mutate(student = as_factor(student))
# remove with 0 on balance
data <- data[data$balance != 0,]

# Task 2
# non-defaulters
# histogram
hist(data[data$default == 0, ]$balance, freq = F, breaks = 50)
lines(density(data[data$default == 0, ]$balance, bw = 150), col = 'blue')

# normal q-q plot
qqnorm(data[data$default == 0, ]$balance)
qqline(data[data$default == 0, ]$balance, col = 'blue')

# q-q plot
qqplot(data[data$default == 0, ]$balance, data$default)


# defaulters
# histogram
hist(data[data$default == 1, ]$balance, freq = F, breaks = 50)
lines(density(data[data$default == 1, ]$balance, bw = 150), col = 'blue')

# normal q-q plot
qqnorm(data[data$default == 1, ]$balance)
qqline(data[data$default == 1, ]$balance, col = 'blue')

# q-q plot
qqplot(data[data$default == 1, ]$balance, data$default)