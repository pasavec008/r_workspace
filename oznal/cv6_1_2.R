library(tidymodels)
library(tidyverse)
library(magrittr)

data <- read.csv('data/Default.csv')
view(data)

data$default <- as.factor(ifelse(data$default == 'No', 0, 1))
data$student <- as.factor(ifelse(data$student == 'No', 0, 1))

data
hist(log10(data$balance))
hist(data$income)
qqnorm(data$balance[data$balance != 0])
qqline(data$balance[data$balance != 0])

data$ID <- 1:length(data$default)
data %<>% select(ID, everything())
data

library(ggplot2)

#toto je daco
ggplot(data, mapping = aes(balance, fill = default)) +
  geom_histogram(alpha=0.6)

ggplot(data, mapping = aes(default, balance, fill = default)) +
  geom_point(alpha=0.6)
autoplot(data)

summary(data[data$default == 1,]$balance)
summary(data[data$default == 0,]$balance)

model <- glm(default ~ balance, data, family = 'binomial')
model
summary(model)
autoplot(model)
View(model)

view(model$fitted.values)

# here we can use any value as decision rule -> default 0.5
data$predicted_default <- ifelse(model$fitted.values > 0.5, 1, 0)
data[data$predicted_default == 1,]

# we can see as we raise balance, how probability of being default changes
# it is up to us what decision rule we use to cut
new_data <- predict(model,
                    newdata = tibble(balance = c(1000,1300,1700,2000,3000)),
                    type = 'response')
new_data
names(new_data) <- c(1000,1300,1700,2000,3000)
new_data
ifelse(new_data > 0.6, 'defaulter', 'non-defaulter')
ifelse(new_data > 0.6, 1, 0)

install.packages('caret')

library(caret)
data$predicted_default <- ifelse(model$fitted.values > 0.01, 1, 0)
confusionMatrix(as_factor(data$predicted_default), as_factor(data$default), positive = '1')

ggplot(data, mapping = aes(balance, fill = as_factor(predicted_default))) +
  geom_histogram(alpha=0.6)