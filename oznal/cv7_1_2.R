library(tidyverse)
library(ggplot2)
library(dplyr)
library(magrittr)
library(MASS)
library(caret)
library(ROCit)

data <- read.csv('../data/Default.csv')
data

data$default <- as.factor(ifelse(data$default == 'No', 0, 1))
data$student <- as.factor(ifelse(data$student == 'No', 0, 1))

ggplot(data = data, mapping = aes(x = balance, fill = default)) +
  geom_histogram(alpha=0.6)

par(mfrow=c(1,2))
hist(data[data$default == 1,]$balance, breaks = 50)
hist(data[data$default == 0,]$balance, breaks = 50)

qqnorm(data[data$default == 1,]$balance)
qqline(data[data$default == 1,]$balance)

qqnorm(data[data$default == 0,]$balance)
qqline(data[data$default == 0,]$balance)

#defaulters have twice as much balance on average
mean(data[data$default == 0,]$balance)
mean(data[data$default == 1,]$balance)

model <- lda(default ~ balance, data)
model$fitted.values

lda_predict <- predict(model)
lda_predict$class

lda_matrix <- confusionMatrix(lda_predict$class, data$default)
lda_matrix$table

# let s compare lda with glm
model_logistic <- glm(default ~ balance, data = data, family = 'binomial')
glm_predict <- ifelse(model_logistic$fitted.values < 0.947, 0, 1) 
glm_matrix <- confusionMatrix(as.factor(glm_predict), data$default)
glm_matrix$table
lda_matrix$table


roc <- rocit(class = model_logistic$y,
             score = model_logistic$fitted.values)

roc2 <- rocit(class = lda_predict$class,
             score = as.integer(data$default))

summary(roc)
summary(roc2)
