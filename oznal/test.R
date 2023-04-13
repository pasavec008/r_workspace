# Main libraries for lot of things
library(tidyverse)
library(tidymodels)
library(magrittr)
library(dplyr)
# Plots
library(ggplot2)
# for autoplot of model
library(ggfortify)
library(ROCit)

data <- crickets
pairs(data)

data$species <- ifelse(data$species == 'O. exclamationis', 1, 0)
cor(data)

### Plots
# Plot for dependency of 2 variables divided by color according to their classes
ggplot(modeldata::crickets, aes(x = temp, y = rate, color = species)) + 
  geom_point() +
  geom_smooth()

hist(crickets$temp, breaks = 7)
hist(crickets$rate)
hist(data$species)

model <- glm(species~rate, data, family = 'binomial')
summary(model)
prediction <- predict(model, type = 'response')
data$prediction <- ifelse(prediction > 0.5, 1, 0)

confusionMatrix(as.factor(data$prediction), as.factor(data$species))$table

model2 <- glm(species~rate:temp, data, family = 'binomial')
summary(model2)
prediction2 <- predict(model2, type = 'response')
data$prediction2 <- ifelse(prediction2 > 0.5, 1, 0)
confusionMatrix(as.factor(data$prediction2), as.factor(data$species))$table

model3 <- lda(species~rate, data)
predicted <- predict(model3)
predicted$class
data$prediction3 <- predicted$class
confusionMatrix(as.factor(data$prediction3), as.factor(data$species))$table

rocit(class = data$prediction, score = data$species)$AUC
rocit(class = data$prediction2, score = data$species)$AUC
rocit(class = data$prediction3, score = data$species)$AUC