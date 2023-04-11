### Libraries
# Main libraries for lot of things
library(tidyverse)
library(tidymodels)
library(magrittr) #pipes
library(dplyr) #pipes
# Plots
library(ggplot2)
# for autoplot of model
library(ggfortify)
# confusion matrix
library(caret)
# for lda() - linear discriminant analysis
library(MASS)
# for ROC - Area under curve is only important thing
library(ROCit)

### Load data
data <- read.csv('data/Default.csv')

### Data cleaning
cleaned_data <- data %>%
  pivot_wider(names_from = Area_SF, values_from = Value_SF) #opposite for pivot_longer
# Subset
data[data$student = 1,]$student
# Add ID to data using length and move ID column as first
data$ID <- 1:length(data$default)
data %>% select(ID, everything())
# Change Yes/No values to 1/0 and change it to factor (because some functions require factors)
data$default <- as.factor(ifelse(data$default == 'No', 0, 1))
data$student <- as.factor(ifelse(data$student == 'No', 0, 1))

# Select particular columns and create new one
data %<>% select(ID, balance, income) %>%
  mutate(balance_income_ratio = balance / income)
# If data are not normal distributed we can log10 them
data$balance <- log10(data$balance)

### Plots
# Plot for dependency of 2 variables divided by color according to their classes
ggplot(modeldata::crickets, aes(x = temp, y = rate, color = species)) + 
  geom_point() +
  geom_smooth()
# Show more graphs on one screen
par(mfrow=c(1,2))
# Basic analysis of data on plots
hist(data$balance)
qqnorm(data$balance, main = 'Balance - normal Q-Q')
qqline(data$balance)
hist(data[data$default == 'Yes',]$balance, breaks = 50, xlab = 'balance', main = 'Defaulters balance')
# Distribution of variable on two classes on one plot
ggplot(data, mapping = aes(balance, fill = default)) +
  geom_histogram(alpha=0.6)
ggplot(data, mapping = aes(default, balance, fill = default)) +
  geom_point(alpha=0.6)
cor(data)

### Models
# Split data to train set and test set
data_split <- initial_split(data, 0.8, strata = T)
data_train <- training(data_split)
data_test <- testing(data_split)
# Ordinary Linear Regression (OLR)
model <- lm(rate ~ temp + species, cric)
summary(model)
# predict data with model (data must have same structure of predicates [best to use train/test sets])
predict(model, kk)
# plot model
# residuals vs fittes -> fitted values are predicted by model (not reality), residual is mistake
autoplot(model)
# calculate residual sum of square (measuring size of mistake by residuals^2, direction is not important)
RSM <- sum(model$residuals^2)