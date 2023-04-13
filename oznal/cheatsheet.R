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
data <- read.csv('/Users/oliverkubicka/Desktop/FIIT/OZNAL/data/Default.csv')
View(data)
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
hist(data$balance, breaks = 50)
qqnorm(data$balance, main = 'Balance - normal Q-Q')
qqline(data$balance)
hist(data[data$default == 1,]$balance, breaks = 50, xlab = 'balance', main = 'Defaulters balance')
# Distribution of variable on two classes on one plot
ggplot(data, mapping = aes(balance, fill = default)) +
  geom_histogram(alpha=0.6)
ggplot(data, mapping = aes(default, balance, fill = default)) +
  geom_point(alpha=0.6)

data %>% mutate(default = as.integer(default), student = as.integer(student)) %>%
  cor()

### Models
# Split data to train set and test set
data_split <- initial_split(crickets, 0.8, strata = 1)
data_train <- training(data_split)
data_test <- testing(data_split)
# Ordinary Linear Regression (OLR)
pairs(crickets[crickets$species == 'O. niveus',])
model <- lm(rate ~ temp + species, data_train)
summary(model)

# polynomial fit of 5th-degree
olr_model <- lm(wage_eur ~ poly(skill_dribbling, 5) + poly(skill_curve, 5), data = players_data)

# predict data with model (data must have same structure of predicates [best to use train/test sets])
data_test[1:7,] 
predict(model, data_test)
# plot model
# residuals vs fittes -> fitted values are predicted by model (not reality), residual is mistake
autoplot(model)
# calculate residual sum of square (measuring size of mistake by residuals^2, direction is not important)
RSM <- sum(model$residuals^2)
RSM

# classification
model <- glm(default ~ balance, data, family = 'binomial')
# In classification we need to use type = response
predict(model,
        newdata = tibble(balance = c(1000,1300,1700,2000,3000)),
        type = 'response'
)

# we are deciding what class to choose on cut-off value (0.7)
data$predicted_default <- ifelse(model$fitted.values > 0.7, 1, 0)
confusionMatrix(as_factor(data$predicted_default), as_factor(data$default), positive = '1')$table

# LDA
model <- lda(default ~ balance, data)
lda_predict <- predict(model)
lda_predict$class

lda_matrix <- confusionMatrix(lda_predict$class, data$default)
lda_matrix$table

# Comparison of ROCs between logistic regression and LDA (higher area under curve wins)
roc <- rocit(class = model_logistic$y,
             score = model_logistic$fitted.values)

roc2 <- rocit(class = lda_predict$class,
              score = as.integer(data$default))

summary(roc)
summary(roc2)
