library(tidyverse)
library(tidymodels)
library(magrittr)

tidymodels_prefer() # resolve package conflicts using tidy models methods
data(ames)

view(ames)
ames$Sale_Price

hist(ames$Sale_Price)
qqnorm(ames$Sale_Price)
qqline(ames$Sale_Price)

hist(log(ames$Sale_Price, 10), freq = F)
qqnorm(log(ames$Sale_Price, 10))
qqline(log(ames$Sale_Price, 10))

set.seed(27)
ames_split <- initial_split(ames, 0.80)
ames_split

ames_train <- training(ames_split)
ames_test <- testing(ames_split)

ames_train
ames_test

data <- read.csv('data/Practicum.5.1.csv')
view(data)

cleaned_data <- data %>%
  pivot_wider(names_from = Area_SF, values_from = Value_SF)

view(cleaned_data)

cleaned_data %<>% mutate(Total_SF = Total_Bsmt_SF + First_Flr_SF + Second_Flr_SF + Open_Porch_SF)
cleaned_data

library(ggplot2)

# show without outliers to see how total_SF affects price
ggplot(cleaned_data[cleaned_data$Total_SF < 7500, ], mapping = aes(x = Total_SF, y = Sale_Price)) +
  geom_point() +
  geom_smooth()

hist(cleaned_data$Sale_Price)
qqnorm(cleaned_data$Sale_Price)
qqline(cleaned_data$Sale_Price)

cleaned_data$Sale_Price <- log10(cleaned_data$Sale_Price)
hist(cleaned_data$Sale_Price)
qqnorm(cleaned_data$Sale_Price)
qqline(cleaned_data$Sale_Price)


cleaned_data %>% select(Sale_Price, Total_SF, Overall_Cond) %>%
  mutate(Overall_Cond = as.integer(as.factor(Overall_Cond))) %>%
  pairs()
cleaned_data %>% select(Sale_Price, Total_SF, Overall_Cond) %>%
  mutate(Overall_Cond = as.integer(as.factor(Overall_Cond))) %>%
  cor()

cleaned_data_split <- initial_split(cleaned_data, 0.8)
cleaned_data_train <- training(cleaned_data_split)
cleaned_data_test <- testing(cleaned_data_split)

library('parsnip')
model <- linear_reg() %>% set_engine('lm')
trained_model <- model %>% fit(Sale_Price ~ Total_SF + Overall_Cond, cleaned_data_train)
tidy(trained_model)

predict(trained_model, cleaned_data_test)
cleaned_data_test %>% select(Sale_Price) %>%
  mutate(predicted = predict(trained_model, cleaned_data_test),
         residuals = Sale_Price - predict(trained_model, cleaned_data_test))

autoplot(trained_model)
cleaned_data_train[!cleaned_data_train$ID %in% c(1625, 1149, 2280),]
