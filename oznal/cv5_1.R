library(tidyverse)
library(tidymodels)
library(magrittr)
options(scipen=999)

tidymodels_prefer() # resolve package conflicts using tidy models methods
data(ames)
View(ames)
hist(x = ames$Sale_Price, freq = F, breaks = 50)


ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

# stratified sampling, which is more complex than random sampling
set.seed(502)
ames_split <- initial_split(ames, prop = 0.8, strata = Sale_Price)
ames_split
ames_train <- training(ames_split)
ames_test <- testing(ames_split)


# tasks
# task 1
df <- read_csv('../data/Practicum.5.1.csv')
hist(df$Sale_Price, breaks = 100)

# task 2
df_grouped <- df %>%
  group_by(ID, Sale_Price) %>%
  summarise(SF_total = sum(Value_SF))
df_grouped
View(df)

# task 3
pairs(df_grouped)

# task 4
library(rsample)
folds <- vfold_cv(df_grouped, v = 5)

fold_1 <- folds$splits[[1]]
fold_1
train <- analysis(fold_1)
test <- assessment(fold_1)

# task 6
model_train <- lm(Sale_Price~SF_total, data = train)
plot(model_train)
qqnorm(train$Sale_Price)

# task 7
#train
coef(model_train)
#test
model_test <- lm(Sale_Price~SF_total, data = test)
coef(model_test)

# task 8
#' We are predicting Sale_Price. Intercept means constant that is always
#' there and SF_total coeficient means that for every increase of 1 in SF_total,
#' the Sale_Price is increased by value of this coeficient, which in our case
#' is about 51

# task 9
summary(model_train)
summary(model_test)
# we can use p value to support our conclusions