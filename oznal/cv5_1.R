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
ames_train <- training(ames_split)
ames_test <- testing(ames_split)


# tasks
# task 1
df <- read_csv('data/Practicum.5.1.csv')
hist(df$Sale_Price, breaks = 100)

# task 2
df_grouped <- df %>%
  group_by(ID, Sale_Price) %>%
  summarise(SF_total = sum(Value_SF))
df_grouped
View(df)

# task 3
pairs(df_grouped)
