library('tidyverse')
library('tidymodels')
library('magrittr')
library('ggplot2')

cric <- modeldata::crickets
cric

# Create a scatter plot with temperature on X and chirps on Y
ggplot(cric, aes(x = temp, y = rate, color = species)) + 
  geom_point() +
  geom_smooth()

# Show correlation
cor(cric$temp, cric$rate)

# Use numeric values in species instead of strings
cric$species <- as.integer(as_factor(cric$species))
view(cric)

# Ordinary Linear Regression (OLR)
model <- lm(rate ~ temp + species, cric)
summary(model)
typeof(model)
model$coefficients

# predict data with model
predict(model, cric[1:3,])

# With tidy we have summary in tibble that can be used in later analysis
tidy(model)
summary(model)

# for each row you can see fitted value and residual (and a few other things)
augment(model)


library('ggfortify')
autoplot(model)
plot(model, 3)

# predict data with model
new_data <- tibble(species = 2, temp = 10:15)
new_data
predict(model, new_data)


data <- read.csv('data/players_22.csv')
view(data)

cleaned_data <- data %>%
  filter(str_detect(player_positions, 'CAM')) %>%
  select(wage_eur, skill_dribbling, skill_curve, preferred_foot)
  
cleaned_data

model <- lm(wage_eur ~ skill_dribbling + skill_curve + preferred_foot, cleaned_data)  
summary(model)

autoplot(model)
pair(model)
ggplot(cleaned_data, mapping = aes(skill_dribbling, wage_eur)) +
  geom_smooth() +
  geom_point()

cleaned_data$preferred_foot <- as.integer(as.factor(cleaned_data$preferred_foot))

pairs(cleaned_data)
cor(cleaned_data)

data <- read.csv('data/Practicum.4.2.csv')
view(data)

data <- data %>% pivot_wider(names_from = Skill, values_from = Value) %>%
  filter(str_detect(player_positions, 'CM')) %>%
  select(wage_eur, preferred_foot, skill_dribbling, skill_curve)

# transformacia do povodneho stavu
#data %>% pivot_longer(cols = skill_moves:skill_fk_accuracy, names_to = 'Skill', values_to = 'Value')

data_temp <- data
data_temp$preferred_foot <- as.integer(as.factor(data$preferred_foot))
pairs(data_temp)
cor(data_temp)

model <- lm(wage_eur ~ preferred_foot + skill_dribbling + skill_curve, data)
summary(model)
autoplot(model)

model <- lm(wage_eur ~ preferred_foot + skill_curve + poly(skill_dribbling, 5), data)
summary(model)
autoplot(model)

#residual sum of square
RSM <- sum(model$residuals^2)
RSM
