library('tidyverse')
library('tidymodels')
library('magrittr')

cric <- modeldata::crickets
model <- summary(lm(data = cric, formula = rate ~ temp + species))
model <- crickets %$% summary(lm(rate ~ temp + species))

model
broom::tidy(model)

#nefunguje?
broom::augment(model)

class(model)
library('ggfortify')
autoplot(model, which = c(1,2,5)) + theme_bw()

summary(model)


#task 1
data <- read.csv('data\\Practicum.4.2.csv')
head(data)
class(data)
typeof(data)
cleaned_data <- data %>%
  filter(player_positions == 'CM') %>%
  pivot_wider(names_from = Skill, values_from = Value)
View(cleaned_data)

#task 2
model <- cleaned_data %$% lm(formula = wage_eur ~ skill_dribbling + skill_curve + preferred_foot)
summary(model)

#task 3 - check for heteroscedasticity
qqline(model$resid)
plot(model$resid ~ model$fitted.values)
plot(cooks.distance(model))
shapiro.test(model$resid)
boxplot(model$residuals)
# we could get rid of outliers for better results

#task 4
summary(model)
#all of predictors have p-value bellow 5% so they are significant

#task 5
cleaned_data
columns_to_plot <- c('skill_dribbling', 'skill_curve')
pairs(cleaned_data[, columns_to_plot])
#we need to use * instead of + to make model with interactions
interaction_model <- cleaned_data %$%
  lm(formula = wage_eur ~ (skill_dribbling * skill_curve) + preferred_foot)

#task 6
summary(model)
summary(interaction_model)
model$coefficients
interaction_model$coefficients

#task 7
all_interaction_model <- cleaned_data %$%
  lm(formula = wage_eur ~ (skill_dribbling + skill_curve + preferred_foot)^2)
summary(all_interaction_model)

# test
plot(cleaned_data$skill_dribbling, cleaned_data$wage_eur)
abline(model, col = "blue")
abline(interaction_model, col = "red")
abline(all_interaction_model, col = "green")
####

#task 8
poly_model <- cleaned_data %$%
  lm(formula = wage_eur ~ poly(skill_dribbling, degree = 3))
summary(poly_model)

degree_five_model <- cleaned_data %$%
  lm(formula = wage_eur ~ poly(skill_dribbling, degree = 5))
summary(degree_five_model)

#task 9
res_sum_model <- sum(model$residuals^2) 
res_sum_model

res_sum_three_model <- sum(poly_model$residuals^2)
res_sum_three_model

res_sum_five_model <- sum(degree_five_model$residuals^2)
res_sum_five_model
