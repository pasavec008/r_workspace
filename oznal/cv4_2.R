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
data <- read.csv('D:\\skola2022_2023\\1letny\\r_workspace\\data\\players_22.csv')
head(data)
class(data)
typeof(data)
cleaned_data <- data %>%
  filter(grepl('CAM,', player_positions)) %>%
  select(sofifa_id, player_positions, wage_eur, skill_dribbling, skill_curve, preferred_foot)
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
#if we choose alpha as 5%, preferred_foot with it's p-value > 0.05
#is not significant