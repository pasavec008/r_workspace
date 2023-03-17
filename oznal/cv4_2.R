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

loadcsv