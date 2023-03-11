library('tidyverse')

df <- read_csv('C:/R/r_workspace/data/players_22.csv')
View(df)

# Slovaks and Czechs
df_czsk <- df[df$nationality_id %in% c(43, 12),]
df_czsk %>%
  group_by(df_czsk$preferred_foot) %>%
  summarize(n())

# All nationalities
df_all_with_perf <- df %>%
  group_by(nationality_name, preferred_foot) %>%
  summarize(n_of_players = n(), performance = sum(overall)/n()) %>%
  arrange(nationality_name)
View(df_all_with_perf)

df_to_graph <- df_all_with_perf %>%
  group_by(preferred_foot) %>%
  summarize(performance = sum(performance) / n())
df_to_graph
barplot(df_to_graph$performance, names.arg = df_to_graph$preferred_foot, col = c('red', 'blue'))


# task 4
df %>% select(nationality_name, preferred_foot) %>%
  filter(nationality_name %in% c('Slovakia', 'Czech Republic')) %>%
  group_by(nationality_name, preferred_foot) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = preferred_foot, values_from = count) %>%
  mutate(Total = Left + Right, Proportion = Left / (Left + Right))


# task 5 - Use table that shows the number of Slovak and Czech
# left/right-footed players. Append column summaries
# (e.g. sum, median, min, max, etc.) to the table as the bottom row(s).
t5 <- df %>% select(nationality_name, preferred_foot) %>%
  filter(nationality_name %in% c("Slovakia", "Czech Republic")) %>%
  group_by(nationality_name, preferred_foot) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = preferred_foot, values_from = count)

t5[nrow(t5) + 1,] <- list('Total', sum(t5$Left), sum(t5$Right))
t5[nrow(t5) + 1,] <- list('Median', median(t5$Left), median(t5$Right))
t5[nrow(t5) + 1,] <- list('Min', min(t5$Left), min(t5$Right))
t5[nrow(t5) + 1,] <- list('Max', max(t5$Left), max(t5$Right))
t5

# task 6 - Retrieve positions at which Slovak left footed players play.
task6 <- df %>% select(nationality_name, preferred_foot, player_positions) %>%
  filter(nationality_name == 'Slovakia' & preferred_foot == 'Left') %>%
  separate_longer_delim(player_positions, delim = ', ') %>%
  group_by(nationality_name, preferred_foot) %>%
  summarise(toString(unique(player_positions)))
task6

# task 7 - Present your result as in a tabular form with counts for each
# position. Only one playing position per column is allowed.
# Convert empty cells/NAs to 0s.
task7 <- df %>% select(nationality_name, preferred_foot, player_positions) %>%
  filter(nationality_name == 'Slovakia' & preferred_foot == 'Left') %>%
  separate_longer_delim(player_positions, delim = ', ') %>%
  group_by(nationality_name, preferred_foot, player_positions) %>%
  summarise(count_on_position = n())
task7

# task 8 - Left footers are thought to be more creative offensive players.
# But how do they fare as defensive players? Let’s find the difference
# in their defensive rating (column defending) and split the resulting
# data according to player’s height. Compare righties and lefties.
task8 <- df %>% select(preferred_foot, defending, height_cm) %>%
  group_by(height_cm,preferred_foot) %>%
  summarise(average = mean(defending), number_of_players = n())
View(task8)

# task 9: Find Slovak and Czech left footed players whose data show the
# biggest difference between their overall and potential scores.
# These guys will be the prime targets for any scout.
# Be gentleman and sort the resulting table in a descending order.
task9 <- df %>% select(short_name, nationality_name, preferred_foot, overall, potential) %>%
  filter(nationality_name %in% c('Slovakia', 'Czech Republic') & preferred_foot == 'Left') %>%
  mutate(diff = potential - overall) %>%
  arrange(desc(diff))
  
task9 

# task 10: Which Slovak and Czech left footed players have the highest
# BMI and at which position do they play?
task10 <- df %>% select(short_name, nationality_name, preferred_foot, weight_kg, height_cm, player_positions) %>%
  filter(nationality_name %in% c('Slovakia', 'Czech Republic') & preferred_foot == 'Left') %>%
  mutate(bmi = weight_kg / (height_cm/100)**2) %>%
  arrange(desc(bmi))
task10
