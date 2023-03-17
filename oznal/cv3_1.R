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
  filter(nationality_name %in% c("Slovakia", "Czech Republic")) %>%
  group_by(nationality_name, preferred_foot) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = preferred_foot, values_from = count) %>%
  mutate(Total = Left + Right, Proportion = Left / (Left + Right))
  