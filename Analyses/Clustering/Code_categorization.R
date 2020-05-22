source('Analyses/Helper_functions.R')

all_activities <- get_min_per_part(df = atussum_0318)
all_activities <- all_activities %>% 
  mutate(code = str_remove(activity, "t"))

descriptions %>% 
  rename(suggested_categorization = description) %>% 
  mutate(activity = str_remove(activity, "t"),
         Code = str_sub(activity, 0, 2)) %>% 
  left_join(simple.codes, by = 'Code') %>% 
  rename(offical_summary_category = Description) %>% 
  left_join(specific.codes, by = c('activity' = 'Code')) %>% 
  rename(offical_name = Description) %>% 
  select(code = activity, offical_name, offical_summary_category, suggested_categorization) %>% 
  left_join(all_activities, by = 'code') %>% 
  mutate(average_minutes = round(weighted.minutes, 2),
         avg_minutes_per_participant = round(minutes.per.participant, 2),
         participation_rate = round(participation.rate, 2)) %>% 
  select(code = activity, offical_name, offical_summary_category, suggested_categorization, 
         average_minutes, avg_minutes_per_participant, participation_rate) %>% 
  write_csv("code_descriptions.csv")


