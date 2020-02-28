library(tidyverse)
source('Analyses/Helper_functions.R')


# EDA ---------------------------------------------------------------------

weigh_it(df = atussum_2018, groups = 'TEAGE', activities = c('t120303', 't120304'))
weigh_it(df = atussum_2018, groups = c('TEAGE', 'TESEX'), activities = c('t120303', 't120304'))
weigh_it(df = atussum_2018, groups = 'TESEX', activities = c('t120303', 't120304'))

weigh_it(df = atussum_2018, groups = 'TESEX') %>% 
  fuzzyjoin::regex_left_join(x = ., y = curated.codes, by = c(activity = 'activity')) %>%
  filter(activity.y != 't50.*') %>% 
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female')) %>%
  group_by(TESEX, description) %>% 
  summarize(weighted.minutes = sum(weighted.minutes)) %>%
  ggplot(aes(x = TESEX, y = weighted.minutes)) +
  geom_col() +
  facet_wrap(~description, scales = 'free_y', ncol = 3) +
  labs(title = "Average daily time spent on activity by age",
       subtitle = "2018 American Time Use Survey",
       x = "Sex",
       y = 'Average hours:minutes per day') +
  theme(strip.text = element_text(size = 6)) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom')
  