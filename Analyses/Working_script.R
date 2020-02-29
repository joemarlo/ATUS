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
  


# income ------------------------------------------------------------------

'HEFAMINC' # family income CPS data

income.levels <- tribble(~HEFAMINC, ~HH.income,
        1, 0,
        2, 5000,
        3, 7500,
        4, 10000,
        5, 12500,
        6, 15000,
        7, 20000,
        8, 25000,
        9, 30000,
        10, 35000,
        11, 40000,
        12, 50000,
        13, 60000,
        14, 75000,
        15, 100000,
        16, 150000
        )

# plot of average minutes per day by activity and household income
atussum_2018 %>% 
  left_join(distinct(atuscps_2018[, c('TUCASEID', 'HEFAMINC')])) %>%
  left_join(income.levels) %>% 
  weigh_it(df = ., groups = c('HH.income')) %>% 
  fuzzyjoin::regex_left_join(x = ., y = curated.codes, by = c(activity = 'activity')) %>%
  filter(activity.y != 't50.*') %>% 
  group_by(HH.income, description) %>% 
  summarize(weighted.minutes = sum(weighted.minutes)) %>%
  ggplot(aes(x = HH.income, y = weighted.minutes, color = weighted.minutes)) +
  geom_point() +
  # geom_smooth(method = lm, formula = y ~ splines::bs(x, 4),
  #             se = FALSE, 
  #             color = blog.color,
  #             linetype = 'dashed') +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(label = function(x) sprintf("%2d:%02d", as.integer(x %/% 60), as.integer(x %% 60))) +
  facet_wrap(~description, scales = 'free_y', ncol = 3) +
  labs(title = "Average daily time spent on activity by household income",
       subtitle = "2018 American Time Use Survey",
       x = "Household income",
       y = 'Average hours:minutes per day') +
  theme(strip.text = element_text(size = 6)) +
  theme(legend.title = element_blank(),
        legend.position = 'none')

  
# plot of average minutes per day by activity, household income, and age
atussum_2018 %>% 
  # join with hh income from CPS
  left_join(distinct(atuscps_2018[, c('TUCASEID', 'HEFAMINC')])) %>%
  left_join(income.levels) %>% 
  mutate(HH.inc.cut = cut(HH.income,
                          breaks = seq(0, 150000, by = 50000), 
                          include.lowest = TRUE, ordered_result = TRUE),
         HH.inc.cut = factor(HH.inc.cut, levels = levels(HH.inc.cut))) %>% 
  weigh_it(df = ., groups = c('HH.inc.cut', 'TEAGE', 'work.status')) %>% 
  fuzzyjoin::regex_left_join(x = ., y = curated.codes, by = c(activity = 'activity')) %>%
  filter(activity.y != 't50.*',
         work.status == TRUE) %>% 
  group_by(HH.inc.cut, description, TEAGE) %>% 
  summarize(weighted.minutes = sum(weighted.minutes)) %>%
  ggplot(aes(x = TEAGE, y = weighted.minutes, color = HH.inc.cut, group = HH.inc.cut)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 4),
              se = FALSE,
              linetype = 'longdash') +
  scale_y_continuous(label = function(x) sprintf("%2d:%02d", as.integer(x %/% 60), as.integer(x %% 60))) +
  scale_color_manual(values = c('darkgreen', 'firebrick3','royalblue3'),
                     labels = c( '$0-50k', '$50k-100', '$100k-150')) +
  facet_wrap(~description, scales = 'free_y') +
  labs(title = "Average daily time spent on activity by household income",
       subtitle = "2018 American Time Use Survey",
       caption = 'Work day defined as working two or more hours',
       x = "Age",
       y = 'Average hours:minutes per day') +
  theme(strip.text = element_text(size = 8)) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom')
  
ggsave(filename = "Plots/Activities_by_age_income.svg",
       plot = last_plot(),
       device = "svg",
       width = 9,
       height = 11)

