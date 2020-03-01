library(tidyverse)
source('Analyses/Helper_functions.R')


# EDA ---------------------------------------------------------------------

apply_weights(df = atussum_2018, groups = 'TEAGE', activities = c('t120303', 't120304'))
apply_weights(df = atussum_2018, groups = c('TEAGE', 'TESEX'), activities = c('t120303', 't120304'))
apply_weights(df = atussum_2018, groups = 'TESEX', activities = c('t120303', 't120304'))

apply_weights(df = atussum_2018, groups = 'TESEX') %>% 
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
  apply_weights(df = ., groups = c('HH.income')) %>% 
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
  apply_weights(df = ., groups = c('HH.inc.cut', 'TEAGE', 'work.status')) %>% 
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
                     labels = c( '$0-50k', '$50-100k', '$100-150k')) +
  facet_wrap(~description, scales = 'free_y', ncol = 2) +
  labs(title = "Average daily time spent on activity by household income during work days",
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
       height = 17)


# games -------------------------------------------------------------------

game.codes <- colnames(atussum_2018)[colnames(atussum_2018) %in% paste0('t', 130101:130199)]

# average minutes per day in activity by income
atussum_2018 %>% 
  # join with hh income from CPS
  left_join(distinct(atuscps_2018[, c('TUCASEID', 'HEFAMINC')])) %>%
  left_join(income.levels) %>% 
  mutate(HH.inc.cut = cut(HH.income,
                          breaks = seq(0, 150000, by = 50000), 
                          include.lowest = TRUE, ordered_result = TRUE),
         HH.inc.cut = factor(HH.inc.cut, levels = levels(HH.inc.cut))) %>%
  apply_weights(df = ., groups = c('HH.inc.cut'),
                activities = game.codes) %>% 
  left_join(specific.codes %>% mutate(Code = paste0('t', Code)), 
            by = c('activity' = 'Code')) %>% 
  ggplot(aes(x = reorder(Description, -weighted.minutes), y = weighted.minutes, 
             group = HH.inc.cut, fill = HH.inc.cut)) +
  geom_col(position = 'stack', color = 'white') +
  scale_fill_discrete(labels = c('$0-50k', '$50-100k', '$100-150k')) +
  coord_flip() +
  labs(title = 'Average daily time spent on activity by household income',
       x = NULL,
       y = 'Minutes') +
  theme(legend.position = 'bottom',
        legend.title = element_blank())

# participation rate
atussum_2018 %>% 
  select(TUFINLWGT, game.codes) %>% 
  pivot_longer(cols = -TUFINLWGT) %>% 
  group_by(name) %>% 
  summarize(part.rate = sum(TUFINLWGT * (value > 0)) / sum(TUFINLWGT)) %>% 
  left_join(specific.codes %>% mutate(Code = paste0('t', Code)), 
            by = c('name' = 'Code')) %>% 
  ggplot(aes(x = reorder(Description, -part.rate), y = part.rate)) +
  geom_col(position = 'stack', color = 'white') +
  # scale_fill_discrete(labels = c('$0-50k', '$50-100k', '$100-150k')) +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  labs(title = 'Average daily time spent on activity by household income',
       x = NULL,
       y = 'Participation rate') +
  theme(legend.position = 'bottom',
        legend.title = element_blank())

# participation rates in activity by income, each one scaled to sum to 1
atussum_2018 %>% 
  select(TUFINLWGT, TUCASEID, game.codes) %>% 
  left_join(distinct(atuscps_2018[, c('TUCASEID', 'HEFAMINC')])) %>%
  # re bucket incomes into five
  mutate(HEFAMINC = case_when(
    HEFAMINC %in% 1:7 ~ 1,
    HEFAMINC %in% 8:11 ~ 8,
    HEFAMINC %in% 12:13 ~ 12,
    HEFAMINC %in% 14:15 ~ 14,
    HEFAMINC == 16 ~ 16)) %>% 
  left_join(income.levels) %>% 
  select(-TUCASEID, -HEFAMINC) %>% 
  pivot_longer(cols = -c('TUFINLWGT', 'HH.income')) %>% 
  group_by(name, HH.income) %>% 
  summarize(part.rate = sum(TUFINLWGT * (value > 0)) / sum(TUFINLWGT)) %>% 
  # filter out low partiicpation rate sports
  group_by(name) %>% 
  mutate(total.part.rate = sum(part.rate)) %>% 
  ungroup() %>% 
  filter(total.part.rate >= quantile(total.part.rate, .30)) %>% 
  left_join(specific.codes %>% mutate(Code = paste0('t', Code)), 
            by = c('name' = 'Code')) %>% 
  filter(!grepl('n\\.e\\.c', Description)) %>%
  group_by(name) %>% 
  mutate(part.rate = part.rate / sum(part.rate),
         # calculate income weighted by part.rate for sorting purposes
         mean.inc = mean(HH.income * part.rate)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(Description, -mean.inc), y = part.rate, 
             group = rev(HH.income), fill = as.factor(HH.income))) +
  geom_col(position = 'stack', color = 'white') +
  scale_y_continuous(labels = NULL) +
  scale_fill_manual(values = scales::seq_gradient_pal(low = '#91ebc0', high = '#1d4f37')
                    (seq(0,1, length.out = 5)),
                    labels = c('$0-25k', '$25-50k', '$50-75k', '$75-150k', '$150k+')) +
  coord_flip() +
  labs(title = 'Sports and recreation by income distribution',
       x = NULL,
       y = 'Proportion of participation',
       caption = '2018 American Time Use Survey') +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        plot.caption = element_text(face = "italic",
                                    size = 5,
                                    color = 'grey50'))

ggsave(filename = "Plots/Activities_by_income.svg",
       plot = last_plot(),
       device = "svg",
       width = 9,
       height = 7)

# eating out --------------------------------------------------------------

specific.codes %>%
  filter(grepl('out', Description, ignore.case = TRUE))




# waiting -----------------------------------------------------------------

specific.codes %>%
  filter(grepl('wait', Description, ignore.case = TRUE)) %>% View



