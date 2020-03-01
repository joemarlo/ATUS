library(tidyverse)
source('Analyses/Helper_functions.R')


# EDA ---------------------------------------------------------------------

apply_weights(df = atussum_0318, groups = 'TESEX') %>% 
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

# plot of average minutes per day by activity and household income
atussum_0318 %>% 
  left_join(distinct(atuscps_0318[, c('TUCASEID', 'HEFAMINC')])) %>%
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
atussum_0318 %>% 
  # join with hh income from CPS
  left_join(distinct(atuscps_0318[, c('TUCASEID', 'HEFAMINC')])) %>%
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
       subtitle = "2003-2018 American Time Use Survey",
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

game.codes <- colnames(atussum_0318)[colnames(atussum_0318) %in% paste0('t', 130101:130199)]

# participation rates in activity by income, each one scaled to sum to 1
atussum_0318 %>% 
  select(TUFNWGTP, TUCASEID, game.codes) %>% 
  left_join(distinct(atuscps_0318[, c('TUCASEID', 'HEFAMINC')])) %>%
  filter(HEFAMINC != -1) %>% 
  # re bucket incomes into five
  mutate(HEFAMINC = case_when(
    HEFAMINC %in% 1:7 ~ 1,
    HEFAMINC %in% 8:11 ~ 8,
    HEFAMINC %in% 12:14 ~ 12,
    HEFAMINC == 15 ~ 15,
    HEFAMINC == 16 ~ 16)) %>% 
  left_join(income.levels) %>% 
  select(-TUCASEID, -HEFAMINC) %>% 
  pivot_longer(cols = -c('TUFNWGTP', 'HH.income')) %>% 
  group_by(name, HH.income) %>% 
  summarize(part.rate = sum(TUFNWGTP * (value > 0)) / sum(TUFNWGTP)) %>% 
  # filter out low participation rate sports
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
                    labels = c('$0-25k', '$25-50k', '$50-100k', '$100-150k', '$150k+')) +
  coord_flip() +
  labs(title = 'Sports and recreational activities',
       subtitle = 'Grouped by household income',
       x = NULL,
       y = 'Proportion of participation',
       caption = '2003-2018 American Time Use Survey') +
  theme(legend.position = 'bottom',
        legend.title = element_blank())

ggsave(filename = "Plots/Activities_by_income.svg",
       plot = last_plot(),
       device = "svg",
       width = 9,
       height = 9)


# waiting -----------------------------------------------------------------

wait.codes <- specific.codes %>%
  filter(grepl('wait', Description, ignore.case = TRUE)) %>% 
  pull(Code) %>% 
  paste0('t', .)

wait.codes <- names(atussum_0318)[names(atussum_0318) %in% wait.codes]

# average wait time by income
atussum_0318 %>% 
  select(TUFNWGTP, TUCASEID, wait.codes) %>% 
  left_join(distinct(atuscps_0318[, c('TUCASEID', 'HEFAMINC')])) %>%
  filter(HEFAMINC != -1) %>% 
  # re bucket incomes into five
  mutate(HEFAMINC = case_when(
    HEFAMINC %in% 1:7 ~ 1,
    HEFAMINC %in% 8:11 ~ 8,
    HEFAMINC %in% 12:14 ~ 12,
    HEFAMINC == 15 ~ 15,
    HEFAMINC == 16 ~ 16)) %>% 
  left_join(income.levels) %>% 
  select(-HEFAMINC) %>% 
  apply_weights(df = ., groups = c('HH.income')) %>% 
  left_join(specific.codes %>% mutate(Code = paste0('t', Code)), 
            by = c('activity' = 'Code')) %>% 
  # filter out low participation rate activties
  group_by(activity) %>% 
  mutate(total.min = sum(weighted.minutes)) %>% 
  ungroup() %>% 
  filter(total.min >= quantile(total.min, .30)) %>% 
  group_by(activity) %>% 
  mutate(weighted.minutes = weighted.minutes / sum(weighted.minutes),
         # calculate income weighted by minutes for sorting purposes
         mean.inc = mean(HH.income * weighted.minutes)) %>%
  ungroup() %>%
  # remove waiting text
  mutate(Description = str_remove(Description, 'Waiting associated with '),
         Description = str_remove(Description, 'Waiting associated w/'),
         Description = str_remove(Description, 'Waiting assoc. w/'),
         Description = str_remove(Description, 'Waiting related to '),
         Description = str_remove(Description, 'Waiting for/'),
         Description = str_to_sentence(Description)) %>% 
  ggplot(aes(x = reorder(Description, -mean.inc), y = weighted.minutes, 
             group = rev(HH.income), fill = as.factor(HH.income))) +
  geom_col(position = 'stack', color = 'white') +
  scale_y_continuous(labels = NULL) +
  scale_fill_manual(values = scales::seq_gradient_pal(low = '#91ebc0', high = '#1d4f37')
                    (seq(0,1, length.out = 5)),
                    labels = c('$0-25k', '$25-50k', '$50-100k', '$100-150k', '$150k+')) +
  coord_flip() +
  labs(title = 'Time spent waiting for or associated with...',
       subtitle = 'Grouped by household income',
       x = NULL,
       y = 'Proportion of minutes waited',
       caption = '2003-2018 American Time Use Survey') +
  theme(legend.position = 'bottom',
        legend.title = element_blank())

ggsave(filename = "Plots/Waiting_by_income.svg",
       plot = last_plot(),
       device = "svg",
       width = 9,
       height = 9)
