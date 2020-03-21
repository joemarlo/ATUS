library(tidyverse)
library(gganimate)
library(maps)
source('Analyses/Helper_functions.R')



# television --------------------------------------------------------------

# scatter plot of watching tv by age and sex
get_minutes(df = atussum_0318, 
            groups = c('TEAGE', 'TESEX'), 
            activities = c('t120303', 't120304'), 
                           simplify = TRUE) %>% 
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female')) %>%
  ggplot(aes(x = TEAGE, y = weighted.minutes, group = TESEX, color = TESEX)) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3),
              se = FALSE, 
              linetype = 'dashed') +
  geom_point(alpha = 0.6) +
  scale_y_continuous(label = function(x) sprintf("%2d:%02d", as.integer(x %/% 60), as.integer(x %% 60))) +
  labs(title = "Average time watching television per day",
       caption = "2003-2018 American Time Use Survey",
       x = "Age",
       y = 'Average hours:minutes per day')

ggsave(filename = "Plots/TV_by_age_sex.svg",
       plot = last_plot(),
       device = "svg",
       width = 7,
       height = 5)


# tv by age and sex with 95% confidence interval
get_SE(df = atussum_0318, groups = c('TEAGE', 'TESEX'), activities = c('t120303', 't120304')) %>% 
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female')) %>% 
  ggplot(aes(x = TEAGE, y = weighted.minutes, group = TESEX, color = TESEX)) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin = weighted.minutes - (2*SE), 
                  ymax = weighted.minutes + (2*SE),
                  fill = TESEX),
              alpha = 0.2) +
  scale_y_continuous(label = function(x) sprintf("%2d:%02d", as.integer(x %/% 60), as.integer(x %% 60))) +
  labs(title = "Average time watching television by age",
       subtitle = 'Range represents 95% confidence interval',
       x = "Age",
       y = 'Average hours:minutes per day',
       caption = "2003-2018 American Time Use Survey")

ggsave(filename = "Plots/TV_by_age_sex_SE.svg",
       plot = last_plot(),
       device = "svg",
       width = 7,
       height = 5)



# housework, cooking & groceries  --------------------------------------------------------

house.codes <- descriptions %>% 
  filter(description == 'Household Activities') %>%
  pull(activity)

atussum_0318 %>% 
  select(TUFNWGTP, TUCASEID, house.codes, TESEX, TUYEAR, TEAGE) %>% 
  filter(TEAGE > 20) %>% 
  mutate(TEAGE = cut(TEAGE, breaks = c(20, 40, 60, 100))) %>% 
  get_min_per_part(groups = c('TESEX', 'TUYEAR', 'TEAGE'), simplify = TRUE) %>%
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female')) %>% 
  select(-minutes.per.participant) %>% 
  pivot_longer(cols = 5:6) %>%
  mutate(name = recode(name, 
                       weighted.minutes = 'Minutes',
                       participation.rate = 'Participation rate',
                       minutes.per.participant = 'Minutes per participant'),
         TEAGE = recode(TEAGE,
                        `(20,40]` = 'Age 20-40',
                        `(40,60]` = '40-60',
                        `(60,100]` = '60+'),
         grouping = paste0(TESEX, '-', TEAGE)) %>%
  ggplot(aes(x = TUYEAR, y = value, group = grouping, color = as.factor(TESEX), )) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = loess,
              se = FALSE,
              aes(linetype = TEAGE)) +
  facet_wrap(~name, ncol = 2, scales = 'free_y') + 
  labs(title = 'Time spent in household activities per day',
       caption = "2003-2018 American Time Use Survey",
       x = NULL,
       y = NULL)

ggsave(filename = "Plots/household.svg",
       plot = last_plot(),
       device = "svg",
       width = 7,
       height = 10)

# cooking -----------------------------------------------------------------

cook.codes <- c('t020201', 't020202', 't020203', 't020299')
grocery.codes <- c('t070101', 't180701')


atussum_0318 %>% 
  select(TUFNWGTP, TUCASEID, cook.codes, grocery.codes, TESEX, TUYEAR, TEAGE) %>% 
  filter(TEAGE > 20) %>% 
  mutate(TEAGE = cut(TEAGE, breaks = c(20, 40, 60, 100))) %>% 
  get_min_per_part(groups = c('TESEX', 'TUYEAR', 'TEAGE'), simplify = TRUE) %>%
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female')) %>% 
  select(-minutes.per.participant) %>% 
  pivot_longer(cols = 5:6) %>%
  mutate(name = recode(name, 
                       weighted.minutes = 'Minutes',
                       participation.rate = 'Participation rate',
                       minutes.per.participant = 'Minutes per participant'),
         TEAGE = recode(TEAGE,
                        `(20,40]` = 'Age 20-40',
                        `(40,60]` = '40-60',
                        `(60,100]` = '60+'),
         grouping = paste0(TESEX, '-', TEAGE)) %>%
  ggplot(aes(x = TUYEAR, y = value, group = grouping, color = as.factor(TESEX), )) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = loess,
              se = FALSE,
              aes(linetype = TEAGE)) +
  facet_wrap(~name, ncol = 2, scales = 'free_y') + 
  labs(title = 'Time spent in cooking and grocery activities per day',
       caption = "2003-2018 American Time Use Survey",
       x = NULL,
       y = NULL)

ggsave(filename = "Plots/cooking.svg",
       plot = last_plot(),
       device = "svg",
       width = 7,
       height = 10)


# Income ------------------------------------------------------------------

# games
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
  select(-HEFAMINC) %>% 
  get_participation(groups = 'HH.income') %>% 
  # filter out low participation rate sports
  group_by(activity) %>% 
  mutate(total.part.rate = sum(participation.rate)) %>% 
  ungroup() %>% 
  filter(total.part.rate >= quantile(total.part.rate, .30)) %>% 
  left_join(specific.codes %>% mutate(Code = paste0('t', Code)), 
            by = c('activity' = 'Code')) %>% 
  filter(!grepl('n\\.e\\.c', Description)) %>%
  group_by(activity) %>% 
  mutate(participation.rate = participation.rate / sum(participation.rate),
         # calculate income weighted by part.rate for sorting purposes
         mean.inc = mean(HH.income * participation.rate)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(Description, -mean.inc), y = participation.rate, 
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
       y = 'Proportion of total participation',
       caption = '2003-2018 American Time Use Survey') +
  theme(legend.position = 'bottom',
        legend.title = element_blank())

ggsave(filename = "Plots/Activities_by_income.svg",
       plot = last_plot(),
       device = "svg",
       width = 7,
       height = 8)


# waiting
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
  get_minutes(groups = 'HH.income') %>% 
  # filter out low participation rate sports
  group_by(activity) %>% 
  mutate(total.min = sum(weighted.minutes)) %>% 
  ungroup() %>% 
  filter(total.min >= quantile(total.min, .30)) %>% 
  left_join(specific.codes %>% mutate(Code = paste0('t', Code)), 
            by = c('activity' = 'Code')) %>% 
  filter(!grepl('n\\.e\\.c', Description)) %>%
  group_by(activity) %>% 
  mutate(weighted.minutes = weighted.minutes / sum(weighted.minutes),
         # calculate income weighted by part.rate for sorting purposes
         mean.inc = mean(HH.income * weighted.minutes)) %>%
  # remove waiting text
  mutate(Description = str_remove(Description, 'Waiting associated with '),
         Description = str_remove(Description, 'Waiting associated w/'),
         Description = str_remove(Description, 'Waiting assoc. w/'),
         Description = str_remove(Description, 'Waiting related to '),
         Description = str_remove(Description, 'Waiting for/'),
         Description = str_to_sentence(Description)) %>% 
  ungroup() %>%
  ggplot(aes(x = reorder(Description, -mean.inc), y = weighted.minutes, 
             group = rev(HH.income), fill = as.factor(HH.income))) +
  geom_col(position = 'stack', color = 'white') +
  scale_y_continuous(labels = NULL) +
  scale_x_discrete(position = 'top') +
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
       width = 7,
       height = 8)


# leisure -----------------------------------------------------------------

leisure.codes <- specific.codes %>%
  filter(grepl('leisure', Description, ignore.case = TRUE)) %>% 
  pull(Code) %>% 
  paste0('t', .)
leisure.codes <- names(atussum_0318)[names(atussum_0318) %in% leisure.codes]

# leisure over time
atussum_0318 %>% 
  get_minutes(groups = 'TUYEAR', activities = leisure.codes, simplify = TRUE) %>%
  ggplot(aes(x = TUYEAR, y = weighted.minutes, fill = weighted.minutes)) +
  geom_col() +
  scale_fill_gradient(low = '#0b2919', high = '#2b7551') +
  labs(title = 'Daily leisure time',
       subtitle = 'Leisure as a primary activity',
       x = NULL,
       y = 'Average minutes',
       caption = '2003-2018 American Time Use Survey') +
  theme(legend.position = 'none')
  
ggsave(filename =  "Plots/Leisure_bars.svg",
       plot = last_plot(),
       device = "svg",
       width = 7,
       height = 4)

# states by leisure activity
leisure.by.state <- atussum_0318 %>% 
  left_join(distinct(atuscps_0318[, c('TUCASEID', 'GESTFIPS')])) %>% 
  get_minutes(groups = 'GESTFIPS', activities = leisure.codes) %>%
  left_join(FIPS[, c('Name', 'FIPS')], by = c(GESTFIPS = 'FIPS')) %>% 
  group_by(Name, GESTFIPS) %>% 
  summarize(total.leisure = sum(weighted.minutes)) %>% 
  ungroup() %>% 
  na.omit()

leisure.by.state %>% 
  fuzzyjoin::stringdist_left_join(map_data("state"), by = c(Name = 'region')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = total.leisure), color = 'white') +
  coord_map(projection = "albers", lat0 = 38, lat1 = 45,
            xlim = c(-120, -75)) +
  scale_fill_gradient(low = '#0b2919', high = '#7dd1a8',
                       name = 'Minutes') +
  labs(title = 'Daily leisure time',
       subtitle = 'Leisure as a primary activity',
       caption = '2003-2018 American Time Use Survey') +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_text(hjust = 0),
        legend.position = 'right')

ggsave(filename = "Plots/Leisure_map.svg",
       plot = last_plot(),
       device = "svg",
       width = 7)

# working tine
work.codes <- descriptions %>% filter(grepl('Work', description)) %>% pull(activity)

atussum_0318 %>% 
  get_min_per_part(groups = 'TUYEAR', activities = work.codes, simplify = TRUE) %>% 
  pivot_longer(cols = 3:5) %>% 
  mutate(name = recode(name, 
                       weighted.minutes = 'Minutes',
                       participation.rate = 'Participation rate',
                       minutes.per.participant = 'Minutes per participant')) %>% 
  ggplot(aes(x = TUYEAR, y = value)) +
  geom_point(alpha = 0.5, color = blog.color) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3),
              se = FALSE, 
              linetype = 'dashed',
              color = blog.color) +
  facet_wrap(~name, scales = 'free_y') +
  labs(title = 'Time spent in work-related activities per day',
       caption = '2003-2018 American Time Use Survey',
       x = NULL,
       y = NULL)
  
ggsave(filename = "Plots/work_hours_year.svg",
       plot = last_plot(),
       device = "svg",
       width = 7,
       height = 4)


# unemployment ------------------------------------------------------------


