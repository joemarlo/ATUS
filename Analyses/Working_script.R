library(tidyverse)
library(gganimate)
library(maps)
source('Analyses/Helper_functions.R')


# summary table -----------------------------------------------------------

# summary table of activities by sex; this takes a few minutes
get_min_per_part(df = atussum_0318, groups = c('TESEX'), simplify = descriptions) %>% 
  # match based on regex code in curated.codes
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female'),
         weighted.hours = round(weighted.minutes / 60, 2),
         participation.rate = round(participation.rate, 4),
         hours.per.participant = round(minutes.per.participant/ 60, 2)) %>% 
  select(-weighted.minutes, -minutes.per.participant ) %>% 
  View('2003-2018 summary')

get_minutes(atussum_0318, 'TESEX', simplify = descriptions) %>% 
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female'),
         weighted.hours = round(weighted.minutes / 60, 2)) %>% 
  select(-weighted.minutes) %>% 
  pivot_wider(names_from = 'TESEX', values_from = 'weighted.hours') %>% 
  View('2003-2018 summary')

# panel plots -------------------------------------------------------------

# facet plots of all the activities by age
get_minutes(atussum_0318, groups = c('TEAGE', 'work.status'), simplify = descriptions) %>% 
  mutate(work.status = recode(as.character(work.status), 'TRUE' = 'Work day', 'FALSE' = 'Leisure day')) %>% 
  ggplot(aes(x = TEAGE, y = weighted.minutes, group = work.status, color = work.status)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 4),
              se = FALSE, 
              linetype = 'dashed') +
  scale_y_continuous(label = function(x) sprintf("%2d:%02d", as.integer(x %/% 60), as.integer(x %% 60))) +
  scale_color_manual(values = c(blog.color, 'coral3')) +
  facet_wrap(~activity, scales = 'free_y', ncol = 3) +
  labs(title = "Average daily time spent on activity by age and working status",
       subtitle = "2003-2018 American Time Use Survey",
       caption = 'Work day defined as working two or more hours',
       x = "Age",
       y = 'Average hours:minutes per day') +
  theme(strip.text = element_text(size = 8),
        legend.position = 'bottom',
        legend.title = element_blank())

ggsave(filename = "Plots/Activities_by_age_work.svg",
       plot = last_plot(),
       device = "svg",
       width = 9,
       height = 17)


# computer use ------------------------------------------------------------

leisure.comp.use <- 't120308'
comp.use <- 't150101'

specific.codes %>%
  filter(grepl('files', Description, ignore.case = TRUE))

# leisure computer use
comp.gif <- atussum_0318 %>% 
  select(TUFNWGTP, TUCASEID, leisure.comp.use, TEAGE, TUYEAR) %>% 
  apply_weights(df = ., groups = c('TEAGE', 'TUYEAR')) %>% 
  group_by(TEAGE, TUYEAR) %>% 
  summarize(total.plant = sum(weighted.minutes)) %>% 
  # group_by(TEAGE) %>% 
  # mutate(total.plant = zoo::rollmean(total.plant, k = 7, fill = NA)) %>%
  # na.omit() %>%
  ggplot(aes(x = TEAGE, y = total.plant)) +
  # geom_line() +
  geom_point(alpha = 0.2, color = blog.color) +
  geom_smooth(method = loess,
              se = FALSE,
              color = blog.color,
              linetype = 'dashed') +
  labs(title = 'Average time spent on leisure computer use: {closest_state}',
       x = 'Age',
       y = 'Average minutes per day') +
  transition_states(TUYEAR, wrap = FALSE)

# animate and save the gif
animate(comp.gif,
        width = 800,
        height = 600,
        fps = 24,
        duration = 14)
anim_save(filename = "Plots/comp.gif")
rm(comp.gif)



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
  

# waiting -----------------------------------------------------------------

wait.codes <- specific.codes %>%
  filter(grepl('wait', Description, ignore.case = TRUE)) %>% 
  pull(Code) %>% 
  paste0('t', .)

wait.codes <- names(atussum_0318)[names(atussum_0318) %in% wait.codes]

# average wait time by income for those who participate
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
  get_min_per_part(groups = c('HH.income')) %>% 
  left_join(specific.codes %>% mutate(Code = paste0('t', Code)), 
            by = c('activity' = 'Code')) %>% 
  # filter out low participation rate activties
  group_by(activity) %>% 
  mutate(total.min = sum(minutes.per.participant)) %>% 
  ungroup() %>% 
  filter(total.min >= quantile(total.min, .30, na.rm = TRUE)) %>% 
  group_by(activity) %>% 
  mutate(weighted.minutes = minutes.per.participant / sum(minutes.per.participant),
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
       subtitle = 'For those that participated',
       x = NULL,
       y = 'Proportion of minutes waited',
       caption = '2003-2018 American Time Use Survey') +
  theme(legend.position = 'bottom',
        legend.title = element_blank())






