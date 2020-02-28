library(tidyverse)
library(gganimate)
source('Analyses/Helper_functions.R')


# EDA ---------------------------------------------------------------------

# weigh_it(df = atussum_2018, groups = 'TEAGE', activities = c('t120303', 't120304'))
# weigh_it(df = atussum_2018, groups = c('TEAGE', 'TESEX'), activities = c('t120303', 't120304'))   

# scatter plot of watching tv by age and sex
weigh_it(df = atussum_2018, groups = c('TEAGE', 'TESEX'), activities = c('t120303', 't120304')) %>% 
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female')) %>% 
  group_by(TEAGE, TESEX) %>% 
  summarize(weighted.minutes = sum(weighted.minutes)) %>% 
  mutate(weighted.hours = weighted.minutes/60) %>% 
  ggplot(aes(x = TEAGE, y = weighted.hours, group = TESEX, color = TESEX)) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3),
              se = FALSE, 
              linetype = 'dashed') +
  geom_point(alpha = 0.6) +
  labs(title = "Average time watching television by age",
       subtitle = "2018 American Time Use Survey",
       x = "Age",
       y = 'Average hours per day') +
  theme(legend.title = element_blank(),
        legend.position = 'bottom')

ggsave(filename = "Plots/TV_by_age_sex.svg",
       plot = last_plot(),
       device = "svg",
       width = 9,
       height = 5)

# adding simple codes -----------------------------------------------------

# facet plots of all the activities by age
weigh_it(df = atussum_2018, groups = c('TEAGE')) %>% 
  fuzzyjoin::regex_left_join(x = ., y = curated.codes, by = c(activity = 'activity')) %>%
  filter(activity.y != 't50.*') %>% 
  group_by(TEAGE, description) %>% 
  summarize(weighted.minutes = sum(weighted.minutes)) %>%
  ggplot(aes(x = TEAGE, y = weighted.minutes)) +
  geom_point(alpha = 0.2, color = blog.color) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 4),
              se = FALSE, 
              color = blog.color,
              linetype = 'dashed') +
  scale_y_continuous(label = function(x) sprintf("%2d:%02d", as.integer(x %/% 60), as.integer(x %% 60))) +
  facet_wrap(~description, scales = 'free_y', ncol = 3) +
  labs(title = "Average daily time spent on activity by age",
       subtitle = "2018 American Time Use Survey",
       x = "Age",
       y = 'Average hours:minutes per day') +
  theme(strip.text = element_text(size = 6)) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom')

ggsave(filename = "Plots/Activities_by_age.svg",
       plot = last_plot(),
       device = "svg",
       width = 9,
       height = 11)

# facet plots of all the activities by age and gender
weigh_it(df = atussum_2018, groups = c('TEAGE', 'TESEX')) %>% 
  fuzzyjoin::regex_left_join(x = ., y = curated.codes, by = c(activity = 'activity')) %>%
  filter(activity.y != 't50.*') %>% 
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female')) %>% 
  group_by(TEAGE, TESEX, description) %>% 
  summarize(weighted.minutes = sum(weighted.minutes)) %>%
  ggplot(aes(x = TEAGE, y = weighted.minutes, group = TESEX, color = TESEX)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 4),
              se = FALSE, 
              # color = blog.color,
              linetype = 'dashed') +
  scale_y_continuous(label = function(x) sprintf("%2d:%02d", as.integer(x %/% 60), as.integer(x %% 60))) +
  facet_wrap(~description, scales = 'free_y', ncol = 3) +
  labs(title = "Average daily time spent on activity by age and sex",
       subtitle = "2018 American Time Use Survey",
       x = "Age",
       y = 'Average hours:minutes per day') +
  theme(strip.text = element_text(size = 6)) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom')

ggsave(filename = "Plots/Activities_by_age_sex.svg",
       plot = last_plot(),
       device = "svg",
       width = 9,
       height = 11)


# add indicator for work day ----------------------------------------------

# facet plots of all the activities by age
weigh_it(df = atussum_2018, groups = c('TEAGE', 'work.status')) %>% 
  # match based on regex code in curated.codes
  fuzzyjoin::regex_left_join(x = ., y = curated.codes, by = c(activity = 'activity')) %>%
  # remove data code
  filter(activity.y != 't50.*') %>% 
  mutate(work.status = recode(as.character(work.status), 'TRUE' = 'Work day', 'FALSE' = 'Leisure day')) %>% 
  group_by(TEAGE, work.status, description) %>%
  summarize(weighted.minutes = sum(weighted.minutes)) %>% 
  ggplot(aes(x = TEAGE, y = weighted.minutes, group = work.status, color = work.status)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 4),
              se = FALSE, 
              linetype = 'dashed') +
  scale_y_continuous(label = function(x) sprintf("%2d:%02d", as.integer(x %/% 60), as.integer(x %% 60))) +
  scale_color_manual(values = c(blog.color, 'coral3')) +
  facet_wrap(~description, scales = 'free_y', ncol = 3) +
  labs(title = "Average daily time spent on activity by age and working status",
       subtitle = "2018 American Time Use Survey",
       caption = 'Work day defined as working two or more hours',
       x = "Age",
       y = 'Average hours:minutes per day') +
  theme(strip.text = element_text(size = 6),
        legend.position = 'bottom',
        legend.title = element_blank(),
        plot.caption = element_text(face = "italic",
                                    size = 5,
                                    color = 'grey50'))

ggsave(filename = "Plots/Activities_by_age_work.svg",
       plot = last_plot(),
       device = "svg",
       width = 9,
       height = 11)


# facet plots of all the activities by age and gender for work only days
weigh_it(df = atussum_2018, groups = c('TEAGE', 'TESEX', 'work.status')) %>% 
  # match based on regex code in curated.codes
  fuzzyjoin::regex_left_join(x = ., y = curated.codes, by = c(activity = 'activity')) %>%
  # remove data code
  filter(activity.y != 't50.*',
         work.status == TRUE) %>% 
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female')) %>% 
  group_by(TEAGE, TESEX, description) %>%
  summarize(weighted.minutes = sum(weighted.minutes)) %>% 
  ggplot(aes(x = TEAGE, y = weighted.minutes, group = TESEX, color = TESEX)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 4),
              se = FALSE, 
              linetype = 'dashed') +
  scale_y_continuous(label = function(x) sprintf("%2d:%02d", as.integer(x %/% 60), as.integer(x %% 60))) +
  # scale_color_manual(values = c('darkolivegreen4', 'darkolivegreen2','coral4', 'coral2')) +
  facet_wrap(~description, scales = 'free_y', ncol = 3) +
  labs(title = "Average daily time spent on activity by age and gender during work days",
       subtitle = "2018 American Time Use Survey",
       caption = 'Work day defined as working two or more hours',
       x = "Age",
       y = 'Average hours:minutes per day') +
  theme(strip.text = element_text(size = 6),
        legend.position = 'bottom',
        legend.title = element_blank(),
        plot.caption = element_text(face = "italic",
                                    size = 5,
                                    color = 'grey50'))

ggsave(filename = "Plots/Activities_by_age_sex_workonly.svg",
       plot = last_plot(),
       device = "svg",
       width = 9,
       height = 11)


# gif ---------------------------------------------------------------------

# facet plots of all the activities by age split by working status
work.gif <- weigh_it(df = atussum_2018, groups = c('TEAGE', 'work.status')) %>% 
  # match based on regex code in curated.codes
  fuzzyjoin::regex_left_join(x = ., y = curated.codes, by = c(activity = 'activity')) %>%
  filter(activity.y != '50') %>%  # exclude generic data code
  group_by(TEAGE, description, work.status) %>%
  summarize(weighted.minutes = sum(weighted.minutes)) %>% 
  mutate(work.status = if_else(work.status, 'An average work day', 'An average leisure day')) %>% 
  ggplot(aes(x = TEAGE, y = weighted.minutes)) +
  geom_point(alpha = 0.1, color = blog.color) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 4),
              se = FALSE, 
              color = blog.color,
              linetype = 'dashed') +
  scale_y_continuous(label = function(x) sprintf("%2d:%02d", as.integer(x %/% 60), as.integer(x %% 60))) +
  facet_wrap(~description, scales = 'free_y', ncol = 3) +
  labs(title = "{closest_state}",
       subtitle = "Average daily time spent on activity by age",
       caption = 'Source: 2018 American Time Use Survey\nWork day defined as working two or more hours',
       x = "Age",
       y = 'Average hours:minutes per day') +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        strip.text = element_text(size = 8),
        legend.position = 'bottom',
        legend.title = element_blank(),
        plot.caption = element_text(face = "italic",
                                    size = 7,
                                    color = 'grey50')) +
  transition_states(work.status, wrap = FALSE)

# animate and save the gif
animate(work.gif,
        width = 800,
        height = 980,
        fps = 24,
        duration = 7)
anim_save(filename = "Plots/work.gif")

