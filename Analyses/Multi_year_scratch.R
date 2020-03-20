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

# scatter plot of watching tv by age and sex
apply_weights(df = atussum_0318, groups = c('TEAGE', 'TESEX'), activities = c('t120303', 't120304')) %>% 
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female')) %>% 
  group_by(TEAGE, TESEX) %>% 
  summarize(weighted.minutes = sum(weighted.minutes)) %>% 
  ggplot(aes(x = TEAGE, y = weighted.minutes, group = TESEX, color = TESEX)) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3),
              se = FALSE, 
              linetype = 'dashed') +
  geom_point(alpha = 0.6) +
  scale_y_continuous(label = function(x) sprintf("%2d:%02d", as.integer(x %/% 60), as.integer(x %% 60))) +
  labs(title = "Average time watching television by age",
       caption = "2003-2018 American Time Use Survey",
       x = "Age",
       y = 'Average hours:minutes per day')

ggsave(filename = "Plots/TV_by_age_sex.svg",
       plot = last_plot(),
       device = "svg",
       width = 9,
       height = 5)

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


# tv confidence interval --------------------------------------------------

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


# housework by sex --------------------------------------------------------

house.codes <- descriptions %>% 
  filter(description == 'Household Activities') %>%
  pull(activity)

atussum_0318 %>% 
  select(TUFNWGTP, TUCASEID, house.codes, TESEX, TUYEAR) %>% 
  get_min_per_part(groups = c('TESEX', 'TUYEAR'), simplify = TRUE) %>%
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female')) %>% 
  pivot_longer(cols = 4:6) %>% 
  ggplot(aes(x = TUYEAR, y = value, group = TESEX, color = as.factor(TESEX))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = loess,
              se = FALSE,
              linetype = 'dashed') +
  # scale_y_continuous(label = function(x) sprintf("%2d:%02d", as.integer(x %/% 60), as.integer(x %% 60))) +
  facet_wrap(~name, ncol = 3, scales = 'free_y') +
  labs(title = 'Average time spent in household activities',
       caption = "2003-2018 American Time Use Survey",
       x = NULL,
       y = NULL)


# cooking -----------------------------------------------------------------

cook.codes <- c('t020201', 't020202', 't020203', 't020299')
grocery.codes <- c('t070101', 't180701')

atussum_0318 %>% 
  select(TUFNWGTP, TUCASEID, cook.codes, TESEX, TUYEAR) %>% 
  get_min_per_part(groups = c('TESEX', 'TUYEAR'), simplify = TRUE) %>%
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female')) %>% 
  pivot_longer(cols = 4:6) %>% 
  ggplot(aes(x = TUYEAR, y = value, group = TESEX, color = as.factor(TESEX))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = loess,
              se = FALSE,
              linetype = 'dashed') +
  facet_wrap(~name, ncol = 3, scales = 'free_y') +
  labs(title = 'Daily time spent cooking',
       caption = "2003-2018 American Time Use Survey",
       x = NULL,
       y = NULL)

atussum_0318 %>% 
  select(TUFNWGTP, TUCASEID, grocery.codes, TESEX, TUYEAR) %>% 
  get_min_per_part(groups = c('TESEX', 'TUYEAR'), simplify = TRUE) %>%
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female')) %>% 
  pivot_longer(cols = 4:6) %>% 
  ggplot(aes(x = TUYEAR, y = value, group = TESEX, color = as.factor(TESEX))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = loess,
              se = FALSE,
              linetype = 'dashed') +
  facet_wrap(~name, ncol = 3, scales = 'free_y') +
  labs(title = 'Daily time spent in grocery shopping',
       caption = "2003-2018 American Time Use Survey",
       x = NULL,
       y = NULL)

# ggsave(filename = "/home/joemarlo/Dropbox/Data/Projects/blog/static/img/posts/ATUS/slide-gallery/tmp4.png",
#        plot = last_plot(),
#        device = "png",
#        width = 9,
#        height = 6)

# security over the years --------------------------------------------------------------------

sec.codes <- specific.codes %>%
  filter(grepl('secu', Description, ignore.case = TRUE)) %>% 
  filter(Description != 'Home security') %>% 
  pull(Code) %>% 
  paste0('t', .)

sec.codes <- names(atussum_0318)[names(atussum_0318) %in% sec.codes]

# average minutes in security
atussum_0318 %>% 
  select(TUFNWGTP, TUCASEID, sec.codes, TUYEAR) %>% 
  get_min_per_part(groups = 'TUYEAR', simplify = TRUE) %>%
  ggplot(aes(x = TUYEAR, y = minutes.per.participant)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3),
              se = FALSE,
              color = blog.color,
              linetype = 'dashed') +
  scale_y_continuous(label = function(x) sprintf("%2d:%02d", as.integer(x %/% 60), as.integer(x %% 60))) +
  labs(title = 'Average time spent in security related activities',
       subtitle = 'For those who participated',
       caption = "2003-2018 American Time Use Survey",
       x = 'Year',
       y = 'Average hours:minutes per day')
  
# houseplants -------------------------------------------------------------

plant.codes <- specific.codes %>%
  filter(grepl('gard', Description, ignore.case = TRUE)) %>% 
  pull(Code) %>% 
  paste0('t', .)

plant.codes <- names(atussum_0318)[names(atussum_0318) %in% plant.codes]

# houseplants
plant.gif <- atussum_0318 %>% 
  select(TUFNWGTP, TUCASEID, plant.codes, TUYEAR, TEAGE) %>% 
  apply_weights(df = ., groups = c('TEAGE', 'TUYEAR')) %>% 
  group_by(TEAGE, TUYEAR) %>% 
  summarize(total.plant = sum(weighted.minutes)) %>% 
  ggplot(aes(x = TEAGE, y = total.plant)) +
  geom_point(alpha = 0.2, color = blog.color) +
  geom_smooth(method = loess,
              se = FALSE,
              color = blog.color,
              linetype = 'dashed') +
  labs(title = 'Average times spent on garden, lawn, and houseplants: {closest_state}',
       x = 'Year',
       y = 'Average minutes per day') +
  transition_states(TUYEAR, wrap = FALSE)

# animate and save the gif
animate(plant.gif,
        width = 800,
        height = 600,
        fps = 24,
        duration = 14)
anim_save(filename = "Plots/plant.gif")
rm(plant.gif)

# houseplants averaged over age
atussum_0318 %>% 
  select(TUFNWGTP, TUCASEID, plant.codes, TUYEAR, TEAGE) %>% 
  apply_weights(df = ., groups = c('TEAGE', 'TUYEAR')) %>% 
  group_by(TEAGE, TUYEAR) %>% 
  summarize(total.plant = sum(weighted.minutes)) %>% 
  group_by(TEAGE) %>% 
  mutate(total.plant = zoo::rollmean(total.plant, k = 5, fill = NA)) %>% 
  na.omit() %>% 
  ggplot(aes(x = TEAGE, y = total.plant)) +
  geom_line() +
  labs(title = 'Average times spent on garden, lawn, and houseplants: {closest_state}',
       x = 'Age',
       y = 'Average minutes per day') +
    transition_states(TUYEAR, wrap = FALSE)


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


# leisure by state --------------------------------------------------------

# GESTFIPS from CPS

leisure.codes <- specific.codes %>%
  filter(grepl('leisure', Description, ignore.case = TRUE)) %>% 
  pull(Code) %>% 
  paste0('t', .)

leisure.codes <- names(atussum_0318)[names(atussum_0318) %in% leisure.codes]

# leisure by age
atussum_0318 %>% 
  apply_weights(groups = c('TEAGE'), activities = leisure.codes) %>% 
  group_by(TEAGE) %>% 
  mutate(total.leisure = sum(weighted.minutes)) %>% 
  ggplot(aes(x = TEAGE, y = total.leisure)) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3),
              se = FALSE,
              color = blog.color,
              linetype = 'dashed') +
  geom_point(alpha = 0.6) +
  labs(title = "Average time participating in leisure",
       subtitle = 'Leisure as a primary activity',
       caption = "2003-2018 American Time Use Survey",
       x = "Age",
       y = 'Average hours per day') +
  theme(legend.title = element_blank(),
        legend.position = 'bottom')

# states by leisure activity
leisure.by.state <- atussum_0318 %>% 
  left_join(distinct(atuscps_0318[, c('TUCASEID', 'GESTFIPS')])) %>% 
  apply_weights(df = ., groups = c('GESTFIPS'), activities = leisure.codes) %>% 
  left_join(FIPS[, c('Name', 'FIPS')], by = c(GESTFIPS = 'FIPS')) %>% 
  group_by(Name, GESTFIPS) %>% 
  summarize(total.leisure = sum(weighted.minutes)) %>% 
  ungroup() %>% 
  na.omit()

# dot plot of leisure by state
leisure.by.state %>% 
  left_join(state.regions, by = c(Name = 'State')) %>% 
  mutate(mean.leisure = mean(total.leisure)) %>% 
  ggplot(aes(x = total.leisure, y = reorder(Name, total.leisure))) +
  geom_point(aes(color = Region)) +
  geom_vline(aes(xintercept = mean.leisure), color = 'grey70') +
  scale_x_continuous(breaks = seq(6, 20, 2)) +
  geom_label(aes(x = mean.leisure + 2, y = 5, 
                label = paste0('State average = ', round(mean.leisure), ' minutes')),
             color = 'white',
             fill = 'grey70') +
  labs(title = 'Average leisure time per day',
       subtitle = 'Leisure as a primary activity',
       caption = '2003-2018 American Time Use Survey',
       x = 'Minutes per day',
       y = NULL) +
  theme(legend.position = 'bottom',
        legend.key = element_rect(fill = NA))

ggsave(filename = "Plots/Leisure_dots.svg",
       plot = last_plot(),
       device = "svg",
       width = 9,
       height = 9)

# state map colored by leisure  
leisure.by.state %>% 
  fuzzyjoin::stringdist_left_join(map_data("state"), by = c(Name = 'region')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = total.leisure), color = 'white') +
  coord_map(projection = "albers", lat0 = 38, lat1 = 45,
            xlim = c(-120, -75)) +
  scale_fill_gradient2(low = '#96d4b6', high = '#11422a',
                       name = 'Minutes') +
  labs(title = 'Daily leisure time',
       subtitle = 'Leisure as a primary activity',
       caption = '2003-2018 American Time Use Survey') +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

ggsave(filename = "Plots/Leisure_map.svg",
       plot = last_plot(),
       device = "svg",
       width = 9)


# job search --------------------------------------------------------------


job.search.codes <- paste0('t', c('050401', '050403', '050404', '050405', '050499', '050401', '180504'))
job.search.codes <- names(atussum_0318)[names(atussum_0318) %in% job.search.codes]

job_search.gif <- atussum_0318 %>% 
  select(TUFNWGTP, TUCASEID, job.search.codes, TEAGE, TUYEAR) %>% 
  apply_weights(df = ., groups = c('TEAGE', 'TUYEAR')) %>% 
  group_by(TEAGE, TUYEAR) %>% 
  summarize(total.search = sum(weighted.minutes)) %>%
  filter(TEAGE < 65) %>% 
  group_by(TEAGE) %>%
  mutate(total.search = zoo::rollmean(total.search, k = 5, fill = NA)) %>%
  na.omit() %>%
  ggplot(aes(x = TEAGE, y = total.search)) +
  geom_point(alpha = 0.2, color = blog.color) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 4),
              color = blog.color,
              se = FALSE,
              linetype = 'longdash') +
  labs(title = 'Average time spent on job search: {closest_state}',
       x = 'Age',
       y = 'Average minutes per day') +
  transition_states(TUYEAR, wrap = FALSE)

# animate and save the gif
animate(job_search.gif,
        width = 600,
        height = 500,
        fps = 24,
        duration = 14)
anim_save(filename = "Plots/job_search.gif")
rm(job_search.gif)


