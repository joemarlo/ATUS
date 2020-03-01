library(tidyverse)
library(gganimate)
library(maps)
source('Analyses/Helper_functions.R')


# scatter plot of watching tv by age and sex
apply_weights(df = atussum_0318, groups = c('TEAGE', 'TESEX'), activities = c('t120303', 't120304')) %>% 
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
       caption = "2003-2018 American Time Use Survey",
       x = "Age",
       y = 'Average hours per day') +
  theme(legend.title = element_blank(),
        legend.position = 'bottom')

ggsave(filename = "Plots/TV_by_age_sex.svg",
       plot = last_plot(),
       device = "svg",
       width = 9,
       height = 5)

# facet plots of all the activities by age
apply_weights(df = atussum_0318, groups = c('TEAGE', 'work.status')) %>% 
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
  facet_wrap(~description, scales = 'free_y', ncol = 2) +
  labs(title = "Average daily time spent on activity by age and working status",
       subtitle = "2003-2018 American Time Use Survey",
       caption = 'Work day defined as working two or more hours',
       x = "Age",
       y = 'Average hours:minutes per day') +
  theme(strip.text = element_text(size = 6),
        legend.position = 'bottom',
        legend.title = element_blank())

ggsave(filename = "Plots/Activities_by_age_work.svg",
       plot = last_plot(),
       device = "svg",
       width = 9,
       height = 17)


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
  apply_weights(df = ., groups = 'TUYEAR') %>% 
  group_by(TUYEAR) %>% 
  summarize(total.sec = sum(weighted.minutes)) %>% 
  ggplot(aes(x = TUYEAR, y = total.sec)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3),
              se = FALSE,
              color = blog.color,
              linetype = 'dashed') +
  labs(title = 'Average weight times in security related activities',
       x = 'Year',
       y = 'Average minutes per day')
  


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

# map of states by leisure activity
# still some issues with matching FIPS codes to states to geojson
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
