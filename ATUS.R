library(tidyverse)
library(gganimate)
source('Helper_functions.R')

# import all the files ----------------------------------------------------
# download data here: https://www.bls.gov/tus/datafiles-2018.htm
#  and store in Inputs/ATUS-2018

dat.files <- list.files('Inputs/ATUS-2018', '*.dat')
files <- lapply(dat.files, function(file) read_csv(paste0("Inputs/ATUS-2018/", file)))
names(files) <- str_remove(dat.files, ".dat")
list2env(files, envir = .GlobalEnv)
rm(files, dat.files)


# import field labels -------------------------------------------------------------
# also see: https://www.bls.gov/tus/lexiconwex2018.pdf

specific_codes <- read_delim("Data/specific_codes.csv", 
                             "+", escape_double = FALSE, trim_ws = TRUE)

simple_codes <- read_delim("Data/simple_codes.csv", 
                             "+", escape_double = FALSE, trim_ws = TRUE)


# EDA ---------------------------------------------------------------------

# weighted values by age
age.weighted <- atussum_2018 %>% 
  select(TUCASEID,
         TUFINLWGT,
         TEAGE,
         matches('^t[0-9]')) %>% 
  pivot_longer(cols = -c('TUCASEID', 'TUFINLWGT', 'TEAGE'),
               names_to = "activity",
               values_to = 'time') %>% 
  mutate(weighted.minutes = TUFINLWGT * time) %>% 
  group_by(activity, TEAGE) %>% 
  summarize(weighted.minutes = sum(weighted.minutes) / sum(TUFINLWGT)) %>% 
  ungroup()
  
# scatter plot of watching tv by age
age.weighted %>% 
  filter(activity %in% c('t120303', 't120304')) %>% #watching TV
  group_by(TEAGE) %>% 
  summarize(weighted.minutes = sum(weighted.minutes)) %>% 
  mutate(weighted.hours = weighted.minutes/60) %>% 
  ggplot(aes(x = TEAGE, y = weighted.hours)) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3),
              se = FALSE, 
              color = 'grey70',
              linetype = 'dashed') +
  geom_point(alpha = 0.8) +
  labs(title = "Average time watching television by age",
       subtitle = "2018 American Time Use Survey",
       x = "Age",
       y = 'Average hours per day')

# add the field description
age.weighted <- age.weighted %>% 
  mutate(Code = str_extract(activity, '^*[0-9][0-9]')) %>% 
  left_join(simple_codes)

# facet plots of all the activities by age
age.weighted %>% 
  filter(Code != '50') %>% 
  group_by(TEAGE, Description) %>% 
  summarize(weighted.minutes = sum(weighted.minutes)) %>%
  ggplot(aes(x = TEAGE, y = weighted.minutes)) +
  geom_point(alpha = 0.2, color = blog.color) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 4),
              se = FALSE, 
              color = blog.color,
              linetype = 'dashed') +
  scale_y_continuous(label = function(x) sprintf("%2d:%02d", as.integer(x %/% 60), as.integer(x %% 60))) +
  facet_wrap(~Description, scales = 'free_y', ncol = 3) +
  labs(title = "Average daily time spent on activity by age",
       subtitle = "2018 American Time Use Survey",
       x = "Age",
       y = 'Average hours:minutes per day') +
  theme(strip.text = element_text(size = 6))
  
ggsave(filename = "Plots/Activities_by_age.svg",
       plot = last_plot(),
       device = "svg",
       width = 9,
       height = 11)
  

# split by working status -------------------------------------------------

# calculate the weights and join with the simple codes
work.age <- atussum_2018 %>% 
  select(TUCASEID,
         TUFINLWGT,
         TEAGE,
         matches('^t[0-9]')) %>% 
  pivot_longer(cols = -c('TUCASEID', 'TUFINLWGT', 'TEAGE'),
               names_to = "activity",
               values_to = 'time') %>% 
  mutate(Code = str_extract(activity, '^*[0-9][0-9]')) %>%
  left_join(simple_codes)

# find the IDs where people we're working (more than 120min)
#  then join  back to work.age then summarize the activities
#  by age and this new work status
work.age.weighted <- work.age %>% 
  filter(Code == '05') %>% # exclude generic data code
  group_by(TUCASEID) %>% 
  mutate(work.status = sum(time) > 120) %>%
  select(TUCASEID, work.status) %>%
  distinct() %>% 
  right_join(work.age) %>% 
  mutate(weighted.minutes = TUFINLWGT * time) %>% 
  group_by(activity, TEAGE, work.status) %>% 
  summarize(weighted.minutes = sum(weighted.minutes) / sum(TUFINLWGT)) %>% 
  ungroup()

# add the field description
work.age.weighted <- work.age.weighted %>% 
  mutate(Code = str_extract(activity, '^*[0-9][0-9]')) %>% 
  left_join(simple_codes)

# facet plots of all the activities by age split by working status
work.age.weighted %>% 
  filter(Code != '50') %>% # exclude generic data code
  group_by(TEAGE, Description, work.status) %>%
  summarize(weighted.minutes = sum(weighted.minutes)) %>% 
  ggplot(aes(x = TEAGE, y = weighted.minutes, group = work.status, color = work.status)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 4),
              se = FALSE, 
              linetype = 'dashed') +
  scale_y_continuous(label = function(x) sprintf("%2d:%02d", as.integer(x %/% 60), as.integer(x %% 60))) +
  scale_color_manual(values = c('#c2886d', blog.color), label = c('Leisure day', 'Work day')) +
  facet_wrap(~Description, scales = 'free_y', ncol = 3) +
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


# facet plots of all the activities by age split by working status
work.gif <- work.age.weighted %>% 
  filter(Code != '50') %>%  # exclude generic data code
  group_by(TEAGE, Description, work.status) %>%
  summarize(weighted.minutes = sum(weighted.minutes)) %>% 
  mutate(work.status = if_else(work.status, 'An average work day', 'An average leisure day')) %>% 
  ggplot(aes(x = TEAGE, y = weighted.minutes)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 4),
              se = FALSE, 
              color = blog.color,
              linetype = 'dashed') +
  scale_y_continuous(label = function(x) sprintf("%2d:%02d", as.integer(x %/% 60), as.integer(x %% 60))) +
  facet_wrap(~Description, scales = 'free_y', ncol = 3) +
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
          
