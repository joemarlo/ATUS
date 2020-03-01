library(tidyverse)
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
  



