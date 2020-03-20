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
