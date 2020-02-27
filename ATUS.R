library(tidyverse)
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
  summarize(weighted.minutes = sum(weighted.minutes) / sum(TUFINLWGT))
  
# area plot of watching tv by age
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
  group_by(TEAGE, Description) %>% 
  summarize(weighted.minutes = sum(weighted.minutes),
            weighted.hours = sum(weighted.minutes)/60) %>% 
  ggplot(aes(x = TEAGE, y = weighted.hours)) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3),
              se = FALSE, 
              color = blog.color,
              linetype = 'dashed') +
  geom_point(alpha = 0.2, color = blog.color) +
  facet_wrap(~Description, scales = 'free_y', ncol = 3) +
  labs(title = "Average time watching television by age",
       subtitle = "2018 American Time Use Survey",
       x = "Age",
       y = 'Average hours per day') +
  theme(strip.text = element_text(size = 7))
  
ggsave(filename = "Plots/Activities_by_age.svg",
       plot = last_plot(),
       device = "svg",
       width = 9,
       height = 9)
  
