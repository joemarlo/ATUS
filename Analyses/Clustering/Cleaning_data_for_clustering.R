source('Analyses/Helper_functions.R')

# remove unneccessary data from environment
# memory management is important as datasets are large
rm(atusact_0318, atusresp_0318, atusrost_0318, atuswgts_0318, atuswho_0318)
gc()

# categorize the activities -----------------------------------------------

curated.codes <- tribble(
  ~activity, ~description,
  't0101.*',  'Sleep',
  't010[2-9].*','Personal Care',
  't019.*',   'Personal Care',
  't1801.*',  'Personal Care',
  't02.*',    'Household Activities',
  't1802.*',  'Household Activities',
  't03.*',    'Caring For Household Member',
  't1803.*',  'Caring For Household Member',
  't04.*',    'Caring For Nonhousehold Members',
  't1804.*',  'Caring For Nonhousehold Members',
  't05.*',    'Work',
  't1805.*',  'Work',
  't06.*',    'Education',
  't1806.*',  'Education',
  't07.*',    'Consumer Purchases',
  't1807.*',  'Consumer Purchases',
  't08.*',    'Professional & Personal Care Services',
  't1808.*',  'Professional & Personal Care Services',
  't09.*',    'Other',
  't1809.*',  'Other',
  't10.*',    'Other',
  't1810.*',  'Other',
  't11.*',    'Eating and Drinking',
  't1811.*',  'Eating and Drinking',
  't12.*',    'Socializing, Relaxing, and Leisure',
  't1812.*',  'Socializing, Relaxing, and Leisure',
  't13.*',    'Sports, Exercise, and Recreation',
  't1813.*',  'Sports, Exercise, and Recreation',
  't14.*',    'Religious and Spiritual',
  't1814.*',  'Religious and Spiritual',
  't15.*',    'Volunteer',
  't1815.*',  'Volunteer',
  't16.*',    'Other',
  't1816.*',  'Other',
  't1818.*',  'Other',
  't1819.*',  'Other',
  't189.*',  'Other',
  't50.*',    'Other'
)

# all codes matched to description
descriptions <- atussum_0318 %>% 
  select(matches('^t[0-9].')) %>% 
  pivot_longer(cols = everything(), names_to = 'activity') %>%
  select(activity) %>% 
  distinct() %>% 
  fuzzyjoin::regex_left_join(y = curated.codes) %>% 
  select(activity = activity.x, description)

# get counts by categorie
all_activities <- get_min_per_part(df = atussum_0318) %>% 
  mutate(code = str_remove(activity, "t"))

# table of activity description with their categorization
descriptions %>% 
  rename(suggested_categorization = description) %>% 
  mutate(activity = str_remove(activity, "t"),
         Code = str_sub(activity, 0, 2)) %>% 
  left_join(simple.codes, by = 'Code') %>% 
  rename(offical_summary_category = Description) %>% 
  left_join(specific.codes, by = c('activity' = 'Code')) %>% 
  rename(offical_name = Description) %>% 
  select(code = activity, offical_name, offical_summary_category, suggested_categorization) %>% 
  left_join(all_activities, by = 'code') %>% 
  mutate(average_minutes = round(weighted.minutes, 2),
         avg_minutes_per_participant = round(minutes.per.participant, 2),
         participation_rate = round(participation.rate, 2)) %>% 
  select(code = activity, offical_name, offical_summary_category, suggested_categorization, 
         average_minutes, avg_minutes_per_participant, participation_rate) %>% 
  write_tsv("Analyses/Clustering/code_descriptions.csv")
rm(all_activities)


# create final datasets  ----------------------------------------------------

# summarize the data for each observations with categorizes equal to 'descriptions'
atus_long <- atussum_0318 %>% 
  # filter to weekend only, no holidays
  filter(TUDIARYDAY %in% c(1, 7),
         TRHOLIDAY == 0) %>% 
  select(c('TUCASEID', matches('^t[0-9].'))) %>% 
  pivot_longer(cols = matches('^t[0-9].'), names_to = 'activity') %>% 
  left_join(descriptions) %>% 
  group_by(TUCASEID, description) %>% 
  summarize(value = sum(value)) %>% 
  ungroup() %>% 
  rename(ID = TUCASEID)

# from CPS data, get race, marriage status, education, and state 
CPS_vars <- atuscps_0318 %>%
  # filter so only person repsonding to ATUS is included
  filter(TULINENO == 1) %>% 
  select(TUCASEID, PEMARITL, PTDTRACE, PEEDUCA, GESTFIPS) %>%
  mutate(
    married = as.numeric(PEMARITL == 1),
    race = case_when(
      PTDTRACE == 1 ~ 'white',
      PTDTRACE == 2 ~ 'black',
      PTDTRACE == 4 ~ 'asian',
      TRUE ~ 'other'
    ),
    education = case_when(
      PEEDUCA < 38 ~ "Did not graduate from HS",
      PEEDUCA == 39 ~ "HS",
      PEEDUCA %in% 40:42 ~ "Some college",
      PEEDUCA == 43 ~ "Bachelors",
      PEEDUCA == 44 ~ "Masters",
      PEEDUCA %in% 44:45 ~ "Doctoral"
    )
  ) %>% 
  left_join(FIPS[, c('Name', 'FIPS')], by = c(GESTFIPS = 'FIPS')) %>%
  select(ID = TUCASEID, married, race, education, state = Name) %>% 
  distinct()

# set income levels to match data code
income.levels.HUFAMINC <- tribble(
  ~HUFAMINC, ~HH.income.new,
  1, 0,
  2, mean(5000, 7499),
  3, mean(7500, 9999),
  4, mean(10000, 12499),
  5, mean(12500, 14999),
  6, mean(15000, 19999),
  7, mean(20000, 24999),
  8, mean(25000, 29999),
  9, mean(30000, 34999),
  10, mean(35000, 39999),
  11, mean(40000, 49999),
  12, mean(50000, 59999),
  13, mean(60000, 74999),
  14, mean(75000, 99999),
  15, mean(100000, 149999),
  16, 150000
)

# from ATUS data, get weights, age, sex, children, income,  
atus_vars <- atussum_0318 %>% 
  select(TUCASEID, survey_weight = TUFNWGTP, age = TEAGE,
         sex = TESEX, age_youngest = TRYHHCHILD, n_child = TRCHILDNUM) %>%
  mutate(age_youngest = ifelse(age_youngest == -1, NA, age_youngest)) %>%
  left_join(distinct(atuscps_0318[, c('TUCASEID', 'HEFAMINC', 'HUFAMINC')])) %>%
  left_join(income.levels) %>% 
  left_join(income.levels.HUFAMINC) %>% 
  mutate(HH.income = pmax(HH.income, HH.income.new, na.rm = TRUE)) %>% 
  select(-c('HEFAMINC', 'HUFAMINC', 'HH.income.new')) %>% 
  rename(ID = TUCASEID,
         HH_income = HH.income) %>% 
  distinct()

# final dataset of demographic variables
demographic_vars <- atus_vars %>% 
  left_join(CPS_vars, by = 'ID') %>% 
  semi_join(atus_long)

# examine counts of each variable
apply(demographic_vars, 2, table, exclude = NULL)

# dummy code age_youngest 
demographic_vars <- demographic_vars %>% 
  mutate(child_under_12 = as.numeric(age_youngest < 12),
         child_under_12 = replace_na(child_under_12, 0))
table(demographic_vars$child_under_12)


# write out the final datasets --------------------------------------------
write_tsv(atus_long, 'Analyses/Clustering/atus.tsv')
write_tsv(demographic_vars, 'Analyses/Clustering/demographic.tsv')


