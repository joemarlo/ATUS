source('Analyses/Helper_functions.R')
library(mclust)
library(NbClust)

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

# table of activity descriptiosn with their categorization
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


# EDA  ----------------------------------------------------

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


write_tsv(atus_long, 'Analyses/Clustering/atus.tsv')
write_tsv(demographic_vars, 'Analyses/Clustering/demographic.tsv')



# anlaysis ----------------------------------------------------------------


# check for NAs
dim(na.omit(atus_long)) == dim(atus_long)

# look at the data
atus_long %>% 
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(~description, scales = "free") + 
  labs(title = 'Feature densities (un-transformed)', 
       x = NULL,
       y = NULL)

# log transform the data
cats_to_log <- unique(atus_scaled$description)[
  !(unique(atus_scaled$description) %in% c('Socializing, Relaxing, and Leisure', 'Sleep'))]
atus_log <- atus_long %>%
  filter(description %in% cats_to_log) %>% 
  group_by(description) %>% 
  mutate(value = log(jitter(value + 0.5, factor = 1))) %>% # added noise to account for log 0
  ungroup() %>% 
  bind_rows(
    atus_long %>%
      filter(!(description %in% cats_to_log))
  )
  
atus_log %>% 
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(~description, scales = "free") + 
  labs(title = 'Feature densities (un-transformed)', 
       x = NULL)

atus_cube_root <- atus_long %>%
  filter(description %in% cats_to_log) %>% 
  group_by(description) %>% 
  mutate(value = value^(1/3)) %>%
  ungroup() %>% 
  bind_rows(
    atus_long %>%
      filter(!(description %in% cats_to_log))
  )

atus_cube_root %>%
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap( ~ description, scales = "free") +
  labs(title = 'Feature densities (cube-transformed)',
       x = NULL)

# check spread of the data
atus_cube_root %>% 
  group_by(description) %>% 
  summarize(var = var(value))

# scale the data ??
atus_scaled <- atus_cube_root %>% 
  group_by(description) %>% 
  mutate(value = scale(value)) %>% 
  ungroup()

# check spread of the data
atus_scaled %>% 
  group_by(description) %>% 
  summarize(var = var(value))

# look at the data
atus_scaled %>% 
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(~description, scales = "free") + 
  labs(title = 'Feature densities (cube and scaled transformed)', 
       x = NULL)

# pivot wider and turn into matrix
atus_wide <- atus_scaled %>% 
  pivot_wider(values_from = value, names_from = description) %>% 
  select(-TUCASEID) %>% 
  as.matrix()


# PCA ---------------------------------------------------------------------

# run PCA
atus_pca <- prcomp(atus_wide)
summary(atus_pca)

pca_plot <- atus_pca$x[ , 1:3]
rgl::plot3d(pca_plot)


# resample data using survey weights --------------------------------------

# simple sample
# atus_only <- sample_n(atus_wide, 100000)

# function to scale [0.1]
scale_01 <- function(x) (x - min(x)) / (max(x) - min(x))

# sample using survey weights
total_rows <- nrow(atus_wide)
sample_size <- 10000
rows_to_keep <- sample(1:total_rows, size = sample_size, prob = scale_01(weights$TUFNWGTP), replace = TRUE)

resampled_atus <- atus_wide[rows_to_keep,]

# pairs plot of resampled data
as_tibble(resampled_atus) %>% 
  GGally::ggpairs(mapping = aes(alpha = 0.2))

# run PCA
atus_pca <- prcomp(resampled_atus)
summary(atus_pca)


# memory management -------------------------------------------------------

rm(atus_long, atus_scaled, weights)
gc()


# hierarchical -------------------------------------------------------------

# distance matrix for features
dist_sc <- dist(resampled_atus, method = 'euclidean')
# try single, centroid, and ward (D2) linkage hier clustering
# hcl_single <- hclust(d = dist_sc, method = 'single')
# hcl_centroid <- hclust(d = dist_sc, method = 'centroid')
hcl_ward <- hclust(d = dist_sc, method = 'ward.D2')

library(dendextend)

dev.off()
# par(mfrow = c(3, 1))
# # nearest neighbors method
# plot(hcl_single, hang = -1, main = 'Single Linkage', 
#      labels = FALSE, xlab = '', sub = '')
# # groups centroid
# plot(hcl_centroid, hang = -1, main = 'Centroid Linkage', 
#      labels = FALSE, xlab = '',  sub = '')
# Ward’s minimum variance method, 
# with dissimilarities are squared before clustering
dend <- as.dendrogram(hcl_ward)
hcl_k <- 4
dend_col <- color_branches(dend, k = hcl_k)
plot(dend_col, main = paste0('Ward (D2) Linkage: K = ', hcl_k))


# memory management -------------------------------------------------------

rm(dend, hcl_k, dend_col)
gc()




# Naive Bayes and SVM -----------------------------------------------------




# clustering --------------------------------------------------------------

# get optimal cluster sizes 
cluster_sizes_hcl_ch <- NbClust(data = nba_feat_sc,
                                # it will likely be harder to interpret clusters
                                # past this amount
                                max.nc = 6,
                                method = 'ward.D2',
                                index = 'ch')

# get optimal cluster sizes 
cluster_sizes_hcl_s <- NbClust(data = nba_feat_sc,
                               # it will likely be harder to interpret clusters
                               # past this amount
                               max.nc = 6,
                               method = 'ward.D2',
                               index = 'silhouette')



