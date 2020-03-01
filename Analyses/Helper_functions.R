require(tidyverse)

# gg theme ----------------------------------------------------------------

theme_custom <- function() {
  theme_gray() +
    theme(
      panel.grid.minor.y = element_line(color = NA),
      panel.grid.major.y = element_line(color = "gray95"),
      panel.grid.minor.x = element_line(color = NA),
      panel.grid.major.x = element_line(color = "gray95"),
      panel.background = element_rect(fill = NA),
      plot.background = element_rect(
        fill = NA,
        color = "gray95",
        size = 10
      ),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      axis.title = element_text(color = "gray30"),
      axis.ticks = element_line(color = NA),
      strip.background = element_rect(fill = "gray95"),
      strip.text = element_text(
        color = "gray30",
        size = 11,
        face = "bold"
      ),
      plot.title = element_text(color = "gray30",
                                face = "bold"),
      plot.subtitle = element_text(size = 10,
                                   color = "gray30"),
      text = element_text(family = "Helvetica"),
      plot.caption = element_text(face = "italic",
                                  size = 6,
                                  color = 'grey50')
    )
}

theme_set(theme_custom())


# import 2018 files ----------------------------------------------------
# download data here: https://www.bls.gov/tus/datafiles-2018.htm
#  and store in Inputs/ATUS-2018

# dat.files <- list.files('Inputs/ATUS-2018', '*.dat')
# files <- lapply(dat.files, function(file) read_csv(paste0("Inputs/ATUS-2018/", file)))
# names(files) <- str_remove(dat.files, ".dat")
# list2env(files, envir = .GlobalEnv)
# rm(files, dat.files)


# import 2003-2018 files ----------------------------------------------------
# download data here: https://www.bls.gov/tus/datafiles-2018.htm
#  and store in Inputs/ATUS-2018

dat.files <- list.files('Inputs/ATUS-2003-2018', '*.dat')
files <- lapply(dat.files, function(file) read_csv(paste0("Inputs/ATUS-2003-2018/", file)))
names(files) <- str_remove(dat.files, ".dat")
list2env(files, envir = .GlobalEnv)
rm(files, dat.files)

# import field labels -------------------------------------------------------------
# also see: https://www.bls.gov/tus/lexiconwex2018.pdf

specific.codes <- read_delim("Data/specific_codes.csv", 
                             "+", escape_double = FALSE, trim_ws = TRUE)

simple.codes <- read_delim("Data/simple_codes.csv", 
                           "+", escape_double = FALSE, trim_ws = TRUE)

curated.codes <- tribble(
  ~activity, ~description,
  't0101.*',  'Sleep',
  't0102.*',  'Personal Care',
  't02.*',    'Household Activities',
  't03.*',    'Caring For Household Member',
  't04.*',    'Caring For Nonhousehold Members',
  't05.*',    'Work',
  't06.*',    'Education',
  't07.*',    'Consumer Purchases',
  't08.*',    'Professional & Personal Care Services',
  't09.*',    'Household Services',
  't10.*',    'Government Services & Civic Obligations',
  't11.*',    'Eating and Drinking',
  't12.*',    'Socializing, Relaxing, and Leisure',
  't13.*',    'Sports, Exercise, and Recreation',
  't14.*',    'Religious and Spiritual Activities',
  't15.*',    'Volunteer Activities',
  't16.*',    'Telephone Calls',
  't18.*',    'Traveling',
  't50.*',    'Data Codes'
)


# survey weighing function ------------------------------------------------

apply_weights <- function(df, groups, activities = NULL){
  # function takes the inputs, calculates the weights, groups
  #   then returns the weighted data
  # if activities is not provided then all t* columsn are used
  
  if (!all("TUCASEID" %in% names(df) & any(c('TUFINLWGT', 'TUFNWGTP') %in% names(df)))) {
    stop("data must contain variables named TUCASEID and (TUFINLWGT or TUFNWGTP)")
  }
  
  # select the correct weighting variable based on the data provided
  weight.var <- c('TUFNWGTP', 'TUFINLWGT')[c('TUFNWGTP', 'TUFINLWGT') %in% names(df)]
  
  # if no activities are explicitly provided then include all of them
  if (is.null(activities)){
    activities <- str_subset(names(df), '^t[0-9]')
    message('No activities explicitly provided. Returning all activities.')
  }
  
  df %>% 
    select(TUCASEID, weight.var, groups, activities) %>%
    pivot_longer(cols = -c('TUCASEID', weight.var, groups),
                 names_to = "activity",
                 values_to = 'time') %>%
    group_by_at(vars(activity, groups)) %>% 
    summarize(weighted.minutes = sum(.data[[weight.var]] * time) / sum(.data[[weight.var]])) %>%
    ungroup()
}


# additions to data -------------------------------------------------------

# add indicator for work day to 2018 summary file
# atussum_2018 <- atussum_2018 %>% 
#   select(contains('t05')) %>% 
#   rowSums() %>% 
#   enframe() %>% 
#   mutate(work.status = value >= 120) %>% 
#   select(work.status) %>% 
#   bind_cols(atussum_2018)

# add indicator for work day to 2003-2018 summary file
atussum_0318 <- atussum_0318 %>% 
  select(contains('t05')) %>% 
  rowSums() %>% 
  enframe() %>% 
  mutate(work.status = value >= 120) %>% 
  select(work.status) %>% 
  bind_cols(atussum_0318)


# income ------------------------------------------------------------------

# family income CPS data is HEFAMINC

income.levels <- tribble(~HEFAMINC, ~HH.income,
                         1, 0,
                         2, 5000,
                         3, 7500,
                         4, 10000,
                         5, 12500,
                         6, 15000,
                         7, 20000,
                         8, 25000,
                         9, 30000,
                         10, 35000,
                         11, 40000,
                         12, 50000,
                         13, 60000,
                         14, 75000,
                         15, 100000,
                         16, 150000
)



# FIPS state codes --------------------------------------------------------

# scrap FIPS state codes
FIPS <- xml2::read_html('https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696') %>% 
  rvest::html_nodes(xpath = '//table[contains(@class, "data")]') %>% 
  rvest::html_table() %>% 
  .[[1]] %>% 
  rename(State = 'Postal Code')


# state regions -----------------------------------------------------------

state.regions <- read_csv('Data/state_regions.csv')


# Other -------------------------------------------------------------------

# default green color code for blog
blog.color <- '#2b7551'

# formating for plot saves
# ggsave(filename = "Plots/TV_by_age_sex.svg",
#        plot = last_plot(),
#        device = "svg",
#        width = 9,
#        height = 5)

