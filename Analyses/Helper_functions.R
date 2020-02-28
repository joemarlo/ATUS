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
      text = element_text(family = "Helvetica")
    )
}

theme_set(theme_custom())


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

weigh_it <- function(df, groups, activities = NULL){
  # function takes the inputs, calculates the weights based on the observations
  #  and the groups then returns the weighted data
  # if activities is not provided then all t* columsn are used
  
  if (!all(c("TUCASEID", "TUFINLWGT") %in% names(df))) {
    stop("data must contain variables named TUCASEID and TUFINLWGT")
  }
  
  if (is.null(activities)) activities <- str_subset(names(df), '^t[0-9]')
  
  df %>% 
    select(TUCASEID, TUFINLWGT, groups, activities) %>%
    pivot_longer(cols = -c('TUCASEID', 'TUFINLWGT', groups),
                 names_to = "activity",
                 values_to = 'time') %>%
    group_by_at(vars(activity, groups)) %>% 
    summarize(weighted.minutes = sum(TUFINLWGT * time) / sum(TUFINLWGT)) %>%
    ungroup()
}


# additions to data -------------------------------------------------------

# add indicator for work day to 2018 summary file
atussum_2018 <- atussum_2018 %>% 
  select(contains('t05')) %>% 
  rowSums() %>% 
  enframe() %>% 
  mutate(work.status = value >= 120) %>% 
  select(work.status) %>% 
  bind_cols(atussum_2018)


# Other -------------------------------------------------------------------

# default green color code for blog
blog.color <- '#2b7551'

# formating for plot saves
# ggsave(filename = "Plots/TV_by_age_sex.svg",
#        plot = last_plot(),
#        device = "svg",
#        width = 9,
#        height = 5)

