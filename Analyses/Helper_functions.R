require(tidyverse)
require(rvest)


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
                                  color = 'grey50'),
      legend.title = element_blank(),
      legend.position = 'bottom',
      legend.key = element_rect(fill = NA)
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

# curated.codes <- tribble(
#   ~activity, ~description,
#   't0101.*',  'Sleep',
#   't010[2-9].*','Personal Care',
#   't019.*',   'Personal Care',
#   't1801.*',  'Personal Care',
#   't02.*',    'Household Activities',
#   't1802.*',  'Household Activities',
#   't03.*',    'Caring For Household Member',
#   't1803.*',  'Caring For Household Member',
#   't04.*',    'Caring For Nonhousehold Members',
#   't1804.*',  'Caring For Nonhousehold Members',
#   't05.*',    'Work',
#   't1805.*',  'Work',
#   't06.*',    'Education',
#   't1806.*',  'Education',
#   't07.*',    'Consumer Purchases',
#   't1807.*',  'Consumer Purchases',
#   't08.*',    'Professional & Personal Care Services',
#   't1808.*',  'Professional & Personal Care Services',
#   't09.*',    'Household Services',
#   't1809.*',  'Household Services',
#   't10.*',    'Government Services & Civic Obligations',
#   't1810.*',  'Government Services & Civic Obligations',
#   't11.*',    'Eating and Drinking',
#   't1811.*',  'Eating and Drinking',
#   't12.*',    'Socializing, Relaxing, and Leisure',
#   't1812.*',  'Socializing, Relaxing, and Leisure',
#   't13.*',    'Sports, Exercise, and Recreation',
#   't1813.*',  'Sports, Exercise, and Recreation',
#   't14.*',    'Religious and Spiritual Activities',
#   't1814.*',  'Religious and Spiritual Activities',
#   't15.*',    'Volunteer Activities',
#   't1815.*',  'Volunteer Activities',
#   't16.*',    'Telephone Calls',
#   't1816.*',  'Telephone Calls',
#   't18.*',    'Traveling',
#   't1818.*',  'Other',
#   't1819.*',  'Other',
#   't189.*',  'Other',
#   't50.*',    'Other'
# )

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
  't09.*',    'Household Services',
  't1809.*',  'Household Services',
  't10.*',    'Other',
  't1810.*',  'Other',
  't11.*',    'Eating and Drinking',
  't1811.*',  'Eating and Drinking',
  't12.*',    'Socializing, Relaxing, and Leisure',
  't1812.*',  'Socializing, Relaxing, and Leisure',
  't13.*',    'Sports, Exercise, and Recreation',
  't1813.*',  'Sports, Exercise, and Recreation',
  't14.*',    'Religious and Volunteer Activities',
  't1814.*',  'Religious and Volunteer Activities',
  't15.*',    'Religious and Volunteer Activities',
  't1815.*',  'Religious and Volunteer Activities',
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


# survey weighing function ------------------------------------------------

apply_weights <- function(df, groups, activities = NULL){
  # function takes the inputs, calculates the weights, groups
  #   then returns the weighted minutes
  # if activities is not provided then all t* columsn are used
  
  message('apply_weights() is deprecated. Use get_minutes()')
  
  if (!all("TUCASEID" %in% names(df) & any(c('TUFINLWGT', 'TUFNWGTP') %in% names(df)))) {
    stop("data must contain variables named TUCASEID and (TUFINLWGT or TUFNWGTP)")
  }
  
  # select the correct weighting variable based on the data provided
  weight.var <- c('TUFNWGTP', 'TUFINLWGT')[c('TUFNWGTP', 'TUFINLWGT') %in% names(df)]
  
  # if no activities are explicitly provided then include all of them
  if (is.null(activities)){
    activities <- str_subset(names(df), '^t[0-9]')
    message('No activities explicitly provided. Returning all activities in dataframe.')
  }
  
  df %>% 
    select(TUCASEID, weight.var, groups, activities) %>%
    pivot_longer(cols = -c('TUCASEID', weight.var, groups),
                 names_to = "activity",
                 values_to = 'time') %>%
    group_by_at(vars(activity, groups)) %>% 
    summarize(weighted.minutes = sum(.data[[weight.var]] * time) / sum(.data[[weight.var]]),
              .groups = 'drop') %>%
    ungroup()
}

get_minutes <- function(df, groups = NULL, activities = NULL, simplify = NULL){
  # function takes the inputs, calculates the weights, groups
  #   then returns the weighted minutes
  # if activities is not provided then all t* columsn are used
  # if simplify = TRUE then activities will be grouped into one
  # if simplify = data frame then activities will be grouped by the dataframe
  #   data frame should contain columns 'activity' with the codes and 'description' representing group names
  
  if (!all("TUCASEID" %in% names(df) & any(c('TUFINLWGT', 'TUFNWGTP') %in% names(df)))) {
    stop("data must contain variables named TUCASEID and (TUFINLWGT or TUFNWGTP)")
  }
  
  # if no activities are explicitly provided then include all of them
  if (is.null(activities)){
    activities <- str_subset(names(df), '^t[0-9]')
    message('get_minutes(): No activities explicitly provided. Returning all activities in dataframe.')
  }
  
  if (isTRUE(simplify)){
    simplify <- tibble(activity = activities, 
                       description = 'All activites provided')
  }
  
  if (!is.null(simplify)) {
    if (!is.data.frame(simplify) | !(all(names(simplify) %in% c('activity', 'description')))) {
      stop(
        "if simplifying then simplify must be a data frame with columns 'activity' and 'description'"
      )
    }
  }
  
  # select the correct weighting variable based on the data provided
  weight.var <- c('TUFNWGTP', 'TUFINLWGT')[c('TUFNWGTP', 'TUFINLWGT') %in% names(df)]
  
  df %>% 
    select(TUCASEID, weight.var, groups, activities) %>%
    pivot_longer(cols = -c('TUCASEID', weight.var, groups),
                 names_to = "activity",
                 values_to = 'time') %>%
    group_by_at(vars(activity, groups)) %>% 
    summarize(weighted.minutes = sum(.data[[weight.var]] * time) / sum(.data[[weight.var]]),
              .groups = 'drop') %>%
    ungroup() %>%
    {
      # if simplifying, then left join to get the descriptions and then
      #  sum the minutes
      if(!is.null(simplify)) {
        left_join(x = .,
                  y = simplify,
                  by = 'activity') %>% 
        select(activity = description, groups, weighted.minutes) %>% 
        group_by_at(vars(activity, groups)) %>%
        summarize(weighted.minutes = sum(weighted.minutes),
                  .groups = 'drop') %>% 
        ungroup()
      } else .
    }
}

get_participation <- function(df, groups = NULL, activities = NULL, simplify = NULL) {
  # function returns the weighted participation rate per groups and activities
  # if activities is not provided then all t* columsn are used
  # if aggregate = TRUE then collapse activities into one activity
  # if simplify = TRUE then activities will be grouped into one
  # if simplify = data frame then activities will be grouped by the dataframe
  #   data frame should contain columns 'activity' with the codes and 'description' representing group names
  
  if (!all("TUCASEID" %in% names(df) & any(c('TUFINLWGT', 'TUFNWGTP') %in% names(df)))) {
    stop("data must contain variables named TUCASEID and (TUFINLWGT or TUFNWGTP)")
  }
  
  # if no activities are explicitly provided then include all of them
  if (is.null(activities)){
    activities <- str_subset(names(df), '^t[0-9]')
    message('get_participation(): No activities explicitly provided. Returning all activities.')
  }
  
  if (isTRUE(simplify)){
    simplify <- tibble(activity = activities, 
                       description = 'All activites provided')
  }
  
  if (!is.null(simplify)) {
    if (!is.data.frame(simplify) | !(all(names(simplify) %in% c('activity', 'description')))) {
      stop(
        "if simplifying then simplify must be a data frame with columns 'activity' and 'description'"
      )
    }
  }
  
  weight.var <- c('TUFNWGTP', 'TUFINLWGT')[c('TUFNWGTP', 'TUFINLWGT') %in% names(df)]
  
  df %>%
    select(TUCASEID, weight.var, groups, activities) %>%
    pivot_longer(
      cols = -c('TUCASEID', weight.var, groups),
      names_to = "activity",
      values_to = 'time'
    ) %>%
    {
      # if simplifying, then left join to get the descriptions and then
      #  sum the minutes
      if (!is.null(simplify)) {
        left_join(x = .,
                  y = simplify,
                  by = 'activity') %>%
          select(TUCASEID, weight.var, groups, activity = description, groups, time)
      } else .
    } %>%
    group_by_at(vars(activity, groups)) %>%
    group_modify(~ {
      # sum the distinct weights that have time > 0
      num <- .x %>%
        filter(time > 0) %>%
        select(TUCASEID, weight.var) %>%
        distinct() %>%
        pull(weight.var) %>%
        sum()
      
      # sum all distinct weights
      denom <- select(.x, TUCASEID, weight.var) %>%
        distinct() %>%
        pull(weight.var) %>%
        sum()
      
      return(tibble(participation.rate = num / denom))
    }) %>%
    ungroup()
}

get_min_per_part <- function(df, groups = NULL, activities = NULL, simplify = NULL) {
  # function returns the weighted minutes per group of people who participated in that activity
  # since it calls get_minutes() and get_participation() it also returns the weighted minutes
  #  and the weighted participation rate
  # if activities is not provided then all t* columsn are used
  # if simplify = TRUE then activities will be grouped into one
  # if simplify = data frame then activities will be grouped by the dataframe
  #   data frame should contain columns 'activity' with the codes and 'description' representing group names
  
  
  min.df <- get_minutes(df = df, groups = groups, activities = activities, simplify = simplify)
  part.df <- get_participation(df = df, groups = groups, activities = activities, simplify = simplify)
  
  if (nrow(min.df) != nrow(part.df)) {
    stop("Issue in matching minutes and participation data.frames. Number of rows does not match.")
  }
  
  rslts <- bind_cols(min.df, part.df) %>% 
    mutate(minutes.per.participant = weighted.minutes / participation.rate) %>% 
    select(activity, groups, weighted.minutes, participation.rate, minutes.per.participant)
  
  # fix NaNs; NaNs produced when participation.rate is 0
  rslts$minutes.per.participant[is.nan(rslts$minutes.per.participant)] <- 0
  
  return(rslts)
}

get_SE <- function(df, groups, activities = NULL) {
  # function returns the standard error of the weighted means
  # see get_minutes() for underlying calculations
  # currently only works with the multi-year 2003-2018 data
  
  # calculate the original statsitic
  y0 <- get_minutes(
    df = df,
    groups = groups,
    activities = activities,
    simplify = TRUE
  )
  
  # repeat the statistic calculation using each weight
  statistics <- apply(atuswgts_0318[, -1], MARGIN = 2, FUN = function(wgt) {
    df[, 'TUFNWGTP'] <- wgt
    minutes <- get_minutes(
      df = df,
      groups = groups,
      activities = activities,
      simplify = TRUE
    )
    return(minutes$weighted.minutes)
    }
  )
  
  # calculate the standard error. See https://www.bls.gov/tus/atususersguide.pdf
  y0$SE <- sqrt((4 / 160) * rowSums((statistics - y0$weighted.minutes) ^ 2))
  
  return(y0)
}


# function testing --------------------------------------------------------

# game.codes <- colnames(atussum_0318)[colnames(atussum_0318) %in% paste0('t', 130101:130199)]
# get_minutes(atussum_0318, 'TESEX', simplify = descriptions)
# grouping <- enframe(game.codes) %>% 
#   mutate(description = c(rep('yes', 30), rep('no', 7))) %>% 
#   select(activity = value, description)
# get_minutes(atussum_0318, 'TESEX', game.codes, simplify = grouping)
# get_participation(atussum_0318, 'TESEX', simplify = descriptions)
# get_participation(atussum_0318, 'TESEX', game.codes, simplify = grouping)
# rm(grouping)
# 
# get_participation(atussum_0318, groups = NULL, activities = game.codes, simplify = TRUE)
# get_minutes(atussum_0318, groups = NULL, activities = game.codes, simplify = TRUE)
# all.equal(
#   apply_weights(atussum_0318, groups = NULL, activities = game.codes),
#   get_minutes(atussum_0318, groups = NULL, activities = game.codes)
# )
# get_min_per_part(atussum_0318, groups = NULL, activities = game.codes, simplify = TRUE)
# 
# # check function against this pdf: https://www.bls.gov/tus/a1-2018.pdf
# get_min_per_part(df = atussum_2018, groups = c('TESEX'), simplify = TRUE) %>%
#   # match based on regex code in curated.codes
#   mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female'),
#          weighted.hours = round(weighted.minutes / 60, 2),
#          participation.rate = round(participation.rate, 4),
#          hours.per.participant = round(minutes.per.participant/ 60, 2)) %>%
#   select(-weighted.minutes, -minutes.per.participant ) %>%
#   pivot_wider(id = activity, names_from = 'TESEX', values_from = 3:5) %>%
#   View('2018 summary')

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

# scrape FIPS state codes if it doesn't exist
if (!file.exists('Data/FIPS.csv')) {
  xml2::read_html('https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696') %>% 
  rvest::html_nodes(xpath = '//table[contains(@class, "data")]') %>% 
  rvest::html_table() %>% 
  .[[1]] %>% 
  rename(State = 'Postal Code') %>% 
  write_csv('Data/FIPS.csv')
}

# read in the data
FIPS <- read_csv('Data/FIPS.csv')


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

