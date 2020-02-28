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

# tmp <- weigh_it(df = atussum_2018, groups = 'TEAGE', activities = c('t120303', 't120304'))
# tmp2 <- weigh_it(df = atussum_2018, groups = 'TEAGE', activities = c('t120303', 't120304'))
# 
# weigh_it(df = atussum_2018, groups = 'TEAGE') %>% 
#   group_by(TEAGE) %>% 
#   summarize(sum = sum(weighted.minutes)) 


# default green color code for blog
blog.color <- '#2b7551'


