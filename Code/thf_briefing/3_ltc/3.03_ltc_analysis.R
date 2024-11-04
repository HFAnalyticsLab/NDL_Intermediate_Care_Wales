## ---------------
## Purpose: Analysis of long-term conditions (multimorbidity and most common conditions) in reablement/matched populations.
## 
## Author: Laura Bentley
## 
## Date created: 25/06/2024
## Notes:
##
## ---------------

## ---- set up ----

# Load libraries
pkgs <- c("dplyr", "ggplot2", "tidyr", "stringr", "janitor") # package list
#lapply(pkgs, install.packages) # install packages
lapply(pkgs, library, character.only = T) # load packages

source("filepaths.R")
source("color_palette.R")

# Read in RCT ltc table
df_ltc_rct <- readRDS(paste0(main_path_lb, "/3_ltc/data/1658_ltc_rct_lb_20240530.rds"))
df_ltc_br <- readRDS(paste0(main_path_lb, "/3_ltc/data/1658_ltc_br_lb_20240530.rds"))

## ---- functions ----
# Leaving this as a section rather than separate script for now - would be best to have all LAs in one script if possible (makes it clearer and easier to export). 

# function to create multimorbidity categories, group, count and calculate percentages
fun_multimorbidity_count_perc <- function(df_ltc){
  
  df_multimorbidity <- df_ltc %>%
    mutate(multimorbidity = ifelse(conds >= 2, '2+', '0-1')) %>% # create multimorbidity categories based on number of conds
    #filter(package_num == 1) %>% # limit to first package per person during study period
    group_by(treat, multimorbidity) %>% # group by case/control and multimorbidity category
    summarise(count = n()) %>% # count rows per multimorbidity category
    ungroup() %>% # ungroup
    group_by(treat) %>%
    mutate(perc = count / sum(count) * 100, # calculate percentage of people per multimorbidity category.
           treat = ifelse(treat == 0, "matched control", "reablement")
           )
}

# function to count prevalence of individual LTCs and calculate percentage of cohort with given condition based on LTCs associated with first package within the study period.
fun_condition_count_perc <- function(df_ltc){
  
  list_all_conditions <- df_ltc_rct %>% subset(select = alc:thy) %>% colnames() # list condition names and assign to list_all_conditions value
  
  df_condition_out <- data.frame() # create blank df to populate
  
  for(cond in list_all_conditions){
    print(paste0("Condition ID:", cond)) # print condition name to track progress
    
    df_condition <- df_ltc %>%
      #filter(package_num == 1) %>% # limit to first package per person during study period
      group_by(treat, get(cond)) %>% # group by condition
      summarise(count = n()) %>% # count number of rows
      ungroup() %>%
      rename(flag = "get(cond)") %>% # rename col
      mutate(condition = cond) %>% # create col with condition name
      group_by(treat, condition) %>% # group by condition
      mutate(total = sum(count),
             perc = count / total * 100, # calculate percentage of people with condition
             rate = count / total * 1000,
             treat = ifelse(treat == 0, "matched_control", "reablement")) %>% 
      filter(flag == 1) # filter to count & percentage with condition (remove without)
      
    
    df_condition_out <- rbind(df_condition_out, df_condition) %>% # bind output to that of previous loop
      arrange(treat, -perc) # order by descending prevalence
      
  }
  
  return(df_condition_out)
  
}

# Function to calculate prevalence rate ratios 

fun_calc_rr <- function(df_condition){
  
  df_condition_rr <- df_condition %>%
    pivot_wider(id_cols = condition, # row per cond (keeping description for joining)
                names_from = treat,
                values_from = c("count", "total", "perc", "rate"), # widen 
                names_glue = "{treat}_{.value}") %>% # name columns 
    mutate(matched_control_lowercl = ((1000/matched_control_total)*(matched_control_count - (sqrt(matched_control_count) * 1.96))),
           matched_control_uppercl = ((1000/matched_control_total)*(matched_control_count + (sqrt(matched_control_count) * 1.96))),
           reablement_lowercl = ((1000/reablement_total)*(reablement_count - (sqrt(reablement_count) * 1.96))),
           reablement_uppercl = ((1000/reablement_total)*(reablement_count + (sqrt(reablement_count) * 1.96))),
           ratio = reablement_rate / matched_control_rate,
           ratio_lowercl = reablement_lowercl / matched_control_uppercl,
           ratio_uppercl = reablement_uppercl / matched_control_lowercl) 
  
  # df_crude_rates_wide <- df_crude_rates_wide %>% select(cond_name, cond_desc, grep("unpaid", names(df_crude_rates_wide)), everything()) 
  
  df_condition_rr[is.na(df_condition_rr)] <- 0 # Replace NAs with 0
  
  return(df_condition_rr)
}

# function to plot multimorbidity by group
fun_plot_multimorbidity <- function(df_multimorbidity, la_name, colour_palette){
  
  plot_multimorbidity <- df_multimorbidity %>%
    ggplot() +
    geom_col(aes(x = multimorbidity, 
                 y = perc,
                 group = factor(treat, 
                                levels = c("reablement", "matched control"),
                                labels = c("Reablement", "Matched control")),
                 fill = factor(treat, 
                               levels = c("reablement", "matched control"),
                               labels = c("Reablement", "Matched control"))),
             position = "dodge") +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, 100)) +
    scale_fill_manual(values = colour_palette) +
    geom_text(aes(x = multimorbidity,
                  y = perc,
                  label = sprintf("%.2f", perc),
                  group = factor(treat, 
                                 levels = c("reablement", "matched control"),
                                 labels = c("Reablement", "Matched control"))),
              position = position_dodge(width = 1),
              vjust = -0.5) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    labs(
      x = "Number of long-term conditions",
      y = 'Percentage (%)',
      title = paste0('Multimorbidity (2+ long-term conditions) in ', la_name),
      fill = "Cohort"
    )
}

## ---- exploration ----

# Look at distribution
# hist(df_ltc_rct$conds) # just for eyeballing data, remove later


## ---- descriptives ----

# group condition counts into multimorbidity categories: 0, 1, 2+
df_multimorbidity_rct <- fun_multimorbidity_count_perc(df_ltc_rct)
df_multimorbidity_br <- fun_multimorbidity_count_perc(df_ltc_br)

# prevalence of each LTC
df_condition_rct <- fun_condition_count_perc(df_ltc_rct)
df_condition_br <- fun_condition_count_perc(df_ltc_br)

# rate ratios for each LTC
df_condition_rr_rct <- fun_calc_rr(df_condition_rct)
df_condition_rr_br <- fun_calc_rr(df_condition_br)

df_names <- c("df_multimorbidity_rct", "df_multimorbidity_br", "df_condition_rr_rct", "df_condition_rr_br")

for(i in df_names){
  write.csv(get(i), paste0(main_path_lb, "/3_ltc/data/1658_", i, "_lb_20240726.csv"), row.names = FALSE)
}


## ---- plots -----

plot_multimorbidity_rct <- fun_plot_multimorbidity(df_multimorbidity_rct, 'Rhondda Cynon Taf', two_colours)
plot_multimorbidity_br <- fun_plot_multimorbidity(df_multimorbidity_br, 'Bridgend', two_colours)

# Save plots
ggsave(paste0(main_path_lb, "/3_ltc/outputs/1658_plot_multimorbidity_rct_lb_20240708.png"), plot_multimorbidity_rct)
ggsave(paste0(main_path_lb, "/3_ltc/outputs/1658_plot_multimorbidity_br_lb_20240708.png"), plot_multimorbidity_br)
