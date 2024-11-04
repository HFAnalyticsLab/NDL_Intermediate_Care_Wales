## ---------------
## Purpose: Demographic analysis of RCT and Bridgend reablement cohorts. Produces counts, percentages and plots by sex, age group, deprivation and rurality.
## 
## Author: Laura Bentley
## 
## Date created: 02/07/2024
##
## Notes:
##
## ---------------

## ---- set up ----

# Load libraries
pkgs <- c("RODBC", "plyr", "dplyr", "ggplot2", "tidyr", "stringr", "janitor", "forcats") # package list. NOTE: plyr used for round_any in plot function, needs to be loaded BEFORE dplyr else creates conflicts for count_perc function.
#lapply(pkgs, install.packages) # install packages
lapply(pkgs, library, character.only = T) # load packages

source("filepaths.R")
source("color_palette.R")
# RODBC
source("RODBC_login_20221031.R")

rct_cohort_raw <- sqlQuery(sql, "SELECT * FROM SAILW1658V.LB_COHORT_RCT")
br_cohort_raw <- sqlQuery(sql, "SELECT * FROM SAILW1658V.LB_COHORT_BR")

## ---- functions ----
# Leaving this as a section rather than separate script for now - would be best to have all LAs in one script if possible (makes it clearer and easier to export). 

# TO DO:
## Round percentage to 2dp
## Ethnicity to be added once data approved, provisioned and ethnicity spine run in project.

# Function to clean data
fun_clean_data <- function(df_raw, la_name){
  
  df_clean <- df_raw %>% 
    mutate(across(TYPE_OF_SERVICE_DELIVERED:RELATIVE_LEVEL_OF_SUPPORT_NEEDS_POST_REABLEMENT, ~na_if(., "No Data"))) %>% # replace "No Data" with NA
    # clean variable names
    clean_names() %>%
    rename(sex = gndr_cd) %>% # rename gndr_cd to sex
    # add age group variable
    mutate(age_group = cut(age,
                           breaks = c(0, 65, 69, 74, 79, 84, 89, Inf),
                           labels = c("Under 65", "65-69", "70-74", "75-79", "80-84", "85-89", "90+")),
           package_dur = reablement_end_date - reablement_start_date,
           la_name = la_name,
           ruc11 = substr(ruc11, start = 1, stop = 5)
    )
  
  return(df_clean)
}


# Function to produce counts and percentages per demographic variable
fun_demog_count_perc <- function(df_cohort){
  
  # create list of demographic variables to loop over in group count
  demog <- c("sex", "age_group", "wimd_2019_quintile", "ruc11")
  
  # create empty data frame to populate with each iteration of for loop
  df_out <- data.frame()
  
  # loop over each element in the demog list to group, count and calculate percentage
  for(i in demog){
    output <- df_cohort %>% 
      # filter to first reablement package received during the study period
      filter(package_num == 1) %>% 
      # group by element from demog list
      group_by(get(i)) %>%
      # count rows (as filtered to first package, this is a person count)
      count() %>%
      # ungroup 
      ungroup() %>%
      # rename variables
      rename(demog_lvl = "get(i)",
             count = n) %>%
      # create a demog_type variable to give the demographic type & calculate percentage per level
      mutate(demog_type = as.character(i),
             perc = count/sum(count)*100) %>%
      # reorder cols
      select(demog_type, everything())
    
    # rbind into dataframe created at start of function
    df_out <- rbind(df_out, output)
  }
  
 df_out <- df_out %>%
   mutate(demog_lvl = case_when(
    demog_type == "sex" ~ factor(demog_lvl,
                                     levels = c(1, 2),
                                     labels = c("male", "female")),
    demog_type == "age_group" ~ factor(demog_lvl,
                                       levels = c("Under 65", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"),
                                       labels = c("Under 65", "65-69", "70-74", "75-79", "80-84", "85-89", "90+")),
    demog_type == "wimd_2019_quintile" ~ factor(demog_lvl,
                                                levels = c(1, 2, 3, 4, 5),
                                                labels = c("1 (most)", "2", "3", "4", "5 (least)")),
    demog_type == "ruc11" ~ demog_lvl)
  )
  
  return(df_out)
}


# PLOT FUNCTION - TO DO:
## Edit fill colours

# Function to plot demographic variables
fun_plot_demog <- function(df_demog, la_name_short, la_name_full, colour_palette){
  
  # create list of demographic variables to loop over for plot
  demog <- c("sex", "age_group", "wimd_2019_quintile", "ruc11")
  
  df_demog <- df_demog %>% 
    group_by(demog_type) %>% 
    mutate(rn = row_number(), # create ordering variable
           var_lab = case_when( # create naming variable for title and x axis
             demog_type == "sex" ~ "sex",
             demog_type == "age_group" ~ "age group",
             demog_type == "wimd_2019_quintile" ~ "WIMD",
             demog_type == "ruc11" ~ "RUC"
           )) # create variable label
  
  plot_list <- list()
  
  for(i in demog){
    max_perc <- df_demog %>% filter(demog_type == i) %>% slice(which.max(perc)) %>% pull(perc)
    
    plot_list[[paste0("plot_", i, "_", la_name_short)]] <- local({
      i <- i
      plot_data <- df_demog %>%
        filter(demog_type == i)
      
      var_lab <- pull(plot_data[1, "var_lab"])
      
      plot_out <- plot_data %>%
        ggplot() +
        geom_col(aes(x = fct_reorder(demog_lvl, rn), # reorder x axis
                     y = perc),
                 fill = colour_palette) +
        scale_y_continuous(expand = c(0, 0),
                           limits = c(0, round_any(max_perc, 10, f = ceiling) + 10),
                           breaks = seq(0, round_any(max_perc, 10, f = ceiling) + 10, 10)
                           ) +
        geom_text(aes(x = demog_lvl,
                      y = perc,
                      label = sprintf("%.2f", perc),
                      group = demog_lvl),
                  vjust = -0.5) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")
        ) +
        labs(
          y = 'Percentage (%)',
          x = var_lab,
          title = paste0("Reablement care packages by ", var_lab, " in ", la_name_full)
        )
      return(plot_out)
    })
  } 
  return(plot_list)
}

## ---- data cleaning ----

df_cohort_rct <- fun_clean_data(rct_cohort_raw, "rct")
df_cohort_br <- fun_clean_data(br_cohort_raw, "bridgend")

# save cleaned data as rds (using rds instead of csv to keep column formatting type)
saveRDS(df_cohort_rct, paste0(main_path_lb, "/1_cohort_creation/data/1658_cohort_rct_clean_lb.rds"))
saveRDS(df_cohort_br, paste0(main_path_lb, "/1_cohort_creation/data/1658_cohort_br_clean_lb.rds"))

## ---- descriptives ----

df_demog_rct <- fun_demog_count_perc(df_cohort_rct)
df_demog_br <- fun_demog_count_perc(df_cohort_br)

# save as csv for export
write.csv(df_demog_rct, paste0(main_path_lb, "/2_demographics/data/1658_demog_rct_lb_20240702.csv"), row.names = FALSE)
write.csv(df_demog_br, paste0(main_path_lb, "/2_demographics/data/1658_demog_br_lb_20240702.csv"), row.names = FALSE)

## ---- plots ----

plot_demog_list <- fun_plot_demog(df_demog_rct, "rct", "Rhondda Cynon Taf", single_colour)
list2env(plot_demog_list, envir = .GlobalEnv)

plot_demog_list <- fun_plot_demog(df_demog_br, "br", "Bridgend", single_colour)
list2env(plot_demog_list, envir = .GlobalEnv)

## assign plot names to list
plots_rct <- c("plot_age_group_rct", "plot_sex_rct", "plot_wimd_2019_quintile_rct", "plot_ruc11_rct")
plots_br <- c("plot_age_group_br", "plot_sex_br", "plot_wimd_2019_quintile_br", "plot_ruc11_br")

## save plots
for(i in plots_rct){
  ggsave(paste0(main_path_lb, "/2_demographics/outputs/1658_", i, "_lb_20240704.png"), get(i))
}

for(i in plots_br){
  ggsave(paste0(main_path_lb, "/2_demographics/outputs/1658_", i, "_lb_20240704.png"), get(i))
}
