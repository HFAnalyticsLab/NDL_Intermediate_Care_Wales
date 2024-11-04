## ---------------
## Purpose: Demographic analysis of general adult popultaion in LAs of interest. Produces counts, percentages and plots by sex, age group, deprivation and rurality, using ONS mid-2022 population estimates.
## 
## Author: Laura Bentley
## 
## Date created: 08/07/2024
##
## Notes:
##
## ---------------

## ---- load_libraries ---- 

pkgs <- c('readxl', 'dplyr', 'data.table', 'readr', 'tidyr', 'janitor', 'ggplot2', 'forcats')
lapply(pkgs, library, character.only = T)


## ---- load_data ---- 

source("filepaths.R")
source("color_palette.R")

# Age and sex raw ONS data
ons_female_raw <- read_excel(paste0(main_path_lb, "2_demographics/data/ons_population_estimates_2022_female.xlsx"))
ons_male_raw <- read_excel(paste0(main_path_lb, "2_demographics/data/ons_population_estimates_2022_male.xlsx"))

# LSOA raw ONS data
ons_lsoa_raw <- read_excel(paste0(main_path_lb, "2_demographics/data/ons_population_estimates_2022_lsoa.xlsx"))
# LSOA conversion and WIMD lookup data
ons_lsoa_conversion_raw <- read_excel(paste0(main_path_lb, "2_demographics/data/ons_lsoa_2021_2011_conversion.xlsx"))
ons_lsoa_wimd_raw <- read_excel(paste0(main_path_lb, "2_demographics/data/ons_lsoa2011_wimd2019.xlsx"))

## RUC lookup data
ons_lsoa_ruc_raw <- read_excel(paste0(main_path_lb, "2_demographics/data/ons_lsoa2011_ruc2011.xlsx"))

## ---- functions ----

# reorganise data function
fun_data_prep <- function(df_ons_female, df_ons_male) {
  
  la_names <- c("Bridgend", "Neath Port Talbot", "Pembrokeshire", "Powys", "Rhondda Cynon Taf") # add LAs of interest to vector
  
  df_ons_female['sex'] <- "female" # add sex variable to female data
  
  df_ons_male['sex'] <- "male" # add sex variable to male data
  
  df_out <- df_ons_female %>%
    rbind(df_ons_male) %>% # combine female and male data into one df
    filter(Name %in% la_names) %>% # filter by LAs of interest
    clean_names() %>% # clean the column names so they aren't numbers
    select(name, sex, "x18":"x90") %>% # remove unnecessary columns
    pivot_longer(!name & !sex, # pivot so age is a variable
                 names_to = "age",
                 values_to = "count_ons") %>% 
    mutate(age = gsub('x', '', age), # remove the "x"s added by the clean_names function
           age = as.numeric(age), # format age as a numeric
           age_group = cut(age, # mutate age group column with our age groups
                           breaks = c(0, 65, 69, 74, 79, 84, 89, Inf),
                           labels = c("Under 65", "65-69", "70-74", "75-79", "80-84", "85-89", "90+")))
}

# function to count ons population and calculate percentages
fun_count_perc_ons <- function(df_ons_prepped, var){
  
  if(var == "sex"){
    df_ons_grouped <- df_ons_prepped %>%
      group_by(name, sex)
  }
  
  if(var == "age_group"){
    df_ons_grouped <- df_ons_prepped %>%
      group_by(name, age_group)
  }
  
  if(var == "wimd"){
    df_ons_grouped <- df_ons_prepped %>%
      left_join(df_ons_lsoa_wimd, by = c("lsoa11cd" = "lsoa_code")) %>% # join wimd
      group_by(name, wimd_2019_overall_quintile)
  }
  
  if(var == "ruc"){
    df_ons_grouped <- df_ons_prepped %>%
      left_join(df_ons_lsoa_ruc, by = c("lsoa11cd" = "lsoa11cd")) %>% # join ruc
      mutate(ruc11 = substr(ruc11, start = 1, stop = 5)) %>% # categorise RUC as rural/urban
      group_by(name, ruc11)
  }
  
  df_ons_count_perc <- df_ons_grouped %>%
    summarise(count_ons = sum(count_ons)) %>% # count number of people by sex/age group per LA
    ungroup() %>%
    group_by(name) %>% # group by la
    mutate(total_ons = sum(count_ons), # calculate totals per la
           percentage_ons = as.numeric(sprintf("%.2f", count_ons / total_ons * 100))) %>% # calculate percentage per sex/age group per la to 2 dp
    arrange(name) %>%
    ungroup()
  
  return(df_ons_count_perc)
}


# plot function
fun_plot_ons <- function(df_ons, la, demog, demog_name, colour_palette){ # function to plot ons estimates. NOTE: removed palette for now.
  
  top_value <- df_ons %>%
    filter(name == la) %>%
    group_by(get(demog)) %>%
    filter(percentage_ons == max(percentage_ons))
  
  df_ons %>%
    filter(name == la) %>%
    ggplot(aes(x = get(demog),
               y = percentage_ons)) +
    geom_col(fill = colour_palette) +
    geom_blank(data = top_value, aes(x = get(demog),
                                     y = percentage_ons * 1.1,
                                     label = percentage_ons)) + # plot blank geom above highest y value so geom_text is not cut off
    labs(title = paste(demog_name, 'distribution of general population in', la, '(based on \n ONS 2022 adult MYE)', sep = " "),
         x = demog_name,
         y = 'Percentage (%)') +
    scale_y_continuous(expand = c(0, 0)) +
    geom_text(aes(x = get(demog),
                  y = percentage_ons,
                  label = percentage_ons,
                  group = get(demog)),
              vjust = -0.5) +
   # scale_fill_manual(values = rev(palette)) + # reversed so consistent with other plots
    theme(
      legend.position = 'none',
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black")
    )
  
}

## ---- data_prep ----

## age and sex
df_ons_age_sex <- fun_data_prep(ons_female_raw, ons_male_raw) # combine male and female data, add age groups and pivot to long format

## WIMD
df_ons_lsoa_conversion <- ons_lsoa_conversion_raw[c("LSOA11CD", "LSOA11NM", "LSOA21CD")]

df_ons_lsoa <- left_join(ons_lsoa_raw, df_ons_lsoa_conversion, by = c("LSOA 2021 Code" = "LSOA21CD")) # join in LSOA11 code and name for later joining with WIMD. Done at this stage to filter by LA name from 2011 (as dused in study pop), incase this changes between 2011 and 2021.  

la_names <- c("Bridgend", "Neath Port Talbot", "Pembrokeshire", "Powys", "Rhondda Cynon Taf") # add LAs of interest to vector

df_ons_lsoa['name'] <- str_sub(df_ons_lsoa$LSOA11NM, end = -6) # Remove lsoa code and space from end of LSOA11 name to leave LA name

# filter(df_ons_lsoa, name %in% la_names & `LSOA 2021 Code` != LSOA11CD) %>% View() # some LSOA codes change from 2011 to 2021, therefore necessary to fully merge

df_ons_lsoa_la <- df_ons_lsoa %>% 
  filter(name %in% la_names) %>% # limit to participating LAs
  select(`LAD 2021 Code`:`LSOA 2021 Name`, F18:F90, M18:M90, LSOA11CD, LSOA11NM, name) %>%
  rowwise() %>%
  mutate(count_ons = sum(c_across("F18":"M90"))) %>% # only need total count, sum across all adult ages for both sexes
  select(-(F18:M90)) %>%
  clean_names() # now ready to join to WIMD data on LSOA11CD

# prep wimd data for joining in count_perc function
df_ons_lsoa_wimd <- ons_lsoa_wimd_raw[c("LSOA code", "WIMD 2019 overall quintile")] # select needed columns
names(df_ons_lsoa_wimd) <- names(clean_names(df_ons_lsoa_wimd)) # clean variable names

# clean RUC data
df_ons_lsoa_ruc <- clean_names(ons_lsoa_ruc_raw)

## ---- count_percent ----

df_ons_sex <- fun_count_perc_ons(df_ons_age_sex, "sex") # calculate counts and percentages by sex

df_ons_age <- fun_count_perc_ons(df_ons_age_sex, "age_group") # calculate counts and percentages by age group

df_ons_wimd <- fun_count_perc_ons(df_ons_lsoa_la, "wimd")

df_ons_ruc <- fun_count_perc_ons(df_ons_lsoa_la, "ruc")

df_names <- c("df_ons_sex", "df_ons_age", "df_ons_wimd", "df_ons_ruc")

# save data files
for(i in df_names){
  write.csv(get(i), paste0(main_path_lb, "2_demographics/data/1658_", i, "_lb_20240708.csv"), row.names = F)
}

## ---- plot ----  

# Create sex plots
plot_rct_ons_sex <- fun_plot_ons(df_ons_sex, "Rhondda Cynon Taf", "sex", "Sex", single_colour)
plot_br_ons_sex <- fun_plot_ons(df_ons_sex, "Bridgend", "sex", "Sex", single_colour)

# Create age plots
plot_rct_ons_age <- fun_plot_ons(df_ons_age, "Rhondda Cynon Taf", "age_group", "Age group", single_colour)
plot_br_ons_age <- fun_plot_ons(df_ons_age, "Bridgend", "age_group", "Age group", single_colour)

# Create wimd plots
plot_rct_ons_wimd <- fun_plot_ons(df_ons_wimd, "Rhondda Cynon Taf", "wimd_2019_overall_quintile", "WIMD", single_colour)
plot_br_ons_wimd <- fun_plot_ons(df_ons_wimd, "Bridgend", "wimd_2019_overall_quintile", "WIMD", single_colour)

# Create RUC plots
plot_rct_ons_ruc <- fun_plot_ons(df_ons_ruc, "Rhondda Cynon Taf", "ruc11", "RUC", single_colour)
plot_br_ons_ruc <- fun_plot_ons(df_ons_ruc, "Bridgend", "ruc11", "RUC", single_colour)

# Assign plot names to variable
plot_names <- c("plot_rct_ons_sex", "plot_br_ons_sex", "plot_rct_ons_age", "plot_br_ons_age", "plot_rct_ons_wimd", "plot_br_ons_wimd", "plot_rct_ons_ruc", "plot_br_ons_ruc")

# Save all plots listed in plot_names variable
for(i in plot_names){
  ggsave(paste0(main_path_lb, "2_demographics/outputs/1658_", i, "_lb_20240708.png"), get(i))
}



