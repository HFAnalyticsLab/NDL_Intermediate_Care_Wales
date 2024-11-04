## ---------------
## Purpose: Provide descriptive analysis and accompanying plots on the types of referral sources in each local authority.
## 
## Author: Jerlyn Peh
## 
## Date created: 17/07/2024
## Notes:
##
## ---------------


# Load libraries
pkgs <- c("RODBC", "data.table", "ggplot2","dplyr") # package list
# lapply(pkgs, install.packages) # install packages
lapply(pkgs, library, character.only = T) # load packages

referrals_path_jp <- paste0(main_project_path_jp, '/4_referral_source')
# source("../../filepaths.R")
source(paste0(main_project_path_jp, "/RODBC_login_20221031.R")) # source RODBC login script

# query data from DB2
rct_cohort <- sqlQuery(sql, "SELECT * FROM SAILW1658V.JP_REFERRAL_PAT_LOC_RCT WHERE DISCH_SEQ =1 ") # read in table with updated referral categories
br_cohort <- sqlQuery(sql, "SELECT * FROM SAILW1658V.LB_COHORT_BR")

colnames(rct_cohort) <- tolower(colnames(rct_cohort))
colnames(br_cohort) <- tolower(colnames(br_cohort))

rct_cohort <- as.data.table(rct_cohort)
br_cohort <- as.data.table(br_cohort)


# Number of unique values (overall i.e. multiple packages per person)
## RCT create counts and proportions
rct_referrals <- rct_cohort[, .(Count = .N), by="location_of_patient_at_time_of_referral_1"][, Total := sum(Count)][, Perc := round(Count / Total * 100, digits = 2)]
names(rct_referrals)[1] <- 'location_of_patient_at_time_of_referral'
rct_referrals$Area <- "RCT"
saveRDS(rct_referrals, paste0(referrals_path_jp,'/data/rct_referrals.rds')) 


## Bridgend create counts and proportions
br_referrals <- br_cohort[, .(Count = .N), by="location_of_patient_at_time_of_referral"][, Total := sum(Count)][, Perc := round(Count / Total * 100, digits = 2)]
br_referrals$Area <- "Bridgend"
saveRDS(br_referrals, paste0(referrals_path_jp,'/data/br_referrals.rds')) 

# merge into one table
# rct_referrals <- readRDS(paste0(referrals_path_jp,'/data/rct_referrals.rds')) # uncomment if reading in RDS
# br_referrals <- readRDS(paste0(referrals_path_jp,'/data/br_referrals.rds')) # uncomment if reading in RDS
referrals_perc <- rbind(rct_referrals, br_referrals)

# save outputs
write.csv(referrals_perc ,paste0(referrals_path_jp,'/outputs/1658_referral_perc_20240722.csv'), row.names = FALSE) # rct csv
saveRDS(referrals_perc, paste0(referrals_path_jp,'/data/1658_referral_perc.rds')) 



# referrals at individual level
# filter where package_num = 1 (i.e. one record per person)
rct_cohort_alf <- rct_cohort[package_num ==1,] 
br_cohort_alf <- br_cohort[package_num ==1,] 

## RCT create counts and proportions
rct_referrals_alf <- rct_cohort_alf[,,][, .(Count = .N), by="location_of_patient_at_time_of_referral_1"][, Total := sum(Count)][, Perc := round(Count / Total * 100, digits = 2)]
names(rct_referrals_alf)[1] <- 'location_of_patient_at_time_of_referral'
rct_referrals_alf$Area <- "RCT"
saveRDS(rct_referrals_alf, paste0(referrals_path_jp,'/data/rct_referrals_alf.rds')) 


## Bridgend create counts and proportions
br_referrals_alf <- br_cohort_alf[, .(Count = .N), by="location_of_patient_at_time_of_referral"][, Total := sum(Count)][, Perc := round(Count / Total * 100, digits = 2)]
br_referrals_alf$Area <- "Bridgend"
saveRDS(br_referrals_alf, paste0(referrals_path_jp,'/data/br_referrals_alf.rds')) 

# merge into one table
# rct_referrals_alf <- readRDS(paste0(referrals_path_jp,'/data/rct_referrals_alf.rds')) # uncomment if reading in RDS
# br_referrals_alf <- readRDS(paste0(referrals_path_jp,'/data/br_referrals_alf.rds')) # uncomment if reading in RDS
referrals_alf_perc <- rbind(rct_referrals_alf, br_referrals_alf)

# save outputs
write.csv(referrals_alf_perc ,paste0(referrals_path_jp,'/outputs/1658_referral_alf_perc_20240722.csv'), row.names = FALSE) # rct csv
saveRDS(referrals_alf_perc, paste0(referrals_path_jp,'/data/1658_referral_alf_perc.rds')) 



#  create plots
# lagpcarers_palette <- c("#28A197", "#801650")
library(scales)
plot_referrals <- function(perc_df, la_name){
  plot_data <- perc_df 
  #fill_label <- ""
  
  
  scale_y_label <- "Percentage %"
  plot_title <- paste("Referral source in", la_name ,"using percentages (via location of patient at time of referral)", sep=" ")
  y_max <- 100
  
  
  plot_out <- ggplot() +
    geom_col(data = plot_data,
             aes(x = location_of_patient_at_time_of_referral,
                 y = Perc,
                 fill = location_of_patient_at_time_of_referral),
             position = "dodge") +
    scale_y_continuous(name = scale_y_label,
                       limits = c(0, y_max),
                       expand = expansion(mult = c(0, 0.05))) +
   
    scale_fill_manual(values =c("#28A197", "#28A197", "#28A197"), guide = "none") +
    scale_x_discrete(labels = wrap_format(20)) +
    
    
    geom_text(data = plot_data,
              aes(x = location_of_patient_at_time_of_referral,
                  y = Perc,
                  label = sprintf("%.1f", Perc)),
              position = position_dodge(width = 1),
              vjust = -.5,
              size = 8) +
    
    
    labs(
      title = plot_title,
      x = ""
    ) +
    theme(
      panel.background = element_rect(fill = "white", color = "grey50"),
      strip.background = element_rect(fill = "grey93", linetype = "solid", color = "black"),
      axis.line = element_line(color = "black"),
      #legend.position = "none",
      axis.text = element_text(size = 14),
      text = element_text(size = 16)
    ) 
  
   return(plot_out)
}


# create plots
## overall
referrals_rct_plot <- plot_referrals(rct_referrals, "Rhondda Cynon Taf")
referrals_br_plot <- plot_referrals(br_referrals, "Bridgend")

## alfs
referrals_alf_rct_plot <- plot_referrals(rct_referrals_alf, "Rhondda Cynon Taf")
referrals_alf_br_plot <- plot_referrals(br_referrals_alf, "Bridgend")



# save plots as svg
ggsave(paste0(referrals_path_jp,'/outputs/1658_referrals_rct_20240722.svg'), referrals_rct_plot, width = 15.0, height = 12.0, units = "in")
ggsave(paste0(referrals_path_jp,'/outputs/1658_referrals_br_20240722.svg'), referrals_br_plot, width = 15.0, height = 12.0, units = "in")

ggsave(paste0(referrals_path_jp,'/outputs/1658_referrals_alf_rct_20240722.svg'), referrals_alf_rct_plot, width = 15.0, height = 12.0, units = "in")
ggsave(paste0(referrals_path_jp,'/outputs/1658_referrals_alf_br_20240722.svg'), referrals_alf_br_plot, width = 15.0, height = 12.0, units = "in")


# save plots as png
ggsave(paste0(referrals_path_jp,'/outputs/1658_referrals_rct_20240722.png'), referrals_rct_plot, width = 15.0, height = 12.0, units = "in")
ggsave(paste0(referrals_path_jp,'/outputs/1658_referrals_br_20240722.png'), referrals_br_plot, width = 15.0, height = 12.0, units = "in")

ggsave(paste0(referrals_path_jp,'/outputs/1658_referrals_alf_rct_20240722.png'), referrals_alf_rct_plot, width = 15.0, height = 12.0, units = "in")
ggsave(paste0(referrals_path_jp,'/outputs/1658_referrals_alf_br_20240722.png'), referrals_alf_br_plot, width = 15.0, height = 12.0, units = "in")




