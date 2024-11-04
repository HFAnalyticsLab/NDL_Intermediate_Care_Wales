## ---------------
## Purpose: Provide descriptive analysis and accompanying plots on the types of reablement services received by individuals in each Local Authority.
## 
## Author: Jerlyn Peh
## 
## Date created: 01/07/2024
## Notes:
##
## ---------------



library(dplyr)
library(data.table)
library(ggplot2)
library(RODBC)

types_of_service_path_jp <- paste0(main_project_path_jp, "/5_service_type") # set file path
# source("../../filepaths.R")
source(paste0(main_project_path_jp, "/RODBC_login_20221031.R")) # source RODBC login script

# query data from DB2
rct_cohort <- sqlQuery(sql, "SELECT * FROM SAILW1658V.LB_COHORT_RCT")
br_cohort <- sqlQuery(sql, "SELECT * FROM SAILW1658V.LB_COHORT_BR")

colnames(rct_cohort) <- tolower(colnames(rct_cohort))
colnames(br_cohort) <- tolower(colnames(br_cohort))

rct_cohort <- as.data.table(rct_cohort)
br_cohort <- as.data.table(br_cohort)


# Number of unique values
## RCT create counts and proportions
rct_explore <- rct_cohort[, .(Count = .N), by="type_of_service_delivered"][, Total := sum(Count)][, Perc := round(Count / Total * 100, digits = 2)]

# provisionally agreed to merge Reablement and Therapy (disclosive) 02/07/2024
rct_explore_masked <- rct_cohort[, service_delievered := ifelse(type_of_service_delivered %in% c("Reablement", "Therapy"), "Reablement + Therapy", type_of_service_delivered), ][, .(Count = .N), by="service_delievered"][, Total := sum(Count)][, Perc := round(Count / Total * 100, digits = 2)]

rct_explore_masked$Area <- "RCT"
saveRDS(rct_explore_masked, paste0(types_of_service_path_jp,'/data/rct_explore_masked.rds')) 


## Bridgend create counts and proportions
br_explore <- br_cohort[,service_delievered := type_of_service_delivered ,][, .(Count = .N), by="service_delievered"][, Total := sum(Count)][, Perc := round(Count / Total * 100, digits = 2)]
br_explore$Area <- "Bridgend"
saveRDS(br_explore, paste0(types_of_service_path_jp,'/data/br_explore.rds')) 

# merge into one table
types_of_svc_perc <- rbind(rct_explore_masked, br_explore)

# save outputs
write.csv(types_of_svc_perc ,paste0(types_of_service_path_jp,'/outputs/1658_service_types_perc_20240711.csv'), row.names = FALSE) # rct csv
saveRDS(types_of_svc_perc, paste0(types_of_service_path_jp,'/data/1658_service_types_perc.rds')) 



#  create plots
# lagpcarers_palette <- c("#28A197", "#801650")
# library(scales)
# plot_service_types <- function(ratio_df, la_name){
#   plot_data <- ratio_df 
#   fill_label <- "Unpaid carers cohort"
#   
#   
#   scale_y_label <- "Percentage %"
#   plot_title <- paste("Types of services delivered in ",la_name," using percentages", sep=" ")
#   y_max <- 100
#   
#   
#   plot_out <- ggplot() +
#     geom_col(data = plot_data,
#              aes(x = service_delievered,
#                  y = Perc,
#                  fill = service_delievered),
#              position = "dodge") +
#     scale_y_continuous(name = scale_y_label,
#                        limits = c(0, y_max),
#                        expand = expansion(mult = c(0, 0.05))) +
#    
#     scale_fill_manual(values =c("#28A197", "#28A197", "#28A197"), guide = "none") +
#     scale_x_discrete(labels = wrap_format(20)) +
#     
#     
#     geom_text(data = plot_data,
#               aes(x = service_delievered,
#                   y = Perc,
#                   label = sprintf("%.1f", Perc)),
#               position = position_dodge(width = 1),
#               vjust = -.5,
#               size = 8) +
#     
#     
#     labs(
#       title = plot_title,
#       x = ""
#     ) +
#     theme(
#       panel.background = element_rect(fill = "white", color = "grey50"),
#       strip.background = element_rect(fill = "grey93", linetype = "solid", color = "black"),
#       axis.line = element_line(color = "black"),
#       #legend.position = "none",
#       axis.text = element_text(size = 14),
#       text = element_text(size = 16)
#     ) 
#   
#    return(plot_out)
# }


# rct_explore_masked <- readRDS(paste0(types_of_service_path_jp,'/data/rct_explore_masked.rds')) # uncomment if reading in rds 
#service_types_rct_plot <- plot_service_types(rct_explore_masked, "Rhondda Cynon Taf")

# br_explore <- readRDS(paste0(types_of_service_path_jp,'/data/br_explore.rds')) # uncomment if reading in rds 
 
#service_types_br_plot <- plot_service_types(br_explore, "Bridgend")

# save plots as svg
#ggsave(paste0(types_of_service_path_jp,'/outputs/1658_service_types_rct_20240711.svg'), service_types_rct_plot, width = 15.0, height = 12.0, units = "in")
#ggsave(paste0(types_of_service_path_jp,'/outputs/1658_service_types_br_20240711.svg'), service_types_br_plot, width = 15.0, height = 12.0, units = "in")

# save plots as png
#ggsave(paste0(types_of_service_path_jp,'/outputs/1658_service_types_rct_20240711.png'), service_types_rct_plot, width = 15.0, height = 12.0, units = "in")
#ggsave(paste0(types_of_service_path_jp,'/outputs/1658_service_types_br_20240711.png'), service_types_br_plot, width = 15.0, height = 12.0, units = "in")



