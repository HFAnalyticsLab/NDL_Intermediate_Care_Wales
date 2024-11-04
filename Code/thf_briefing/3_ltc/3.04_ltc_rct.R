## ---------------
## Purpose: Combines long-term conditions data extract with RCT cohort data. Adapted from code written by Dr Will Parry, sourced from HFAnalyticsLab GitHub. Original code: https://github.com/HFAnalyticsLab/CMD_multimorbidity/blob/master/R/03_Create_Cambridge_Score.R
## 
## Author: Laura Bentley
## 
## Date created: 30/05/2024
##
## Notes:
##
## ---------------

## ---- load_libraries ---- 

pkgs <- c('dplyr', 'data.table', 'janitor')
lapply(pkgs, library, character.only = T)


## ---- login ----

source("filepaths.R")

source(paste0(main_path_lb, "/RODBC_login_20221031.R"))

## ---- load_data ---- 

lb_ltc_cancer_rct_raw <- sqlQuery(sql, "SELECT * FROM SAILW1658V.LB_LTC_CANCER_LKUP_RCT") # look up for first recorded cancer Read code within 5 years of index date
lb_ltc_ckd_rct_raw <- sqlQuery(sql, "SELECT * FROM SAILW1658V.LB_LTC_CKD_LKUP_RCT") # look up for highest of 2 most recent kidney function tests <60ml/min
lb_ltc_medcode_rct_raw <- sqlQuery(sql, "SELECT * FROM SAILW1658V.LB_LTC_MEDCODE_LKUP_RCT") # look up for all other 'medcode' Read codes within specified time
lb_ltc_prodcode_rct_raw <- sqlQuery(sql, "SELECT * FROM SAILW1658V.LB_LTC_PRODCODE_LKUP_RCT") # look up for all 'prodcode' Read codes within specified period of time with prescription count (pcount) greater than or equal to Rx

lb_cam_codelist_raw <- sqlQuery(sql, "SELECT * FROM SAILW1658V.LB_CAM_CODELIST")

# Read in RCT cohort
jp_matched_rct_cohort_raw <- sqlQuery(sql, "SELECT * FROM SAILW1658V.JP_MATCHED_COHORT_RCT")
# convert variable names to lower case
names(jp_matched_rct_cohort_raw) <- tolower(names(jp_matched_rct_cohort_raw))
  #readRDS(paste0(main_path_lb, "/cohort_creation/data/1658_cohort_rct_clean_lb.rds"))


## ---- data_prep ---- 

# Create dummy table with every REF code 
df_cam_codelist <- lb_cam_codelist_raw %>% select(REF) %>% distinct() %>% 
  mutate(ALF_PE = as.integer(NA),
         REABLEMENT_START_DATE = as.Date(NA),
         FLAG = 0) %>%
  select(ALF_PE, REABLEMENT_START_DATE, REF, FLAG)

df_ltc_rct_lb <- rbindlist(list(lb_ltc_cancer_rct_raw, lb_ltc_ckd_rct_raw, lb_ltc_medcode_rct_raw, lb_ltc_prodcode_rct_raw), fill = TRUE)[ # bind lookup tables into one df
  , .(ALF_PE, REABLEMENT_START_DATE, REF, FLAG = 1)] %>% # restrict to required variables and add a flag (to indicate the ref exists)
  rbind(df_cam_codelist) %>% # bind dummy table so dcast creates a column for every REF
  dcast(... ~ REF, value.var = 'FLAG', fill = 0) %>% # cast table wide (where patients do not have condition, fill with 0). NOTE: dummy table leads to an artificial row inserted with NA ALF_PE and REABLEMENT_START_DATE
  .[, `:=` (ANX = ifelse(ANX140 != 0 | ANX141 != 0, 1, 0), # include logic where necessary and flag condition in new column
            AST = ifelse(AST127 != 0 & AST142 != 0, 1, 0),
            DEP = ifelse(DEP152 != 0 | DEP153 != 0, 1, 0),
            EPI = ifelse(EPI155 != 0 & EPI156 != 0, 1, 0),
            IBS = ifelse(IBS161 != 0 | IBS162 != 0, 1, 0),
            PNC = ifelse(PNC166 != 0 | (PNC167 != 0 & EPI155 == 0), 1, 0),
            PSO = ifelse(PSO171 != 0 & PSO172 != 0, 1, 0),
            SCZ = ifelse(SCZ175 != 0 | SCZ176 != 0, 1, 0),
            ANX140 = NULL, ANX141 = NULL, AST127 = NULL, AST142 = NULL, # remove all ref columns used with logic rules
            DEP152 = NULL, DEP153 = NULL, EPI155 = NULL, EPI156 = NULL, 
            IBS161 = NULL, IBS162 = NULL, PNC166 = NULL, PNC167 = NULL, 
            PSO171 = NULL, PSO172 = NULL, SCZ175 = NULL, SCZ176 = NULL
  )]

df_ltc_rct_lb <- clean_names(df_ltc_rct_lb)
condrefs_lb <- names(df_ltc_rct_lb) %>% tail(-2) %>% sort() # get column names, remove ALF & start date and order alphabetically
condcodes_lb <- substr(condrefs_lb, 1, 3) # create new column names using only the condition letters
setnames(df_ltc_rct_lb, condrefs_lb, condcodes_lb) # rename condition columns using reference vector to use first 3 letters
setcolorder(df_ltc_rct_lb, c('alf_pe', 'reablement_start_date', condcodes_lb)) # set column order to alphabetical

df_ltc_rct_lb[, conds := rowSums(.SD), .SDcols = 3:length(condcodes_lb)] # add a count of how many conditions are flagged (sum all columns exc ALF_PE)

df_full_ltc_rct_lb <- df_ltc_rct_lb # copy full ltc info to a new data frame so it's not lost in the next step.
condnames_lb <- names(df_full_ltc_rct_lb) %>% tail(-2) # get column names, remove ALF & start date

df_ltc_rct_lb <- df_ltc_rct_lb[jp_matched_rct_cohort_raw, on = .(alf_pe = alf_pe, reablement_start_date = reablement_start_date)] # join to full cohort. NOTE: this removes row with NA alf introduced by dummy table.
#df_ltc_rct_lb <- df_ltc_rct_lb[ , .SD, .SDcols = -(1:length(condcodes_lb) + 1)] # Remove all individual condition variables
cohortcols_lb <- names(jp_matched_rct_cohort_raw) # create vector with cohort column names
setcolorder(df_ltc_rct_lb, c(cohortcols_lb, condnames_lb)) # reorder so conditions column is at end

df_ltc_rct_lb[, (condnames_lb) := lapply(.SD, nafill, fill = 0), .SDcols = condnames_lb] # replace NAs with 0 in condition columns

df_ltc_rct_lb_dist <- df_ltc_rct_lb %>% distinct() # Note: no change following distinct

## ---- save_data ---- 
saveRDS(df_ltc_rct_lb, paste0(main_path_lb, "/3_ltc/data/1658_ltc_rct_lb_20240530.rds")) # save data frame
