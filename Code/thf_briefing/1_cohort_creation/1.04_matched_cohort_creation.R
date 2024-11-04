## ---------------
## Script name: 1658_matchit_jp_20240703.R
## 
## Purpose: Create matched cohort from the matching pool in each Local Authority using R MatchIt package.
## 
## Author: Jerlyn Peh
## 
## Date created: 03/07/2024
## Notes:
##
## ---------------

# Load libraries
pkgs <- c("AMR","RODBC", "data.table", "ggplot2", "tidyr", "stringr", "janitor", "lubridate", "stats", "sqldf", "MatchIt","dplyr") # package list
# lapply(pkgs, install.packages) # install packages
lapply(pkgs, library, character.only = T) # load packages


# source files
# source("../../filepaths.R")
source(paste0(main_project_path_jp, "/RODBC_login_20221031.R")) # source RODBC login script


# load reablement cohorts from Db2, filtering only index cases.
rct_cohort <- sqlQuery(sql, "SELECT * FROM SAILW1658V.LB_COHORT_RCT WHERE PACKAGE_NUM = 1")
br_cohort <- sqlQuery(sql, "SELECT * FROM SAILW1658V.LB_COHORT_BR")


# load matching pool (mpool) from Db2
rct_mpool <- sqlQuery(sql, "SELECT * FROM SAILW1658V.JP_MATCHING_COHORT_RCT")
br_mpool <- sqlQuery(sql, "SELECT * FROM SAILW1658V.JP_MATCHING_COHORT_BRIDGEND")


# load matching pool (mpool) from Db2
lb_gp_reg_lkup <- sqlQuery(sql, "SELECT * FROM SAILW1658V.LB_SAIL_GP_REG_LOOKUP")



## prepare data
# - column names to lower case
colnames(lb_gp_reg_lkup) <- tolower(colnames(lb_gp_reg_lkup))

colnames(rct_cohort) <- tolower(colnames(rct_cohort))
colnames(rct_mpool) <- tolower(colnames(rct_mpool))

colnames(br_cohort) <- tolower(colnames(br_cohort))
colnames(br_mpool) <- tolower(colnames(br_mpool))


# - add sex description to reablement cohort and matching pool
rct_cohort$sex <- ifelse(rct_cohort$gndr_cd == 1, "Male", "Female") 
rct_mpool$sex <- ifelse(rct_mpool$gndr_cd == 1, "Male", "Female") 


## Matching functions
#cases <- rct_cohort
#mpool <- rct_mpool

# matching function
exact_match <- function(cases, mpool){
  
  # 1. Create binary flag to distinguish cases from controls (treat = 1 for reablement recipients)
  cases$treat <- 1
  mpool$treat <- 0
  
  
  # 2. Select required columns for matching and remove duplicates
  cases_subcols <- cases %>% select(treat, sex, alf_pe, wob, death_dt) 
  
  mpool_subcols <- mpool %>% select(treat, sex, alf_pe, wob, death_dt) %>%  distinct_all()
  
  
  # 3. Merge into single dataset
  cases_mpool_subcols <- rbind(cases_subcols, mpool_subcols) 
  
  
  # 4. Exact match on sex (WOB resulted in too little controls for matching hence removed - 08/07/2024)
  matched_out <- matchit(treat ~  sex,
                         data = cases_mpool_subcols,
                         method = "exact")
  
  # 5. Extract data from MatchIt object and convert to data.table object
  matched_out_data <- match.data(matched_out)
  matched_out_data <- as.data.table(matched_out_data)
  
  # 6. In exact match, cases and controls with similar characteristics are placed in the same subclass. Hence, there can be multiple cases and controls in each subclass.
  # - create a sequence number of cases in each subclass and select the maximum sequence number (i.e reablement recipients per subclass)
  # - merge this the max sequence number for each subclass back to the case and controls only table
  # - merge extended information
  # - clean the controls dataframe
  # -- add TreatSeq col
  # -- select required cols
  # -- merge extended information (RUC, wimd description, etc)
  
  # 6a. Create sequence number for cases in each subclass to get total number of reablement recipients within.
  max_mcase_seq <- matched_out_data[treat == 1, treat_seq := seq_len(.N), by  = .(subclass) ][!is.na(treat_seq), .(max_seq = max(treat_seq)), by = subclass]
  
  
  # 6b. Add above to matched dataset and store as a new object
  matched_out_data2 <- matched_out_data %>% merge(max_mcase_seq, all.x = TRUE)
  
  
  # 6c. Add other characteristics to apply exclusion criteria to cases and matched cohort
  cases_details <- cases %>% select(alf_pe, wob, sex, death_dt, lsoa2011_cd, lsoa_desc, wimd_2019_quintile, ruc11cd, ruc11, start_dt, end_dt, reablement_start_date, reablement_end_date)
  mpool_details <- mpool %>% select(alf_pe,  wob, sex, death_dt, lsoa2011_cd, lsoa_desc, wimd_2019_quintile, ruc11cd, ruc11, start_dt, end_dt) %>% distinct_all()
  mpool_details$reablement_start_date <- NA 
  mpool_details$reablement_end_date <- NA 
  
  # join details table into one
  case_mpool_details <- rbind(cases_details, mpool_details)

  
  # merge details with matched data
  matched_out_data3 <- matched_out_data2 %>% merge(case_mpool_details, by = c("alf_pe", "wob", "sex", "death_dt"), all.x = TRUE)
  matched_out_data3 <- as.data.table(matched_out_data3)
  
  mcases_df <- matched_out_data3[matched_out_data3$treat == 1,]
  mcontrols_df <- matched_out_data3[matched_out_data3$treat == 0,]
  
  
  # run pair match using cases and controls
  matched_pair_df <- select_best_match(mcases_df, mcontrols_df)
  
  # add age and age group at index date
  matched_pair_df$indexdt_age <- floor(time_length(difftime(matched_pair_df$reablement_start_date, matched_pair_df$wob), "years")) 
  matched_pair_df$indexdt_agegroup <- age_groups(matched_pair_df$indexdt_age, split_at = "fives")
  
  
  return(matched_pair_df)
  
  }

# function is responsible for imputing reablement start date to controls, applying exclusion criteria and selecting best match for control.
select_best_match <- function(mcases_df, mcontrols_df){
  
  matched_pairs <- data.frame()
  stop <- FALSE
  
  # loop through each subclass
  subclass_list <- sort(unique(as.integer(mcases_df$subclass)), decreasing = TRUE)
  
  
  for(class in subclass_list){
    print(paste0("Subclass ID: " , class))
    
    
    # get all cases and controls in the subclass; place them in respective dataframes.
    cases_list <- mcases_df[subclass == class, ]
    cases_list <- cases_list[order(treat_seq)]
    controls_list <- mcontrols_df[subclass == class, ][, treat_seq := NULL][, reablement_start_date := NULL][, reablement_end_date := NULL] # drop treat_seq column
    
    
    # loop through each case within the subclass to select right controls to match 1-1
    for(case_id in cases_list$treat_seq){
      print(paste0("case ID: " , case_id))
      # give controls treatSeq of current case
      # controls_list$treat_seq <- case_id
      
      
      # select the case with the case_id and select columns to be imputed
      case_info <- cases_list[subclass == class & treat_seq == case_id, .(subclass, treat_seq, case_wimd = wimd_2019_quintile , reablement_start_date, reablement_end_date, wob )] 
      
      # create age on reablement and corresponding age group; drop WOB and age column as not for imputing
      case_info$indexdt_age <- floor(time_length(difftime(case_info$reablement_start_date, case_info$wob), "years")) 
      case_info$case_indexdt_agegroup <- age_groups(case_info$indexdt_age, split_at = "fives")
      case_info[, `:=`(indexdt_age = NULL, wob = NULL)]
      
      # impute index details to each control using merge
      controls_imputed <- controls_list %>% merge(case_info, by = c("subclass"), all.x=TRUE)
      
      
      # Run through exclusion criteria -
      #   1. Remove controls who died before imputed reablement start date (i.e death date is null or death date is before)
      remaining_controls1 <- controls_imputed[is.na(controls_imputed$death_dt) | controls_imputed$death_dt > controls_imputed$reablement_start_date, ] 
      
      
      #   2. Remove controls who are not within the same age group
      #     2.1 create age on reablement, remove any aged under 0, and corresponding age group; drop age column
      remaining_controls1a <- remaining_controls1
      remaining_controls1a$indexdt_age <- floor(time_length(difftime(remaining_controls1a$reablement_start_date, remaining_controls1a$wob), "years")) 
      remaining_controls1a <- remaining_controls1a[remaining_controls1a$indexdt_age > 0] 
      remaining_controls1a$indexdt_agegroup <- age_groups(remaining_controls1a$indexdt_age, split_at = "fives")
      remaining_controls1a[, `:=`(indexdt_age = NULL)]
      
      #     2.2 apply age group exclusion 
      remaining_controls2 <- remaining_controls1a[remaining_controls1a$indexdt_agegroup == remaining_controls1a$case_indexdt_agegroup, ]
      
      
      #   3. Remove records where WIMD is not the same as the case (rename variables!)
      remaining_controls3 <- remaining_controls2[remaining_controls2$wimd_2019_quintile == remaining_controls2$case_wimd, ] 
      
      
      #   4. Remove records where WIMD doesn't relate to reablement start date
      remaining_controls4 <- remaining_controls3[remaining_controls3$start_dt < remaining_controls3$reablement_start_date & remaining_controls3$end_dt > remaining_controls3$reablement_start_date, ] 
      
      
      #   5. Remove records not registered at Welsh SAIL GP for full year prior to reablement start date
     
      #     5.1 join with gp reg lookup table and create flag
      leftjoin_controls_reglkup <- merge(remaining_controls4, lb_gp_reg_lkup, by = "alf_pe", all.x =TRUE)
      leftjoin_controls_reglkup2 <- leftjoin_controls_reglkup[reablement_start_date - years(1) >= start_date & reablement_start_date <= end_date, sail_gp_flag := 1, ]
     
      #     5.2 Remove those without a year of SAIL GP Registration (i.e. sail-gp_flag == NULL)
      remaining_controls5 <- leftjoin_controls_reglkup2[ leftjoin_controls_reglkup2$sail_gp_flag == 1,]
      
      
      # check number of remaining matches. 
      # If 0, break out of the loop, otherwise, randomly select 1 control to form matched pair to be stored in matched pair list.
      if (nrow(remaining_controls5) < 1) {
        
        print(paste0("Case ID: " , case_id, " in subclass: ", class, " has insufficient matches"))
        stop <- TRUE
        
      } else {
        
        # define column names to select (to add age group at reablement)
        cnames <- c("alf_pe" , "wob" , "sex", "death_dt", "lsoa2011_cd","lsoa_desc", "wimd_2019_quintile",  "ruc11cd", "ruc11", "reablement_start_date", "reablement_end_date","subclass", "treat" )
        
        # set seed to randomly select a matched control
        set.seed(1658)
        matched_control <- remaining_controls5 %>% sample_n(1) %>%  select(all_of(cnames))
        
        # append case and selected controls to matched_pairs list
        matched_pairs <- rbind(matched_pairs, cases_list[subclass == class & treat_seq == case_id, .SD, .SDcols = cnames ] , matched_control)
        
        # remove selected control from control list to prevent being matched multiple times
        controls_list <- controls_list[controls_list$alf_pe != matched_control$alf_pe, ]
        
      }
      
      
    }
    
    # checks stop flag value. Breaks out of loop if stop == TRUE
    if(stop){break}
  }
  
  return(matched_pairs)
}


# call function
rct_matchedpairs <- exact_match(rct_cohort, rct_mpool)
br_matchedpairs <- exact_match(br_cohort, br_mpool)


# validating matched pairs
# 1. count of cases and controls
xtabs(~ treat, data = rct_matchedpairs ) # equal number

# 2. age distribution by treat flag (treat = 1 refers to reablement recipients; treat = 0 refers to controls)
xtabs(~ treat + indexdt_agegroup, data = rct_matchedpairs ) # equal number

# 3. wimd distribution
xtabs(~ treat + wimd_2019_quintile, data = rct_matchedpairs ) # equal number

# 4. ruc distribution
xtabs(~ treat + ruc11, data = rct_matchedpairs ) # similar distibution



# save to db2 
sqlQuery(sql, "DROP TABLE SAILW1658V.JP_MATCHED_COHORT_RCT;")

sqlQuery(sql, "CREATE TABLE SAILW1658V.JP_MATCHED_COHORT_RCT
(
ALF_PE BIGINT
, WOB DATE
, SEX VARCHAR(7)
, DEATH_DT DATE
, LSOA11_CD VARCHAR(10)
, LSOA_DESC VARCHAR(50)
, WIMD_DESC INTEGER
, RUC11_CD VARCHAR(5)
, RUC_DESC VARCHAR(50)
, REABLEMENT_START_DATE DATE
, REABLEMENT_END_DATE DATE
, SUBCLASS VARCHAR(2)
, TREAT INTEGER
, AGE_INDEXDT INTEGER
, AGEGROUP_INDEXDT VARCHAR(7)
);")
    
sqlSave(sql, rct_matchedpairs, "SAILW1658V.JP_MATCHED_COHORT_RCT", rownames = F, append = TRUE , fast = F, verbose = T)



sqlQuery(sql, "DROP TABLE SAILW1658V.JP_MATCHED_COHORT_BR;")

sqlQuery(sql, "CREATE TABLE SAILW1658V.JP_MATCHED_COHORT_BR
(
ALF_PE BIGINT
, WOB DATE
, SEX VARCHAR(7)
, DEATH_DT DATE
, LSOA11_CD VARCHAR(10)
, LSOA_DESC VARCHAR(50)
, WIMD_DESC INTEGER
, RUC11_CD VARCHAR(5)
, RUC_DESC VARCHAR(50)
, REABLEMENT_START_DATE DATE
, REABLEMENT_END_DATE DATE
, SUBCLASS VARCHAR(2)
, TREAT INTEGER
, AGE_INDEXDT INTEGER
, AGEGROUP_INDEXDT VARCHAR(7)
);")

sqlSave(sql, br_matchedpairs, "SAILW1658V.JP_MATCHED_COHORT_BR", rownames = F, append = F , fast = F, verbose = T)