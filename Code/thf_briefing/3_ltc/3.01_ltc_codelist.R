# This is adapted from code written by Dr Will Parry, sourced from HFAnalyticsLab GitHub
# Original code: https://github.com/HFAnalyticsLab/CMD_multimorbidity/blob/master/R/02_Create_CamCodeList.R

pkgs <- c('here', 'xml2', 'purrr', 'rvest', 'stringr', 'data.table', 'plyr', 'dplyr', 'httr') #package list
#lapply(pkgs, install.packages, character.only=T) # install packages
lapply(pkgs, library, character.only=T) #load packages

here::here() #check here sees root directory for project
savedir <- here::here('Cambridge_files', '/') #create path for saving Cambridge files (need end slash)
dir.create(savedir, showWarnings = FALSE) #create folder if it doesn't exist

## Access the web page containing the Cambridge code lists and find the file hyperlinks ----
camsite <- 'https://www.phpc.cam.ac.uk/pcu/research/research-groups/crmh/cprd_cam/codelists/v11/'#NB: address could change
filelinks <- read_html(camsite) %>% #read Cambridge code list web page
  html_nodes("a") %>% html_attr("href") %>% # find all links and get URLs
  str_subset("zip") %>% .[1:46] # restrict to zip files for 'Current Code Lists on 38 Common Conditions'

## Download and extract the Cambridge zip files ----
files <- str_split(filelinks, "/V11/", simplify=T)[,2] #split the online paths to extract the filenames
saveas <- map2_chr(savedir, files, paste0) #create filepaths to save to savedir
map2(filelinks, saveas, download.file) #download the files from Cambridge and save
map(saveas, unzip, exdir = "./Cambridge_files") #unzip the zip files
map(saveas, unlink) #delete the original zip files

## Access the web page containing the Hanlon code lists and find the file hyperlink ----
hansite <- 'https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8901063/'#NB: address could change
hansite <- GET(hansite, add_headers('user-agent' = 'Public Health employment')) # set user agent otherwise blocked by pubmed
hanfilelinks <- read_html(hansite) %>% #read drugcode list web page
  html_nodes("a") %>% html_attr("href") %>% # find all link and get URL
  str_subset("004.csv") %>% .[1] # restrict to 'Read codes for drugs used in LTC definitions. LTC, long-term condition.'
hanfilelinks <- paste0('https://www.ncbi.nlm.nih.gov', hanfilelinks) # add back in the start of the web address

## Download the Hanlon prodcode file ----
# hanfiles <- str_split(hanfilelinks, "/bin/", simplify=T)[,2] #split the online paths to extract the filenames
hansaveas <- map2_chr(savedir, 'hanson_drug_codes.csv', paste0) #create filepath to save to
map2(hanfilelinks, hansaveas, download.file) #download the file from Hanlon and save

## Create a single long data table of codes for the various conditions ----
descriptions <- list.files(savedir, pattern = 'DESCRIPTION', full.names=T) %>% #list files with DESCRIPTION in the filename
  map(fread) %>% rbindlist(use.names=T, fill=T) %>% #read them all and bind them together
  .[, .SD[1], by = SCHEMA_NUMBER] %>% #take first row for each schema number (to get rid of repetitions)
  .[, codes:=strsplit(ALLCODES, split = ';', fixed = T)] %>% #split ALLCODES into vector of codes
  .[, ref:=paste0(`CONDITION CODE`, SCHEMA_NUMBER)] #create a reference code from the condition code and schema number

logic <- fread(here::here('Appendix_1_Cam_UD_based_logic.csv')) #read logic file into data table

#'Appendix_1_Cam_UD_based_logic.csv' was created from unique values of the USAGE DEFINITION field in the descriptions
#The following additional fields were added...
#- read: contains the number of days of history to include when looking for read codes 
#- Rx: contains the number of prescriptions required as defined by the prod codes
#- logic: contains a logical operator for how to combine read and prod criteria
#- special: an aide-memoire, identifies conditions where the rules are unusual and must be dealt with separately

descriptions <- descriptions %>% merge(logic, all.x = T) #merge in logic rules

codelist <- with(descriptions, 
                 list(cond = `CONDITION CODE`, #create list of inputs to make long table
                      cond_desc = `CONDITION DESCRIPTION`, # keep condition description code for labelling graphs in output
                      ref = ref, 
                      type = TYPE, 
                      code = codes, # NB: these are medcodes & prodcodes
                      read = read, 
                      Rx = Rx, 
                      logic = logic, 
                      special = special, 
                      ud = `USAGE DEFINITION`)) %>%
  pmap_dfr(data.table) #send inputs to pmap_dfr to create and join data tables together

codelist[, code:=as.numeric(code)] #convert code to numeric variable (previously character, needed for later use)

prodcodelist <- codelist %>% filter(type == 'PRODCODES') # separate prodcodes into table

medcodelist <- codelist %>% filter(type == 'MEDCODES') # separate medcodes into table

## Add read codes to medcode list----
readcodes <- list.files(savedir, pattern = 'MC_V1', full.names=T) %>% #list files with MC_V1 in file names
  map(fread) %>% rbindlist(use.names=T, fill=T) #read them all and bind them together to create list of all medcodes and readcodes NB: this does not include prodcode conditions

codelist_diag <- medcodelist %>% merge(readcodes[ , c("medcode", "readcode")], by.x = "code", by.y = "medcode", all.x = TRUE) %>% # replace codelist df with medcode list and added readcode equivalents
  distinct() %>% # distinct to get rid of exact repetitions (some codes are duplicated in the downloaded files for the same condition - e.g. rheumatoid arthritis: 5723, 7196, 12575)
  rename(medcode = code) # rename 'code' variable to avoid later confusion

saveRDS(codelist_diag, here::here('cam_code_list_diag.rds')) #save to lookups directory

## Add read codes to prodcode list ----
prodcodes_raw <- read.csv(hansaveas) # read in hanson_drug_codes.csv

drug_diag <- prodcodes_raw %>% select(drug_diagnosis_name) %>% distinct() %>%
  filter(!grepl('dyspepsia', drug_diagnosis_name)) %>% # remove codes for dyspepsia - this is not contained in the Cassell version of Cambridge LTCs
  arrange() %>% # order alphabetically
  mutate(ref = c('DEP153', 'ANX141', 'AST127', 'CON150', 'EPI156', 'IBS162', 'MIG164', 'PNC166', 'PNC167', 'PSO172', 'SCZ176')) # Add ref (for condition and schema) - taken from 'descriptions' table

prodcodes <- prodcodes_raw %>% 
  filter(!grepl('dyspepsia', drug_diagnosis_name)) %>% # remove codes for dyspepsia
  merge(drug_diag, all = TRUE) %>% # merge in ref from drug_diag
  select(readcode, ref) %>% # select readcode and ref columns only
  merge(descriptions) %>%
  select(-ALLCODES, -codes)
         
codelist_drug <- prodcodes %>%
  rename(cond = 'CONDITION CODE',
         cond_desc = 'CONDITION DESCRIPTION',
         type = TYPE,
         ud = 'USAGE DEFINITION') %>% # rename columns to match the diag table
  select(cond, cond_desc, ref, type, read, Rx, logic, special, ud, readcode) # select columns to match the diag table

saveRDS(codelist_drug, here::here('cam_code_list_drug.rds')) #save to lookups directory

## Create final codelist ----
codelist_wales <- codelist_diag %>%
  select(-medcode) %>% # drop medcode column as unnecessary  
  rbind(codelist_drug) # combine the diag codes and drug codes

saveRDS(codelist_wales, here::here('cam_codelist_wales.rds')) #save to lookups directory
