## Project Description

- This Networked Data Lab analysis by the NDL lab in Wales focusses on providing new insights into reablement care at a local authority level in Wales, including exploration of the demographics and health of those accessing reablement care.
- Please note that these research outputs have not yet been peer-reviewed and should be treated as preliminary.

## Data Sources

- Annual District Death Extract (ADDE) (as at 01/04/2024) - used to determine individuals who meet the study criteria for inclusion in the cohort.
- Patient Episode Database Wales (PEDW) (as at 04/08/2024) - used to determine hospital referrals to reablement care services.
- Reablement care data for Bridgend Local Authority (as at 07/08/2024) - used to identify individuals in Bridgend who accessed reablement care during the study period.
- Reablement care data for Rhondda Cynon Taf Local Authority (as at 04/07/2024) - used to identify individuals in RCT who accessed reablement care during the study period.
- Welsh Demographic Service Dataset (WDSD) (as at 04/08/2024) - used to determine individuals who meet the study criteria for inclusion in the cohort and their demographic information.
- Wales Longitudinal General Practice (WLGP) (as at 04/08/2024) - used to identify individuals who meet the study criteria for inclusion in the cohort, and to determine long-term conditions.

## Requirements

SQL was written to query a DB2 database in SAIL Project 1658. R scripts were written in R 4.3.3 and run in RStudio.

## Getting started (Health Foundation Briefing)

- Under each section folder, the following folder structure is required:
	* data
	* outputs
	* scripts	

- Section 1: Cohort creation 
  * 1.01_sail_gp_registration_lookup.sql: Creates lookup table for continuous periods of registration at a SAIL GP.
  * 1.02_cohort_creation.sql: Creates reablement cohort using inclusion criteria
  * 1.03_matching_cohort.sql: Creates matching pool of potential 'non-reablement' matches
  * 1.04_matched_cohort_creation.R: creates matched cohort

- Section 2: Demographics
  * 2.01_demographics.R: Demographic analysis of reablement cohorts. Produces counts, percentages and plots by sex, age group, deprivation and rurality.
  * 2.02_ons_demographics.R: Demographic analysis of general adult popultaion in LAs of interest. Produces counts, percentages and plots by sex, age group, deprivation and rurality, using ONS mid-2022 population estimates.

- Section 3: LTC (long-term conditions)
  * 3.01_ltc.sql: Extracts long-term conditions from WLGP.
  * 3.02_ltc.R: Combines long-term conditions data extract with  Bridgend cohort data. Adapted from code written by Dr Will Parry, sourced from HFAnalyticsLab GitHub. Original code: https://github.com/HFAnalyticsLab/CMD_multimorbidity/blob/master/R/03_Create_Cambridge_Score.R
  * 3.03_ltc_analysis.R: Analysis of long-term conditions (multimorbidity and most common conditions) in reablement/matched populations.

- Section 4: Referral Source
  * 4.01_referrals.R: Provide descriptive analysis and accompanying plots on the types of referral sources in each local authority.

- Section 5: Service Type
  * 5.01_types_of_services.R: Provide descriptive analysis and accompanying plots on the types of reablement services received by individuals in each Local Authority.
