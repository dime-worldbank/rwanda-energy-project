# MASTER-DATASETS.R 
# RAW-INTERMEDIATE-FINAL datasets are all here 
# Prerequisite - run PATHS.R
# Datapaths are defined at Github / rwanda-energy-project/PATHS.R

#--------------------------
# BASELINE RAW DATA 
#--------------------------

#### Households ####
# List of vulnerable households 
complete_status <- read_xlsx(
  # n=8268 unique households, villages 193
  # complete_status and baseline_hh_treatment has identical 
  path = file.path(DATA_SCOPE, "vulnerable households in sample villages_final.xlsx")) %>% 
  mutate(hh_head_name = paste0(first_name, " ", last_name)) 

baseline_hh_treatment <- read.xlsx( 
  #n=8268 unique households, 193 villages, 23 variables 
  file.path(DATA_SCOPE, "Latest Scope", "household_list(survey status & scope & treatment).xlsx")) 

# raw survey data used for Baseline Analysis pipeline 
hfc_constr_raw <- read_xlsx(
  # n=5542 of Baseline Survey Response with 1716 variables
  # 5392 distinct households, from 187 village
  # imported at 1. descriptives shorter.R and then filtered for eligible villages and conditions
  # modified 1973 households is located at OUTPUT_ANALYSIS/hfc_constr_files/
  # https://www.dropbox.com/scl/fi/px88yt1wz9rdubxg1iu20/hfc_constr_20250927_182531.xlsx?rlkey=8tr7aqvioctykmn55bqvg57d2&dl=0
  file.path(OUTPUT_HFC, "hfc_constr_0728.xlsx"))

# TODO - unsure of this data's origin, suspecting this is also raw survey data but with more sample size 
baseline_hh_og   <- read.csv(
  # n=5569 of Baseline Survey Response with 1181 variables
  # 5399 distinct households, from 190 villages 
  # Eligible, non-eligible, and consented, not consenting all mixed
  file.path(DATA_CTO_BL, "REP_baseline_test_WIDE (2).csv"))

#### Villages ####
baseline_treatment <- read.xlsx( 
  # n= 193 villages and their survey, scope status
  file.path(DATA_CTO_BL, "scope_193_0807.xlsx")) #village treatment

#--------------------------
# BASELINE INTERMEDIATE DATA 
#--------------------------
#### Households ####

#### Villages ####
villages_181 = read.xlsx(
  # n=181, 181 unique villages 
  # this dataset is created using 'complete_status' and 
  # filtering for `Dropped from scope due to 15kv` == "No"
  # output of 0. Complete Status for EDCL.R 
  # Tabulation from here is used for the Baseline Analysis report 
  file.path(OUTPUT_ANALYSIS, "Surveyed 181 villages.xlsx")) # list of 181 villages 

#--------------------------
# BASELINE FINAL DATA 
#--------------------------

vulnerable_hh <- openxlsx::read.xlsx( 
  # n=1973, 1973 unique households, 180 villages across 10 variables 
  file.path(DATA_CTO_ML, "0-midline-vulnerable-household-sample.xlsx"))

baseline_1973 <- read.xlsx(
  # n=1973, 1973 unique households, 180 villages across 1727 variables
  file.path(DATA_ANALYSIS, "baseline_1973_clean.xlsx"))


#--------------------------
# MIDLINE RAW DATA 
#--------------------------

# Households #### 
# Eligible households 
vulnerable_hh <- openxlsx::read.xlsx( 
  # n=1973, 1973 unique households, 180 villages across 10 variables 
  file.path(DATA_CTO_ML, "0-midline-vulnerable-household-sample.xlsx"))

baseline_1973 <- read.xlsx(
  # n=1973, 1973 unique households, 180 villages across 1727 variables
  file.path(DATA_ANALYSIS, "baseline_1973_clean.xlsx"))

# Survey response 


#--------------------------
# MIDLINE INTERMEDIATE DATA 
#--------------------------

villages_180 <- read.csv (
  file.path(DATA_CTO_ML, "A-villages-180.csv")
)

preload_data <- read.csv(
  # n=1973, 1973 unique households across 180 villages 
  # output of A-create-primary-phone-preload.R
  file.path(DATA_CTO_ML, "A-surveycto-preload-data.csv"))

