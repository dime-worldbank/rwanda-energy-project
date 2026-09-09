#----------------------------------------------------------------------
#Author: Yeji Kim
#Date: 2026-08-25
#Key Functions: 
# 1. Inspect the discrepency villages sample size 
# Context: There are 181 sample villages (baseline analysis report) yet tabulating baseline_1973_clean shows 180
# Locating the cause of discrepency is central because these villages will be the basis of midline survey.
# baseline_1973_clean is the output of the below scipt of cleaning pipeline 
# https://github.com/dime-worldbank/rwanda-energy-project/blob/main/baseline%20analysis/script/1.%20descriptives%20shorter.R
#----------------------------------------------------------------------

#-----------#
# Setup ####
#-----------#

# Clear environment (Optional)
# rm(list = ls())

# Libraries 
pacman::p_load(here, openxlsx, dplyr, tidyr, stringr, readr)

# Set path
here() #should be individual-path/github/rwanda-energy-project
source("PATHS.R")

#-----------#
# Import Data ####
#-----------#

# Households ####
complete_status <- read_xlsx(
  # Data source used in 0. complete status for EDCL.R 
  path = file.path(DATA_SCOPE, "vulnerable households in sample villages_final.xlsx")) %>% 
  mutate(hh_head_name = paste0(first_name, " ", last_name))

baseline_hh_og   <- read.csv(
  # n=5569 of Baseline Survey Response with 1181 variables
  # 5399 distinct households, from 190 villages 
  # Eligible, non-eligible, and consented, not consenting all mixed
  file.path(DATA_CTO_BL, "REP_baseline_test_WIDE (2).csv"))

hfc_constr_raw <- read_xlsx(
  # n=5542 of Baseline Survey Response with 1716 variables
  # 5392 distinct households, from 187 village
  file.path(OUTPUT_HFC, "hfc_constr_0728.xlsx"))

baseline_1973 <- read.xlsx(
  # n=1973, 1973 unique households, 180 villages across 1727 variables
  file.path(DATA_ANALYSIS, "baseline_1973_clean.xlsx"))

# Villages #### 
villages_181 = read.xlsx(
  # Outcome of '0. complete status for EDCL.R' script in baseline analysis pipeline  
  # villages_181 is created using 'complete_status' and filtering for `Dropped from scope due to 15kv` == "No"
  file.path(OUTPUT_ANALYSIS, "Surveyed 181 villages.xlsx")) # list of 181 villages 

#-----------#
# Analysis
#-----------#

# Step 1 ####
# Filter out the mismatch village 
villages_180 = baseline_1973 |> group_by(village) |> summarise(n=n()) |> ungroup()
village_1 = villages_181 |> anti_join(villages_180 |> mutate(villageid_key = as.character(village)), by = "villageid_key") 
#32110109, Rwamigega

# Step 2 ####
# Inspect the survey response from the villages 
hfc_rutsiro_0 = village_1 |> 
  left_join(hfc_constr_raw |> mutate(village = as.character(village)), by = join_by("villageid_key" == "village"))

# [H1] Are these households in vulnerable households list 
hfc_rutsiro_simple = hfc_rutsiro_0 |>
  select(village, villageid_key, hh_id, consent, A1_1, A1_2, A1_2_3month, A1_3)

# Which rows in hfc_rutsiro_0 is not in the vulnerable household lists?
# NONE, meaning that all the household from village '32110109' were vulnerable households
flag = hfc_rutsiro_simple |> anti_join(complete_status, by = join_by("hh_id" == "household_id"))

# [H2] Were these answers not valid? 
# In order to be considered valid, 
# A1_1 How many people in total live in your household and use your household as primary residence?
# A1_2 Does this hh has connection to the national grid?
# A1_2_3month Was this hh connected to the grid in the last 3 months
# A1_3 Does this household use off-grid solar systems? 
# valid case 1: Give consent + if connected to grid, should be within 3 month, and not connected to SHS
# valid case 2: Give consent + not connected to grid nor SHS 

hfc_valid = hfc_rutsiro_simple |>
  filter((consent == 1 & A1_2 == 1 & A1_2_3month == 1 & A1_3 == 0) | # case 1
         (consent == 1 & A1_2 == 0 & A1_3 == 0)) # case 2

# Finally, do baseline_1973 pass this valid answer test? 
# If yes, the hfc_valid_all should also have sample size of 1973
hfc_valid_all = baseline_1973 |>
  filter((consent == 1 & A1_2 == 1 & A1_2_3month == 1 & A1_3 == 0) | 
           (consent == 1 & A1_2 == 0 & A1_3 == 0))
# YESS!!!!!! It does 

# [H3] Locate where it happened in baseline analysis pipeline 

# Below script is from 1. descriptives shorter.R 
# (part of rwanda-energy-project/baseline analysis/script pipeline)
# before performing descriptive analysis, there is this chunck of code that will filter the sample 
# originally it inputs 'hfc_constr_raw' and replace it with hfc_rutsiro_0

#A1_1 How many people in total live in your household and use your household as primary residence?

hfc_constr_valid <- hfc_rutsiro_0 %>% 
  filter(consent == 1) %>%
  filter(!is.na(A1_1)) %>%
  distinct(hh_head_name, hh_id, A1_2, A1_3, .keep_all = TRUE) %>%
  filter(village %in% villages_181$villageid_key) %>%
  group_by(hh_id) %>%
  arrange(desc(hh_head_name %in% complete_status$hh_head_name)) %>% 
  slice(1) %>%
  ungroup()

## Again this validates H2 that this pipeline has created the omission of village '32110109' 