#----------------------------------------------------------------------
#Author: Yeji Kim
#Date: 2026-09-01
#Purpose: extract 180 sample villages 
#Output: villages_180_full = read_csv(file.path(OUTPUT_ML, "A-villages-180.csv"))
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
baseline_1973 <- read.xlsx(
  # n=1973, 1973 unique households, 180 villages across 1727 variables
  file.path(DATA_ANALYSIS, "baseline_1973_clean.xlsx"))

vulnerable_hh <- openxlsx::read.xlsx( 
  # n=1973, 1973 unique households, 180 villages across 10 variables 
  file.path(DATA_CTO_ML, "0-midline-vulnerable-household-sample.xlsx"))

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

# Step 1-2
# Filter out the 180 villages from vulnerable_hh
villages_180_full = vulnerable_hh |> 
  select(district, sector, cell, village, villageid_key) |>
  group_by(villageid_key) |> 
  slice(1) |>
  mutate(district = str_to_title(district))

# Step 1-3 
# Check if the villages match with baseline_1973 villages
check = villages_180_full |> anti_join(villages_180 |> mutate(villageid_key = as.character(village)), by  = "villageid_key") 

# Save 180 villages
write_csv(villages_180_full, file.path(OUTPUT_ML, "A-villages-180.csv"))