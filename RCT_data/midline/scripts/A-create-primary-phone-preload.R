# Date: 2026-08-19 
# Author: Yeji-Kim
# Purpose: Create pre-load dataset containing useful sample identifiers, phone numbers, and treatment assignment 

# Notes: Midline survey will follow-up on the 1973 households of baseline survey respondents. 
# Constructing preload dataset has two aims. First, the 'contact information' of the survey uses baseline cell-phone number, instead of inquiring the phone number redundantly, 
# second, we pull treatment status of sample households and for SHS and SHS+Readyboard treatment arm, we want to encourage the eligible households to take up SHS subsidies in the next cycle. 

# Input: 
# Main functions: clean_phone
# Output: 

# Clear environment
rm(list = ls())

# Libraries
pacman::p_load(here, openxlsx, dplyr, tidyr, stringr, readxl)
            
# Set path
here()
source("PATHS.R")

# Load Data 
vulnerable_hh <- openxlsx::read.xlsx(file.path(DATA_CTO_ML, "0-midline-vulnerable-household-sample.xlsx")) #contains NID, admin
baseline_1973 <- read.xlsx(file.path(DATA_ANALYSIS, "baseline_1973_clean.xlsx")) #contains phone number
baseline_treatment <- read.xlsx(file.path(DATA_CTO_BL, "scope_193_0807.xlsx")) #village treatment

# Data source used in 0. complete status for EDCL.R 
complete_status <- read_xlsx(path = file.path(DATA_SCOPE, "vulnerable households in sample villages_final.xlsx")) %>% 
  mutate(hh_head_name = paste0(first_name, " ", last_name))
# villages_181 is created using 'complete_status' and filtering for `Dropped from scope due to 15kv` == "No"
villages_181 = read.xlsx(file.path(OUTPUT_ANALYSIS, "Surveyed 181 villages.xlsx")) # list of 181 villages 


# 1. check distinctiveness of "hh_id" 
sum(duplicated(baseline_1973$hh_id))
sum(duplicated(vulnerable_hh$household_id))

# carry only relevant variables for pre-load data 
baseline_phone <- baseline_1973 |>
  select("hh_id", "phonenumber", "second_phonenumber", contains("coordinate")) 

# clean_phone function
clean_phone <- function(x) {
  # keep digits only → "O", spaces, "+", etc. gone
  digits <- str_remove_all(x, "[^0-9]")   
  # Add leading 0 if number is in 9-digit format
  digits <- if_else(
    str_detect(digits, "^7[0-9]{8}$"), 
    paste0("0", digits),
    digits
    )
  # Keep only valid 10-digit Rwanda mobile number
  if_else(
    str_detect(digits, "^07[0-9]{8}$"),
    digits,
    NA_character_
  )
}

# apply function
baseline_phone <- baseline_phone |>
  mutate(
    phone_clean  = clean_phone(phonenumber),
    phone2_clean = clean_phone(second_phonenumber)
  ) 

# Join 
pre_load_phone <- vulnerable_hh |> 
  left_join(
    baseline_phone |> mutate(hh_id = as.character(hh_id)), 
    by = join_by(household_id == hh_id)
    ) 

# Treatment assignment 
baseline_treatment <- baseline_treatment |> select(village_id, treat)

pre_load_data <- pre_load_phone |>
  left_join(
    baseline_treatment, by = join_by("villageid_key" == "village_id")
  )

# order the preload data for the SurveyCTO format
# The first row of each csv file should consist of a header 

pre_load_data <- pre_load_data |>
  rename(village_id = villageid_key,
         household_id_ley = household_id)

# Summary
pre_load_data |> group_by(treat) |> count() |> ungroup() |> mutate(pct = scales::percent(n/sum(n), accuracy = 0.1))
# # A tibble: 4 × 3
# treat     n pct  
# <chr> <int> <chr>
# 1 C       530 26.9% Control
# 2 T1      537 27.2% Readyboard
# 3 T2      483 24.5% Solar off grid
# 4 T3      423 21.4% Readyboard & Solar off grid 

n_distinct(pre_load_data$villageid_key) 
# [1] 180

pre_load_data |> count(is.na(phone_clean), is.na(phone2_clean)) |>
  mutate(pct = scales::percent(n / sum(n), accuracy = 0.1))
# is.na(phone_clean) is.na(phone2_clean)   n   pct
# 1              FALSE               FALSE 813 41.2%
# 2              FALSE                TRUE 719 36.4%
# 3               TRUE               FALSE  24  1.2%
# 4               TRUE                TRUE 417 21.1%

write.csv( 
  pre_load_data, 
  file = file.path(DATA_CTO_ML, "A-surveycto-preload-data.csv"),
  row.names = TRUE
)

write.csv( 
  pre_load_data, 
  file = file.path(DATA_ML, "A-surveycto-preload-data.csv"),
  row.names = TRUE
)
