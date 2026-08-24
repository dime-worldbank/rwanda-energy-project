# Date: 2026-08-19 
# Author: Yeji-Kim
# Purpose: Create pre-load dataset containing useful sample identifiers, phone numbers, and treatment assignment 

# Notes: Midline survey will follow-up on the 1973 households of baseline survey respondents. 
# Constructing preload dataset has two aims. First, the 'contact information' of the survey uses baseline cell-phone number, instead of inquiring the phone number redundantly, 
# second, we pull treatment status of sample households and for SHS and SHS+Readyboard treatment arm, we want to encourage the eligible households to take up SHS subsidies in the next cycle. 

# Input: 
# Main functions:
# Output: 

# Clear environment
rm(list = ls())

# Libraries
pacman::p_load(here, openxlsx, dplyr, tidyr, stringr)
            
# Set path
here()
source("PATHS.R")

# Load Data 

#baseline_hh_og   <- read.csv(file.path(DATA_CTO_BL, "REP_baseline_test_WIDE (2).csv"))

vulnerable_hh <- openxlsx::read.xlsx(file.path(DATA_CTO_ML, "0-midline-vulnerable-household-sample.xlsx")) #contains NID, admin
baseline_1973 <- read.xlsx(file.path(DATA_ANALYSIS, "baseline_1973_clean.xlsx")) #contains phone number
baseline_treatment <- read.xlsx(file.path(DATA_CTO_BL, "scope_193_0807.xlsx")) #village treatment

# 1. check distinctiveness of "hh_id" 
sum(duplicated(baseline_1973$hh_id))
sum(duplicated(vulnerable_hh$household_id))

# carry only relevant variables for pre-load data 
baseline_phone <- baseline_1973 |>
  select("hh_id", "phonenumber", "second_phonenumber", contains("coordinate")) 

# clean_phone function
clean_phone <- function(x) {
  digits <- str_remove_all(x, "[^0-9]")   # keep digits only → "O", spaces, "+", etc. gone
  digits <- str_remove(digits, "^0+")     # rule 1: strip leading zero(s)
  # rules 2 & 3: valid only if exactly 9 digits starting with 7, else NA
  if_else(str_detect(digits, "^7[0-9]{8}$"), digits, NA_character_)
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

# Summary
pre_load_data |> group_by(treat) |> count()
# treat     n
# <chr> <int>
# 1 C       530 Control
# 2 T1      537 Readyboard
# 3 T2      483 Solar off grid
# 4 T3      423 Readyboard & Solar off grid 

n_distinct(pre_load_data$villageid_key) 
# [1] 180

# Output
write.csv( 
  pre_load_data, 
  file = file.path(DATA_CTO_ML, "1-surveycto-preload-data.csv"),
  row.names = TRUE
)
  
# Thoughts
# For SurveyCTO phone numbers, we can apply constraint for a 9-digits number starting with 7 or 2
