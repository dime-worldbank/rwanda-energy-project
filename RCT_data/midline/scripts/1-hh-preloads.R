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

# Load Data ####
vulnerable_hh <- openxlsx::read.xlsx(file.path(DATA_CTO_ML, "0-midline-vulnerable-household-sample.xlsx")) #contains NID, admin
baseline_1973 <- read.xlsx(file.path(DATA_ANALYSIS, "baseline_1973_clean.xlsx")) #contains phone number
baseline_treatment <- read.xlsx(file.path(DATA_CTO_BL, "scope_193_0807.xlsx")) |> select(village_id, treat) #village_id, and treat for 193 villages 


# Sanity check ####
# 1. check distinctiveness of "hh_id" 
sum(duplicated(baseline_1973$hh_id)) # should be 0
sum(duplicated(vulnerable_hh$household_id)) # should be 0 

# 2.1. one row per village 
baseline_treatment |> count(village_id) |> filter(n>1) # should be 0
n_distinct(baseline_treatment$village_id) # should be 193

# Data trim ####
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

# Join with vulnerable households sample 
pre_load_phone <- vulnerable_hh |> 
  left_join(
    baseline_phone |> mutate(hh_id = as.character(hh_id)), 
    by = join_by(household_id == hh_id)
    ) 

# Treatment assignment - add labels 
baseline_treatment <- baseline_treatment |> 
  mutate(
    treat_label = recode_values(
      treat,
      "C"  ~ "Control",
      "T1" ~ "Readyboard",
      "T2" ~ "Solar off grid",
      "T3" ~ "Readyboard & Solar off grid"
    )
  )

pre_load_data <- pre_load_phone |>
  left_join(
    baseline_treatment, by = join_by("villageid_key" == "village_id")
  ) |>
  rename(householdid_key = household_id)

# Sanity check ####

# treatment assignment 
# Check 1. Did all households get treatment? 
pre_load_data |> 
  count(treat, treat_label)|> 
  mutate(pct = n / sum(n)) |>
  janitor::adorn_totals("row") |>
  mutate(pct = scales::percent(pct, accuracy = 0.1))

# Check 2. Is treatment assignment constant within village?
pre_load_data |>
  summarise(n_arms = n_distinct(treat), .by = villageid_key) |>
  filter(n_arms > 1) # should be 0 

# Check 3. Does Household ID correctly encodes its village_id?
# If hhid does not include its village_id, treatment arms (assigned at village level) at household levels may be corrupted 
## household_id = villageid_key * 10000 + hh_number * 10 + (digit sum mod 10)
id_check <- pre_load_data |>
  mutate(
    id              = as.character(householdid_key),
    village_from_id = str_sub(id, 1, -5),
    check_digit     = as.integer(str_sub(id, -1, -1)),
    base            = paste0(str_sub(id, 1, -2), "0"),
    check_expected  = vapply(strsplit(base, ""), \(d) sum(as.integer(d)) %% 10, numeric(1))
  )

id_check |> filter(village_from_id != villageid_key)   # expect 0 rows: hh sits in the village its ID says
id_check |> filter(check_digit != check_expected)      # expect 0 rows: ID not corrupted

# Clean phone function
# Check 1. Does all household's phone number went through this function? 

pre_load_data |> count(is.na(phone_clean), is.na(phone2_clean)) |>
  mutate(pct = n / sum(n)) |>
  janitor::adorn_totals("row") |>
  mutate(pct = scales::percent(pct, accuracy = 0.1))

# Data Filtering #### 
# Select household heads only 
household_head = pre_load_data |> 
  select(villageid_key, village, cell, sector, district, householdid_key, first_name, last_name, nid) |>
  mutate(household_id = householdid_key)

# Filter T2, T3 for SHS eligible households 
# we want T2 + T3 (906 households)

SHS_eligible = pre_load_data |>
  filter(treat == "T2" | treat == "T3") |>
  select(villageid_key, village, cell, sector, district, householdid_key, treat, treat_label) |>
  mutate(eligible = 1) # add a tag for SurveyCTO

SHS_eligible |> 
  summarise(n_treat = n_distinct(treat), .by = villageid_key) |>
  filter(n_treat > 1) # should be 0 

n_distinct(SHS_eligible$householdid_key) # should be 906

# how many households in each district, village will get the nudge?
SHS_summary = SHS_eligible |>
  mutate(district = str_to_title(district)) |>
  group_by(district, treat, treat_label) |>
  summarise(n_village = n_distinct(villageid_key), 
            n_hh = n_distinct(householdid_key)) 
  
# Save preload ####

write.csv( 
  pre_load_data, 
  file = file.path(DATA_ML_PRELOAD, "contact_info.csv"),
  row.names = TRUE
)

write.csv(
  household_head,
  file = file.path(DATA_ML_PRELOAD, "household_head.csv")
)

write.csv(
  SHS_eligible,
  file = file.path(DATA_ML_PRELOAD, "shs_eligible.csv")
)
