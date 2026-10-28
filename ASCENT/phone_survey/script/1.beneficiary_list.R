#----------------------------------------------------------------------#
#### ASCENT phone survey: clean cooking beneficiary list            ####
#----------------------------------------------------------------------#

# Run from the repo root

#--------------------------------------
# 0. Setup
#--------------------------------------

library(tidyverse, janitor, readr)

source("PATHS.R")

# Make sure the ASCENT paths loaded
stopifnot(exists("DROPBOX_ASCENT"), dir.exists(DROPBOX_ASCENT))
stopifnot(exists("DATA_ASCENT_PRELOAD"), dir.exists(DATA_ASCENT_PRELOAD))

#--------------------------------------
# 1. Load raw beneficiary list
#--------------------------------------

beneficiary_raw <- read_csv(
  file.path(DATA_ASCENT_PRELOAD, "clean-cooking-beneficiaries-list-raw.csv")
)

#--------------------------------------
# 2. Glimpse and clean variable name 
#--------------------------------------

glimpse(beneficiary_raw)

sum(is.na(beneficiary_raw$`Household ID`)) # 13,173

sum(is.na(beneficiary_raw$`Phone number`)) # none missing!
sum(is.na(beneficiary_raw$`Product name`)) # 0 
sum(is.na(beneficiary_raw$`Date of purchase/ confirmation in ET`)) # 0 

n_distinct(beneficiary_raw$`Product name`) #26
table(beneficiary_raw$`Product name`, beneficiary_raw$`Tier level`) 
# except Hisha Eco stove 1 and Super Power Gas Cooker_Single, which is Tier 4, 
# the rest is tier 5 

beneficiary = beneficiary_raw |>
  janitor::clean_names()

names(beneficiary)

beneficiary = beneficiary |>
  mutate(
    date_of_purchase = date_of_purchase_confirmation_in_et,
    paid_by_user = amount_to_be_paid_by_end_user
  )

# For preload we need the stove type, whether they are LPG or EPC, and their date of purchase

#--------------------------------------
# 3. EPC and LPG categorization 
# Review -- check categorization 
#--------------------------------------

beneficiary <- beneficiary %>%
  mutate(
    stove_type = case_when(
      product_name %in% c(
        "ALY-K3 Pressure cooker",
        "EBMS E Smart Pressure cooker 6L",
        "Panda EPC",
        "Teka Neza Pressure Cooker",
        "Tembo Pressure Cooker",
        "Ubushyuhe EPC"
      ) ~ "EPC",
      
      product_name %in% c(
        "Alliup No 1 LPG stove",
        "Alliup No 1 Metal LPG Stove",
        "Alpha 1",
        "BBoxx 12 Kg",
        "BURNER & VALVE TIANLONG",
        "EMGAS LPG Stove",
        "EMS LPG Stove",
        "Hello Gas II",
        "Hello Gas III",
        "Hisha Eco stove 1",
        "Lemi Cookstove 12kg",
        "Love Stove",
        "Love Stove 10kg",
        "Love Stove 12 kg",
        "Neseltec S1578",
        "Super Power Gas Cooker (Double)",
        "Super Power Gas Cooker_Single",
        "Tekera rimwe",
        "Terimbere LPG Stove"
      ) ~ "LPG",
      
      TRUE ~ "Other"
    )
  )

table(beneficiary$product_name, beneficiary$stove_type)

#--------------------------------------
# 4. Drop missing households 
# Review - check for the decision 
#--------------------------------------

beneficiary = beneficiary |>
  filter(!is.na(household_id))

n_distinct(beneficiary$household_id)

beneficiary = beneficiary |>
  rename(householdid_key = household_id)
  
# Save output 

write_csv(
  beneficiary,
  file.path(DATA_ASCENT_PRELOAD, "beneficiaries-list-intermediate.csv")
)


