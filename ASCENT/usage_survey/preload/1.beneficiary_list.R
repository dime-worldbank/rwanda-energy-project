#----------------------------------------------------------------------#
#### ASCENT phone survey: clean cooking beneficiary list            ####
### Purpose: Clean the raw beneficiary list 
### Function 0: Check for missing obs in key variables 
### Function 1: Remove missing hhid 
### Function 2: From product name, categorize them into stove_type (LPG and EPC)
### Function 3: Add official admin codes (province, sector, id keys) from the
###             EAQIP village list, matched on district + cell + village names
#----------------------------------------------------------------------#

# Run from the repo root

#--------------------------------------
# 0. Setup
#--------------------------------------

library(tidyverse)   # includes readr; janitor is called as janitor::

source("PATHS.R")

# Make sure the ASCENT paths loaded
stopifnot(exists("DROPBOX_ASCENT"), dir.exists(DROPBOX_ASCENT))
stopifnot(exists("DATA_ASCENT_PRELOAD"), dir.exists(DATA_ASCENT_PRELOAD))

# Official village list (all villages in Rwanda, with codes), used for the EAQIP baseline
stopifnot(exists("DROPBOX_EAQIP"), dir.exists(DROPBOX_EAQIP))
ADMIN_FILE <- file.path(DROPBOX_EAQIP, "questionnaires/01-baseline/vills.csv")

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

#--------------------------------------
# 5. Add official admin codes
#--------------------------------------
# The beneficiary list has district, cell and village NAMES only: no codes and no
# sector. The household ID does not encode the location either (even its first two
# digits map to several districts). The form's cascade needs district -> sector ->
# cell -> village codes, so they are taken from the official village list (ADMIN_FILE,
# 14,837 villages), joined on EXACT district + cell + village names.
#
# Checked 2026-11-05 (households with an ID): 5,522 of 5,525 locations match exactly
# one official village (43,033 households). No location is unmatched. Normalising
# case / spaces / apostrophes adds nothing: the names come from the same source.
#
# AMBIGUOUS: 3 locations, 6 households (2 each). Their district + cell + village name
# exists in TWO different sectors of the official list (sector is a level between
# district and cell, and a cell name can repeat in another sector of the same
# district). The beneficiary list has no sector, so the names alone cannot tell
# which village is meant. Codes are left NA and admin_match = "ambiguous":
#
#   District   Cell       Village    Official candidates (sector, villageid_key)
#   Gasabo     Gasagara   Gasagara   Gikomero 12030103  or  Rusororo 12140202
#   Gatsibo    Kigabiro   Kigabiro   Gasange  53010102  or  Remera   53120305
#   Nyagatare  Kabeza     Kabeza     Gatunda  52010202  or  Rwimiyaga 52130202
#
# None of the 6 is in the usage-survey sample drawn 2026-11-05. To resolve them, ask
# the program team (or the household) for the sector. Note: some sector names in the
# official file are in capitals (e.g. GATUNDA); names are kept as in the file.

admin <- read_csv(ADMIN_FILE, col_types = cols(.default = col_character())) |>
  mutate(n_official = n(), .by = c(district_key, cell_key, village_key))

admin_unique <- admin |>
  filter(n_official == 1) |>
  select(district = district_key, cell = cell_key, village = village_key,
         provinceid_key, province_key, districtid_key, sectorid_key, sector_key, cellid_key, villageid_key)
admin_ambiguous <- admin |>
  filter(n_official > 1) |>
  distinct(district = district_key, cell = cell_key, village = village_key) |>
  mutate(ambiguous = TRUE)

beneficiary = beneficiary |>
  left_join(admin_unique, by = c("district", "cell", "village")) |>
  left_join(admin_ambiguous, by = c("district", "cell", "village")) |>
  mutate(admin_match = case_when(!is.na(villageid_key) ~ "unique",
                                 ambiguous %in% TRUE ~ "ambiguous",
                                 .default = "none")) |>
  select(-ambiguous)

table(beneficiary$admin_match) # unique 43,033 | ambiguous 6

# New spellings in a future beneficiary list must not slip through silently
if (any(beneficiary$admin_match == "none")) {
  stop(sum(beneficiary$admin_match == "none"), " household(s) whose district + cell + village is not in ",
       basename(ADMIN_FILE), ". Check the spelling before continuing.")
}
if (sum(beneficiary$admin_match == "ambiguous") != 6) {
  warning("Ambiguous households: ", sum(beneficiary$admin_match == "ambiguous"),
          " (6 on 2026-11-05). Update the note in section 5.")
}

# Save output

write_csv(
  beneficiary,
  file.path(DATA_ASCENT_PRELOAD, "beneficiaries-list-intermediate.csv")
)


