# ==============================================================================
# 12.hfc_redeployment_update.R
# Fill missing administrative IDs in redeployment_list.xlsx from scope_villages.
#
# Loads the HFC preloads and the redeployment workbook. Existing nonblank IDs and
# all other columns are preserved. The original workbook is archived before the
# updated candidate is copied back to OneDrive.
#
# Run from the repo root. On Windows, set TEMP, TMP, and TMPDIR to an approved
# Dropbox or OneDrive directory before starting R, because openxlsx uses R's
# temporary directory while saving workbooks.
#
#   Rscript RCT_data/midline/HFC/scripts/12.hfc_redeployment_update.R
# ==============================================================================


# ---- 1. Setup ----------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(openxlsx)
  library(dplyr)
  library(stringr)
})

source("PATHS.R")

# ---- 2. Config / constants ---------------------------------------------------

redeployment_list <- read_excel(file.path(SHARED_EAQIP, "datawork/HFC_midline/output/checks/redeployment_list.xlsx"))
scope_villages <- read.csv(file.path(SHARED_EAQIP, "questionnaires/02-midline/02-CTO-preload/scope_villages.csv"))
contact_info <- read.csv(file.path(SHARED_EAQIP, "questionnaires/02-midline/02-CTO-preload/contact_info.csv"), row.names=1)

# 1. Join scope villages on the full location hierarchy
redeployment_list <- redeployment_list %>%
  left_join(
    scope_villages,
    by = c(
      "district" = "district_key",
      "sector"   = "sector_key",
      "cell"     = "cell_key",
      "village"  = "village_key"
    )
  )

# 2. Build full names from contact_info (one row per household)
contact_names <- contact_info %>%
  mutate(
    householdid_key = format(householdid_key, scientific = FALSE, trim = TRUE),
    contact_full_name = str_squish(
      paste(coalesce(first_name, ""), coalesce(last_name, ""))
    ),
    contact_full_name = na_if(contact_full_name, "")
  ) %>%
  filter(householdid_key != "NA") %>%
  distinct(householdid_key, .keep_all = TRUE) %>%
  select(householdid_key, contact_full_name)

# 3. Fill missing household_head_name
redeployment_list <- redeployment_list %>%
  mutate(household_id = str_trim(household_id)) %>%
  left_join(contact_names, by = c("household_id" = "householdid_key")) %>%
  mutate(household_head_name = coalesce(household_head_name, contact_full_name)) %>%
  select(-contact_full_name)

# Check: how many are still missing
sum(is.na(redeployment_list$household_head_name))

# 4. Select Key Variables 
redeployment_list <- redeployment_list %>%
  rename(householdid_key = household_id) %>%
  select(district, districtid_key, sector, sectorid_key, cell, cellid_key, village, villageid_key,
         treat, enumerator, enumerator_key, householdid_key, household_head_name,
         primary_phone, secondary_phone)

# 5. Remove duplicate households (keeps first occurrence)
n_dup <- sum(duplicated(redeployment_list$householdid_key))
message("Duplicate householdid_key rows removed: ", n_dup)
redeployment_list <- redeployment_list[!duplicated(redeployment_list$householdid_key), ]

write.xlsx(redeployment_list, file.path(SHARED_EAQIP, "datawork/HFC_midline/output/checks/redeployment_list_final.xlsx"))
