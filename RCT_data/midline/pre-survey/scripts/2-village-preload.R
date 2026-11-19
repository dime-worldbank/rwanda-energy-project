#----------------------------------------------------------------------
#Author: Yeji Kim
#Date: 2026-09-01 (village list), 2026-09-03 (admin codes); merged 2026-11-19
#Purpose: 1. extract the 180 midline sample villages with their baseline treatment arm
#         2. check administrative IDs and assign admin codes at village level
#         3. write the village preload for SurveyCTO and the coded village lists
#
# Paths: see README.md in this folder
# Input:
#   baseline_1973_clean.xlsx, 0-midline-vulnerable-household-sample.xlsx,
#   Surveyed 181 villages.xlsx, scope_193_0807.xlsx, 0.Rwanda-Villages-Masterfile.xlsx
# Output:
#   villages-4-districts-masterfile.xlsx, villages-180-coded.xlsx (00-village-scope)
#   scope_villages.csv (02-CTO-preload; read by the midline HFC and the SurveyCTO form)
# Used by: 2b-villages-fuzzy-matching.R, 2c-EDCL-180-sample.R, MASTER-DATASETS.R, midline HFC scripts
#----------------------------------------------------------------------

#-----------#
# Setup ####
#-----------#

rm(list = ls())
pacman::p_load(here, openxlsx, readxl, dplyr, tidyr, stringr, readr)
source(here("PATHS.R"))

#-----------#
# Import Data ####
#-----------#

# Households ####
baseline_1973 <- read.xlsx(
  # n=1973, 1973 unique households, 180 villages across 1727 variables
  file.path(DATA_ANALYSIS, "baseline_1973_clean.xlsx"))

vulnerable_hh <- read.xlsx(
  # n=1973, 1973 unique households, 180 villages across 10 variables
  file.path(DATA_CTO_ML, "0-midline-vulnerable-household-sample.xlsx"))

# Villages ####
villages_181 <- read.xlsx(
  # Outcome of '0. complete status for EDCL.R' script in baseline analysis pipeline
  # villages_181 is created using 'complete_status' and filtering for `Dropped from scope due to 15kv` == "No"
  file.path(OUTPUT_ANALYSIS, "Surveyed 181 villages.xlsx"))

baseline_treatment <- read.xlsx(
  # n= 193 villages and their survey, scope status
  file.path(DATA_CTO_BL, "scope_193_0807.xlsx"))

# Complete National Villages List
villages_list <- read_excel(
  file.path(DATA_RCT, "0.Rwanda-Villages-Masterfile.xlsx"))

#-----------#
# 1. The 180 sample villages ####
#-----------#

# Pull sample villages from baseline sample
villages_180 <- baseline_1973 |> group_by(village) |> summarise(n = n()) |> ungroup()
# The one village in the 181 list that is not in the baseline sample
village_1 <- villages_181 |> anti_join(villages_180 |> mutate(villageid_key = as.character(village)), by = "villageid_key")
#32110109, Rwamigega

# Filter out the 180 villages from vulnerable_hh
villages_180_full <- vulnerable_hh |>
  select(district, sector, cell, village, villageid_key) |>
  group_by(villageid_key) |>
  slice(1) |>
  ungroup() |>
  mutate(district = str_to_title(district))

# Check if the villages match with baseline_1973 villages
check <- villages_180_full |> anti_join(villages_180 |> mutate(villageid_key = as.character(village)), by = "villageid_key")

# Merge the 180 villages treatment status
villages_180_full_treatment <- villages_180_full |>
  inner_join(baseline_treatment |> select(strata, treat, village_id, name) |>
               rename(village = name,
                      villageid_key = village_id),
             by = join_by(village, villageid_key))
stopifnot(nrow(villages_180_full_treatment) == nrow(villages_180_full))

# From which strata are the 13 dropped villages from?
villages_dropped <- baseline_treatment |>
  select(strata, treat, village_id, name) |>
  rename(village = name, villageid_key = village_id) |>
  anti_join(villages_180_full, by = join_by(village, villageid_key)) |>
  arrange(strata, treat)

# What is the size of the overall treatment arms?
baseline_sum <- baseline_treatment |> group_by(treat) |> summarize(baseline = n())
midline_sum <- villages_180_full_treatment |> group_by(treat) |> summarize(midline = n())

treatment_dist <- baseline_sum |>
  full_join(midline_sum, by = "treat") |>
  mutate(dropped = baseline - midline)

treatment_dist <- treatment_dist |>
  bind_rows(
    treatment_dist |> summarise(
      treat = "Total",
      across(c(baseline, midline, dropped), sum)
    )
  )

#----------------------------------#
# 2. Sanity check of admin IDs ####
#----------------------------------#

# Is ID uniquely assigned for each administrative level?
district_flag <- villages_list |> group_by(District) |>
  summarize(n = n_distinct(Distr_ID)) |> filter(n>1) # 0
sector_flag <- villages_list |> group_by(District, Sector) |>
  summarize(n = n_distinct(Sector_ID)) |> filter(n>1) # 1
cell_flag <- villages_list |> group_by(District, Sector, Cell) |>
  summarize(n = n_distinct(Cell_ID)) |> filter(n>1) # 0

# One sector and 6 villages have more than one ID assigned
village_flag <- villages_list |>
  group_by(District, Sector, Cell, Name) |>
  summarise(n = n_distinct(Village_ID),
            ids = paste(sort(unique(Village_ID)), collapse = ",")) |> filter(n>1)

# Within our scope, ONE village named "Rusekabuye" has two IDs 41130307,41130308
# It is not in our 180 villages, so the master list can be used as the reference
Rusekabuye_name <- villages_180_full |> filter(village == "Rusekabuye") # 0
Rusekabuye_ID <- villages_180_full |> filter(villageid_key == "41130307" | villageid_key == "41130308") # 0

# Two variables for village names: check if Name and Name.original differ in the four districts
villages_list_4d <- villages_list |>
  filter(District %in% c("Karongi", "Rulindo", "Rusizi", "Rutsiro")) |>
  mutate(Vill.name.flag = if_else(Name == Name.original, 0, 1)) |>
  rename(Village = Name,
         Village_unclean = Name.original) |>
  select(Prov_ID, Province, Distr_ID, District, Sector_ID, Sector, Cell_ID, Cell, Village_ID, Village, Village_unclean, Vill.name.flag) |>
  mutate(across(c(Province, District, Sector, Cell, Village), str_to_title))

name_flag <- villages_list_4d |> filter(Vill.name.flag > 0) # 8 observations

#--------------#
# 3. Join ####
#--------------#

# If the village names are clean it should have 180 obs
villages_ij <- villages_list_4d |>
  inner_join(villages_180_full |> mutate(Village_ID = as.character(villageid_key))
             |> rename(
                       District = district,
                       Sector = sector,
                       Cell = cell,
                       Village = village),
             by = c("District", "Sector", "Cell", "Village", "Village_ID")) |>
  select(-villageid_key)
stopifnot(nrow(villages_ij) == nrow(villages_180_full))

villages_scope <- villages_ij |>
  rename(provinceid_key = Prov_ID,
         province_key = Province,
         districtid_key = Distr_ID,
         district_key = District,
         sectorid_key = Sector_ID,
         sector_key = Sector,
         cellid_key = Cell_ID,
         cell_key = Cell,
         villageid_key = Village_ID,
         village_key = Village) |>
  select(-c(Village_unclean, Vill.name.flag))

#--------------#
# Save Output ####
#--------------#

write.xlsx(
  villages_list_4d,
  file.path(DATA_ML_SCOPE, "villages-4-districts-masterfile.xlsx"),
  overwrite = TRUE
)

write.xlsx(
  villages_ij,
  file.path(DATA_ML_SCOPE, "villages-180-coded.xlsx"),
  overwrite = TRUE
)

write.csv(
  villages_scope,
  file.path(DATA_ML_PRELOAD, "scope_villages.csv")
)

# Inspect only (not saved)
villages_dropped  # the 13 dropped villages
treatment_dist    # arm retention, baseline to midline
name_flag         # villages with two names
