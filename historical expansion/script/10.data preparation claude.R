#######################################
# Purpose: Data Preparation for Nightlight DID Analysis
# Author: XIAOMING ZHANG
# Date: March 11, 2026
#######################################

# Libraries ----
pacman::p_load(
  tidyverse,
  sf,
  readxl,
  writexl,
  dplyr
)
conflicted::conflict_prefer("select", "dplyr")
# Paths ----
dropbox <- "C:/Users/wb614406/Dropbox"

data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Historical Expansion/data"
)

historical_data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Historical data"
)

output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Historical Expansion/outputs"
)

# 1. Data read ----

expansion_join_organize <- read_xlsx(
  file.path(
    output_path,
    "expansion_join.xlsx"
  )
)


ntl <- read_xlsx(path = file.path(path = file.path(data_path, "Nightlight", "data", "LRCC-DVNL data", "village_ntl(2010-2022).xlsx")))
 
# 2. Data construction ----

# Select only necessary columns from expansion_join_organize
expansion_join_clean <- expansion_join_organize |>
   mutate(
    status = case_when(
      electrified_year == 9999 ~ "never_elec",
      electrified_year <= 2011 ~ "always_elec",
      electrified_year >= 2012 & electrified_year <= 2014 ~ "elec12_14",
      electrified_year >= 2015 & electrified_year <= 2017 ~ "elec15_17",
      electrified_year >= 2018 & electrified_year <= 2020 ~ "elec18_20",
      TRUE ~ "never_elec"
    )
  ) %>%
  dplyr::select(
    # Identifiers
    village_id,
    sector_id,
    cell_id,
    District,
    
    # Treatment status
    electrified_year,
    status,
    
    # Infrastructure variables for FE interactions
    cell_office,
    health_center,
    primary_school,
    secondary_school,
    sector_district_office,
    industry,
    market,
    imidugudu
  )


expansion_claude <- expansion_join_clean |>
  dplyr::select(village_id, sector_id, cell_id, District, status, electrified_year) |> 
  dplyr::filter(status %in% c( "elec15_17", "never_elec")) |>
  mutate(
    elec15_17 = ifelse(status == "elec15_17", 1, 0)
  )

write_xlsx(expansion_claude, path = file.path(output_path, "expansion_claude.xlsx"))
# Select village_id and all NTL year columns from ntl
ntl_clean <- ntl |> 
  dplyr::select(village_id, starts_with("ntl_"))

# Join them together
expansion_ntl_clean <- expansion_join_clean |>
  left_join(ntl_clean, by = "village_id")

# View result
View(expansion_ntl_clean)

write_xlsx(expansion_ntl_clean, path = file.path(output_path, "expansion_ntl_claude.xlsx"))

# 4. Export data ----



