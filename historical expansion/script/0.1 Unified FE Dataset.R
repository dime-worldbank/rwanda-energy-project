#######################################
#Purpose: Create unified long-format dataset for FE analyses
#Author: XIAOMING ZHANG
#Date: January 2026
#Description: Single source for all three electrification windows (12-14, 15-17, post-13)
######################################################

pacman::p_load(knitr, lfe, fixest, modelsummary, stargazer, tidyverse, dplyr, 
               here, sf, haven, ggplot2, readxl, writexl, janitor, randomizr, 
               RCT, purrr, RODBC, DBI)

getwd()

dropbox <- 'C:/Users/wb614406/Dropbox'

data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Historical Expansion/data"
)

output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Historical Expansion/outputs"
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# STEP 1: Create electrification indicator variables for each window----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Using expansion_join from your main dataset (0. Main dataset.R)
# This assumes expansion_join is already loaded in your environment

fe_dataset <- expansion_join |>
  mutate(
    # 2012-2014 window
    elec12_14 = ifelse(electrified_year %in% c("2012", "2013", "2014"), 1, 0),
    
    # 2015-2017 window
    elec15_17 = ifelse(electrified_year %in% c("2015", "2016", "2017"), 1, 0),

    # Never electrified
    never_electrified = ifelse(electrified_year == 9999, 1, 0),
    
    # Post-2013 window (for EARP)
    EARP = ifelse(earp_lv == 1 | earp_mv == 1, 1, 0)
  ) |>
  # Apply common sample restrictions
  anti_join(earp_existing_mv, by = c("village_id" = "Village_ID")) |>
  filter(!District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu")) |>
  # Create derived variables
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer > 0, 1, 0),
    consumer = residential_consumer + non_residential_consumer
  )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# STEP 3: Load ISIC economic census data (2011, 2014, 2017, 2020)----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ec_2011 <- read_xlsx(path = file.path(data_path, "2011", "group_long_2011(isic).xlsx")) |>
  mutate(village_id = as.character(village_id)) |>
  rename(num_establishment = n) |>
  group_by(village_id, isic_level1) |>
  summarise(
    num_establishment = sum(num_establishment, na.rm = TRUE),
    total_employee = sum(total_employee, na.rm = TRUE),
    .groups = "drop"
  ) |>
  select(village_id, num_establishment, total_employee, isic_level1) |>
  mutate(year = 2011)

ec_2014 <- read_xlsx(path = file.path(data_path, "2014", "group_long_2014(isic).xlsx")) |>
  mutate(village_id = as.character(village_id)) |>
  rename(num_establishment = n, isic_level1 = isic_level1_main) |>
  group_by(village_id, isic_level1) |>
  summarise(
    num_establishment = sum(num_establishment, na.rm = TRUE),
    total_employee = sum(total_employee, na.rm = TRUE),
    .groups = "drop"
  ) |>
  select(village_id, num_establishment, total_employee, isic_level1) |>
  mutate(year = 2014)

ec_2017 <- read_xlsx(path = file.path(data_path, "2017", "group_long_2017(isic).xlsx")) |>
  mutate(village_id = as.character(village_id)) |>
  rename(num_establishment = n, isic_level1 = isic_1_digit) |>
  group_by(village_id, isic_level1) |>
  summarise(
    num_establishment = sum(num_establishment, na.rm = TRUE),
    total_employee = sum(total_employee, na.rm = TRUE),
    .groups = "drop"
  ) |>
  select(village_id, num_establishment, total_employee, isic_level1) |>
  mutate(year = 2017)

ec_2020 <- read_xlsx(path = file.path(data_path, "2020", "group_long_2020(isic).xlsx")) |>
  mutate(village_id = as.character(village_id)) |>
  rename(num_establishment = n) |>
  group_by(village_id, isic_level1) |>
  summarise(
    num_establishment = sum(num_establishment, na.rm = TRUE),
    total_employee = sum(total_employee, na.rm = TRUE),
    .groups = "drop"
  ) |>
  select(village_id, num_establishment, total_employee, isic_level1) |>
  mutate(year = 2020)

ec_all <- bind_rows(ec_2011, ec_2014, ec_2017, ec_2020)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# STEP 4: Create long format dataset with all ISIC combinations----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Select key variables from fe_dataset for joining
fe_dataset_select <- fe_dataset |>
  select(
    village_id, elec12_14, elec15_17, EARP,
    cell_id, sector_id, district_id,
    cell_office, health_center, primary_school, secondary_school,
    sector_district_office, industry, market, imidugudu,
    residential_consumer, non_residential_consumer,
    log1_residential_consumer, log1_non_residential_consumer
  )

# Create complete ISIC-year combinations and join with electrification variables
fe_dataset_long <- fe_dataset_select |>
  # Get all villages from fe_dataset
  distinct(village_id) |>
  # Expand to all year-isic combinations
  crossing(
    year = c(2011, 2014, 2017, 2020),
    isic_level1 = unique(ec_all$isic_level1)
  ) |>
  # Now join with ec_all to get establishment counts
  left_join(ec_all, by = c("village_id", "year", "isic_level1")) |>
  # Fill missing combinations with 0
  mutate(
    num_establishment = coalesce(num_establishment, 0),
    total_employee = coalesce(total_employee, 0)
  ) |>
  # Join with fe_dataset variables
  left_join(fe_dataset_select, by = "village_id") |>
  mutate(
    isic = as.character(isic_level1),
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    imidugudu_year = paste0(imidugudu, "_", year),
    log1_num_establishment = log(num_establishment + 1),
    log1_total_employee = log(total_employee + 1)
  ) |>
  select(-isic_level1) |>
  mutate(
    private_sector = ifelse(isic %in% c("3", "7", "9", "19"), 1, 0)
  )

saveRDS(fe_dataset_long, file.path(output_path, "fe_dataset_long.rds"))
