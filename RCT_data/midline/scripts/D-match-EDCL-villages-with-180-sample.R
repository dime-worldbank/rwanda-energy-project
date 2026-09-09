#Author: Yeji Kim
#Date: 2026-09-09
#Purpose: Per-district filtering of EDCL household lists with village lists

#Key Functions: 
#*_reviewed_coded: attach master admin codes to the reviewed villages.
#*_EDCL_villages_list: stack coded-reviewed + grid-matched → the full EDCL village list.
#*_EDCL_sample_match: inner_join against the 180 sample → keep only EDCL villages inside the midline sample.
#*_EDCL_sample_households: left_join the harmonized households onto those villages → EDCL customers in the sample villages.

# Clean environment (Optional)
rm(list = ls())

# libraries
library(pacman)
pacman::p_load(readxl, writexl, here, tidyverse)
source(here("PATHS.R"))

#---------------------------
# Import data 
#---------------------------
districts = c("Karongi", "Rulindo", "Rusizi", "Rutsiro")
#district_name = c("KARONGI", "RULINDO", "RUSIZI", "RUTSIRO")
variables = c("DISTRICT", "SECTOR", "CELL", "VILLAGE")

# Complete Villages List 
villages_list_fd_raw <- read_excel(
  file.path(DATA_ML_SCOPE, "B.Rwanda-4-Districts-Villages-Masterfile.xlsx"))

# Sample Village Data
villages_180_full = read_excel(file.path(DATA_ML_SCOPE, "B.villages-180-coded.xlsx"))

# Clean the Master list for easy merging
villages_list_fd <- villages_list_fd_raw |>
  rename(DISTRICT = District, SECTOR = Sector,
         CELL = Cell, VILLAGE = Village) |>
  mutate(across(all_of(variables), str_to_title))

# Output from C-villages-fuzzy-matching
# Harmonized household-level observations
for (d in districts) {
  assign(paste0(d, "_harmonized"),
         read_excel(file.path(DATA_ML_ELIGIBILITY, "intermediate",
                              "C-grid-customer-list-harmonized.xlsx"), sheet = d))
}

# Matched unique villages (EDCL ∩ master)
for (d in districts) {
  obj <- read_excel(file.path(DATA_ML_ELIGIBILITY, "intermediate",
                              "C-EDCL-and-master-matched-villages.xlsx"), sheet = d) |>
    select(-any_of(c("Village_unclean", "Vill.name.flag")))
  assign(paste0(d, "_grid_matched"), obj)
}

# Manually reviewed unmatched villages (VILLAGE_CLEAN filled in)
for (d in districts) {
  obj <- read_excel(file.path(DATA_ML_ELIGIBILITY, "intermediate",
                              "C-manual-village-fuzzy-matching-yeji-review.xlsx"), sheet = d) |>
    mutate(VILLAGE_CLEAN = as.character(VILLAGE_CLEAN)) |>
    select(DISTRICT, SECTOR, CELL, VILLAGE_CLEAN) |> #keep only the cleaned names 
    rename(VILLAGE = VILLAGE_CLEAN)
  assign(paste0(d, "_reviewed"), obj)
}

#--------------------------------------------------------
# Per-district cleaned EDCL village lists and Filtering 
#--------------------------------------------------------

# --- Karongi
# Assign Code to Administrative level of reviewed using the master village lists (villages_list_fd_raw)
Karongi_reviewed_coded <- Karongi_reviewed |>
  left_join(villages_list_fd, by = variables) |>
  select(-any_of(c("Village_unclean", "Vill.name.flag")))

# Create a EDCL villages list that matched with harmonized master villages list 
Karongi_EDCL_villages_list <- bind_rows(Karongi_reviewed_coded, Karongi_grid_matched)

# Keep EDCL villages that only belongs to the midline villages   
Karongi_EDCL_sample_match <- villages_180_full |>
  rename(DISTRICT = District,
         SECTOR = Sector,
         CELL = Cell,
         VILLAGE = Village) |>
  inner_join(Karongi_EDCL_villages_list, by = ) 

# Use that limited EDCL villages list to filter the harmonized household sample
Karongi_EDCL_sample_households <- Karongi_EDCL_sample_match |>
  left_join(Karongi_harmonized, by = variables)

# --- Rulindo
# Assign Code to Administrative level of reviewed using the master village lists (villages_list_fd_raw)
Rulindo_reviewed_coded <- Rulindo_reviewed |>
  left_join(villages_list_fd, by = variables) |>
  select(-any_of(c("Village_unclean", "Vill.name.flag")))

# Create a EDCL villages list that matched with harmonized master villages list 
Rulindo_EDCL_villages_list <- bind_rows(Rulindo_reviewed_coded, Rulindo_grid_matched)

# Keep EDCL villages that only belongs to the midline villages   
Rulindo_EDCL_sample_match <- villages_180_full |>
  rename(DISTRICT = District,
         SECTOR = Sector,
         CELL = Cell,
         VILLAGE = Village) |>
  inner_join(Rulindo_EDCL_villages_list, by = ) 

# Use that limited EDCL villages list to filter the harmonized household sample
Rulindo_EDCL_sample_households <- Rulindo_EDCL_sample_match |>
  left_join(Rulindo_harmonized, by = variables)

# Rusizi ----------
# Assign Code to Administrative level of reviewed using the master village lists (villages_list_fd_raw)
Rusizi_reviewed_coded <- Rusizi_reviewed |>
  left_join(villages_list_fd, by = variables) |>
  select(-any_of(c("Village_unclean", "Vill.name.flag")))

# Create a EDCL villages list that matched with harmonized master villages list 
Rusizi_EDCL_villages_list <- bind_rows(Rusizi_reviewed_coded, Rusizi_grid_matched)

# Keep EDCL villages that only belongs to the midline villages   
Rusizi_EDCL_sample_match <- villages_180_full |>
  rename(DISTRICT = District,
         SECTOR = Sector,
         CELL = Cell,
         VILLAGE = Village) |>
  inner_join(Rusizi_EDCL_villages_list, by = ) 

# Use that limited EDCL villages list to filter the harmonized household sample
Rusizi_EDCL_sample_households <- Rusizi_EDCL_sample_match |>
  left_join(Rusizi_harmonized, by = variables)

# Rutsiro ---------
# Assign Code to Administrative level of reviewed using the master village lists (villages_list_fd_raw)
Rutsiro_reviewed_coded <- Rutsiro_reviewed |>
  left_join(villages_list_fd, by = variables) |>
  select(-any_of(c("Village_unclean", "Vill.name.flag")))

# Create a EDCL villages list that matched with harmonized master villages list 
Rutsiro_EDCL_villages_list <- bind_rows(Rutsiro_reviewed_coded, Rutsiro_grid_matched)

# Keep EDCL villages that only belongs to the midline villages   
Rutsiro_EDCL_sample_match <- villages_180_full |>
  rename(DISTRICT = District,
         SECTOR = Sector,
         CELL = Cell,
         VILLAGE = Village) |>
  inner_join(Rutsiro_EDCL_villages_list, by = ) 

# Use that limited EDCL villages list to filter the harmonized household sample
Rutsiro_EDCL_sample_households <- Rutsiro_EDCL_sample_match |>
  left_join(Rutsiro_harmonized, by = variables)

#---------------
# Save Outputs
#---------------

# EDCL sample list from EDCL list with candidates from master vill list
EDCL_sample_hh <- setNames(
  lapply(districts, \(d) {
    get(paste0(d, "_EDCL_sample_households")) |>
    select(-any_of(c("Village_unclean", "Vill.name.flag")))  
  }),
  districts
)

writexl::write_xlsx(
  EDCL_sample_hh,
  path = file.path(DATA_ML_ELIGIBILITY, "final", "D-EDCL-sample-households-260909.xlsx")
)
