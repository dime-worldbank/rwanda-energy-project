#Author: Yeji Kim
#Date: 2026-09-03
#Purpose: EDCL list villages name cleaning with master village list data 
#Key Functions

# Clean environment (Optional)
# rm(list = ls())

# libraries
library(pacman)
pacman::p_load(readxl, writexl, here, tidyverse)
source(here("PATHS.R"))

#---------------------------
# Import data 
#---------------------------

# Complete Villages List 
villages_list_fd_raw <- read_excel(
  file.path(DATA_ML_SCOPE, "B.Rwanda-4-Districts-Villages-Masterfile.xlsx"))

# Sample Village Data
villages_180_full = read_excel(file.path(DATA_ML_SCOPE, "B.villages-180-coded.xlsx"))
funny_names = read_excel(file.path(DATA_ML_SCOPE, "B.double-names-villages.xlsx"))

# Grid customers per Disticts 
Karongi_raw <- read_excel(
  file.path(DATA_ML_ELIGIBILITY, "raw", "260827-grid-customer-list.xlsx"), 
  sheet = "Karongi"
  )

Rulindo_raw <- read_excel(
  file.path(DATA_ML_ELIGIBILITY,  "raw", "260827-grid-customer-list.xlsx"), 
  sheet = "Rulindo"
)

Rutsiro_raw <- read_excel(
  file.path(DATA_ML_ELIGIBILITY,  "raw", "260827-grid-customer-list.xlsx"), 
  sheet = "Rutsiro"
)

Rusizi_raw <- read_excel(
  file.path(DATA_ML_ELIGIBILITY,  "raw", "260827-grid-customer-list.xlsx"), 
  sheet = "Rusizi"
)

#---------------------------
# Helper Function 
#---------------------------

districts = c("Karongi", "Rulindo", "Rusizi", "Rutsiro")
district_name = c("KARONGI", "RULINDO", "RUSIZI", "RUTSIRO")
variables = c("DISTRICT", "SECTOR", "CELL", "VILLAGE")

# Cleaning function 1
clean_admin <- function(df) {
  df |> 
    mutate(across(all_of(variables),
                  ~ str_squish(str_to_title(str_remove_all(.x, "[^[:alpha:] ]")
                                            # remove everything that isn't a letter or space
                  ))))
}

# Cleaning function 2 
harmonize_district <- function(df, districts) {
  df |>
    filter(DISTRICT == str_to_upper(districts)) |>
    clean_admin()
}

# Cleaning function 3 
pull_villages <- function(df, districts) {
  df |>
    select(all_of(variables)) |> distinct(VILLAGE, .keep_all = TRUE) 
}

#---------------------------
# 1. Harmonize Administrative Text
#---------------------------

# 1-1. Apply clean_admin function via harmonize_district
# Clean the DISTRICT, SECTOR, CELL, VILLAGE and create object by district
for (i in districts) {
  raw_obj <- get(paste0(i, "_raw"))
  result  <- harmonize_district(raw_obj, i)
  assign(paste0(i, "_harmonized"), result) 
}

# Note:
# This will drop 3 obs (Nyamagabe) from Rusizi_raw
# This will drop 3 obs (--) from Rutsiro_raw

# Clean the Master list for easy merging
villages_list_fd <- villages_list_fd_raw |>
  rename(DISTRICT = District, SECTOR = Sector,
         CELL = Cell, VILLAGE = Village) |>
  mutate(across(all_of(variables), str_to_title))


# 1-2. Pull UNIQUE Villages from EDCL Customer List

for (i in districts) {
  harmonized_obj <- get(paste0(i, "_harmonized"))
  result <- pull_villages(harmonized_obj, i)
  assign(paste0(i, "_h_villages"), result)
}

#---------------------------
# 2. First match using harmonized admin texts
#---------------------------

Karongi_grid_matched <- Karongi_h_villages |>
  inner_join(
    villages_list_fd, by = variables
  ) # 296/345 matches 

Rulindo_grid_matched <- Rulindo_h_villages |>
  inner_join(
    villages_list_fd, by = variables
  ) # 304/345 matches 

Rusizi_grid_matched <- Rusizi_h_villages |>
  inner_join(
    villages_list_fd, by = variables
  ) # 420/456 matches 

Rutsiro_grid_matched <- Rutsiro_h_villages |>
  inner_join(
    villages_list_fd, by = variables
  ) # 341/379 matches 

#---------------------------
# 3. Fuzzy Match per District
#---------------------------

# Each District follows the same steps 
#3-1. Find the EDCL customer villages whose names don't line up with the 
# official village list (master list), then note which cells those villages sit in.

#3-2. For each of those cells, gather every village the official list records there
# This creates the pool of names the unmatched ones were probably meant to be.

#3-3. Lay out those candidate villages side by side, one row per cell 

#3-4. Put each unmatched EDCL village next to the candidates from its cell,
# so a reviewer can eyeball the list and pick the right name manually

# ---- Karongi ----
#3-1. 
Karongi_grid_unmatched <- Karongi_h_villages |>
  anti_join(villages_list_fd, by = variables)

Karongi_unmatched <- Karongi_grid_unmatched |>
  select(DISTRICT, SECTOR, CELL) |>
  distinct()

#3-2.
Karongi_unmatched_villages <- Karongi_unmatched |>
  left_join(villages_list_fd, by = join_by(DISTRICT, SECTOR, CELL)) |>
  distinct() #just in case

#3-3. 
Karongi_cell_villages <- Karongi_unmatched_villages |>
  mutate(idx = row_number(), .by = c(DISTRICT, SECTOR, CELL)) |>
  pivot_wider(
    id_cols      = c(DISTRICT, SECTOR, CELL),
    values_from  = VILLAGE,
    names_from   = idx,
    names_prefix = "VILLAGE_OPTIONS_"
  )

#3-4. 
Karongi_unmatched_options <- Karongi_grid_unmatched |>
  left_join(Karongi_cell_villages, by = join_by(DISTRICT, SECTOR, CELL))

# ---- Rulindo ----
#3-1.
Rulindo_grid_unmatched <- Rulindo_h_villages |>
  anti_join(villages_list_fd, by = variables)

Rulindo_unmatched <- Rulindo_grid_unmatched |>
  select(DISTRICT, SECTOR, CELL) |>
  distinct()

#3-2.
Rulindo_unmatched_villages <- Rulindo_unmatched |>
  left_join(villages_list_fd, by = join_by(DISTRICT, SECTOR, CELL)) |>
  distinct()

#3-3.
Rulindo_cell_villages <- Rulindo_unmatched_villages |>
  mutate(idx = row_number(), .by = c(DISTRICT, SECTOR, CELL)) |>
  pivot_wider(
    id_cols      = c(DISTRICT, SECTOR, CELL),
    values_from  = VILLAGE,
    names_from   = idx,
    names_prefix = "VILLAGE_OPTIONS_"
  )

#3-4.
Rulindo_unmatched_options <- Rulindo_grid_unmatched |>
  left_join(Rulindo_cell_villages, by = join_by(DISTRICT, SECTOR, CELL))

# ---- Rusizi ----
Rusizi_grid_unmatched <- Rusizi_h_villages |>
  anti_join(villages_list_fd, by = variables)

Rusizi_unmatched <- Rusizi_grid_unmatched |>
  select(DISTRICT, SECTOR, CELL) |>
  distinct()

Rusizi_unmatched_villages <- Rusizi_unmatched |>
  left_join(villages_list_fd, by = join_by(DISTRICT, SECTOR, CELL)) |>
  distinct()

Rusizi_cell_villages <- Rusizi_unmatched_villages |>
  mutate(idx = row_number(), .by = c(DISTRICT, SECTOR, CELL)) |>
  pivot_wider(
    id_cols      = c(DISTRICT, SECTOR, CELL),
    values_from  = VILLAGE,
    names_from   = idx,
    names_prefix = "VILLAGE_OPTIONS_"
  )

Rusizi_unmatched_options <- Rusizi_grid_unmatched |>
  left_join(Rusizi_cell_villages, by = join_by(DISTRICT, SECTOR, CELL))

# ---- Rutsiro ----
Rutsiro_grid_unmatched <- Rutsiro_h_villages |>
  anti_join(villages_list_fd, by = variables)

Rutsiro_unmatched <- Rutsiro_grid_unmatched |>
  select(DISTRICT, SECTOR, CELL) |>
  distinct()

Rutsiro_unmatched_villages <- Rutsiro_unmatched |>
  left_join(villages_list_fd, by = join_by(DISTRICT, SECTOR, CELL)) |>
  distinct()

Rutsiro_cell_villages <- Rutsiro_unmatched_villages |>
  mutate(idx = row_number(), .by = c(DISTRICT, SECTOR, CELL)) |>
  pivot_wider(
    id_cols      = c(DISTRICT, SECTOR, CELL),
    values_from  = VILLAGE,
    names_from   = idx,
    names_prefix = "VILLAGE_OPTIONS_"
  )

Rutsiro_unmatched_options <- Rutsiro_grid_unmatched |>
  left_join(Rutsiro_cell_villages, by = join_by(DISTRICT, SECTOR, CELL))

#---------------------------
# Export Output 
#---------------------------

# Raw file to Harmonized Household Level Observations 
harmonized <- setNames(
  lapply(districts, \(d) {
    get(paste0(d, "_harmonized"))
  }),
  districts
)

writexl::write_xlsx(
  harmonized, 
  path = file.path(DATA_ML_ELIGIBILITY, "intermediate", "C-grid-customer-list-harmonized.xlsx")
)

# matched-and-unique-villages-list from EDCL customers
matched_villages <- setNames (
  lapply(districts, \(d) {
    get(paste0(d, "_grid_matched"))
  }),
  districts
)

writexl::write_xlsx(
  matched_villages,
  path = file.path(DATA_ML_ELIGIBILITY, "intermediate", "C-EDCL-and-master-matched-villages.xlsx")
)

# Unmatched villages from EDCL list with candidates from master vill list
unmatched_options <- setNames(
  lapply(districts, \(d) {
    get(paste0(d, "_unmatched_options")) |>
      mutate(VILLAGE_CLEAN = NA_character_) |>
      relocate(VILLAGE_CLEAN, .after = VILLAGE)
  }),
  districts
)

writexl::write_xlsx(
  unmatched_options,
  path = file.path(DATA_ML_ELIGIBILITY, "intermediate", "C-manual-village-fuzzy-matching-raw.xlsx")
)