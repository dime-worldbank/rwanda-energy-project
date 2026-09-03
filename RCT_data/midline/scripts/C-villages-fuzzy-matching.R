#Author: Yeji Kim
#Date: 2026-09-03
#Purpose: Sample village matching with 

rm(list = ls())

# libraries
library(pacman)
pacman::p_load(readxl, here)
source(here("PATHS.R"))

#---------------------------
# Import data 
#---------------------------
# Complete Villages List 
villages_list_fd <- read.xlsx(
  file.path(DATA_ML, "B.Rwanda-4-Districts-Villages-Masterfile.xlsx"))

# Sample Village Data
villages_180_full = read_xlsx(file.path(DATA_ML, "B.villages-180-coded.xlsx"))
funny_names = read_xlsx(file.path(DATA_ML, "B.double-names-villages.xlsx"))

# Grid customers per Disticts 
Karongi_raw <- read_excel(
  file.path(DATA_ML, "SHS-eligibility-check", "260827-grid-customer-list.xlsx"), 
  sheet = "Karongi"
  )

Rulindo_raw <- read_excel(
  file.path(DATA_ML, "SHS-eligibility-check", "260827-grid-customer-list.xlsx"), 
  sheet = "Rulindo"
)

Rutsiro_raw <- read_excel(
  file.path(DATA_ML, "SHS-eligibility-check", "260827-grid-customer-list.xlsx"), 
  sheet = "Rutsiro"
)

Rusizi_raw <- read_excel(
  file.path(DATA_ML, "SHS-eligibility-check", "260827-grid-customer-list.xlsx"), 
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
# 1. Harmonize Villages Name Per District 
#---------------------------

# Apply clean_admin function
# Clean the DISTRICT, SECTOR, CELL, VILLAGE 
for (i in districts) {
  raw_obj <- get(paste0(i, "_raw"))
  result  <- harmonize_district(raw_obj, i)
  assign(paste0(i, "_harmonized"), result) 
}

# Note:
# This will drop 3 obs (Nyamagabe) from Rusizi_raw
# This will drop 3 obs (--) from Rutsiro_raw

#---------------------------
# 1-2. Pull Unique Villages from Customer List
#---------------------------

for (i in districts) {
  harmonized_obj <- get(paste0(i, "_harmonized"))
  result <- pull_villages(harmonized_obj, i)
  assign(paste0(i, "_h_villages"), result)
}

#---------------------------
# 2. Fuzzy Matching
#---------------------------

# OMG why is this not running????
# Start from HERE tomorrow 
village_matched = function(df, districts) {
  df |>
    inner_join(
    villages_list_fd |> 
      filter(District == districts) |>
    rename(DISTRICT = District,
           SECTOR = Sector,
           CELL = Cell,
           VILLAGE = Village), 
    by = variables)
}

for (i in districts) {
  harmonized_villages <- get(paste0(i, "_h_villages"))
  result <- village_matched(harmonized_villages, i)
  assign(paste0(i, "_matched"), result)
}


Karongi_grid_matched <- Karongi_h_villages |>
  inner_join(
    villages_list_fd |> filter(District == "Karongi") |>
      rename(DISTRICT = District, SECTOR = Sector,
             CELL = Cell, VILLAGE = Village),
    by = variables
  ) # 296/345 matches 

Rulindo_grid_matched <- Rulindo_h_villages |>
  inner_join(
    villages_list_fd |> filter(District == "Rulindo") |>
      rename(DISTRICT = District, SECTOR = Sector,
             CELL = Cell, VILLAGE = Village),
    by = variables
  ) # 296/345 matches 


# test with Karongi 
Karongi_grid_matched <- Karongi_h_villages |>
  inner_join(
    villages_list_fd |> filter(District == "Karongi") |>
      rename(DISTRICT = District, SECTOR = Sector,
             CELL = Cell, VILLAGE = Village),
    by = variables
  ) # 296/345 matches 


Karongi_grid_unmatched <- Karongi_h_villages |>
  anti_join(
    villages_list_fd |> filter(Distr_ID == "31") |>
      rename(DISTRICT = District, SECTOR = Sector,
             CELL = Cell, VILLAGE = Village),
    by = variables
  )
# extract the villages list by districts 
for (i in districts) {
  result <- pull_district_villages(villages_180_full, i)
  assign(paste0(i, "_m_villages"), result)
}
  
# Step 1. Sanity check with inner join
Karongi_ijoin = Karongi_h_villages |> 
  inner_join(Karongi_m_villages, 
             by= join_by("VILLAGE" == "village"))

Karongi_ajoin = Karongi_h_villages |>
  anti_join(Karongi_m_villages, 
            by= join_by("VILLAGE" == "village"))