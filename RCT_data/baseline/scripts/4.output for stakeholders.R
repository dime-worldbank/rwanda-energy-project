##############
#Author: Xiaoming Zhang
#Date: 7.30.2024
#Purpose: Randomization primary construction
#############


pacman::p_load(knitr, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr)

getwd()

#Dropbox----


if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}


path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/data/data/four_district_2402.xlsx"
)


data_path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/data/data"
)

scope_path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/data/Updated scope villages& households"
)

output_path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/outputs"
)



four_scope_newly <- read_xlsx(path = file.path(scope_path, "scope_193_0730.xlsx"))



###For EDCL----
unique_lots <- unique(four_scope_newly$lot)  # Get unique lots


for (lot in unique_lots) {
  readyboard_village <- four_scope_newly %>% 
    filter(treat == 1 |treat == 3 ) %>% 
    filter(lot == !!lot) %>% # Use `!!` to evaluate `lot` variable
    select(village_id, name, cell, sector, district, province)
  
  readyboard_household <- household_head %>% 
    filter(villageid_key %in% readyboard_village$village_id) %>% 
    select(villageid_key, village, cell, sector, district, code, first_name, last_name, gender, nid )
  
  list <- list("village list" = readyboard_village, "household list" = readyboard_household)
  
  file_name <- paste0("Lot_", lot, ".xlsx")
  write_xlsx(list, path = file.path(scope_path, "EDCL", file_name))
}


###For ready board subsidy----

for (lot in unique_lots) {
  offgrid_village <- four_scope_newly %>% 
    filter(treat == 2 |treat == 3) %>% 
    filter(lot == !!lot) %>% # Use `!!` to evaluate `lot` variable
    select(village_id, name, cell, sector, district, province)
  
  
  file_name <- paste0("Lot_", lot, ".xlsx")
  write_xlsx(offgrid_village, path = file.path(scope_path, "Eligibility Tool", file_name))
}


offgrid_village <- four_scope_newly %>% 
  filter(treat == 2 |treat == 3) %>% 
  select(village_id, name, cell, sector, district, province)


write_xlsx(offgrid_village, path = file.path(scope_path, "Eligibility Tool", "offgrid_subsidy.xlsx"))

###For survey firm----'
household_select <- read_xlsx(file.path(scope_path, "scope_households_0729.xlsx"), sheet = "selected")
household_backup <- read_xlsx(file.path(scope_path, "scope_households_0729.xlsx"), sheet = "backup")



for (lot in unique_lots) {
  # Create village data frame
  village <- four_scope_newly %>% 
    filter(lot == !!lot) %>% 
    select(village_id, name, cell, sector, district, province)
  
  # Extract village_id as a vector for filtering
  village_ids <- village %>% pull(village_id)
  
  # Filter household_select using the extracted village_ids
  households <- household_select %>% 
    filter(villageid_key %in% village_ids) %>% 
    select(villageid_key, village, cell, sector, district, code, first_name, last_name, gender, nid)
  
  # Filter household_backup using the extracted village_ids
  back_up <- household_backup %>% 
    filter(villageid_key %in% village_ids) %>% 
    select(villageid_key, village, cell, sector, district, code, first_name, last_name, gender, nid)
  
  # Create a list with the data frames
  list <- list("village list" = village, "household list" = households, "backup household" = back_up)
  
  # Define the file name and write to Excel
  file_name <- paste0("Lot_", lot, ".xlsx")
  write_xlsx(list, path = file.path(scope_path, "Lattanzio", file_name))
}