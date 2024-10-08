##############
#Author: Xiaoming Zhang
#Date: 8.28.2024
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
  "Rwanda Energy/EAQIP/datawork/RCT_data",
  "baseline/data/data/four_district_2402.xlsx"
)


data_path <- file.path(
  DROPBOX,
  "Rwanda Energy/EAQIP/datawork/RCT_data",
  "baseline/data/data"
)

scope_path <- file.path(
  DROPBOX,
  "Rwanda Energy/EAQIP/datawork/RCT_data",
  "baseline/data/Updated scope villages& households"
)

output_path <- file.path(
  DROPBOX,
  "Rwanda Energy/EAQIP/datawork/RCT_data",
  "baseline/outputs"
)



four_scope_newly <- read_xlsx(path = file.path(scope_path, "scope_193_0807.xlsx"))

##For NM----


household_select <- read_xlsx(path = file.path(scope_path, "household_select_0807.xlsx"))

set.seed(082801)
household_backcheck <- household_select %>%
  group_by(villageid_key) %>%
  nest() %>%
  mutate(sampled = map(data, ~sample_n(.x, size = 4))) %>%
  select(villageid_key, sampled) %>%
  unnest(cols = sampled)


household_backcheck_backup <-
  household_select %>% 
  filter(!household_id %in% household_backcheck$household_id)

set.seed(082802)
household_backcheck_backup <- household_backcheck_backup %>%
  group_by(villageid_key) %>%
  nest() %>%
  mutate(sampled = map(data, ~sample_n(.x, size = 1))) %>%
  select(villageid_key, sampled) %>%
  unnest(cols = sampled)


unique_lots <- unique(four_scope_newly$lot)  # Get unique lots



for (lot in unique_lots) {
  village <- four_scope_newly %>% 
    filter(lot == !!lot) %>% 
    select(village_id, name, cell, sector, district, province)
  
  village_join <- village %>% 
    select(village_id)
  
  households  <- household_backcheck %>% 
    semi_join(village_join, by = c("villageid_key" = "village_id")) %>% 
    select(villageid_key, village, cell, sector, district, household_id, first_name, last_name, gender, nid)
  
  back_up <- household_backcheck_backup %>% 
    semi_join(village_join, by = c("villageid_key" = "village_id")) %>% 
    select(villageid_key, village, cell, sector, district, household_id, first_name, last_name, gender, nid)
  
  list_to_write <- list("village list" = village, "household list" = households, "backup household" = back_up)
  
  file_name <- paste0("Lot_", lot, ".xlsx")
  
  write_xlsx(list_to_write, path = file.path(scope_path, "Backcheck", file_name ))
}




###For EDCL----
unique_lots <- unique(four_scope_newly$lot)  # Get unique lots


for (lot in unique_lots) {
  readyboard_village <- four_scope_newly %>% 
    filter(treat == "T1" |treat == "T3" ) %>% 
    filter(lot == !!lot) %>% # Use `!!` to evaluate `lot` variable
    select(village_id, name, cell, sector, district, province)
  
  readyboard_household <- household_head %>% 
    filter(villageid_key %in% readyboard_village$village_id) %>% 
    select(villageid_key, village, cell, sector, district, first_name, last_name, gender, nid )
  
  list <- list("village list" = readyboard_village, "household list" = readyboard_household)
  
  file_name <- paste0("Lot_", lot, ".xlsx")
  write_xlsx(list, path = file.path(scope_path, "EDCL", file_name))
}

readyboard <- four_scope_newly %>% 
  filter(treat == "T1" | treat == "T3") %>% 
  group_by(lot) %>% 
  summarise(vulnerable = sum(hh_head_06))

write_xlsx(readyboard, path = file.path(scope_path, "EDCL", "readyboard distribution per lot.xlsx"))

###For ready board subsidy----

for (lot in unique_lots) {
  offgrid_village <- four_scope_newly %>% 
    filter(treat == "T2" |treat == "T3") %>% 
    filter(lot == !!lot) %>% # Use `!!` to evaluate `lot` variable
    select(village_id, name, cell, sector, district, province)
  
  
  file_name <- paste0("Lot_", lot, ".xlsx")
  write_xlsx(offgrid_village, path = file.path(scope_path, "Eligibility Tool", file_name))
}


offgrid_village <- four_scope_newly %>% 
  filter(treat == "T2" |treat == "T3") %>% 
  select(village_id, name, cell, sector, district, province)


write_xlsx(offgrid_village, path = file.path(scope_path, "Eligibility Tool", "offgrid_subsidy.xlsx"))

###For survey firm----

unique_lots <- unique(four_scope_newly$lot)


for (lot in unique_lots) {
  village <- four_scope_newly %>% 
    filter(lot == !!lot) %>% 
    select(village_id, name, cell, sector, district, province)
  
  village_join <- village %>% 
    select(village_id)
  
  households  <- household_select %>% 
    semi_join(village_join, by = c("villageid_key" = "village_id")) %>% 
    select(villageid_key, village, cell, sector, district, household_id, first_name, last_name, gender, nid)
  
  back_up <- household_backup %>% 
    semi_join(village_join, by = c("villageid_key" = "village_id")) %>% 
    select(villageid_key, village, cell, sector, district, household_id, first_name, last_name, gender, nid)
  
  list_to_write <- list("village list" = village, "household list" = households, "backup household" = back_up)
  
  file_name <- paste0("Lot_", lot, ".xlsx")
  
  write_xlsx(list_to_write, path = file.path(scope_path, "Lattanzio", file_name))
}



#For surveyCTO----

villages <- read.csv(file = file.path(data_path, "vills.csv"))

villages <- villages %>% 
  mutate(villageid_key = as.character(villageid_key))

admin_raw <- left_join(four_scope_newly, villages, by = c("village_id" = "villageid_key"))

admin_raw <- admin_raw %>% 
  rename(
    num_to_survey = surveyed,
    villageid_key = village_id,
    treatment = treat
  ) %>% 
  mutate(
    sector_key = str_to_title(sector_key)
  ) %>% 
  select(ends_with("key"), num_to_survey, treatment)

hfc_data_path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/HFC"
)

write.csv(admin_raw, file = file.path(hfc_data_path, "data", "admin_raw.csv"))


