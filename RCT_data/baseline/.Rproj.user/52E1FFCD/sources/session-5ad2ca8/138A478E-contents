##############
#Author: Xiaoming Zhang
#Date: 11.19.2024
#Purpose: Randomization primary construction
#############

pacman::p_load(knitr, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr)

getwd()

#Dropbox----


if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}


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


#read files----


household_csv <- read.csv("C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/questionnaires/cto attachments/household_head.csv")
household_join <- read_xlsx(path = file.path(scope_path, "household_join_1111.xlsx"))
household_head <- read_xlsx(path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/data/household_head_clean.xlsx")

scope <-read_xlsx(path = file.path(scope_path, "scope_193_0807.xlsx"))
scope_csv <- read.csv("C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/questionnaires/REP Backcheck survey/cto attachments/scope_villages.csv")

household_select <- read_xlsx(path = file.path(scope_path, "household_select_0807.xlsx"))
household_backup <- read_xlsx(path = file.path(scope_path, "household_backup_0807.xlsx"))  




#Select replacement----
household_scope <- household_head %>% 
  filter(villageid_key %in% scope$village_id) 

household_replacement <- household_scope %>% 
  anti_join(household_join, by = c("household_id")) 
  


set.seed(1118)  # Ensure reproducibility

# Add a random order to the replacement households
household_replacement_rand <- household_replacement %>%
  group_by(villageid_key) %>%
  mutate(random_order = sample(row_number())) %>%
  ungroup()


# Add a column to distinguish backup households
household_backup <- household_backup %>%
  mutate(source = "backup")

# Add a column to the replacement households
household_replacement_rand <- household_replacement_rand %>%
  mutate(source = "replacement")

# Combine backup and randomized replacement households
household_combined <- bind_rows(household_backup, household_replacement_rand) %>%
  group_by(villageid_key) %>%
  arrange(villageid_key, source, random_order) %>%
  ungroup()

# Remove the source column if not needed
household_combined <- household_combined %>%
  select(-source)



write.csv(household_combined, file.path(scope_path, "household_replacement.csv"))

write_xlsx(household_combined, path = file.path(scope_path, "household_replacement.xlsx"))

#Read back up and household head


nrow(household_select) + nrow(household_combined) == nrow(household_scope)

#Survey CTO----

household_all <- household_select %>% 
  mutate(source = "all")

household_combined <- household_combined %>% 
  mutate(source = "other")

household_head_all <- bind_rows(household_all,household_combined) %>%
  group_by(villageid_key) %>%
  arrange(villageid_key, source, random_order) %>%
  ungroup() %>% 
  select(-source, -random_order)

write.csv(household_head_all,  file.path(scope_path, "household_head.csv"))



##For survey firm----

unique_lots <- unique(scope$lot)



for (lot in unique_lots) {
  village <- scope %>% 
    filter(lot == !!lot) %>% 
    select(village_id, name, cell, sector, district, province)
  
  village_join <- village %>% 
    select(village_id)
  
  households  <- household_select %>% 
    semi_join(village_join, by = c("villageid_key" = "village_id")) %>% 
    select(villageid_key, village, cell, sector, district, household_id, first_name, last_name, gender, nid)
  

  replacement <- household_combined %>% 
    semi_join(village_join, by = c("villageid_key" = "village_id")) %>% 
    select(villageid_key, village, cell, sector, district, household_id, first_name, last_name, gender, nid)
  
  list_to_write <- list("village list" = village, "household list" = households,"replacement_household" = replacement)
  
  file_name <- paste0("Lot_", lot, ".xlsx")
  
  write_xlsx(list_to_write, path = file.path(scope_path, "Survey firm", file_name))
}




#Check----


filter <- household_replacement_rand %>% 
  filter(household_id %in% household_select$household_id)

household_check <- bind_rows(household_select, household_backup)

filter <- household_join %>% 
  filter(household_id %in% household_check$household_id)

karongi_household <- read_xlsx(path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/Updated scope villages& households/Survey firm/Lot_Karongi.xlsx", sheet = "household list")
karongi_replacement <- read_xlsx(path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/Updated scope villages& households/Survey firm/Lot_Karongi.xlsx", sheet = "replacement_household")

filter <- karongi_replacement %>% 
  filter(household_id %in% karongi_household$household_id)



rutsiro_household <- read_xlsx(path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/Updated scope villages& households/Survey firm/Lot_Rutsiro.xlsx", sheet = "household list")
rutsiro_replacement <- read_xlsx(path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/Updated scope villages& households/Survey firm/Lot_Rutsiro.xlsx", sheet = "replacement_household")

filter <- rutsiro_replacement %>% 
  filter(household_id %in% rutsiro_household$household_id)




rulindo_household <- read_xlsx(path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/Updated scope villages& households/Survey firm/Lot_Rulindo.xlsx", sheet = "household list")
rulindo_replacement <- read_xlsx(path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/Updated scope villages& households/Survey firm/Lot_Rulindo.xlsx", sheet = "replacement_household")

filter <- rulindo_replacement %>% 
  filter(household_id %in% rulindo_household$household_id)


rusizi_1_household <- read_xlsx(path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/Updated scope villages& households/Survey firm/Lot_Rusizi-1.xlsx", sheet = "household list")
rusizi_1_replacement <- read_xlsx(path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/Updated scope villages& households/Survey firm/Lot_Rusizi-1.xlsx", sheet = "replacement_household")

filter <- rusizi_1_replacement %>% 
  filter(household_id %in% rusizi_1_household$household_id)


rusizi_2_household <- read_xlsx(path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/Updated scope villages& households/Survey firm/Lot_Rusizi-2.xlsx", sheet = "household list")
rusizi_2_replacement <- read_xlsx(path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/Updated scope villages& households/Survey firm/Lot_Rusizi-2.xlsx", sheet = "replacement_household")

filter <- rusizi_2_replacement %>% 
  filter(household_id %in% rusizi_2_household$household_id)
