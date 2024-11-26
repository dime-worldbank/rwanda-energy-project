##############
#Author: Xiaoming Zhang
#Date: 6.27.2024
#Purpose: household head
#############


pacman::p_load(tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, stargazer, olsrr, fuzzyjoin)

getwd()

#Dropbox path----


if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/data/data"
)

output_path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/outputs"
)


#Read files-----

four_district_2406 <- read_xlsx( path = file.path(data_path, "four_district_2406.xlsx"))

rusizi_head <- read_xlsx(path = file.path (data_path, "hh_head", "Rusizi data June 27,2024.xlsx"))
rusizi_head_old <- read_xlsx(path = file.path(data_path, "hh_head", "RUSIZI HOUSEHOLDS DATA PROVIDED TO EDCL 2023.xlsx"))


rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))

village_list <- rwa_villages %>% 
  st_drop_geometry()

village_join <- village_list %>% 
  select(District, Sector, Cell, Name, Village_ID) %>% 
  clean_names() %>% 
  rename(village = name) %>% 
  mutate(across(everything(), ~ str_to_title(.)))



#clean rusizi----
rusizi_head_join<- rusizi_head_old %>% 
  clean_names() %>% 
  filter(category == 1) %>% 
  select(district, sector, cell, village, code, first_name, last_name, gender, nid) %>% 
  mutate(
    first_name = str_to_title(first_name),
    last_name = str_to_title(last_name),
    first_name = ifelse(first_name %in% c("X", "Xxx", "."), NA, first_name),
    last_name = ifelse(last_name %in% c("X", "Xxx", "."), NA, last_name),
    village_id = substr(code, 1, 8)
  )

sum(is.na(rusizi_head_join$village_id))

filter_rusizi <- rusizi_head_join %>% 
  group_by(code) %>% 
  summarise(
    n = n()
  ) %>% 
  filter( n >2)

#clean rusizi.2-----
rusizi_head_join.2<- rusizi_head %>% 
  clean_names() %>% 
  filter(category == 1) %>% 
  mutate(
    last_name = str_split_fixed(household_name, " ", 2)[, 1],  # Extract the first part as last name
    first_name = str_split_fixed(household_name, " ", 2)[, 2]  # Extract the rest as first name
  ) %>%
  select(district, sector, cell, village, code, first_name, last_name, gender, nid) %>% 
  mutate(
    district = str_to_title(district),
    first_name = str_to_title(first_name),
    last_name = str_to_title(last_name),
    first_name = ifelse(first_name %in% c("X", "Xxx", "."), NA, first_name),
    last_name = ifelse(last_name %in% c("X", "Xxx", "."), NA, last_name),
    village_id = substr(code, 1, 8)
  )

filter_rusizi <- rusizi_head_join.2 %>% 
  group_by(code) %>% 
  summarise(
    n = n()
  ) %>% 
  filter( n >2)

rusizi_new <- rusizi_head_join.2 %>% 
  group_by(village_id) %>% 
  summarise(
    n_new = n()
  )

#merge with datafile----

rusizi_village <- four_district_2406 %>% 
  filter(district == "Rusizi")

rusizi_village <- left_join(rusizi_village, rusizi_new, by = c("village_id"))

filter <- rusizi_village %>% 
  filter(n_new != hh_head_06)












