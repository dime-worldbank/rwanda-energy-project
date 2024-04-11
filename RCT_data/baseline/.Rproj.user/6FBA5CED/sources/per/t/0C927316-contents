##############
#Author: Xiaoming Zhang
#Date: 4.11.2024
#Purpose: household head
#############


pacman::p_load(tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, stargazer, olsrr)

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

#Read files------

four_district_2404 <- read_xlsx( path = file.path(data_path, "four_district_2404.xlsx"))

scope_0408 <- read_xlsx( path = file.path(data_path, "Scope villages_040824.xlsx"))
scope_info <- read_xlsx(path = file.path(data_path, "Scope Villages.xlsx"))
karongi_head <- read_xlsx(path = file.path (data_path, "hh_head", "KARONGI HH Heads.xlsx"))
rulindo_head <- read_xlsx(path = file.path (data_path, "hh_head", "Copie de Rulindo_Heads_of_HHs.xlsx"))
rutsiro_head <- read_xlsx(path = file.path (data_path, "hh_head", "RUTSIRO HH Heads.xlsx"))
rusizi_head <- read_xlsx(path = file.path (data_path, "hh_head", "RUSIZI HOUSEHOLDS DATA PROVIDED TO EDCL 2023.xlsx"))


rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))

village_list <- rwa_villages %>% 
  st_drop_geometry()


#clean karongi-----

karongi_head_join<- karongi_head %>%
  clean_names() %>% 
  filter(category == 1) %>% 
  select(code, status, category, first_name, last_name, gender, nid) %>% 
  mutate(
    first_name = str_to_title(first_name),
    last_name = str_to_title(last_name),
    first_name = ifelse(first_name %in% c("X", "Xxx", "."), NA, first_name),
    last_name = ifelse(last_name %in% c("X", "Xxx", "."), NA, last_name),
    village_id = substr(code, 1, 8)
  )

#clean rulindo----
rulindo_head_join<- rulindo_head %>%
  clean_names() %>% 
  filter(category == 1) %>% 
  select(code, status, category, first_name, last_name, gender, nid) %>% 
  mutate(
    first_name = str_to_title(first_name),
    last_name = str_to_title(last_name),
    first_name = ifelse(first_name %in% c("X", "Xxx", "."), NA, first_name),
    last_name = ifelse(last_name %in% c("X", "Xxx", "."), NA, last_name),
    village_id = substr(code, 1, 8)
  )

#clean rutsiro----
rutsiro_head_join<- rutsiro_head %>%
  clean_names() %>% 
  rename(
    category = cat,
    code = code_hh,
    last_name = lastname,
    gender = sex,
    nid = id
         ) %>% 
  filter(category == 1) %>% 
  mutate(
    status = NA
  ) %>% 
  select(code, status, category, first_name, last_name, gender, nid) %>% 
  mutate(
    first_name = str_to_title(first_name),
    last_name = str_to_title(last_name),
    first_name = ifelse(first_name %in% c("X", "Xxx", "."), NA, first_name),
    last_name = ifelse(last_name %in% c("X", "Xxx", "."), NA, last_name),
    village_id = substr(code, 1, 8)
  )


#clean rusizi----
rusizi_head_join<- rusizi_head %>%
  clean_names() %>%  
  mutate(
    status = NA
  ) %>% 
  filter(category == 1) %>% 
  select(code, status, category, first_name, last_name, gender, nid) %>% 
  mutate(
    first_name = str_to_title(first_name),
    last_name = str_to_title(last_name),
    first_name = ifelse(first_name %in% c("X", "Xxx", "."), NA, first_name),
    last_name = ifelse(last_name %in% c("X", "Xxx", "."), NA, last_name),
    village_id = substr(code, 1, 8)
  )

#join all four----

head_join <- rbind(karongi_head_join, rulindo_head_join, rutsiro_head_join, rusizi_head_join)

village_list_join <- village_list %>% 
  rename(
    provinceid_key = Prov_ID,
    province_key = Province, 
    districtid_key = Distr_ID,
    district_key = District,
    sectorid_key = Sector_ID,
    sector_key = Sector,
    cellid_key = Cell_ID,
    cell_key = Cell,
    villageid_key = Village_ID,
    village_key = Name
  ) %>% 
  select(provinceid_key, province_key, districtid_key, district_key, sectorid_key, sector_key, cellid_key, cell_key, villageid_key, village_key) %>% 
  filter(district_key %in% c("Karongi", "Rulindo", "Rutsiro", "Rusizi"))


household_head <- left_join(village_list_join, head_join, by = c("villageid_key" = "village_id"))

household_head_check <- household_head %>% 
  group_by(villageid_key) %>% 
  summarise( n = n())


four_district_check <- left_join(four_district_2404, household_head_check, by = c("village_id" = "villageid_key"))


filter <- four_district_check %>% 
  filter(ubudehe_1 > n & status == "newly" & scope_2403 == 1) %>% 
  rename( hh_head_name = n) %>% 
  select(village_id, name, cell, sector, district, province, ubudehe_1, hh_head_name)

write_xlsx(filter, path = file.path(output_path, "household_head_mismatch.xlsx"))

#scope----

four_district_scope <- four_district_2404 %>% 
  filter(status == "newly" & scope_2403 == 1)

household_head_scope <- household_head %>% 
  filter(villageid_key %in% four_district_scope$village_id)

household_number <- household_head_scope %>% 
  group_by(villageid_key) %>% 
  summarize(
    n = n()
  ) 

household_head_scope_check <- household_head_scope %>% 
  group_by(villageid_key) %>% 
  summarize(
    n = n()
  ) 

household_head_scope.1 <- household_head_scope %>%
  group_by(villageid_key) %>% 
  mutate(
    hh_index = paste0(villageid_key, "_", row_number()),
    status = ifelse(is.na(status), "Regular", status)
    ) %>% 
  filter(status == "Regular")

write_xlsx(household_head_scope.1, path = file.path(data_path, "household_head.xlsx"))

write_xlsx(household_number, path = file.path(data_path, "household_number.xlsx"))

pull_data(
  'household_head', 
  'first_name', 
  'hh_index', 
  concat(
    ${village_id} + '_' +index()
    )
  )






