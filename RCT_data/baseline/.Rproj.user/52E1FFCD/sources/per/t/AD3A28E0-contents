##############
#Author: Xiaoming Zhang
#Date: 5.7.2024
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


#Read files 0513-----

four_district_2404 <- read_xlsx( path = file.path(data_path, "four_district_2404.xlsx"))

scope_0408 <- read_xlsx( path = file.path(data_path, "Scope villages_040824.xlsx"))
scope_info <- read_xlsx(path = file.path(data_path, "Scope Villages.xlsx"))
karongi_head <- read_xlsx(path = file.path (data_path, "hh_head", "Karongi_hh_2024.xlsx"))
rulindo_head <- read_xlsx(path = file.path (data_path, "hh_head", "Copie de Rulindo_Heads_of_HHs.xlsx"))
rutsiro_head <- read_xlsx(path = file.path (data_path, "hh_head", "RUTSIRO HH_2024.xlsx"))
rusizi_head <- read_xlsx(path = file.path (data_path, "hh_head", "RUSIZI HOUSEHOLDS DATA PROVIDED TO EDCL 2023.xlsx"))


rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))

village_list <- rwa_villages %>% 
  st_drop_geometry()


#clean karongi-----
karongi_head_join <- karongi_head %>%
  clean_names() %>% 
  mutate(
    code = loda_code,
    status = "Regular",
    category = 1,
    gender = NA,
    last_name = sub("^(.*?)\\t.*$", "\\1", head_names),# Extract first name before the tab
    first_name = sub("^.*\\t(.*?)$", "\\1", head_names)    # Extract last name after the tab
  ) %>% 
  select(code, status, category, first_name, last_name, gender, nid) %>% 
  mutate(
    first_name = str_to_title(first_name),
    last_name = str_to_title(last_name),
    first_name = ifelse(first_name %in% c("X", "Xxx", "."), NA, first_name),
    last_name = ifelse(last_name %in% c("X", "Xxx", "."), NA, last_name),
    village_id = substr(code, 1, 8)
  )
sum(is.na(karongi_head$code))
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

sum(is.na(rulindo_head$Code))
#clean rutsiro----
rutsiro_head_join<- rutsiro_head %>%
  clean_names() %>% 
  filter(category == 1) %>% 
  mutate(
    status = "Regular"
  ) %>% 
  select(code, status, category, first_name, last_name, gender, nid) %>% 
  mutate(
    first_name = str_to_title(first_name),
    last_name = str_to_title(last_name),
    first_name = ifelse(first_name %in% c("X", "Xxx", "."), NA, first_name),
    last_name = ifelse(last_name %in% c("X", "Xxx", "."), NA, last_name),
    village_id = substr(code, 1, 8)
  )

sum(is.na(rutsiro_head$Code))

#clean rusizi----
rusizi_head_join<- rusizi_head %>%
  clean_names() %>%  
  mutate(
    status = "Regular"
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

sum(is.na(rusizi_head$Code))
#join all four----

head_join <- rbind(karongi_head_join, rulindo_head_join, rutsiro_head_join, rusizi_head_join)

head_join <- head_join %>% 
  mutate(
    status = ifelse(is.na(status), "Regular", status)
  ) %>% 
  filter(status == "Regular") %>% 
  distinct(code, .keep_all = TRUE)

nrow(head_join ) == n_distinct(head_join$code)

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

duplicate_check <- household_head %>% 
  group_by(code) %>% 
  summarise(n = n()) %>% 
  filter( n >= 2)

household_head_number<- household_head %>% 
  group_by(villageid_key) %>% 
  summarise( n = n())

View(household_head_number)

four_district_2405 <- left_join(four_district_2404, household_head_number, by = c("village_id" = "villageid_key"))

four_district_2405 <- four_district_2405 %>% 
  rename(
    hh_head_04 = hh_head,
    hh_head_05 = n
  )

write_xlsx(four_district_2405, path = file.path(data_path, "four_district_2405.xlsx"))


#Read files------


filter <- four_district_2405 %>% 
  filter( ubudehe_1 > hh_head_05 & status == "newly" & scope_2403 == 1 & hh_head_05 <20 ) %>% 
  select(village_id, name, cell, sector, district, province, ubudehe_1, hh_head_05)


write_xlsx(filter, path = file.path(output_path, "household_head_number_mismatch.xlsx"))



