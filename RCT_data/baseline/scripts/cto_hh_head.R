##############
#Author: Xiaoming Zhang
#Date 6.20.2024 
#Purpose: hh_head and backcheck
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

four_district_2405 <- read_xlsx( path = file.path(data_path, "four_district_2405.xlsx"))

scope_0408 <- read_xlsx( path = file.path(data_path, "Scope villages_040824.xlsx"))
scope_info <- read_xlsx(path = file.path(data_path, "Scope Villages.xlsx"))
karongi_head <- read_xlsx(path = file.path (data_path, "hh_head", "KARONGI HH Heads.xlsx"))
rulindo_head <- read_xlsx(path = file.path (data_path, "hh_head", "Copie de Rulindo_Heads_of_HHs.xlsx"))
rutsiro_head <- read_xlsx(path = file.path (data_path, "hh_head", "RUTSIRO HH_2024.xlsx"))
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
sum(is.na(karongi_head$Code))
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

sum(is.na(rutsiro_head$Code))

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

sum(is.na(rusizi_head$Code))
#join all four----

head_join <- rbind(karongi_head_join, rulindo_head_join, rutsiro_head_join, rusizi_head_join)

head_join <- head_join %>% 
  mutate(
    status = ifelse(is.na(status), "Regular", status)
  ) %>% 
  filter(status == "Regular") %>% 
  distinct(code, .keep_all = TRUE)


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
  summarise(n = n(),
            district_key = district_key,
            sector_key = sector_key,
            cell_key = cell_key,
            village_key = village_key) %>% 
  filter( n >= 2)

household_head_check <- household_head %>% 
  group_by(villageid_key) %>% 
  summarise( n = n())

View(household_head_check)

four_district_check <- left_join(four_district_2405, household_head_check, by = c("village_id" = "villageid_key"))

four_district_check <- four_district_check %>% 
  rename(
    hh_head = n
  )

View(four_district_check)

write_xlsx(four_district_check, path = file.path(data_path, "four_district_2405.xlsx"))


filter <- four_district_check %>% 
  filter( ubudehe_1 > hh_head & status == "newly" & scope_2403 == 1) %>% 
  select(village_id, name, cell, sector, district, province, ubudehe_1, hh_head)

filter <- filter  %>% 
  filter(hh_head < 20) %>% 
  mutate(ifelse(hh_head == 1, NA, hh_head))

write_xlsx(filter, path = file.path(output_path, "household_head_number_mismatch.xlsx"))

#scope----

four_district_scope <- four_district_check %>% 
  filter(status == "newly" & scope_2403 == 1)

household_head_scope <- household_head %>% 
  filter(villageid_key %in% four_district_scope$village_id)


n_distinct(household_head_scope$village_id)

household_number <- household_head_scope %>% 
  group_by(villageid_key) %>% 
  summarize(
    n = n()
  ) 
 

household_head_scope.1 <- household_head_scope %>%
  group_by(villageid_key) %>% 
  mutate(
    hh_index = paste0(villageid_key, "_", row_number())
  ) %>% ungroup()



write_xlsx(household_head_scope.1, path = file.path(data_path, "household_head.xlsx"))

write_xlsx(household_number, path = file.path(data_path, "household_number.xlsx"))

surveycto_scope <- household_head_scope.1 %>% 
  distinct(villageid_key, .keep_all = TRUE) %>% 
  select(ends_with("key")) 

write.csv(surveycto_scope, file = file.path(data_path, "scope_villages.csv"))




#Left_join hh head with scope village----


scope_villages <- read.csv(file = file.path(data_path, "scope_villages.csv"))

scope_villages <- scope_villages %>% 
  mutate( villageid_key = as.character(villageid_key))

household_number <-  read_xlsx(path = file.path(data_path, "household_number.xlsx"))

admin_raw <- left_join(scope_villages, household_number, by = c("villageid_key"))

admin_raw <- admin_raw %>% 
  rename(
    num_to_survey = n
  ) %>% 
  mutate(
    sector_key = str_to_title(sector_key)
  ) %>% 
  select(ends_with("key"), num_to_survey)

hfc_data_path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/HFC"
)

  
write.csv(admin_raw, file = file.path(hfc_data_path, "admin_raw.csv"))


#Randomly select for the backcheck----



backcheck <- block_ra(
  blocks = household_head_scope.1$districtid_key,
  num_arms = 2,
  prob_each = c(0.1, 0.9)
)

household_backcheck <- household_head_scope.1  %>% 
  filter()

household_backcheck$treatment <- backcheck

