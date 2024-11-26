##############
#Author: Xiaoming Zhang
#Date: 6.17.2024
#Purpose: Rulindo household head
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


#Read files 0617-----

four_district_2405 <- read_xlsx( path = file.path(data_path, "four_district_2405.xlsx"))

four_district_2405 <- four_district_2405 %>% 
  select(-hh_head_05)

scope_0408 <- read_xlsx( path = file.path(data_path, "Scope villages_040824.xlsx"))
scope_info <- read_xlsx(path = file.path(data_path, "Scope Villages.xlsx"))

rulindo_head <- read_xlsx(path = file.path (data_path, "hh_head", "RULINDO SRIS 17_04_2024 (3).xlsx"))
rulindo_head_old <- read_xlsx(path = file.path (data_path, "hh_head", "Copie de Rulindo_Heads_of_HHs.xlsx"))

mismatch <- read_xlsx(path = file.path(data_path, "household_head_number_mismatch.xlsx"))

rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))

village_list <- rwa_villages %>% 
  st_drop_geometry()

village_join <- village_list %>% 
  select(District, Sector, Cell, Name, Village_ID) %>% 
  clean_names() %>% 
  mutate(across(everything(), ~str_to_title(.))) %>% 
  rename(
    village = name
  )

mismatch <- mismatch %>% 
  filter(district %in% c("Rulindo") & hh_head_05 == 1)



#clean rulindo----
# The new rulindo_hh_head list
rulindo_head <- rulindo_head %>% 
  mutate(across(everything(), ~ str_to_title(.)))

rulindo_head <- left_join(rulindo_he.keep_all = TRUE) %>% 
  filter(village_id %in% mismatchad, village_join, by = c("district", "sector", "cell", "village"))


rulindo_head <- rulindo_head %>% 
  distinct(national_id, $village_id)

table(rulindo_head$village_id)
table(mismatch$village_id)


#Rulindo_head_old----

rulindo_head_old <- rulindo_head_old%>%
  clean_names() %>% 
  filter(category == 1) %>% 
  select(district, sector, cell, village, code, first_name, last_name, gender,  nid) %>% 
  mutate(
    first_name = str_to_title(first_name),
    last_name = str_to_title(last_name),
    first_name = ifelse(first_name %in% c("X", "Xxx", "."), NA, first_name),
    last_name = ifelse(last_name %in% c("X", "Xxx", "."), NA, last_name),
    village_id = substr(code, 1, 8)
  ) %>% 
  distinct(code, .keep_all = TRUE)

rulindo_head_old <- rulindo_head_old %>% 
  filter(village_id %in% mismatch$village_id)


write_xlsx(mismatch, path = file.path(data_path, "Rulindo_17.xlsx"))
