##############
#Author: Xiaoming Zhang
#Date: 11.7.2024
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



scope_villages <- read.csv( "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/questionnaires/cto attachments/scope_villages.csv")

enumerator_district <- read_xlsx(path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/questionnaires/enumerator_deploy.xlsx", sheet = "District")
enumerator_sector <- read_xlsx(path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/questionnaires/enumerator_deploy.xlsx", sheet = "Sector")

enumerator <- read.csv("C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/questionnaires/cto attachments/pilot_enumerators.csv")




district <- scope_villages %>% 
  select(districtid_key, district_key) 

district <- district %>% 
  distinct(district_key, .keep_all = TRUE)

enumerator_district <- enumerator_district %>% 
  clean_names() %>% 
  rename(
    district_key = district,
    enumerator_key = enumerators
  )

enumerator_join <- left_join(enumerator_district, district, by = c("district_key"))
enumerator_join <- left_join(enumerator_join, enumerator, by = c("enumerator_key"))

write_csv(enumerator_join, "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/questionnaires/enumerator_new.csv")

