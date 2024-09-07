
##########################
#Author: Xiaoming Zhang
#Date of last modification: 08202024
#purpose:NISR establishment census 2023
############################


#library----
# install.packages("plm")

pacman::p_load(fixest, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, plm, haven, stringr, modelsummary, kableExtra, stargazer, lfe)


#read file----

if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX, "Rwanda Energy/datawork/Historical data"
)


#2023

ec_2023 <- read_dta(file = file.path(data_path, "2023", "data-rwa-nisr-ec-2023v2.dta"))

View(ec_2023)

descriptives <- ec_2023 %>% 
  filter(q3 ==1) %>% 
  summarise(
    n = n(),
    total_employee = sum(Total_workers),
    male_manager = sum(),
    female_manager = sum()
  )
View(descriptives)


#Prepare dataset----
rwa_long <- read_xlsx( path = file.path(data_path, "nisr stata", "Attachment data", "rwa_long.xlsx"))
village_2011 <- read_xlsx(path = file.path(data_path, "Establishment census", "village_level_2011.xlsx"))
village_2014 <- read_xlsx(path = file.path(data_path, "Establishment census", "village_level_2014.xlsx"))
village_2017 <- read_xlsx(path = file.path(data_path, "Establishment census", "village_level_2017.xlsx"))
village_2020 <- read_xlsx(path = file.path(data_path, "Establishment census", "village_level_2020.xlsx"))

rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))
village_join <- rwa_villages %>% 
  st_drop_geometry()


key_2011 <-village_2011%>% 
  select(village_id, num_establishment,total_employee, male_manager, female_manager, employed_capital_1, employed_capital_2, employed_capital_3, employed_capital_4) %>% 
  mutate(
    year = 2011
  )

key_2014 <-village_2014%>% 
  select(village_id,num_establishment, total_employee, male_manager, female_manager, employed_capital_1, employed_capital_2, employed_capital_3, employed_capital_4) %>% 
  mutate(
    year = 2014
  )

key_2017 <-village_2017%>% 
  select(village_id,num_establishment, total_employee, male_manager, female_manager,employed_capital_1, employed_capital_2, employed_capital_3, employed_capital_4) %>% 
  mutate(
    year = 2017
  )

key_2020 <-village_2020%>% 
  select(village_id, num_establishment,total_employee,male_manager, female_manager, employed_capital_1, employed_capital_2, employed_capital_3, employed_capital_4) %>% 
  mutate(
    year = 2020
  )





#Join dataset----

key_all <- bind_rows(key_2011, key_2014, key_2017, key_2020) 

key_descriptives <- key_all %>% 
  group_by(year) %>% 
  summarise(
    male_manager = sum(male_manager, na.rm = TRUE),
    female_manager = sum(female_manager, na.rm = TRUE)
  )
