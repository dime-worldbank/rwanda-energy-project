##############
#Author: Xiaoming Zhang
#Date: 10.8.2024
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


#Read_hh----

juru_hh <- read_xlsx(path = file.path(data_path, "JURU HH NEED EUCL.xlsx"))
bugesera_villages <- read_xlsx(path = file.path(data_path, "Bugesera_Impact Ass._Villages.xlsx"))
rwa_villages <- read.csv(file.path(data_path, "vills.csv"))


#village file----
bugesera_villages_join <- rwa_villages %>% 
  filter(district_key %in% "Bugesera") %>% 
  filter(sector_key %in% "Juru") %>% 
  filter(village_key %in% bugesera_villages$Village) %>% 
  filter(!cell_key %in% "Musovu")


write.csv(bugesera_villages_join, file.path(data_path, "pilot villages", "Bugesera villages.csv"))

#Household-----

juru_hh_clean <- juru_hh %>% 
  filter(Category == 1) %>% 
  mutate(
    village_id = substr(Code, 1, 8)
    )

set.seed(123)  # for reproducibility

juru_hh_group <- juru_hh_clean %>%
  group_by(village_id) %>%
  mutate(sample_type = case_when(
    row_number(Code) <= 10 ~ "Main",
    row_number(Code) > 10 & row_number(Code) <= 12 ~ "Backup",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(sample_type)) %>%
  arrange(village_id, sample_type)  %>% 
  rename(
    household_id = Code, 
    villageid_key = village_id
  ) %>% 
  mutate(
    household_id = as.character(household_id)
  )


juru_main <- juru_hh_group %>% 
  filter(sample_type == "Main") %>% 
  select(-sample_type)

juru_backup <- juru_hh_group %>% 
  filter(sample_type == "Backup") %>% 
  select(-sample_type)


juru_backcheck <- juru_main %>%
  group_by(villageid_key) %>%
  sample_n(2) %>%
  ungroup()

write_xlsx(juru_backcheck, path = file.path(data_path, "pilot villages", "Juru_bachcheck_households.xlsx"))


write_xlsx(
  list(Main = juru_main, Backup = juru_backup), 
  path = file.path(data_path, "pilot villages", "juru_households.xlsx")
)

write.csv(juru_hh_group, file.path(data_path, "pilot villages", "household_head.csv"))
