##############
#Author: Xiaoming Zhang
#Date: 3.27.2025
#Purpose: Households approached for EDCL
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

karongi_households <- read_xlsx(path = file.path(scope_path, "Survey Firm", "Lot_Karongi.xlsx"), sheet = "household list")
karongi_replacement <- read_xlsx(path = file.path(scope_path, "Survey Firm", "Lot_Karongi.xlsx"), sheet = "replacement_household")
rutsiro_households <- read_xlsx(path = file.path(scope_path, "Survey Firm", "Lot_Rutsiro.xlsx"), sheet = "household list")
rutsiro_replacement <- read_xlsx(path = file.path(scope_path, "Survey Firm", "Lot_Rutsiro.xlsx"), sheet = "replacement_household")
rulindo_households <- read_xlsx(path = file.path(scope_path, "Survey Firm", "Lot_Rulindo.xlsx"), sheet = "household list")
rulindo_replacement <- read_xlsx(path = file.path(scope_path, "Survey Firm", "Lot_Rulindo.xlsx"), sheet = "replacement_household")
rusizi1_households<- read_xlsx(path = file.path(scope_path, "Survey Firm", "Lot_Rusizi-1.xlsx"), sheet = "household list")
rusizi1_replacement <- read_xlsx(path = file.path(scope_path, "Survey Firm", "Lot_Rusizi-1.xlsx"), sheet = "replacement_household")
rusizi2_households<- read_xlsx(path = file.path(scope_path, "Survey Firm", "Lot_Rusizi-2.xlsx"), sheet = "household list")
rusizi2_replacement <- read_xlsx(path = file.path(scope_path, "Survey Firm", "Lot_Rusizi-2.xlsx"), sheet = "replacement_household")


karongi_household <- rbind(karongi_households, karongi_replacement)
karongi_household <- karongi_household %>% 
  arrange(villageid_key)

rutsiro_household <- rbind(rutsiro_households, rutsiro_replacement)
rutsiro_household <- rutsiro_household %>% 
  arrange(villageid_key)

rulindo_household <- rbind(rulindo_households, rulindo_replacement)
rulindo_household <- rulindo_household %>% 
  arrange(villageid_key)

rusizi1_household <- rbind(rusizi1_households, rusizi1_replacement)
rusizi1_household <- rusizi1_household %>% 
  arrange(villageid_key)

rusizi2_household <- rbind(rusizi2_households, rusizi2_replacement)
rusizi2_household <- rusizi2_household %>% 
  arrange(villageid_key)


households <- rbind(karongi_household, rutsiro_household, rulindo_household, rusizi1_household, rusizi2_household)

hfc_constr <- read_xlsx(path = file.path(scope_path, "hfc_constr.xlsx"))

n_distinct(hfc_constr$hh_id)

households <-households %>% 
  mutate(
    'Approached by Lattanzio' = ifelse(household_id %in% hfc_constr$hh_id, "Yes", "No")
  )

#Completed households----


hfc_complete <- hfc_constr %>% 
  filter(!is.na(A1_1))

households <- households %>% 
  mutate(
    `Completed by Lattanzio` = ifelse(household_id %in% hfc_complete$hh_id, "Yes", "No")
  )


#Read out 15kv rulindo----


rulindo_15kv <- read_xlsx(path = file.path(scope_path, "Villages falling in 15kV_EPC Rulindo.xlsx"))

# households_clean <- households %>% 
#  mutate(case_when(villageid_key %in% rulindo_15kv$Code_vill)
# 
# write_xlsx(households_clean, path = file.path(scope_path, "vulnerable households in sample villages.xlsx"))


#Filter finished villages----


village_check <- read_xlsx(path = file.path(scope_path, "HFC_REP_24 (1).xlsx"), sheet = "village_data")


village_finish <- village_check %>% 
  filter(attempt == hh_head_06 | complete >= 20)



households_clean <- households %>% 
  mutate(
    `In Fully Completed village` = ifelse(villageid_key %in% village_finish$villageid_key , "Yes", "No"),
    `Dropped from scope due to 15kv` = ifelse(villageid_key%in% rulindo_15kv$Code_vill, "Yes", "No")
  )

write_xlsx(households_clean, path = file.path(scope_path, "vulnerable households in sample villages.xlsx"))



#Village treatment and readyboard estimation-----

admin_raw <- read.csv(file.path(data_path, "admin_raw.csv"))


admin_raw <- admin_raw %>% 
  select(treatment, villageid_key) %>% 
  mutate(
    villageid_key = as.character(villageid_key)
  )


households_clean <- left_join(households_clean, admin_raw, by = c("villageid_key"))

households_clean_sum <- households_clean %>% 
  filter(`Dropped from scope due to 15kv` == "No") %>% 
  filter(treatment ==  "T1" | treatment == "T3") %>% 
  rename(`Attempted by Lattanzio` = `Approached by Lattanzio`) %>% 
  group_by(district) %>% 
  summarise(
    attempted = sum(`Attempted by Lattanzio` == "Yes"),
    completed = sum(`Completed by Lattanzio` == "Yes"),
    total_households = n()
  ) %>% 
  mutate(
    `completed/attempted` = completed/attempted,
    estimate_accept = `completed/attempted`*total_households
  )  


households_clean_sum <- households_clean_sum %>%
  left_join(
    data.frame(district = c("KARONGI", "RUTSIRO", "Rulindo", "Rusizi"),
               `EDCL proposed readyboard` = c("224", "221", "224", "1102"))
  )



#Checking Readyboard list-----


data_path_1 <- file.path(
  DROPBOX,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data"
)

karongi <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Readyboard by lot","Lot_Karongi.xlsx"), sheet = "household list")
rutsiro <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Readyboard by lot","Lot_Rutsiro.xlsx"), sheet = "household list")

karongi_epc<- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Ready Board Name List -Lot_Karongi-20250601.xlsx"), sheet = "household list")
rutsiro_epc<- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Ready Board Name List - Lot_Rutsiro-20250601.xlsx"), sheet = "household list")

























