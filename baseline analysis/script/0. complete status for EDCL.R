##############
#Author: Xiaoming Zhang
#Date:8.5
#Purpose: Completion status of survey for EDCL
#############


pacman::p_load(knitr, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr)
library(googlesheets4)
getwd()


# Import Data ----
dropbox <- 'C:/Users/wb614406/Dropbox'

hfc_data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/HFC/data"
)

hfc_output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/HFC/output"
)

output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/baseline analysis/output"
)


screening_data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Screening/data"
)

data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/baseline analysis/data"
)

data_path_2 <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/Updated scope villages& households"
)

screening_data_path <- file.path(dropbox,   "Rwanda Energy/EAQIP/datawork/Screening/data")

screening_raw <- read.csv(file.path(screening_data_path, "REP_screening_WIDE.csv"))

enumerator <- read.csv(file.path(screening_data_path, "pilot_enumerators.csv"))

screening_enumerator <- left_join(screening_raw, enumerator, by = c("enumerator" = "enumeratorid_key")) %>% 
  select(hh_id, enumerator, enumerator_key) %>% 
  rename(enumerator_screening = enumerator,
         enumerator_key_screening = enumerator_key) %>% 
   mutate(hh_id = as.character(hh_id))


hfc_constr_raw <- read_xlsx(file.path(hfc_output_path, "hfc_constr_0728.xlsx"))
screening_raw <- read_xlsx(file.path(hfc_output_path, "hfc_constr_0728.xlsx"))

complete_status <- read_xlsx(path = file.path(data_path_2, "vulnerable households in sample villages.xlsx")) %>% 
  mutate(hh_head_name = paste0(first_name, " ", last_name))


#Treatment=====
treatment_raw <- read.csv(file.path(
  hfc_data_path, "admin_raw.csv"
))

treatment <-  read.csv(file.path(
    hfc_data_path, "admin_raw.csv"
  )) %>% 
  mutate(villageid_key = as.numeric(villageid_key)) %>% 
  select(villageid_key, treatment)


treatment_193 <- read_xlsx(path = file.path(data_path, "scope_193_0807.xlsx"))


rand_newly_sum <- treatment_193 %>% 
  group_by(strata) %>% 
  summarise(
    C = sum(treat == "C"),
    T1 = sum(treat == "T1"),
    T2 = sum(treat == "T2"),
    T3 = sum(treat == "T3"),
    sum = n()
  ) 


summarise_row <-rand_newly_sum %>% 
  summarise(
    strata = "Total",
    C = sum(rand_newly_sum$C),
    T1 = sum(rand_newly_sum$T1),
    T2 = sum(rand_newly_sum$T2),
    T3 = sum(rand_newly_sum$T3),
    sum = sum(rand_newly_sum$sum)
  )

rand_newly_sum <- bind_rows(rand_newly_sum, summarise_row)

writeLines(sub("\\\\begin\\{table\\}", "\\\\begin{table}[h!]", 
               kable(rand_newly_sum, format = "latex", booktabs = TRUE, linesep = "", 
                     caption = "Stratified randomization result")), 
           file.path(output_path, "tables", "stratified randomization results.tex"))


##Compare with 181 villages on treatment
village_181 <- complete_status %>% 
  filter(
    `Dropped from scope due to 15kv` == "No"
  ) %>% 
  distinct(villageid_key, .keep_all = TRUE)

treatment_181 <- treatment_193 %>% 
  filter(village_id %in% village_181$villageid_key)


treatment_compare_193 <- treatment_193 %>% 
  group_by(treat) %>% 
  summarise(
    `Before dropping 15kV villages` = n()
  )

treatment_compare_181 <- treatment_181 %>% 
  group_by(treat) %>% 
  summarise(
    `After dropping 15kV villages` = n()
  )

treatment_compare <- left_join(treatment_compare_193, treatment_compare_181, by = c("treat"))

treatment_compare <- treatment_compare %>%
  mutate(
    treat = recode(treat,
                   "C"  = "Control",
                   "T1" = "T1 (Readyboard)",
                   "T2" = "T2 (SHS)",
                   "T3" = "T3 (Readyboard & SHS)")
  ) %>%
  bind_rows(
    summarise(., 
              treat = "Total",
              across(where(is.numeric), sum, na.rm = TRUE))
  )

writeLines(sub("\\\\begin\\{table\\}", "\\\\begin{table}[h!]", 
               kable(treatment_compare, format="latex", booktabs=TRUE, linesep="", 
                     caption="Distribution of villages by treatment arm", align=c("l","c","c"))), 
           file.path(output_path, "tables", "treatment compare.tex"))


#Complete status------



hfc_constr <- hfc_constr_raw %>%
  filter(consent == 1) %>%
  filter(!is.na(A1_1)) %>%
  distinct(hh_head_name, hh_id, A1_2, A1_3, .keep_all = TRUE) %>%
  filter(village %in% village_181$villageid_key) %>%
  group_by(hh_id) %>%
  arrange(desc(hh_head_name %in% complete_status$hh_head_name)) %>%
  slice(1) %>%
  ungroup() %>% 
  left_join(treatment, by = c("village" = "villageid_key"))

hfc_constr_193 <- hfc_constr_raw %>% 
  filter(consent == 1) %>%
  filter(!is.na(A1_1)) %>%
  distinct(hh_head_name, hh_id, A1_2, A1_3, .keep_all = TRUE) %>%
  group_by(hh_id) %>%
  arrange(desc(hh_head_name %in% complete_status$hh_head_name)) %>%
  slice(1) %>%
  ungroup() %>% 
  mutate(village = as.character(village)) %>% 
  left_join(treatment, by = c("village" = "villageid_key"))



hfc_duplicate <- hfc_constr %>% 
  filter(duplicated(hh_id) | duplicated(hh_id, fromLast = TRUE)) %>% 
  select(hh_id, hh_head_name, gender)


hfc_second <- hfc_constr %>% 
  filter(starttime >= as.Date("2025-06-23")) 
  

##0825 exckyded 15kv villages' complete

enumerator <- hfc_constr_raw %>% 
  select(hh_id, enumerator, enumerator_key)


complete_status <- complete_status %>% 
  mutate(`Completed by Lattanzio` = ifelse(household_id %in% hfc_constr$hh_id, "Yes", "No")) %>% 
  mutate(`Completed by Lattanzio(including dropped villages)` = ifelse(household_id %in% hfc_constr_193$hh_id, "Yes", "No")) %>% 
  mutate(`Approached by Lattanzio(both rounds)` = ifelse(household_id %in% hfc_constr_raw$hh_id | household_id %in% screening_raw$hh_id, "Yes", "No")) %>% 
  mutate(`Approached second round` = ifelse(household_id %in% hfc_second$hh_id , "Yes", "No" )) %>% 
  mutate(`Screened second round` = ifelse(household_id %in% screening_raw$hh_id, "Yes", "No")) %>% 
  mutate(hh_head_name = paste0(first_name, " ", last_name)) 
  
  

complete_save <- complete_status %>% 
  select(  -`In Fully Completed village`, -hh_head_name, -`Approached by Lattanzio`)  


complete_save_join <- left_join(complete_save, enumerator, by = c("household_id" = "hh_id")) %>% 
  distinct(household_id, .keep_all = TRUE)

complete_save_join <- left_join(complete_save_join, screening_enumerator, by = c("household_id" = "hh_id")) %>% 
  distinct(household_id, .keep_all = TRUE) 

complete_save_join <- complete_save_join %>% 
  mutate(enumerator = ifelse(is.na(enumerator), enumerator_screening, enumerator),
         enumerator_key = ifelse(is.na(enumerator_key), enumerator_key_screening, enumerator_key )) %>% 
  select(-enumerator_screening, -enumerator_key_screening)


write_xlsx(complete_save, path = file.path(data_path_2, "survey status of vulnerable households in sample villages_final.xlsx"))
write_xlsx(complete_save_join, path = file.path(data_path_2, "survey status of vulnerable households in sample villages_final(enumerator).xlsx"))

write_xlsx(complete_status, path = file.path(data_path_2, "vulnerable households in sample villages_0825.xlsx")) 
write_xlsx(complete_status, path = file.path(hfc_output_path, "vulnerable households in sample villages_0825.xlsx")) 



summary_by_district <- complete_status %>%
  filter(`Dropped from scope due to 15kv` == "No") %>% 
  group_by(district) %>%
  summarise(
    n_villages   = n_distinct(villageid_key),
    n_households = n_distinct(household_id),
    n_completed  = sum(`Completed by Lattanzio` == "Yes", na.rm = TRUE),
    n_reached    = sum(`Approached by Lattanzio(both rounds)` == "Yes", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  bind_rows(
    summarise(
      .,
      district = "Total",
      n_villages   = sum(n_villages),
      n_households = sum(n_households),
      n_completed  = sum(n_completed),
      n_reached    = sum(n_reached)
    )
  ) %>% 
  mutate(district = str_to_title(district))

writeLines(sub("\\\\begin\\{table\\}", "\\\\begin{table}[h!]", 
               kable(summary_by_district, format = "latex", booktabs = TRUE, linesep = "", 
                     caption = "
                     Achievements by district")), 
           file.path(output_path, "tables", "achievements by district.tex"))





summary_by_district.1<- complete_status %>%
  filter(`Dropped from scope due to 15kv` == "No") %>% 
  group_by(district) %>%
  summarise(
    `Number of Villages`             = n_distinct(villageid_key),
    `Number of Households Reached`   = sum(`Approached by Lattanzio(both rounds)` == "Yes", na.rm = TRUE),
    `Number of Households Surveyed`  = sum(`Completed by Lattanzio` == "Yes", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  bind_rows(
    summarise(
      .,
      district = "Total",
      `Number of Villages`            = sum(`Number of Villages`, na.rm = TRUE),
      `Number of Households Reached`  = sum(`Number of Households Reached`, na.rm = TRUE),
      `Number of Households Surveyed` = sum(`Number of Households Surveyed`, na.rm = TRUE)
    )
  ) %>% 
  mutate(district = str_to_title(district)) %>% 
  rename(District = district)


writeLines(sub("\\\\begin\\{table\\}", "\\\\begin{table}[h!]", 
               kable(summary_by_district.1, format = "latex", booktabs = TRUE, linesep = "", 
                     caption = "
                     Achievements by district")), 
           file.path(output_path, "tables", "achievements by district.1.tex"))



write_xlsx(complete_status, path = file.path(data_path_2, "vulnerable households in sample villages_final.xlsx")) 
write_xlsx(complete_status, path = file.path(hfc_output_path, "vulnerable households in sample villages_final.xlsx")) 

  
  
#Summary using hfc_constr for 1973 households



district_treatment_matrix <- complete_status %>%
  group_by(district, treatment) %>%
  summarise(n_villages = n_distinct(village), .groups = "drop") %>%
  pivot_wider(
    names_from = treatment,
    values_from = n_villages,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  bind_rows(
    summarise(
      .,
      district = "Total",
      across(where(is.numeric), sum)
    )
  )




kable(summary_by_district, format = "latex", booktabs = TRUE, linesep = "") 


summary_by_district_treatment <- complete_status %>%
  filter(`Dropped from scope due to 15kv` == "No") %>% 
  group_by(district) %>%
  summarise(
    n_villages   = n_distinct(villageid_key),
    n_households = n_distinct(household_id),
    n_completed  = sum(`Completed by Lattanzio` == "Yes", na.rm = TRUE),
    n_reached    = sum(`Approached by Lattanzio(both rounds)` == "Yes", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  bind_rows(
    summarise(
      .,
      district = "Total",
      n_villages   = sum(n_villages),
      n_households = sum(n_households),
      n_completed  = sum(n_completed),
      n_reached    = sum(n_reached)
    )
  )








summary_by_district_treatment.2 <- hfc_constr %>%
  group_by(district_key , treatment) %>%
  summarise(
    n_villages   = n_distinct(village),
    n_completed  = n(),
    .groups = "drop"
  ) %>%
  bind_rows(
    summarise(
      .,
      district_key = "Total",
      n_villages   = sum(n_villages),
      n_completed  = sum(n_completed)
      )
  )

