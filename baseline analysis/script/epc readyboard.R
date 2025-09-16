##############
#Author: Xiaoming Zhang
#Date: 3.13.2025
#Purpose: Compare EDCL list
#output: master but without updated household surveyed status

#############


pacman::p_load(knitr, stargazer, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr, lfe, install = TRUE)
library(googlesheets4)
getwd()


# Import Data ----
dropbox <- 'C:/Users/wb614406/Dropbox'

output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/baseline analysis/output"
)

data_path_1 <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data"
)

hfc_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/HFC/data"
)


data_path_2 <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/Updated scope villages& households"
)




#Complete-----

complete  <- read_xlsx(path = file.path(data_path_2, "vulnerable households in sample villages.xlsx"))


complete_second_round <- read_xlsx(path = file.path(data_path_2, "completed_second_round.xlsx")) %>% 
  mutate(
    hh_id = as.character(hh_id)
  )


complete <- left_join(complete, complete_second_round, by = c("household_id" = "hh_id"))



complete_join <- complete %>% 
  mutate(complete_2 = ifelse(`Completed by Lattanzio` == "Yes" | `Completed Second Round` == "Yes", "Yes", "No")) %>% 
  mutate(complete_2 = ifelse(is.na(complete_2), "No", complete_2)) 

treatment <- read_xlsx(path = file.path(data_path_2, "scope_193_0807.xlsx")) %>%
  select(village_id, treat) %>%
  filter(treat %in% c("T1", "T3"))

complete_krr <- complete_join %>% 
  mutate(across(c(district, sector, cell, village), ~ str_to_title(.))) %>%
  filter(villageid_key %in% treatment$village_id) %>%
  mutate(key = paste0(first_name, last_name, nid))


#EPC readyboard list-------
## Rulindo----
rulindo_epc <- read_xlsx(path = file.path(data_path_2, "Lot-Rulindo-20250601.xlsx"))
rulindo_epc <- rulindo_epc %>% 
  mutate(
    nid = ifelse(nid == "1198780122610117,", "1198980075181012", nid)
  ) %>% 
  mutate(readyboard = ifelse(is.na(scope), 1, 0)) %>% 
  select(
    villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, readyboard
  ) 


##Karongi----
karongi_epc <- read_xlsx(path = file.path(data_path_2, "Beneficiary List of Karongi.xlsx"), sheet = "household list") %>% 
  mutate(readyboard = ifelse(is.na(Comments), 1, 0)) %>%
  select(
    villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, readyboard
  ) 
##Rutsiro=====
rutsiro_epc <- read_xlsx(path = file.path(data_path_2, "Beneficiary List of Rutsiro.xlsx"), sheet = "household list") %>% 
mutate(readyboard = ifelse(is.na(Comments), 1, 0)) %>%
  select(
    villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, readyboard
  ) 
##Rusizi----

rusizi1_epc_scope <- read_xlsx(path = file.path(data_path_2, "Beneficiary List of Rusizi_1.xlsx"), sheet = "Within scope") %>% 
  mutate(readyboard = 1) %>%
  select(
    villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, readyboard
  ) 

rusizi1_epc_noscope <- read_xlsx(path = file.path(data_path_2, "Beneficiary List of Rusizi_1.xlsx"), sheet = "Outside of scope") %>% 
  mutate(readyboard = 0) %>%
  select(
    villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, readyboard
  ) 

rusizi1_epc <- rbind(rusizi1_epc_scope, rusizi1_epc_noscope)

rusizi2_epc_scope <- read_xlsx(path = file.path(data_path_2, "Beneficiary List of Rusizi_2.xlsx"), sheet = "Within scope") %>% 
  mutate(readyboard = 1) %>%
  select(
    villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, readyboard
  ) 
rusizi2_epc_noscope <- read_xlsx(path = file.path(data_path_2, "Beneficiary List of Rusizi_2.xlsx"), sheet = "Outside of scope") %>% 
  mutate(readyboard = 0) %>%
  select(
    villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, readyboard
  ) 

rusizi2_epc <- rbind(rusizi2_epc_scope, rusizi2_epc_noscope)

rusizi_epc <- rbind(rusizi1_epc_scope)
epc_join <- rbind(karongi_epc, rulindo_epc, rutsiro_epc, rusizi1_epc, rusizi2_epc) %>% 
  mutate(
    key = paste0(first_name, last_name, nid),
    nid_epc = nid
  ) %>% 
  select(key, readyboard, nid_epc) %>% 
  left_join(complete_krr, epc_join, by = c("key" = "key")) %>%
  mutate(
    household_id = case_when(
      nid_epc == "1195970047063046" ~ "411001060384",
      nid_epc == "1198870126907080" ~ "411001060519",
      nid_epc == "1198180147332076" ~ "411502040210",
      TRUE ~ household_id
    )
  ) %>% 
  distinct(household_id, .keep_all = TRUE) %>%
  select(
    household_id, readyboard
  )

master <- full_join(complete_krr, epc_join) 

master <- master %>%
  distinct(key, .keep_all = TRUE)
#Analysis-------



master <- master %>%
  filter(`Dropped from scope due to 15kv` == "No") %>% 
  mutate(readyboard = ifelse(is.na(readyboard), 0, readyboard)) %>% 
  mutate(
    vulnerable = 1
  )


complete  <- read_xlsx(path = file.path(data_path_2, "vulnerable households in sample villages.xlsx"))


complete_second_round <- read_xlsx(path = file.path(data_path_2, "completed_second_round.xlsx"))  

complete_survey_hfc <- complete %>% 
  mutate(
    `Complete second round` = ifelse(household_id %in% complete_second_round$hh_id, "Yes", "No")
  ) %>% 
  mutate(
    `Completed_households` = ifelse(`Completed by Lattanzio` == "Yes" | `Complete second round` == "Yes", "Yes", "No")
  )  %>% 
  filter(`Completed_households` == "Yes") 




complete_survey <- read.csv(file.path(hfc_path,"Baseline second round",  "REP_baseline_test_WIDE (2).csv")) %>% 
  mutate(
    hh_id = as.character(hh_id)
  ) %>% 
  filter(hh_id %in% complete_survey_hfc$household_id) 


master <- master %>% 
  mutate(
    surveyed = ifelse(household_id %in% complete_survey$hh_id, 1, 0)
  ) %>% 
  mutate(
    vulnerable = 1
  )





# write_xlsx(master, path = file.path(data_path_2, "master_krr.xlsx"))




#Analysis-----
analysis <- master %>% count(district, readyboard, surveyed)


noreadyboard <- master %>%
  group_by(villageid_key) %>%
  filter(all(readyboard == 0)) %>%
  ungroup() %>%
  distinct(villageid_key, .keep_all = TRUE) %>%
  select(villageid_key, village, cell, sector, district)



# Create a named list by district
noreadyboard_list <- noreadyboard %>%
  group_split(district) %>%
  setNames(unique(noreadyboard$district))

# Write to Excel
write_xlsx(noreadyboard_list, path = file.path(data_path_2,  "Village with no readyboard(karongi&rutsiro&rusizi).xlsx"))

# Read dime files------
# 
rulindo_dime <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rulindo.xlsx"),
                          sheet = "household list")
rulindo_dime_village <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rulindo.xlsx"),
                          sheet = "village list")


rutsiro_dime <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rutsiro.xlsx"),
                          sheet = "household list")
rutsiro_dime_village <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rutsiro.xlsx"),
                          sheet = "village list")


karongi_dime <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Karongi.xlsx"),
                          sheet = "household list")
karongi_dime_village <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Karongi.xlsx"),
                          sheet = "village list")





##Rusizi lot 1---------

rusizi1_dime <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rusizi-1.xlsx"),
                         sheet = "household list")
rusizi1_dime_village <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rusizi-1.xlsx"),
                                 sheet = "village list")

rusizi1_check <- master %>% 
  filter(villageid_key %in% rusizi1_epc_scope$villageid_key) 

nrow(rusizi1_check)
sum(rusizi1_check$surveyed == 1, na.rm = TRUE)
sum(rusizi1_check$readyboard == 1, na.rm = TRUE)
sum(rusizi1_check$readyboard == 1 & rusizi1_check$surveyed == 1, na.rm = TRUE)
n_distinct(rusizi1_epc_scope$villageid_key)
n_distinct(rusizi1_dime$villageid_key)
nrow(rusizi1_dime)
##Rusizi lot 2-------


rusizi2_dime <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rusizi-2.xlsx"),
                          sheet = "household list")
rusizi2_dime_village <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rusizi-2.xlsx"),
                                  sheet = "village list")

rusizi2_check <- master %>% 
  filter(villageid_key %in% rusizi2_epc_scope$villageid_key) 

nrow(rusizi2_check)
sum(rusizi2_check$surveyed == 1, na.rm = TRUE)
sum(rusizi2_check$readyboard == 1, na.rm = TRUE)

sum(rusizi2_check$readyboard == 1 & rusizi2_check$surveyed == 1, na.rm = TRUE)
n_distinct(rusizi2_epc_scope$villageid_key)
n_distinct(rusizi2_dime$villageid_key)
nrow(rusizi2_dime)

rusizi_dime <- rbind(rusizi1_dime_village, rusizi2_dime_village)



  
# ##Rulindo----
# rulindo_out_scope <- complete %>% 
#   filter(`Dropped from scope due to 15kv` == "Yes") 
# 
# rulindo_nord_village <- rulindo_dime_village %>% 
#   filter(
#     !village_id %in% rulindo_epc$villageid_key 
#   ) %>% 
#   filter(
#     !village_id %in% rulindo_out_scope$villageid_key
#   )
# 
# rulindo_nord_hh <- rulindo_dime %>% 
#   filter(
#     !villageid_key %in% rulindo_epc$villageid_key 
#   ) %>% 
#   filter(
#     !villageid_key %in% rulindo_out_scope$villageid_key
#   )
# 
# rulindo_list <- list(
#   "village_list" = rulindo_nord_village,
#   "household_list" = rulindo_nord_hh
# )
# 
# write_xlsx(rulindo_list, path = file.path(data_path_2, "rulindo_no_readyboard.xlsx"))
# 
# #Rutsiro----
# 
# rutsiro_readyboard <- rutsiro_epc %>% 
#   filter(readyboard == 1)
# 
# rutsiro_nord_village <- rutsiro_dime_village %>% 
#   filter(
#     !village_id %in% rutsiro_readyboard$villageid_key 
#   ) 
# 
# rutsiro_nord_hh <- rutsiro_dime %>% 
#   filter(
#     !villageid_key %in% rutsiro_readyboard$villageid_key 
#   ) 
# 
# write_xlsx(list(
#   "village_list" = rutsiro_nord_village,
#   "household_list" = rutsiro_nord_hh
# ), path = file.path(data_path_2, "rutsiro_no_readyboard.xlsx"))
# #Karongi----
# 
# karongi_readyboard <- karongi_epc %>% 
#   filter(readyboard == 1)
# 
# karongi_nord_village <- karongi_dime_village %>%
#   filter(
#     !village_id %in% karongi_readyboard$villageid_key 
#   )
# 
# karongi_nord_hh <- karongi_dime %>%
#   filter(
#     !villageid_key %in% karongi_readyboard$villageid_key 
#   )
# 
# write_xlsx(list(
#   "village_list" = karongi_nord_village,
#   "household_list" = karongi_nord_hh
# ), path = file.path(data_path_2, "karongi_no_readyboard.xlsx"))
# 



