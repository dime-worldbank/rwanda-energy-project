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

complete  <- read_xlsx(path = file.path(data_path_2, "survey status of vulnerable households in sample villages.xlsx"))

complete_survey <- complete %>% 
  filter(`Completed by Lattanzio` == "Yes")

treatment <- read_xlsx(path = file.path(data_path_2, "scope_193_0807.xlsx")) %>%
  select(village_id, treat) %>%
  filter(treat %in% c("T1", "T3"))

complete_krr <- complete %>% 
  mutate(across(c(district, sector, cell, village), ~ str_to_title(.))) %>%
  filter(villageid_key %in% treatment$village_id) %>%
  mutate(key = paste0(first_name, last_name, nid))


#EPC readyboard list-------
## Rulindo----
rulindo_epc <- read_xlsx(path = file.path(data_path_2, "Readyboard EPC negotiation", "Beneficiary List of Rulindo.xlsx"))
rulindo_epc <- rulindo_epc %>% 
  mutate(
    nid = ifelse(nid == "1198780122610117,", "1198980075181012", nid)
  ) %>% 
  mutate(readyboard = ifelse(is.na(scope), 1, 0)) %>% 
  rename(comment = scope) %>% 
  select(
    villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, readyboard, comment
  ) 
  

##Karongi----
# karongi_epc_old <- read_xlsx(path = file.path(data_path_2, "Readyboard EPC negotiation", "Beneficiary List of Karongi.xlsx"), sheet = "household list") %>%
#   mutate(readyboard = ifelse(is.na(Comments), 1, 0)) %>%
#   rename(comment = Comments) %>%
#   select(
#     villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, readyboard, comment
#   )

karongi_epc <- read_xlsx(path = file.path(data_path_2, "Readyboard EPC negotiation", "Ready Board Name List -Lot_Karongi-20250601.xlsx"), sheet = "household list") %>%
  mutate(readyboard = 1,
         comment = NA) %>% 
  select(
    villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, readyboard, comment
  ) 
# 
# 
# old_ready_karongi <- karongi_epc_old %>%
#   filter(readyboard == 1)
# 
# new_ready_karongi <- karongi_epc %>%
#   filter(readyboard == 1)
# 
# # Which are only in old
# only_in_old_karongi <- anti_join(old_ready_karongi, new_ready_karongi,
#                                  by = c("villageid_key", "nid"))
# 
# # Which are only in new
# only_in_new_karongi <- anti_join(new_ready_karongi, old_ready_karongi,
#                                  by = c("villageid_key", "nid"))

##Rutsiro=====
# rutsiro_epc_old <- read_xlsx(path = file.path(data_path_2, "Readyboard EPC negotiation", "Beneficiary List of Rutsiro.xlsx"), sheet = "household list") %>%
#   mutate(readyboard = ifelse(is.na(Comments), 1, 0)) %>%
#   rename(comment = Comments) %>%
#   select(
#     villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, readyboard, comment
#   )


rutsiro_epc<- read_xlsx(path = file.path(data_path_2, "Readyboard EPC negotiation", "Ready Board Name List - Lot_Rutsiro-20250601.xlsx"), sheet = "household list") %>% 
  mutate(readyboard = 1,
         comment = NA) %>% 
  select(
    villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, readyboard, comment
  ) 

#   
# old_ready <- rutsiro_epc_old %>%
#   filter(readyboard == 1)
# 
# new_ready <- rutsiro_epc %>%
#   filter(readyboard == 1)
# 
# # Which are only in old
# only_in_old <- anti_join(old_ready, new_ready,
#                          by = c("villageid_key", "nid"))
# 
# # Which are only in new
# only_in_new <- anti_join(new_ready, old_ready,
#                          by = c("villageid_key", "nid"))
#   





##Rusizi----

rusizi1_epc_scope <- read_xlsx(path = file.path(data_path_2, "Readyboard EPC negotiation", "Beneficiary List of Rusizi_1 (Final).xlsx"), sheet = "Within scope") %>% 
  mutate(readyboard = 1,
         comment = "Within scope") %>%
  select(
    villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, readyboard, comment
  ) 

rusizi1_epc_noscope <- read_xlsx(path = file.path(data_path_2, "Readyboard EPC negotiation", "Beneficiary List of Rusizi_1 (Final).xlsx"), sheet = "Outside of scope") %>% 
  mutate(readyboard = 0,
         comment = "Outside of scope") %>%
  select(
    villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, readyboard, comment
  ) 

rusizi1_epc <- rbind(rusizi1_epc_scope, rusizi1_epc_noscope)

rusizi2_epc_scope <- read_xlsx(path = file.path(data_path_2,"Readyboard EPC negotiation",  "Beneficiary List of Rusizi_2.xlsx"), sheet = "Within scope") %>% 
  mutate(readyboard = 1,
         comment = "Within scope") %>%
  select(
    villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, readyboard, comment
  ) 
rusizi2_epc_noscope <- read_xlsx(path = file.path(data_path_2,"Readyboard EPC negotiation",  "Beneficiary List of Rusizi_2.xlsx"), sheet = "Outside of scope") %>% 
  mutate(readyboard = 0,
         comment = "Outside of scope") %>%
  select(
    villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, readyboard, comment
  ) 

rusizi2_epc <- rbind(rusizi2_epc_scope, rusizi2_epc_noscope)

rusizi_epc <- rbind(rusizi1_epc_scope)
epc_join <- rbind(karongi_epc, rulindo_epc, rutsiro_epc, rusizi1_epc, rusizi2_epc) %>% 
  mutate(
    key = paste0(first_name, last_name, nid),
    nid_epc = nid
  ) %>% 
  select(key, readyboard, comment, nid_epc) %>% 
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
    household_id, readyboard, comment
  )

master <- full_join(complete_krr, epc_join) 

dupes <- master %>%
  filter(duplicated(key) | duplicated(key, fromLast = TRUE))

master <- master %>%
  distinct(key, .keep_all = TRUE)



#Analysis-------



master <- master %>%
  filter(`Dropped from scope due to 15kv` == "No") %>% 
  mutate(readyboard = ifelse(is.na(readyboard), 0, readyboard)) %>% 
  mutate(
    vulnerable = 1
  ) %>% 
  mutate(
    lot = ifelse(villageid_key %in% rusizi1_dime_village$village_id, "Rusizi-1", 
                 ifelse(villageid_key %in% rusizi2_dime_village$village_id, "Rusizi-2", district))
  )


master <- master %>% 
  mutate(
    surveyed = ifelse(household_id %in% complete_survey$household_id, 1, 0)
  ) %>% 
  mutate(
    vulnerable = 1
  ) 
  

master_readyboard <- master %>%
  group_by(villageid_key) %>%
  filter(any(readyboard == 1, na.rm = TRUE)) %>%  # Keep only villages with at least one readyboard
  ungroup() %>%
  group_by(lot) %>%
  summarise(
    village = n_distinct(villageid_key),  # Count of unique villages per lot
    .groups = "drop"  # Optional: drops the grouping after summarise
  )


master_readyboard <- master %>%
  group_by(villageid_key) %>%
  filter(any(readyboard == 1, na.rm = TRUE)) %>%  # keep only villages with at least one readyboard
  ungroup() %>%
  group_by(lot) %>%
  summarise(
    n         = n(),                               # total rows in this lot after the filter
    village   = n_distinct(villageid_key),         # unique villages per lot
    n_surveyed = sum(surveyed == 1, na.rm = TRUE), # count of complete == 1 per lot
    .groups = "drop"
  )


lot_summary <- master_df %>%
  group_by(village, lot) %>%
  summarise(has_readyboard = any(readyboard == 1, na.rm = TRUE), .groups = "drop") %>%
  group_by(lot) %>%
  summarise(
    n_villages = sum(has_readyboard),   # number of villages with at least one readyboard
    complete   = all(has_readyboard)    # TRUE if all villages in this lot have at least one readyboard
  )
# write_xlsx(master, path = file.path(data_path_2, "master_krr.xlsx"))


#Completed by village---



#Analysis-----
analysis <- master %>% count(lot, readyboard, surveyed)


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



