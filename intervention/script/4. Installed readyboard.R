#######################################################
#Purpose: installed readyboard
#Author: Xiaoming
#Date: 9.25.2025
####################################################################



pacman::p_load(knitr, stargazer, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr, lfe, install = TRUE)
library(googlesheets4)
getwd()


##Rusizi----

rusizi1_epc_scope <- read_xlsx(
  path = file.path(data_path_2, "Readyboard EPC negotiation", "Beneficiary List of Rusizi_1 (Final).xlsx"),
  sheet = "Within scope"
) %>%
  mutate(
    readyboard = ifelse(`Status 1` %in% c("Available", "available"), 1, 0),
    comment    = ifelse(`Status 1` %in% c("Available", "available"), "Within scope", "Outside of scope")
  ) %>%
  select(
    villageid_key, village, cell, sector, district,
    first_name, last_name, gender, nid,
    readyboard, comment
  )

rusizi1_installed <- read_xlsx(path =file.path(data_path_2, "Readyboard EPC negotiation", "Rusizi_installed.xlsx" ), sheet = "lot 1")  %>% 
  clean_names() %>% 
  mutate(
    nid = ifelse(id == "1197270035899039", "1197270035896039", id)
  ) %>%
  rename(installed_id = id) %>% 
  mutate(installed = 1 ) %>% 
  select(village_id, nid, installed, installed_id)

dup_df.1 <- tibble(duplicate_id = rusizi1_installed$installed_id[duplicated(rusizi1_installed$installed_id)])


dup_df.1 <- rusizi1_installed %>% 
  filter(installed_id %in% dup_df.1$duplicate_id)

# 
# write_xlsx(
#   dup_df.1,
#   path = file.path(data_path_2, "Readyboard EPC negotiation", "Rusizi", "Duplicate in Rusizi Lot1 readyboard recipient.xlsx")
# )

check <- rusizi1_installed %>%
  filter(!nid %in% rusizi1_dime$nid)



rusizi2_epc_scope <- read_xlsx(path = file.path(data_path_2,"Readyboard EPC negotiation",  "Beneficiary List of Rusizi_2 (Final).xlsx"), sheet = "Within scope") %>% 
  mutate(readyboard = ifelse(`Status` == "Available", 1, 0),
         comment = ifelse(`Status` == "Available", "Within scope", "Outside of scope")) %>%
  select(
    villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, readyboard, comment
  ) 



rusizi2_installed <- read_xlsx(path =file.path(data_path_2, "Readyboard EPC negotiation", "Rusizi_installed.xlsx" ), sheet = "lot 2")   %>% 
  clean_names() %>% 
  mutate(
    nid = ifelse(id == "1195770018361066", "12630104", id)
  ) %>%
  rename(installed_id = id) %>% 
  mutate(installed = 1 ) %>% 
  select(village_id, nid, installed, installed_id)

dup_df.2 <- tibble(duplicate_id = rusizi2_installed$installed_id[duplicated(rusizi2_installed$installed_id)])


dup_df.2 <- rusizi2_installed %>% 
  filter(installed_id %in% dup_df.2$duplicate_id)

# 
# write_xlsx(
#   dup_df.2,
#   path = file.path(data_path_2, "Readyboard EPC negotiation", "Rusizi", "Duplicate in Rusizi Lot1 readyboard recipient.xlsx")
# )

check <- rusizi2_installed %>%
  filter(!nid %in% rusizi2_dime$nid)




# write_xlsx(
#   list(
#     lot1_duplicate = dup_df.1,
#     lot2_duplicate = dup_df.2,
#     lot2_installed_notvulnerable = rusizi2_installed_outscope
#    
#   ),
#   path = file.path(data_path_2, "Readyboard EPC negotiation", "Rusizi_installed_dups&not found.xlsx")
# )


#Join to master--------

rusizi_installed <- rbind(rusizi1_installed, rusizi2_installed)

rusizi_installed_join <- rusizi_installed %>% 
  select(-village_id)


# 
#   filter(!is.na(nid)) %>% 
#   distinct(nid, .keep_all = TRUE)


master_installed <- left_join(master, rusizi_installed_join)



write_xlsx(master_installed, path = file.path(data_path_2, "Readyboard EPC negotiation", "master_installed.xlsx"))



