##############
#Author: Xiaoming Zhang
#Date: 3.13.2025
#Purpose: Compare EDCL list
#############


pacman::p_load(knitr, stargazer, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr, lfe)
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




#Check rulindo-----

complete  <- read_xlsx(path = file.path(data_path_2, "vulnerable households in sample villages.xlsx"))


complete_second_round <- read_xlsx(path = file.path(data_path_2, "completed_second_round.xlsx")) %>% 
  mutate(
    hh_id = as.character(hh_id)
  )


complete <- left_join(complete, complete_second_round, by = c("household_id" = "hh_id"))


complete_join <- complete %>% 
  select(nid, `Approached by Lattanzio`, `Completed by Lattanzio`, `In Fully Completed village`, `Dropped from scope due to 15kv`, `Completed Second Round`)

complete_join <- complete_join %>% 
  mutate(complete_2 = ifelse(`Completed by Lattanzio` == "Yes" | `Completed Second Round` == "Yes", "Yes", "No")) %>% 
  mutate(complete_2 = ifelse(is.na(complete_2), "No", complete_2)) 


#Check rulindo----

##epc----

rulindo_epc <- read_xlsx(path = file.path(data_path_2, "Lot-Rulindo-20250601.xlsx"))

rulindo_epc <- rulindo_epc %>% 
  mutate(
    nid = ifelse(nid == "1198780122610117,", "1198980075181012", nid)
  ) %>% 
  filter(is.na(scope))

rulindo_epc <- left_join(rulindo_epc, complete_join, by = "nid")


sum(rulindo_epc$complete_2 == "Yes", na.rm = TRUE)


##dime----
rulindo_15 <- complete %>% 
  filter(`Dropped from scope due to 15kv` == "Yes") 

rulindo_dime <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rulindo.xlsx"),
                          sheet = "household list")

rulindo_dime_join <- left_join(rulindo_dime, complete_join, by = "nid") %>% 
  filter(!villageid_key %in% rulindo_15$villageid_key) %>% 
  filter(villageid_key %in% rulindo_epc$villageid_key)

sum(rulindo_dime_join$complete_2 == "Yes", na.rm = TRUE)

rulindo_check <- rulindo_dime_join %>% 
  group_by(villageid_key) %>% 
  summarise(
    vulnerable_hh = n(),
    complete = sum(complete_2 == "Yes", na.rm = TRUE)
  )


rulindo_check_epc <- rulindo_epc %>% 
  group_by(villageid_key) %>% 
  summarise(
    readyboard_hh = n(),
    complete_epc = sum(complete_2 == "Yes", na.rm = TRUE)
  )


rulindo_check <- left_join(rulindo_check, rulindo_check_epc, by = "villageid_key") 
















#check rulindo 2

rulindo_site2 <- read_xlsx(path = file.path(data_path_2, "Rulindo_Site2.xlsx"), sheet = "in_scope")

rulindo_noscope <- read_xlsx(path = file.path(data_path_2, "Rulindo_Site2.xlsx"), sheet = "out_scope")


rulindo_list <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rulindo.xlsx"),
                          sheet = "household list")

rulindo_site2 <- rulindo_site2 %>% 
  clean_names() %>% 
  mutate(
    comments = "in_scope"
  )


rulindo_noscope <- rulindo_noscope %>% 
  clean_names() %>% 
  mutate(
    comments = "out_scope"
  )


rulindo_scope <- rbind(rulindo_site2, rulindo_noscope)


rulindo_distinct <- rulindo_scope %>% 
  distinct(nid, first_name, last_name, villageid_key, .keep_all = TRUE) %>% 
  mutate(
    check= paste0(nid, first_name, last_name)
  )

rulindo_6 <- rulindo_list %>%
  filter(
    villageid_key %in% rulindo_distinct$villageid_key
  ) %>%
  distinct(nid, first_name, last_name, villageid_key, .keep_all = TRUE) %>%
  mutate(
    check = paste0(nid, first_name, last_name)
  )
# 
# 
# rulindo_check0 <- rulindo_6 %>% 
#   filter(
#     !nid %in% rulindo_distinct$nid
#   )
# 
# 
# rulindo_check <- rulindo_distinct %>% 
#   filter(
#     !nid %in% rulindo_6$nid
#   )

complete  <- read_xlsx(path = file.path(data_path_2, "vulnerable households in sample villages.xlsx"))

rulindo_complete <- complete %>% 
  filter(
    villageid_key %in% rulindo_distinct$villageid_key
  ) %>% 
  filter(`Completed by Lattanzio`== "Yes") 


rulindo_scope_1 <- rulindo_scope %>% 
  filter(
    comments == "in_scope"
  )



rulindo_check <- rulindo_complete %>% 
  filter(
    nid %in% rulindo_scope_1$nid
  )



#Check deployment plan-----

complete  <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","vulnerable households in sample villages.xlsx"))

deployment <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","deployment.xlsx"))


comp_deploy <- complete %>% 
  select(
    villageid_key,district, sector, cell, village, `Approached by Lattanzio`, `In Fully Completed village`
  ) 


scope_village <- complete %>% 
  select(
    villageid_key, district, sector, cell, village
  ) %>% 
  distinct(villageid_key, .keep_all = TRUE)


scope_village <- scope_village %>% 
  mutate(
    across(
      c(district, sector, cell, village),
      ~ str_to_title(.x)
    
  ))


deployment <- deployment %>% 
  mutate(
    across(
      c(district, sector, cell, village),
      ~ str_to_title(.x)
    )
  )


deployment <- left_join(deployment, scope_village, by = c("district","sector", "cell", "village"))

deployment_noid <- deployment %>% 
  filter(
    is.na(villageid_key)
  )

#join with complete list----
comp_deploy <- complete %>% 
  select(
    villageid_key,district, sector, cell, village, `Approached by Lattanzio`, `In Fully Completed village`, `Dropped from scope due to 15kv`
  )  %>% 
  filter(
    `In Fully Completed village` == "No" &`Approached by Lattanzio` == "No" & `Dropped from scope due to 15kv` == "No"
  ) %>%
  group_by(
    villageid_key, district, sector, cell, village
  ) %>%
  summarise(
    n = n()
  ) %>% 
  ungroup() %>% 
  mutate(
    across(
      c(district, sector, cell, village),
      ~ str_to_title(.x)
  ))

deploy_list <- complete %>% 
  select(
    villageid_key,district, sector, cell, village, `Approached by Lattanzio`, `In Fully Completed village`, `Dropped from scope due to 15kv`
  )  %>% 
  filter(
    `In Fully Completed village` == "No" &`Approached by Lattanzio` == "No" & `Dropped from scope due to 15kv` == "No"
  ) %>% 
  mutate(
    across(
      c(district, sector, cell, village),
      ~ str_to_title(.x)
    )
  ) 

write_xlsx(deploy_list, path = file.path(data_path_1, "Updated scope villages& households", "Household list for deployment.xlsx"))


deployment_join <- deployment %>% 
  select(villageid_key, hh_survey)

comp_deploy <- left_join(comp_deploy, deployment_join, by = "villageid_key")

comp_deploy <- comp_deploy %>% 
  rename(
    dime_hh_number = n,
    lattanzio_hh_number = hh_survey
  )
comp_deploy_na <- comp_deploy %>% 
  filter(is.na(lattanzio_hh_number))

write_xlsx(comp_deploy_na, path = file.path(data_path_1, "Updated scope villages& households", "deployment_discrepency.xlsx"))


#Deploy anti match------
deployment <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","deployment.xlsx"))

deployment <- deployment %>% 

  mutate(
    across(
      c(district, sector, cell, village),
      str_to_title            # title-case every selected column
    )
  ) %>% 
  mutate(
    village = case_when(
      village == "Kabyingo" ~ "Kabyigo",
      village == "Muhondo"  ~ "Ruhondo",
      village == "Rubugu"   ~ "Rubuye",
      village == "Gashengeri" ~ "Gahengeri",
      TRUE                  ~ village          # keep all other names unchanged
    )
  )  

deployment <- left_join(deployment, scope_village, by = c("district","sector", "cell", "village"))

deployment <- deployment %>%
  mutate(
    villageid_key = ifelse(is.na(villageid_key), "31080104", villageid_key)
  )





deployment_join <- deployment %>% 
  select(villageid_key, district, sector, cell, village, hh_survey)

complete_report_join <- complete %>% 
  mutate(
    status = ifelse(`In Fully Completed village` == "Yes", "Completed Village", 
                    ifelse(`Dropped from scope due to 15kv` == "Yes", "Dropped Village", "To be Surveyed"))
  ) %>% 
  select(villageid_key, status) %>% 
  distinct(villageid_key, .keep_all =TRUE)

complete_report_join_2 <- complete%>% 
  filter(
    `Approached by Lattanzio` == "No" & `In Fully Completed village` == "No" & `Dropped from scope due to 15kv` == "No"
  ) %>% 
  group_by(villageid_key) %>% 
  summarise(
    tobe_surveyed_hh = n()
  )

complete_report_join <- left_join(complete_report_join, complete_report_join_2, by = "villageid_key")

deployment_report<- left_join(deployment, complete_report_join, by = c("villageid_key"))


deployment_report <- deployment_report %>% 
  distinct(villageid_key, .keep_all = TRUE)

deployment_report_anti <- left_join(complete_report_join_2, 
                                    deployment_join, 
                                    by = c("villageid_key"))


deployment_report_anti<- deployment_report_anti %>% 
  distinct(villageid_key, .keep_all = TRUE)


#Write to file

write_xlsx(
  list(
    "Deployment Report" = deployment_report,
    "Deployment Anti Join" = deployment_report_anti
  ),
  path = file.path(data_path_1, "Updated scope villages& households", "deployment_report.xlsx")
)











karongi_village <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Readyboard by lot","Lot_Karongi.xlsx"), sheet = "village list")
rutsiro_village <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Readyboard by lot","Lot_Rutsiro.xlsx"), sheet = "village list")
rulindo_village <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Readyboard by lot","Lot_Rulindo.xlsx"), sheet = "village list")

karongi_epc<- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Ready Board Name List -Lot_Karongi-20250601.xlsx"), sheet = "household list")
rutsiro_epc<- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Ready Board Name List - Lot_Rutsiro-20250601.xlsx"), sheet = "household list")
rulindo_epc <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","Rulindo_readyboard_status.xlsx"), sheet = "readyboard")

karongi_hh <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Readyboard by lot","Lot_Karongi.xlsx"), sheet = "household list")
rutsiro_hh <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Readyboard by lot","Lot_Rutsiro.xlsx"), sheet = "household list")
rulindo_hh <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Readyboard by lot","Lot_Rulindo.xlsx"), sheet = "household list")


complete  <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","vulnerable households in sample villages.xlsx"))

#rulindo village-----
rulindo <- complete %>% 
  filter(district == "Rulindo")

rulindo_out <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","vulnerable households in sample villages.xlsx"), sheet = "Sheet2")

rulindo_village <- rulindo_village %>% 
  filter(!village_id %in% rulindo_out$villageid_key)

rulindo_hh <- rulindo_hh %>% 
  filter(!villageid_key %in% rulindo_out$villageid_key)

#Only in EPC villages----


karongi_epc_hh <- karongi_hh %>% 
  filter(villageid_key %in% karongi_epc$villageid_key)

nrow(karongi_epc_hh)

rutsiro_epc_hh <- rutsiro_hh %>%
  filter(villageid_key %in% rutsiro_epc$villageid_key)

nrow(rutsiro_epc_hh)

rulindo_epc_hh <- rulindo_hh %>%
  filter(villageid_key %in% rulindo_epc$villageid_key)


nrow(rulindo_epc_hh)

####Complete------


complete_latt <- complete %>% 
  filter(`Completed by Lattanzio` == "Yes")

karongi_surveyed <-  karongi_epc_hh %>% 
  filter(
    nid %in% complete_latt$nid
  )

nrow(karongi_surveyed)

rutsiro_surveyed <-  rutsiro_epc_hh %>%
  filter(
    nid %in% complete_latt$nid
  )

nrow(rutsiro_surveyed)

rulindo_surveyed <-  rulindo_epc_hh %>%
  filter(
    nid %in% complete_latt$nid
  )

nrow(rulindo_surveyed)


#Overlap with EPC

karongi_overlap <- karongi_surveyed %>% 
  filter(
    nid %in% karongi_epc$nid & last_name %in% karongi_epc$last_name
  )

nrow(karongi_overlap)


rutsiro_overlap <- rutsiro_surveyed %>%
  filter(
    nid %in% rutsiro_epc$nid & last_name %in% rutsiro_epc$last_name
  )

nrow(rutsiro_overlap)

rulindo_overlap <- rulindo_surveyed %>%
  filter(
    nid %in% rulindo_epc$nid & last_name %in% rulindo_epc$last_name
  )

nrow(rulindo_overlap)



#Check subset----

n_distinct(karongi_epc$villageid_key)

n_distinct(rutsiro_epc$villageid_key)

n_distinct(rulindo_epc$villageid_key)

check_karongi <- karongi_epc %>% 
  filter(!villageid_key %in% karongi_village$village_id)

nrow(check_karongi)

check_rutsiro <- rutsiro_epc %>%
  filter(!villageid_key %in% rutsiro_village$village_id)

nrow(check_rutsiro)

check_rulindo <- rulindo_epc %>%
  filter(!villageid_key %in% rulindo_village$village_id)

nrow(check_rulindo)

check_karongi_hh <- karongi_epc %>% 
  filter(!nid %in% karongi_hh$nid)

nrow(check_karongi_hh)

check_rutsiro_hh <- rutsiro_epc %>%
  filter(!nid %in% rutsiro_hh$nid)

nrow(check_rutsiro_hh)

check_rulindo_hh <- rulindo_epc %>%
  filter(!nid %in% rulindo_hh$nid)

nrow(check_rulindo_hh)

#Those not in the scope-----


household_list <- rbind(karongi_hh, rutsiro_hh, rulindo_hh)

karongi_epc_village <- karongi_village %>% 
  filter(village_id %in% karongi_epc$villageid_key)


rutsiro_epc_village <- rutsiro_village %>%
  filter(village_id %in% rutsiro_epc$villageid_key)

rulindo_epc_village <- rulindo_village %>%
  filter(village_id %in% rulindo_epc$villageid_key)


epc_village <- rbind(karongi_epc_village, rutsiro_epc_village, rulindo_epc_village)


karongi_epc_join <- karongi_epc %>% 
  select(-`No.`)

rutsiro_epc_join <- rutsiro_epc %>%
  select(-`No.`)

rulindo_epc <- rulindo_epc %>%
  select(-`Comments`)

epc_list <- rbind(karongi_epc_join, rutsiro_epc_join, rulindo_epc)

epc_list <- epc_list %>% 
  mutate(
    id = paste0(nid, first_name, last_name)
  )
household_epc <- household_list %>% 
  filter(villageid_key %in% epc_village$village_id) %>% 
  mutate(
    id = paste0(nid, first_name, last_name)
  )


household_not <- household_epc %>% 
  filter(!id %in% epc_list$id )  
  
household_notkr <- household_not %>% 
filter(district != "Rulindo")

write_xlsx(household_notkr, path = file.path(data_path_1, "Updated scope villages& households", "household_no_readyboard(karongi&rutsiro).xlsx"))

write_xlsx(household_not, path = file.path(data_path_1, "Updated scope villages& households", "household_not_in_epc(karongi&rutsiro&rulindo).xlsx"))

household_yes <- household_epc %>% 
  filter(id %in% epc_list$id )



epc_duplicate <- epc_list %>% 
  group_by(id) %>% 
  filter(n() > 1) %>% 
  ungroup()


write_xlsx(epc_duplicate, path = file.path(data_path_1, "Updated scope villages& households", "epc_duplicate(karongi&rutsiro).xlsx"))






#Commented out of scope----

karongi_scope <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","Beneficiary List of Karongi.xlsx"), sheet = "household list")

karongi_scope_epc <- karongi_scope %>% 
  filter(is.na(Comments)) %>% 
  mutate(
    id = paste0(nid, first_name, last_name)
  ) %>% 
  distinct(id, .keep_all = TRUE)

karongi_epc <- karongi_epc %>% 
  mutate(
    id = paste0(nid, first_name, last_name)
  ) %>% 
  distinct(id, .keep_all = TRUE)

karongi_match <- karongi_epc %>% 
  filter(
    !id %in% karongi_scope_epc$id
  )

karongi_reverse_match <- karongi_scope_epc %>%
  filter(!id %in% karongi_epc$id)


karongi_epc_belong <- karongi_epc %>%
  filter(!id %in% household_epc$id)

karongi_scope_epc_beling <- karongi_scope_epc%>% 
  filter(!id %in% household_epc$id)


##Rutsiro-------
rutsiro_scope <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","Beneficiary List of Rutsiro.xlsx"), sheet = "household list")


rutsiro_scope_epc <- rutsiro_scope %>% 
  filter(is.na(Comments)) %>% 
  mutate(
    id = paste0(nid, first_name, last_name)
  ) %>% 
  distinct(id, .keep_all = TRUE)


rutsiro_epc <- rutsiro_epc %>%
  mutate(
    id = paste0(nid, first_name, last_name)
  ) %>% 
  distinct(id, .keep_all = TRUE)


rutsiro_match <- rutsiro_epc %>%
  filter(
    !id %in% rutsiro_scope_epc$id
  )

rutsiro_reverse_match <- rutsiro_scope_epc %>% 
  filter(!id %in% rutsiro_epc$id)

rutsiro_epc_belong <- rutsiro_epc %>% 
  filter(!id %in%household_epc$id)


rutsiro_scope_epc_beling <- rutsiro_scope_epc %>%
  filter(!id %in% household_epc$id)







karongi_scope_count <- karongi_scope %>% 
  group_by(Comments) %>% 
  summarise(
    n = n()
  ) 


rutsiro_scope_count <- rutsiro_scope %>%
  group_by(Comments) %>% 
  summarise(
    n = n()
  )


##Check with vulnerable household------

karongi_hh <- karongi_hh %>% 
  mutate(
    id = paste0(nid, first_name, last_name)
  ) 

karongi_vulnerable_check <- karongi_scope %>% 
  mutate(
    id = paste0(nid, first_name, last_name)
  ) %>%
  filter(id %in% karongi_hh$id)

rutsiro_hh <- rutsiro_hh %>%
  mutate(
    id = paste0(nid, first_name, last_name)
  )

rutsiro_vulnerable_check <- rutsiro_scope %>%
  mutate(
    id = paste0(nid, first_name, last_name)
  ) %>%
  filter(id %in% rutsiro_hh$id)
#Combine and output----

match <- rbind(karongi_match, rutsiro_match)

reverse_match <- rbind(karongi_reverse_match, rutsiro_reverse_match)

write_xlsx(match, path = file.path(data_path_1, "Updated scope villages& households", "receipient_0601_notfoundin_0606(karongi&rutsiro).xlsx"))

write_xlsx(reverse_match, path = file.path(data_path_1, "Updated scope villages& households", "receipient_0606_notfoundin_0601(karongi&rutsiro).xlsx"))



write_xlsx(
  list(
    "0601_notfoundin_0606" = match,
    "0606_notfoundin_0601" = reverse_match
  ),
  path = file.path(data_path_1, "Updated scope villages& households", "comparison_karongi_rutsiro.xlsx")
)


#Same check with before--------

karongi_village <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Readyboard by lot","Lot_Karongi.xlsx"), sheet = "village list")
rutsiro_village <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Readyboard by lot","Lot_Rutsiro.xlsx"), sheet = "village list")

karongi_epc <- karongi_scope_epc
rutsiro_epc <- rutsiro_scope_epc

karongi_hh <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Readyboard by lot","Lot_Karongi.xlsx"), sheet = "household list")
rutsiro_hh <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Readyboard by lot","Lot_Rutsiro.xlsx"), sheet = "household list")

karongi_hh <- karongi_hh %>% 
  mutate(
    id = paste0(nid, first_name, last_name)
  )

rutsiro_hh <- rutsiro_hh %>%
  mutate(
    id = paste0(nid, first_name, last_name)
  )

complete  <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","vulnerable households in sample villages.xlsx"))

#Only in EPC villages----


karongi_epc_hh <- karongi_hh %>% 
  filter(villageid_key %in% karongi_epc$villageid_key)

nrow(karongi_epc_hh)

rutsiro_epc_hh <- rutsiro_hh %>%
  filter(villageid_key %in% rutsiro_epc$villageid_key)

nrow(rutsiro_epc_hh)


####Complete------


complete_latt <- complete %>% 
  filter(`Completed by Lattanzio` == "Yes") %>% 
  mutate(
    id = paste0(nid, first_name, last_name)
  )

complete_latt_duplicate <- complete_latt%>% 
  group_by(id) %>% 
  filter(n() > 1) %>% 
  ungroup()


karongi_surveyed <-  karongi_epc_hh %>% 
  filter(
    id %in% complete_latt$id
  )

nrow(karongi_surveyed)

rutsiro_surveyed <-  rutsiro_epc_hh %>%
  filter(
    nid %in% complete_latt$nid
  )

nrow(rutsiro_surveyed)



#Overlap with EPC

karongi_overlap <- karongi_surveyed %>% 
  filter(
    id %in% karongi_epc$id 
  )

nrow(karongi_overlap)


rutsiro_overlap <- rutsiro_surveyed %>%
  filter(
    id %in% rutsiro_epc$id
  )

nrow(rutsiro_overlap)




#Check subset----

n_distinct(karongi_epc$villageid_key)

n_distinct(rutsiro_epc$villageid_key)

n_distinct(rulindo_epc$villageid_key)

check_karongi <- karongi_epc %>% 
  filter(!villageid_key %in% karongi_village$village_id)

nrow(check_karongi)

check_rutsiro <- rutsiro_epc %>%
  filter(!villageid_key %in% rutsiro_village$village_id)

nrow(check_rutsiro)

check_rulindo <- rulindo_epc %>%
  filter(!villageid_key %in% rulindo_village$village_id)

nrow(check_rulindo)

check_karongi_hh <- karongi_epc %>% 
  filter(!nid %in% karongi_hh$nid)

nrow(check_karongi_hh)

check_rutsiro_hh <- rutsiro_epc %>%
  filter(!nid %in% rutsiro_hh$nid)

nrow(check_rutsiro_hh)

check_rulindo_hh <- rulindo_epc %>%
  filter(!nid %in% rulindo_hh$nid)

nrow(check_rulindo_hh)
































# 
# 
# #Other------
# 
# 
# karongi_village <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Readyboard by lot","Lot_Karongi.xlsx"), sheet = "village list")
# rutsiro_village <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Readyboard by lot","Lot_Rutsiro.xlsx"), sheet = "village list")
# 
# karongi_epc<- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Ready Board Name List -Lot_Karongi-20250601.xlsx"), sheet = "household list")
# rutsiro_epc<- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Ready Board Name List - Lot_Rutsiro-20250601.xlsx"), sheet = "household list")
# 
# nrow(karongi_epc)
# nrow(karongi)
# nrow(rutsiro_epc)
# nrow(rutsiro)
# 
# karongi_report_village <- karongi_village %>% 
#   filter(!village_id %in% karongi_epc$villageid_key)
# 
# karongi_report_household <- karongi %>% 
#   filter(!villageid_key %in% karongi_epc$villageid_key)
# 
# karongi_report_list <- list(
#   village_list = karongi_report_village,
#   household_list = karongi_report_household
# )
# 
# # Write to Excel file with two sheets
# write_xlsx(karongi_report_list, path = file.path(data_path_1, "Updated scope villages& households","EDCL","Karongi_discrepency_0603.xlsx"))
# 
# rutsiro_report_village <- rutsiro_village %>%
#   filter(!village_id %in% rutsiro_epc$villageid_key)
# 
# rutsiro_report_household <- rutsiro %>%
#   filter(!villageid_key %in% rutsiro_epc$villageid_key)
# 
# 
# rutsiro_report_list <- list(
#   village_list = rutsiro_report_village,
#   household_list = rutsiro_report_household
# )
# 
# # Write to Excel file with two sheets
# write_xlsx(rutsiro_report_list, path = file.path(data_path_1, "Updated scope villages& households","EDCL","Rutsiro_discrepency_0603.xlsx"))
# 
# 
# 
# 
# 
# 
# 
# 
# karongi_village_check <- karongi_epc %>% 
#   filter(!villageid_key %in% karongi$villageid_key) 
# 
# nrow(karongi_village_check)
# 
# rutsiro_village_check <- rutsiro_epc %>% 
#   filter(!villageid_key %in% rutsiro$villageid_key)
# 
# nrow(rutsiro_village_check)
# 
# karongi_id_check <- karongi_epc %>% 
#   filter(!nid%in% karongi$nid)
# 
# nrow(karongi_id_check)
# 
# rutsiro_id_check <- rutsiro_epc %>% 
#   filter(!nid%in% rutsiro$nid)
# 
# nrow(rutsiro_id_check)
# 
# n_distinct(rutsiro_epc$villageid_key)
# n_distinct(karongi_epc$villageid_key)
# n_distinct(karongi$villageid_key)
# n_distinct(rutsiro$villageid_key)
# 
# #hfc which were accomplished
# 
# complete  <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","vulnerable households in sample villages.xlsx"))
# 
# complete <- complete %>% 
#  filter(`Completed by Lattanzio` == "Yes")
# 
# karongi_surveyed <-  karongi_hh %>% 
#   filter(
#     villageid_key %in% karongi_epc$villageid_key,
#     nid %in% complete$nid
#   )
# 
# rutsiro_surveyed <-  rutsiro %>%
#   filter(
#     villageid_key %in% rutsiro_epc$villageid_key,
#     nid %in% complete$nid
#   )
# 
# epc <- complete %>% 
#   filter(
#     nid %in% karongi_epc$nid | nid %in% rutsiro_epc$nid
#   ) %>% 
#   group_by(district) %>% 
#   summarise( n = n())
#  
# rusizi_prob <- complete %>% 
#   filter(
#     nid %in% karongi_epc$nid | nid %in% rutsiro_epc$nid
#   ) %>%
#   filter(district == "Rusizi")
# 
# 
# rutsiro_match <- rutsiro_epc %>% 
#   filter(
#     !nid %in% rutsiro$nid
#   )
# 
# 
# karongi_match <- karongi_epc %>%
#   filter(
#     !nid %in% karongi$nid
#   )
