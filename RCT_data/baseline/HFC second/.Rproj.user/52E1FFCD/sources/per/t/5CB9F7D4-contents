

screening_survey <- read_xlsx(path= file.path(data_path_2, "screening_survey.xlsx")) 
eligible_households <- read_xlsx(path = file.path(data_path_2, "Eligible_households.xlsx"))

complete_second_round <-  hfc_constr %>% 
  filter(finish == 1) %>% 
  filter(consent == 1) %>% 
  filter(!is.na(A1_1)) %>% 
  distinct(hh_head_name, hh_id, A1_2, A1_3, .keep_all = TRUE)


complete.1 <- read_xlsx(path = file.path(data_path_2, "vulnerable households in sample villages.xlsx")) %>% 
  mutate(
    `Screened second round` = ifelse(household_id %in% screening_survey$hh_id, "Yes", "No"),
    `Eligible for second round` = ifelse(household_id %in% eligible_households$hh_id, "Yes", "No"),
    `Completed second round` = ifelse(household_id %in% complete_second_round$hh_id, "Yes", "No"),
    `In Fully Completed Village.1` = ifelse(villageid_key %in% complete_village$villageid_key, "Yes", "No")
  )





left_out <- complete.1 %>% 
  filter(
    `Screened second round` == "No" & 
      `Approached by Lattanzio` == "No"& 
      `In Fully Completed Village` == "No" & 
      `Dropped from scope due to 15kv` == "No"
  )
