

#For all villages----

##status----
screening_survey_village <- screening_survey %>% 
  group_by(village) %>% 
  summarise(
    hh_screened = n(),
    hh_eligible = sum(status == "Eligible", na.rm = TRUE),
    hh_not_eligible = sum(status == "Not Eligible", na.rm = TRUE),
    hh_not_in_deployment_list = sum(status == "Not in Deployment List", na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    village = as.character(village)
  )



village_check <- deployment_villages %>% 
  left_join(
    screening_survey_village,
    by =  c("villageid_key" = "village")
  ) %>% 
  mutate(
    percent_eligible = paste0(round(hh_eligible / hh_screened * 100, 2), "%")
  )

##by date survey----

screening_survey_by_day <- screening_survey %>% 
  group_by(village, startdate) %>% 
  summarise(
    hh_screened = n(),
    .groups = "drop"
  )

screening_survey_by_day_wide <- screening_survey_by_day %>%
  pivot_wider(
    names_from = startdate,
    values_from = hh_screened,
    values_fill = list(hh_screened = 0)  # Fill missing combinations with 0
  ) %>%
  mutate(
    village = as.character(village)
  )

village_check <- village_check %>% 
  left_join(
    screening_survey_by_day_wide,
    by = c("villageid_key" = "village")
  ) %>% 
  mutate(
    surveys_expected = ifelse(hh_eligible > 20, 20, hh_eligible)
  ) %>% 
  select(
    villageid_key, village, cell, sector, district, hh_tobe_surveyed, hh_screened, hh_eligible,percent_eligible, surveys_expected, hh_not_eligible, hh_not_in_deployment_list,  everything()
  ) 


hfc_sheet %>%
  
  sheet_write(data = village_check, sheet = "Village check")



#District----

screening_district_check <- screening_survey %>% 
  group_by(district_key) %>% 
  summarise(
    hh_screened = n(),
    hh_eligible = sum(status == "Eligible", na.rm = TRUE),
    hh_not_eligible = sum(status == "Not Eligible", na.rm = TRUE),
    hh_not_in_deployment_list = sum(status == "Not in Deployment List", na.rm = TRUE)
  ) %>% 
  ungroup() 

district_check <-
  bind_rows(screening_district_check, 
            screening_district_check %>%
              summarise(
                district_key = "Total",
                hh_screened = sum(hh_screened, na.rm = TRUE),
                hh_eligible = sum(hh_eligible, na.rm = TRUE),
                hh_not_eligible = sum(hh_not_eligible, na.rm = TRUE),
                hh_not_in_deployment_list = sum(hh_not_in_deployment_list, na.rm = TRUE)
              )
  ) %>% 
  mutate(
    percent_eligible = paste0(round(hh_eligible / hh_screened * 100, 2), "%")
  )


hfc_sheet %>%
  
  sheet_write(data = district_check, sheet = "District check")


#Filter for only eligible households----

household_head <- read.csv(file.path(data_path, "household_head_baseline2024.csv"))


household_head <- household_head %>% 
  select(household_id, first_name, last_name, nid)


screening_survey <- left_join(screening_survey, 
                              household_head, 
                              by = c("hh_id" = "household_id"))


eligible_households <- screening_survey %>% 
  filter(status == "Eligible") %>% 
  select(
    district, district_key, sector, sector_key, cell, cell_key, village, village_key, hh_id, phonenumber, second_phonenumber, submissiondate, startdate, first_name, last_name, nid
  ) 



eligible_single <- screening_survey %>% 
  filter(hh_id == "410101090141" | hh_id == "360203040402" | hh_id == "360203050100") %>% 
  select(
    district, district_key, sector, sector_key, cell, cell_key, village, village_key, hh_id, phonenumber, second_phonenumber, submissiondate, startdate, first_name, last_name, nid
  )  %>% 
  distinct(hh_id, .keep_all = TRUE)

eligible_households <- rbind(eligible_households, eligible_single)

hfc_sheet %>%
  
  sheet_write(data = eligible_households, sheet = "Eligible households")


write_xlsx(eligible_households, 
           path = file.path(output_path, "Eligible_households.xlsx"))


