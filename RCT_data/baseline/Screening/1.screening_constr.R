##############
#Author: Xiaoming Zhang
#Date: 6.13.2025
#Purpose: Compare EDCL list
#############


pacman::p_load(knitr, stargazer, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr, lfe)
library(googlesheets4)
getwd()


# Import Data ----
dropbox <- 'C:/Users/wb614406/Dropbox'




data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Screening/data"
)

output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Screening/outputs"
)

#Read files------

deployment_households <- read_xlsx(path = file.path(data_path, "Household and village list for deployment.xlsx"), sheet = "Household_List")
deployment_villages <-deployment_households %>% 
  group_by(villageid_key, village, cell, sector, district) %>% 
  summarise(
    hh_tobe_surveyed = n()
  ) %>% 
  ungroup()

scope_village <- deployment_villages %>% 
  select(-hh_tobe_surveyed) %>%
  mutate(district = str_to_title(district)) %>% 
  rename(
    district_key = district,
    sector_key = sector,
    cell_key = cell,
    village_key = village
  )

screening_raw <- read.csv(file.path(data_path, "REP_screening_WIDE.csv"))


#Data cleaning-----


date_variables_dmy <- screening_raw %>%
  select(
    SubmissionDate, starttime, endtime
  ) %>%
  names()

screening_raw <- screening_raw %>%
  mutate(
    across(
      all_of(date_variables_dmy),
      ~ mdy_hms(., tz = Sys.timezone())
    )
  )

date_variables_ymd <- screening_raw %>%
  select(
    ends_with("_start"), starts_with("end")
  ) %>%
  names()

screening_raw <- screening_raw %>%
  mutate(
    across(
      all_of(date_variables_ymd),
      ~ ymd_hms(., tz = Sys.timezone())
    )
  )
screening_raw <- screening_raw %>% 
  mutate(
    submissiondatetime     = SubmissionDate,
    submissiondate         = lubridate::date(submissiondatetime),
    startdate              = lubridate::date(starttime),
    enddate                = lubridate::date(endtime)
  ) 

screening_survey <- screening_raw %>% 
  filter(starttime >= as.Date("2025-06-16"))



screening_survey <- screening_survey %>% 
  mutate(
    across(where(is.character), ~ ifelse(.x == "", NA, .x))
  ) %>%
  mutate(
    village = as.character(village)
  ) %>% 
  left_join(
    scope_village,
    by = c("village" = "villageid_key")
  ) %>% 
  mutate(
    status = case_when(
      hh_id %in% deployment_households$household_id & !is.na(eligible) ~ "Eligible",
      hh_id %in% deployment_households$household_id & is.na(eligible) ~ "Not Eligible",
      !hh_id %in% deployment_households$household_id ~ "Not in Deployment List"
      
    )
  )

enumerator <- read.csv(file.path(data_path, "pilot_enumerators.csv")) %>% 
  select(
    enumerator_key, enumeratorid_key
  ) 

screening_survey <- left_join(
  screening_survey,
  enumerator,
  by = c("enumerator" = "enumeratorid_key")
)





not_in_deployment <- screening_survey %>% 
  filter(
    status == "Not in Deployment List"
  ) 

write_xlsx(not_in_deployment, path = file.path(output_path, "Not_in_Deployment_List.xlsx"))

hfc_sheet <- googledrive::drive_get(paste0("HFC_Screening_25"))


hfc_sheet %>%
  
  sheet_write(data = not_in_deployment, sheet = "Not in Deployment List")




