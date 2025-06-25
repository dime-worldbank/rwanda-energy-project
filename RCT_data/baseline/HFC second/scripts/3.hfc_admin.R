########################################################################################################
#                                                                                                      #
#                     HIGH-FREQUENCY CHECKS  -- ADMINISTRATIVE-LEVEL HIGH-FREQUENCY CHECKS             #
#                                                                                                      #
########################################################################################################

## PURPOSE      Create one or multiple administrative-level check sheets in the HFC dashboard.
## District 
## Villages  
## Households

## AUTHOR      Xiaoming Zhang (adapted from Adrien Ciret & Marc-Andrea Fiorina & Juliana Guerrero)

## LAST UPDATE  April 16th , 2023

########################################################################################################

#Import Data ----
# 


## libraries

library(tidyverse)
library(lubridate)
library(googlesheets4)
library(readxl)
library(dplyr)
library(tidyr)

dropbox <- 'C:/Users/wb614406/Dropbox'

## Raw High Frequency Data (REMEMBER TO REENCRYPT)



admin_raw <- read.csv(file.path(
  data_path, "admin_raw.csv"
))

admin_raw <- admin_raw %>% 
  mutate(across(as.character()))

villageid_join <- admin_raw %>% 
  select(villageid_key, treatment)


hfc_constr <- hfc_constr %>% 
  mutate(
    finish = ifelse(is.na(A1_1), 0, finish)
  ) %>% 
  mutate(
    caseid = paste0(hh_head_name, hh_id, A1_2, A1_3)
  )
#1. Village ----

# For each geographic/administrative unit (in this case, village), we want to check:
# Number of submissions
# Number of submissions per day
# Progress (%age of expected surveys completed)

##Submission by day----
village_check_by_day <- hfc_constr %>%
  group_by(village, submissiondate) %>%
  summarize(
    num_surveys = n()
  ) %>% 
  ungroup() %>%
  pivot_wider(
    names_from = submissiondate,
    values_from = num_surveys
  ) %>%
  mutate(across(
    -village,
    ~ ifelse(is.na(.x), 0, .x)
  )) %>% 
  select(village, order(colnames(.)))


complete <- hfc_constr %>% 
  filter(finish == 1) %>% 
  filter(consent == 1) %>% 
  filter(!is.na(A1_1)) %>% 
  distinct(hh_head_name, hh_id, A1_2, A1_3, .keep_all = TRUE)

village_completion <- hfc_constr %>% 
  group_by(village) %>% 
  summarise(
    attempt = n(),
    complete = sum( caseid %in% complete$caseid, na.rm = TRUE),
    no_complete = sum(finish== 0), 
    no_locate = sum(locate ==0, na.rm = TRUE),
    have_electricity = sum(A1_2 == 1 | A1_3 == 1, na.rm = TRUE),
    completion_rate_secondround = paste0(round(complete / attempt, 3) * 100, "%")
  ) %>%
  ungroup()


village_check <- left_join(admin_raw, village_completion, by = c("villageid_key" = "village")) 

village_check <- village_check%>%
  left_join(village_check_by_day, by = c("villageid_key" = "village"))


#Join with first round and limit to 181 villages

first_round <- read_xlsx(path = file.path(data_path, "first_round.xlsx"))

first_round <- first_round %>% 
  rename(
    firstround_complete = n_firstround
  )

village_check <- left_join(village_check, first_round, by = c("villageid_key" = "village"))


village_check <- village_check %>%
  mutate(
    complete_all = rowSums(
      select(., complete, firstround_complete), 
      na.rm = TRUE
    )
  ) %>%
 
  mutate(
    progress = paste0(round(complete_all/ num_to_survey, 3) * 100, "%")
  ) %>% 
  filter(
    villageid_key %in% village_181$villageid_key
  ) %>% 
  select(
    X,provinceid_key, province_key, districtid_key, district_key,sectorid_key, sector_key,cellid_key, cell_key,
    
    villageid_key, village_key, treatment, hh_head_06, num_to_survey, attempt,
    complete,completion_rate_secondround,  firstround_complete, complete_all, progress, no_complete, no_locate,
    have_electricity,  everything()
  ) 


# saving output

hfc_sheet %>%
  
  sheet_write(data = village_check, sheet = "village_data")

1



#2. Treatment completion----

treatment_completion <- village_check %>% 
  group_by(treatment) %>% 
  summarise(
    num_to_survey = sum(num_to_survey, na.rm = TRUE),
    complete_all = sum(complete_all, na.rm = TRUE)
  ) %>% 
  mutate(
    progress = paste0(round(complete_all/num_to_survey, 3) * 100, "%")
  )

treatment_completion <- as.data.frame(treatment_completion) %>% 
  mutate(stats = rownames(treatment_completion)) %>% 
  select(stats, everything())

# saving output

hfc_sheet %>%
  
  sheet_write(data = treatment_completion, sheet = "treatment_data")

1



# 3. District----
##Submission by day----
district_check_by_day <- hfc_constr %>%
  group_by(districtid_key, submissiondate) %>%
  summarize(
    num_surveys = n()
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = submissiondate,
    values_from = num_surveys
  ) %>%
  mutate(across(
    -districtid_key,
    ~ ifelse(is.na(.x), 0, .x)
  )) %>%
  select(districtid_key, order(colnames(.)))


##Checks----



district_check<- village_check %>%
  group_by(districtid_key, district_key) %>%
  summarise(
    num_to_survey = sum(num_to_survey, na.rm = TRUE),
    attempt = sum(attempt, na.rm = TRUE),
    complete = sum(complete, na.rm = TRUE),
    firstround_complete = sum(firstround_complete, na.rm = TRUE),
    complete_all = sum(complete_all, na.rm = TRUE),
    completion_rate_secondround = paste0(round(complete / attempt, 3) * 100, "%")
  ) %>%
  ungroup()


district_check <- district_check%>%
  select(districtid_key, district_key, num_to_survey, everything()) %>% 
  mutate(
    progress = paste0(round(complete_all / num_to_survey, 3) * 100, "%")
  ) %>%
  left_join(district_check_by_day) 




# saving output

hfc_sheet %>%
  
  sheet_write(data = district_check, sheet = "district_data")

1




