########################################################################################################
#                                                                                                      #
#                                HIGH-FREQUENCY CHECKS  -- CONSTRUCT/CLEAN                             #
#                                                                                                      #
########################################################################################################


## PURPOSE      High-frequency checks

## AUTHOR      Xiaoming Zhang (adapted from Adrien Ciret & Marc-Andrea Fiorina)

## LAST UPDATE  Feburary 20th, 2024


########################################################################################################

## libraries

library(tidyverse)
library(lubridate)
library(googlesheets4)



# 1. Import Data ----
dropbox <- 'C:/Users/wb614406/Dropbox'

## Raw High Frequency Data (REMEMBER TO REENCRYPT)
raw <- read.csv(file.path(dropbox,
                           'Rwanda Energy/datawork/RCT_data/baseline/HFC"'),
                 stringsAsFactors = FALSE)


# Variables submissiondate, starttime, endtime are recorded in dmy hms hence treated accordingly
date_variables_dmy <- raw %>%
  
  select(
    
    SubmissionDate, starttime, endtime
    
  )%>%
  
  names()

#Change date and timezone

raw <- raw %>%
  mutate(
    across(
      date_variables_dmy,
      
      ~ (mdy_hms(.,
                 
                 tz = Sys.timezone() ))))


# Module specific time stamps are recorded in ymd hms hence treated accordingly
date_variables_ymd <- raw %>%
  
  select(
    
    ends_with("_start"), starts_with("end")
    
  ) %>% names()


raw <- raw %>%
  
  mutate(across(date_variables_ymd,
                
                ~ (ymd_hms(.,
                           
                           tz = Sys.timezone() )
                   
                )))
#   

## 1. Overall Construction ----

hfc_constr <- raw #%>% filter(starttime >=as.Date("2021-07-08"))


## Date Stuff - extracting dates from date-time variable

hfc_constr <- hfc_constr %>%
  
  mutate(
    
    submissiondatetime     = SubmissionDate,
    
    submissiondate         = lubridate::date(submissiondatetime),
    
    startdate              = lubridate::date(starttime),
    
    enddate                = lubridate::date(endtime)
  ) 

## Module Duration -- TO BE ADAPTED IF THERE ARE CHANGES TO TIME STAMPS

hfc_constr$survey_dur <- with(hfc_constr, round(as.numeric(difftime(end_survey, survey_start, units = "mins"))), 2)

hfc_constr$id_dur <- with(hfc_constr, round(as.numeric(difftime(end_mod_a_roster, id_start, units = "mins"))), 2)

hfc_constr$roster_dur <- with(hfc_constr, round(as.numeric(difftime(end_hh_roster, hh_roster_start, units = "mins"))), 2)

hfc_constr$land_dur <- with(hfc_constr, round(as.numeric(difftime(end_land, land_start, units = "mins"))), 2)

hfc_constr$land_old_own_dur <- with(hfc_constr, round(as.numeric(difftime(end_land_old_own, land_old_own_start, units = "mins"))), 2)

hfc_constr$crops_livestock_dur <- with(hfc_constr, round(as.numeric(difftime(end_crops_livestock, crops_livestock_start, units = "mins"))), 2)

hfc_constr$permanent_crop_dur <- with(hfc_constr, round(as.numeric(difftime(end_permanent_crop, permanent_crop_start, units = "mins"))), 2)

hfc_constr$seasonal_crops_dur <- with(hfc_constr, round(as.numeric(difftime(end_seasonal_crops, seasonal_crops_start, units = "mins"))), 2)

hfc_constr$inputs_dur <- with(hfc_constr, round(as.numeric(difftime(end_inputs, inputs_start, units = "mins"))), 2)

hfc_constr$livestock_dur <- with(hfc_constr, round(as.numeric(difftime(end_livestock, livestock_start, units = "mins"))), 2)

hfc_constr$access_dur <- with(hfc_constr, round(as.numeric(difftime(end_access, access_start, units = "mins"))), 2)


## Negative duration due to SCTO issues

hfc_constr <- hfc_constr %>%
  
  mutate(
    
    negative_durs = rowSums(select(., ends_with("dur")) < 0, na.rm = TRUE),
    
    negative_mods = NA_character_
    
  )

for(i in 1:nrow(hfc_constr)) {
  
  if(hfc_constr$negative_durs[i] > 0) {
    
    negative_modules <- hfc_constr %>%
      
      slice(i) %>%
      
      select(ends_with("dur") & which(. < 0)) %>%
      
      names() %>%
      
      str_to_upper() %>%
      
      pluck(1)
    
    hfc_constr$negative_mods[i] <- negative_modules
    
  }
  
}


hfc_constr <- hfc_constr %>%
  
  mutate(
    
    across(
      
      ends_with("dur"),
      
      ~ case_when(
        
        .x < 0 ~ NA_real_,
        
        TRUE   ~ as.numeric(.x)
        
      )
      
    )
    
  )


## Average Duration by Module Overall
duration_mods <- hfc_constr %>%
  
  select(ID_05,ID_05_enter, SubmissionDate, ends_with("dur")) %>%
  
  arrange(SubmissionDate) 



hfc_sheet <- googledrive::drive_get(paste0("HFC_HHSurvey_23"))
# In case googledrive function asks me to enter '1' to continue

1

hfc_sheet %>%
  
  sheet_write(data = duration_mods, sheet = "duration_data")

# In case googlesheets4 asks me to enter '1' to continue
1

## Marked Complete 
## Encoding Consent Variable - # Consent is imported as character type, encode Yes = 1 , No = 0
hfc_constr <- hfc_constr %>%
  mutate(survey_complete_c=ifelse(survey_complete==1,'Yes','No'),
         consent_c=ifelse(consent==1,'Yes','No'))




## Enumerator Names 

enumerators <- read.csv(file.path(dropbox,
                                  'Rwanda Roads Data/Primary data/HH survey/endline/data/pilot/enumerator_list.csv'))

# add enumerator name 
hfc_constr = hfc_constr %>% 
  left_join(enumerators,by=c('ID_03'='id'))

########################################################################################################

## 5. Export Data duplicates files ----


duplicates = hfc_constr %>% filter(consent==1) %>% 
  group_by(ID_05_enter) %>% mutate(n=n()) %>% 
  filter(n>1) %>% 
  select(ID_05,ID_05_enter,ID_03,enumerator_name,pl_id_06,pl_id_07,pl_id_08,pl_id_09,pl_id_11,
         starttime,survey_date) %>% 
  rename(HHID=ID_05) %>% 
  filter(!is.na(ID_05_enter))

hfc_sheet %>%
  
  sheet_write(data = duplicates, sheet = "Duplicates")

1

# export duplicates
write.csv(duplicates,
          file.path(dropbox,
                    'Rwanda Roads Data/Primary data/HH survey/endline/data/raw/outputs/duplicates_0914.csv'),
          row.names=F)

# export clean data
# write.csv(hfc_constr,
#           'C:/Users/wb605157/Dropbox/Rwanda Roads Data/Primary data/HH survey/endline/data/raw/hh_data_clean.csv',
#           row.names=F)
# 