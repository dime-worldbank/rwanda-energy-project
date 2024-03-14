########################################################################################################
#                                                                                                      #
#                                HIGH-FREQUENCY CHECKS  -- CONSTRUCT/CLEAN                             #
#                                                                                                      #
########################################################################################################


## PURPOSE      High-frequency checks

## AUTHOR      Xiaoming Zhang (adapted from Adrien Ciret & Marc-Andrea Fiorina)

## LAST UPDATE  March, 13th, 2024


########################################################################################################

## libraries

library(tidyverse)
library(lubridate)
library(googlesheets4)



# 1. Import Data ----
dropbox <- 'C:/Users/wb614406/Dropbox'

## Raw High Frequency Data (REMEMBER TO REENCRYPT)
raw <- read.csv(file.path(dropbox,
                           'Rwanda Energy/datawork/RCT_data/baseline/data/HFC/REP_baseline_test_WIDE.csv'),
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

hfc_constr$survey_dur <- with(hfc_constr, round(as.numeric(difftime(endtime, starttime, units = "mins"))), 2)

# hfc_constr$id_dur <- with(hfc_constr, round(as.numeric(difftime(end_mod_a_roster, id_start, units = "mins"))), 2)

hfc_constr$roster_dur <- with(hfc_constr, round(as.numeric(difftime(A_endtime, A_starttime, units = "mins"))), 2)

hfc_constr$health_dur <- with(hfc_constr, round(as.numeric(difftime(B1_endtime, B1_starttime, units = "mins"))), 2)

hfc_constr$housing_assets_dur <- with(hfc_constr, round(as.numeric(difftime(C_endtime, C_starttime, units = "mins"))), 2)

hfc_constr$business_dur <- with(hfc_constr, round(as.numeric(difftime(D_endtime, D_starttime, units = "mins"))), 2)

hfc_constr$savings_dur <- with(hfc_constr, round(as.numeric(difftime(E_endtime, E_starttime, units = "mins"))), 2)

hfc_constr$mobile_dur <- with(hfc_constr, round(as.numeric(difftime(F_endtime, F_starttime, units = "mins"))), 2)

hfc_constr$agriculture_dur <- with(hfc_constr, round(as.numeric(difftime(G_endtime, G_starttime, units = "mins"))), 2)

hfc_constr$energy_dur <- with(hfc_constr, round(as.numeric(difftime(H_endtime, H_starttime, units = "mins"))), 2)

hfc_constr$cleancooking_dur <- with(hfc_constr, round(as.numeric(difftime(I_endtime, I_starttime, units = "mins"))), 2)

hfc_constr$wtp_dur <- with(hfc_constr, round(as.numeric(difftime(J_endtime, J_starttime, units = "mins"))), 2)

hfc_constr$mental_health_dur <- with(hfc_constr, round(as.numeric(difftime(B2_endtime, B2_starttime, units = "mins"))), 2)

hfc_constr$social_desirability_dur <- with(hfc_constr, round(as.numeric(difftime(K_endtime, K_starttime, units = "mins"))), 2)

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
  
  select(ID_02,ID_03, SubmissionDate, ends_with("dur")) %>% #This is adding household ID number
  
  arrange(SubmissionDate) 



hfc_sheet <- googledrive::drive_get(paste0("HFC_REP_24"))
# In case googledrive function asks me to enter '1' to continue

1

hfc_sheet %>%
  
  sheet_write(data = duration_mods, sheet = "duration_data")

# In case googlesheets4 asks me to enter '1' to continue
1

## Marked Complete 
## Encoding Consent Variable - # Consent is imported as character type, encode Yes = 1 , No = 0
hfc_constr <- hfc_constr %>%
  mutate(survey_complete_c=ifelse(finish ==1,'Yes','No'),
         consent_c=ifelse(consent==1,'Yes','No'))




## Enumerator Names 

enumerators <- read.csv(file.path(dropbox,
                                  "Rwanda Energy/questionnaires/cto attachments/pilot_enumerators.csv"))

# add enumerator name 
hfc_constr <- hfc_constr %>% 
  left_join(enumerators,by=c('ID_01'='enumeratorid_key'))

########################################################################################################

## 5. Export Data duplicates files ----


duplicates <- hfc_constr %>% 
  filter(consent==1) %>% 
  group_by(ID_03) %>%  #household id
  mutate(n=n()) %>% 
  filter(n>1) %>% 
  select(ID_02,enumerator_key,ID_07, ID_08, ID_09, ID_10,
         starttime, SubmissionDate, n) %>% 
  rename(HHID = ID_02) %>% 
  filter(!is.na(ID_03))
  
  
  
  
hfc_sheet %>%
  
  sheet_write(data = duplicates, sheet = "duplicates")

1

# export duplicates
write.csv(duplicates,
          file.path(dropbox,
                    'Rwanda Energy/datawork/RCT_data/baseline/data/HFC/duplicates_pilot.csv'),
          row.names=F)

# export clean data
# write.csv(hfc_constr,
#           'C:/Users/wb605157/Dropbox/Rwanda Roads Data/Primary data/HH survey/endline/data/raw/hh_data_clean.csv',
#           row.names=F)
# 