########################################################################################################
#                                                                                                      #
#                HIGH-FREQUENCY CHECKS AND BACKCHECK COMPARISON                                        #
#                                                                                                      #
########################################################################################################

## PURPOSE      Comparison between main HH survey and BC survey
## AUTHOR      Xiaoming Zhang (adapted from Juliana Guerrero, Adrien Ciret & Marc-Andrea Fiorina)
## LAST UPDATE  October 9, 2024

########################################################################################################


##########################################################################
## data
##########################################################################

##output path

bc_output <- file.path("C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/questionnaires/REP Backcheck survey/Backcheck List")


##Backcheck select for the next day----


set.seed(0624)

household_head <- read.csv(file.path(data_path, "household_head.csv"))

head_join <- household_head %>% 
  select(
    household_id, first_name, last_name
  )  %>% 
  rename(
    first = first_name,
    last = last_name
  )
# Select 20 households for the main list
main_select <- hfc_constr %>% 
  filter(
    submissiondate >= Sys.Date()
  ) %>% 
  select(
    district, district_key, sector, sector_key, cell, cell_key, village, village_key,
    hh_id, locate, consent,  first_name, last_name, phonenumber, second_phonenumber, submissiondate, startdate
  ) %>% 
  sample_n(min(n(), 25)) %>% 
  left_join(head_join, by = c("hh_id" = "household_id"))  %>% 
  select(-first_name, -last_name)

# Write to the same Excel file as separate sheets
write_xlsx(
  main_select,
  path = file.path(bc_output,paste0("backcheck_", format(Sys.Date() , "%Y%m%d"), ".xlsx")))




#Household_head.csv----
# household_yesterday <- read_xlsx(path = file.path(bc_output, paste0("household_head", "20241220", ".xlsx")))

household_yesterday <- read_xlsx(path = file.path(bc_output, paste0("household_head", format(Sys.Date() - 1, "%Y%m%d"), ".xlsx")))

household_join <- rbind(main_select, household_yesterday)

write_xlsx(household_join, path = file.path(bc_output, paste0("household_head", format(Sys.Date() , "%Y%m%d"), ".xlsx")))

write.csv(household_join, "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/questionnaires/REP Backcheck survey/cto attachments/household_head.csv" )

# 
# 
# 
# ##Audio check----
# 
# main_select <- main_select %>% 
#   mutate(hh_id = as.numeric(household_id))
# 
# audio <- anti_join(hfc_constr, main_select, by = ("hh_id"))
# 
#   audio_select <- audio %>% 
#   mutate(
#     audio_check = ifelse(grepl("media", audio, ignore.case = TRUE), audio, NA)
#   ) %>% 
#     filter(!is.na(audio_check)) %>% 
#     filter(submissiondate >= "2025-01-10") %>% 
#   select(
#     district, district_key, sector, sector_key, cell, cell_key, village, village_key, hh_id, submissiondate, enumerator, enumerator_key, phonenumber, audio
#   ) %>% 
#   mutate(
#     hh_id = as.character(hh_id)
#   ) %>%
#   sample_n(min(n(), 20))
# 
# 
# # Create a new folder with the date as the name
# audio_folder <- file.path("C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/questionnaires/REP Backcheck survey/Audio Check", format(Sys.Date(), "%Y%m%d"))
# dir.create(audio_folder, showWarnings = FALSE)
# 
# # Define the media folder path
# source_folder <- "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/HFC/data/media"
# 
# # Extract the filenames (without 'media\' prefix)
# audio_filenames <- basename(audio_select$audio)
# 
# # Full paths to the audio files in the media folder
# audio_files <- file.path(source_folder, audio_filenames)
# 
# # Check which files exist and filter them
# existing_files <- audio_files[file.exists(audio_files)]
# 
# # Copy the filtered files to the new folder
# file.copy(existing_files, audio_folder)
# 
# write_xlsx(audio_select, path = file.path(audio_folder, "audiocheck_list.xlsx"))
# 
# 




##Backcheck data clean----

raw <- read.csv(file = file.path(data_path, "REP_backcheck_WIDE.csv"))


# 1. Construction----
#Variables submissiondate, starttime, endtime are recorded in dmy hms hence treated accordingly
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
      ~ (mdy_hms(.,tz = Sys.timezone() ))))


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


## Date filter

hfc_constr_bc <- raw %>%
  filter(starttime >= as.Date("2024-11-11"))

##NA
hfc_constr_bc <- hfc_constr_bc %>%
  mutate(
    across(
      where(is.numeric),
      ~ case_when(
        .x %in% c(-66, -88, -77, -99) ~ NA_real_,
        TRUE                     ~ .x
      )
    )
  )

## Date construct
hfc_constr_bc <- hfc_constr_bc %>%
  mutate(
    submissiondatetime     = SubmissionDate,
    submissiondate         = lubridate::date(submissiondatetime),
    startdate              = lubridate::date(starttime),
    enddate                = lubridate::date(endtime)
  ) 





#Join enumerators
enumerators <- read.csv(file.path(data_path, "pilot_enumerators_bc.csv"))

# add enumerator name 
hfc_constr_bc <- hfc_constr_bc %>% 
  left_join(enumerators,by=c('enumerator'='enumeratorid_key'))


##Village-key

village <- read.csv(file.path(
  data_path, "admin_raw.csv"
))

village<- village %>% 
  select( ends_with("key"))

hfc_constr_bc <- left_join(hfc_constr_bc, village, by = c("village" = "villageid_key",
                                                    "cell" = "cellid_key",
                                                    "sector" = "sectorid_key",
                                                    "district" = "districtid_key"))


enumerator_bc <- hfc_constr_bc %>% 
  group_by(enumerator, enumerator_key, submissiondate) %>% 
  summarise(num_surveyed = n(), .groups = "drop") %>% 
  pivot_wider(
    names_from = submissiondate,   
    values_from = num_surveyed,
    values_fill = list(num_surveyed = 0)
  ) %>% 
  mutate(Total = rowSums(select(., -enumerator, -enumerator_key))) %>% 
  relocate(Total, .after = enumerator_key)

write_xlsx(enumerator_bc, path = file.path(data_path, "enumerator_backcheck_progress.xlsx"))
# 2. Backcheck full data ----
backcheck_select <- hfc_constr_bc %>% 
  select(
    hh_id, submissiondate, formdef_version, enumerator, enumerator_key, surveyed, 
    A1_1, H1_1, H2_1, H4_1, H5_1, H6_1, H8_6, H8_7, 
    D1_1, E1_1, formal_savings, informal_savings, 
    F1_1, G1_1, G1_2, G1_3
  ) %>% 
  rename_with(~ paste0(., "_bc"), 
              -c(hh_id, submissiondate)) %>% 
  filter(
    formdef_version_bc >= 2024100807
  ) 


main_select <- hfc_constr %>% 
  select(
    hh_id, district_key, sector_key, cell_key, village_key, village, district,  submissiondate, formdef_version, enumerator, enumerator_key, 
    A1_1, H1_1, H2_1, H4_1, H5_1, H6_1, H8_6, H8_7, 
    D1_1, E1_1, formal_savings, informal_savings, 
    F1_1, G1_1, G1_2, G1_3
  )

backcheck_join <- left_join(backcheck_select, main_select, by = c("hh_id", "submissiondate"))


backcheck_data <- backcheck_join%>% 
  mutate(
      issue = case_when(
        A1_1 != A1_1_bc ~ "Household member number mismatch",
        H1_1 != H1_1_bc ~ "Grid connection mismatch", 
        H2_1 != H2_1_bc ~ "Solar connection mismatch",
        H4_1 != H4_1_bc ~ "Candel mismatch", 
        H5_1 != H5_1_bc ~ "Biomass mismatch",
        H6_1 != H6_1_bc ~ "Kerosenemismatch", 
        H8_6 != H8_6_bc ~ "lightbulb mismatch", 
        H8_7 != H8_7_bc ~ "lightbulb number mismatch", 
        D1_1 != D1_1_bc ~ "Business mismatch", 
        E1_1 != E1_1_bc ~ "Household banking mismatch", 
        formal_savings != formal_savings ~ "Formal savings mismatch", 
        informal_savings != informal_savings ~ "Informal savings mismatch", 
        F1_1 != F1_1_bc ~ "Tablet number mismatch", 
        G1_1 != G1_1_bc ~ "Land ownership mismatch", 
        G1_2 != G1_2_bc ~ "Land rental mismatch", 
        G1_3 != G1_3_bc ~ "Livestock mismatch",
        .default = "No issue found"
    )
  ) %>% 
  select(
      hh_id, submissiondate,  
      enumerator,enumerator_key, enumerator_bc, enumerator_key_bc, issue, 
      A1_1, A1_1_bc, H1_1, H1_1_bc, H2_1, H2_1_bc, H4_1, H4_1_bc, 
      H5_1, H5_1_bc, H6_1, H6_1_bc, H8_6, H8_6_bc, H8_7, H8_7_bc, 
      D1_1, D1_1_bc, E1_1, E1_1_bc, 
      formal_savings, formal_savings_bc, informal_savings, informal_savings_bc, 
      F1_1, F1_1_bc, G1_1, G1_1_bc, G1_2, G1_2_bc, G1_3, G1_3_bc, 
      district_key, sector_key, cell_key, village_key, village, district,
      formdef_version, formdef_version_bc
    )
  


hfc_sheet %>%
  sheet_write(data = backcheck_data, sheet = "backcheck_full_data")

1


#3. Backcheck summary----

backcheck_summary <- backcheck_data %>%
  group_by(submissiondate, issue) %>%
  summarise(
    n = n()
  ) %>% 
  ungroup() %>%
  pivot_wider(
    names_from = submissiondate,   
    values_from = n,               
    values_fill = 0                
  ) %>%
  mutate(total_n = rowSums(across(-issue))) %>% 
  select(issue, total_n, everything()) %>% 
  rename(
    `backcheck issue` = issue
  )

hfc_sheet %>%
  sheet_write(data = backcheck_summary, sheet = "backcheck_summary")

1







