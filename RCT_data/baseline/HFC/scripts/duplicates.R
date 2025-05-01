#Duplicate check-----


duplicates_enumerator <- hfc_constr %>% 
  filter(consent == 1) %>% 
  group_by(hh_id) %>%  # Group by household ID
  mutate(n = n_distinct(enumerator)) %>%  # Count distinct enumerators for each household
  filter(n > 1) %>%  # Keep only households with more than 1 enumerator
  ungroup() %>% 
  select(enumerator, enumerator_key, district, district_key, sector, sector_key, 
         cell, cell_key, village, village_key, hh_id, hh_head_name, A1_2, A1_3,
         starttime, submissiondate, n) %>% 
  rename(
    grid_connect = A1_2,
    offgrid_connect = A1_3
  ) %>% 
  arrange(desc(hh_id))

# Write to Google Sheet
hfc_sheet %>%
  sheet_write(data = duplicates_enumerator, sheet = "duplicates_enumerator")


1

duplicates_general <- hfc_constr %>% 
  filter(hh_id %in% duplicates_enumerator$hh_id | hh_id %in% duplicates$hh_id) %>% 
  select(starttime, endtime, submissiondate, enumerator, enumerator_key,
         district, sector, cell, village, hh_id, consent, finish, location,phonenumber,comment,
         
         
         
         #Duration
         survey_duration,
         roster_duration,
         energy_duration,
         wellbeing_duration,
         willingness_duration,
         housing_assets_duration,
         business_duration,
         savings_duration,
         mobile_duration,
         livestock_duration,
         cleancooking_duration,
         mental_health_duration,
         social_desirability_duration,
         all_duration,
         
         
         #household head roster
         starts_with("A1"), hh_head_name, gender, marital, head_age_calculate, education, high_edu,  starts_with("A2"),
         starts_with("A3"),
         
         #energy wellbeing
         starts_with("B4"), starts_with("B5"),
         
         #housing assets
         starts_with("C1"), starts_with("C2"), starts_with("C3"),
         
         #business
         starts_with("D1"), starts_with("D2"), starts_with("D3"),starts_with("D4"),
         
         #savings
         starts_with("E1"), formal_savings, informal_savings, starts_with("E2"), starts_with("E3"), starts_with("E4"),
         
         
         # #mobile
         # starts_with("F"),
         
         #land and agriculture
         starts_with("G"),
         
         #Energy
         starts_with("H"),
         
         #clean_cooking
         starts_with("I"),
         
         #willingness
         
         starts_with("J"),
         
         #mental health
         starts_with("B1"), starts_with("B2"), starts_with("B3"),
         
         #desirability
         starts_with("K")
         
  ) %>% 
  arrange(desc(hh_id))






write_xlsx(duplicates_general, path = file.path(data_path, "duplicates_general_01082025.xlsx"))

duplicate_issue <- read_xlsx(path = file.path(data_path, "duplicates_general.xlsx"), sheet = "need_report")

duplicate_need_report <- hfc_constr %>% 
  filter(hh_id %in% duplicate_issue$hh_id) %>% 
  select(starttime, endtime, submissiondate, enumerator, enumerator_key,
         district, district_key, sector, sector_key, cell, cell_key, village, village_key, hh_id, phonenumber, comment
         
  ) %>% 
  arrange(desc(hh_id))

write_xlsx(duplicate_need_report, path = file.path(data_path, "duplicate_report.xlsx"))



#411503010308----



duplicate <- hfc_constr %>% 
  filter(hh_id == "411503010308")



audio_ntahondi <- hfc_constr %>% 
  mutate(
    audio_check = ifelse(grepl("media", audio, ignore.case = TRUE), audio, NA)
  ) %>% 
  filter(
    hh_id == "411503010308"
  ) %>% 
  select(
    district, district_key, sector, sector_key, cell, cell_key, village, village_key, hh_id, submissiondate, enumerator, enumerator_key, phonenumber, audio
  ) %>% 
  mutate(
    hh_id = as.character(hh_id)
  ) 


# Create a new folder with the date as the name
audio_folder <- file.path("C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/questionnaires/REP Backcheck survey/Audio Check/411503010308.xlsx")
dir.create(audio_folder, showWarnings = FALSE)

# Define the media folder path
source_folder <- "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/HFC/data/media"

# Extract the filenames (without 'media\' prefix)
audio_filenames <- basename(audio_ntahondi$audio)

# Full paths to the audio files in the media folder
audio_files <- file.path(source_folder, audio_filenames)

# Check which files exist and filter them
existing_files <- audio_files[file.exists(audio_files)]

# Copy the filtered files to the new folder
file.copy(existing_files, audio_folder)

write_xlsx(audio_ntahondi, path = file.path(audio_folder, "audiocheck_list.xlsx"))


#Solution to duplicates----


hfc_duplicate_free <- hfc_constr %>% 
  distinct(hh_head_name, hh_id, A1_2, A1_3, .keep_all = TRUE)

  
  
  # %>% 
  # group_by(hh_id) %>% 
  # summarise(n = n()) %>% 
  # filter(n > 1) 

#Duplicate output----


duplicate_output <- duplicates_general %>% 
  filter(!is.na(gender)) %>% 
  group_by(hh_id) %>% 
  filter(n_distinct(hh_head_name) > 1) %>% 
  ungroup() %>% 
  select(starttime, endtime, submissiondate, enumerator, enumerator_key,
         district, sector, cell, village,  hh_id, hh_head_name, phonenumber, comment
         
  ) %>% 
  arrange(desc(hh_id))

write_xlsx(duplicate_output, path = file.path(data_path, "duplicate_report_01272025.xlsx"))











