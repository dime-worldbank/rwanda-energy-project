##########################
###HFC issues
#11.17.2024
#Xiaoming Zhang
#######################################


pacman::p_load(knitr, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr)

getwd()

karongi <- read_xlsx(path = file.path(data_path, "Daily achievements_1125.xlsx"), sheet = "Karongi")
rutsiro <- read_xlsx(path = file.path(data_path, "Daily achievements_1125.xlsx"), sheet = "Rutsiro")
rulindo <- read_xlsx(path = file.path(data_path, "Daily achievements_1125.xlsx"), sheet = "Rulindo")
rusizi <- read_xlsx(path = file.path(data_path, "Daily achievements_1125.xlsx"), sheet = "Rusizi")


daily_achievements <- bind_rows(karongi, rutsiro, rulindo, rusizi)

day_mapping <- data.frame(
  input_value = c(paste0("Day ", 1:14), as.character(1:14)),
  date_value = rep(seq.Date(from = as.Date("2024-11-11"), 
                            to = as.Date("2024-11-24"), 
                            by = "days"), 2)
)

# Clean and transform `daily_achievements`
daily_achievements <- daily_achievements %>% 
  rename(
    date = ...1,
    order = ...2
  ) %>% 
  clean_names() %>% 
  mutate(across(everything(), str_to_title)) %>% 
  mutate(
    successful = ifelse(is.na(successful), "No", successful)
  ) %>% 
  mutate(
    reason_for_unsucessful = ifelse(successful == "Yes", NA, reason_for_unsucessful)
  ) %>% 
  # Replace `date` values based on the mapping
  left_join(day_mapping, by = c("date" = "input_value")) %>% 
  mutate(date = as.character(date_value)) %>% 
  select(-date_value)


for (i in 2:nrow(daily_achievements)) {
  if (is.na(daily_achievements$date[i])) {
    daily_achievements$date[i] = daily_achievements$date[i - 1]
  }
}

daily_achievements <- daily_achievements %>% 
  filter(!is.na(district))



#Other issues more -------
combine_reasons <- function(reason) {
  # Trim whitespace and ensure the input is clean
  reason <- trimws(reason)
  
  if (is.na(reason)) {
    return("Completed")
  } else if (grepl("died|passed away|deceased", reason, ignore.case = TRUE)) {
    return("Death-Related")
  } else if (grepl("migrated|migration", reason, ignore.case = TRUE)) {
    return("Migration-Related")
  } else if (grepl("not available|not found|wasn't around|not around", reason, ignore.case = TRUE)) {
    return("Unavailable")
  } else if (grepl("solar energy|has solar", reason, ignore.case = TRUE)) {
    return("Solar Energy Related")
  } else if (grepl("connected to grid|connected to the grid|grid|electricity", reason, ignore.case = TRUE)) {
    return("Grid Connection Related")
  } else if (grepl("old|speech impairment", reason, ignore.case = TRUE)) {
    return("Old Age-Related")
  } else if (grepl("tomorrow", reason, ignore.case = TRUE)) {
    return("Surveying Tomorrow")
  } else {
    return("Others")
  }
}


# Apply the function to the 'reason_for_unsucessful' column
daily_achievements$grouped_reason <- sapply(daily_achievements$reason_for_unsucessful, combine_reasons)

# View the results
table(daily_achievements$grouped_reason)

#Group by district----

issue_analysis <- daily_achievements %>% 
  group_by(grouped_reason, district) %>% 
  summarise(
    count = n(),
    .groups = 'drop'
  ) %>% 
  ungroup() %>% 
  # Calculate percentage within each district
  group_by(district) %>% 
  mutate(
    percent_issue = paste0(round(count / sum(count), 4) * 100, "%"),  # Percentage of observations within the district
    stats = paste0(count, " (", percent_issue, ")")
  ) %>% 
  ungroup() %>% 
  arrange(desc(count)) %>% 
  select(grouped_reason, district, stats) %>%
  pivot_wider(names_from = district, values_from = stats) 

summarise <- daily_achievements %>% 
  group_by(district) %>% 
  summarise(
    count = n()
  ) %>%   
  pivot_wider(names_from = district, values_from = count) %>% 
  mutate(
    grouped_reason = "Total"
  ) %>% 
  select(grouped_reason, Rulindo, Rutsiro, Rusizi, Karongi)

issue_analysis <- rbind(issue_analysis, summarise)




daily_track <- daily_achievements %>% 
  group_by(submissiondate) %>% 
  







# 
# #Group by issue
# issue_analysis_reason <- daily_achievements %>% 
#   group_by(grouped_reason) %>% 
#   summarise(
#     count = n(),
#   ) %>% 
#   mutate(
#     percent_issue = paste0(round(count / sum(count), 4) * 100, "%"),  # Percentage of observations within the district
#     stats = paste0(count, " (", percent_issue, ")")
#   ) %>% 
#   ungroup() %>% 
#   arrange(desc(count)) %>% 
#   select(grouped_reason,  stats) %>% 
#   rename(
#     total_count = stats
#   )
# 
# summarise_reason <- daily_achievements %>% 
#   summarise(
#     count = n()
#   ) %>%   
#   mutate(
#     grouped_reason = "Total"
#   ) %>% 
#   select(grouped_reason, count) %>% 
#   rename(
#     total_count = count
#   )
# 
# issue_analysis_reason <- rbind(issue_analysis_reason, summarise_reason)
# 
# 
# issue_analysis <- left_join(issue_analysis, issue_analysis_reason, by = c("grouped_reason"))
# 
# 
# 
# custom_order <- c("Completed", "Solar Energy Related", "Grid Connection Related","Migration-Related", "Unavailable", 
#                    "Old Age-Related", "Death-Related", "Others", "Total")
# 
# # Reorder rows based on the custom order
# issue_analysis <- issue_analysis %>%
#   mutate(grouped_reason = factor(grouped_reason, levels = custom_order)) %>%
#   arrange(grouped_reason)
# 
# 
# 
# write_xlsx(issue_analysis, path = "C:/Users/wb614406/Downloads/issue_analysis.xlsx")
# 
# 
# 



#Enumerator----


gloriose <- hfc_constr %>% 
  filter(enumerator == 31 & submissiondate >= "2024-11-11") %>% 
  select(
    district, district_key, sector, sector_key, cell, cell_key, village, village_key, hh_id, phonenumber, audio
  )



source_folder <- "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/HFC/data/media"

# Extract the filenames (without 'media/' prefix)
audio_filenames <- basename(gloriose$audio)

# Full paths to the audio files in the media folder
audio_files <- file.path(source_folder, audio_filenames)

# Check which files exist and filter them
existing_files <- audio_files[file.exists(audio_files)]

gloriose_folder <- "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/questionnaires/REP Backcheck survey/Audio Check/gloriose"

dir.create(gloriose_folder, showWarnings = FALSE)

# Copy the filtered files to the new folder
file.copy(existing_files, gloriose_folder)

write_xlsx(gloriose, path = file.path(gloriose_folder, "survey by gloriose.xlsx"))

write.csv(hfc_constr, file.path(data_path, "baseline_raw_11152024.csv"))



#Enumerator francine----


francine <- hfc_constr %>% 
  filter(enumerator == 11 & submissiondate >= "2024-11-11") %>% 
  select(
    district, district_key, sector, sector_key, cell, cell_key, village, village_key, hh_id, phonenumber, audio
  )

filter <- francine %>% 
  filter(hh_id %in% )

source_folder <- "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/HFC/data/media"

# Extract the filenames (without 'media/' prefix)
audio_filenames <- basename(francine$audio)

# Full paths to the audio files in the media folder
audio_files <- file.path(source_folder, audio_filenames)

# Check which files exist and filter them
existing_files <- audio_files[file.exists(audio_files)]

francine_folder <- "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/questionnaires/REP Backcheck survey/Audio Check/francine"

dir.create(francine_folder, showWarnings = FALSE)

# Copy the filtered files to the new folder
file.copy(existing_files, francine_folder)

write_xlsx(francine, path = file.path(francine_folder, "survey by francine.xlsx"))

write.csv(hfc_constr, file.path(data_path, "baseline_raw_11152024.csv"))




#For households with primary and secondary energy source distribution----

primary_energy <- hfc_constr %>%
  select(H3_1, H3_1_label,  A1_2, H1_1, A1_3, H2_1, submissiondate, village, village_key, hh_id, enumerator, enumerator_key, phonenumber) %>%
  filter(
    H3_1_label == "Solar home systems" | H3_1_label == "Grid"
  ) %>% 
   rename(
    "offgrid_question1" = A1_3,
    "offgrid_question2" = H2_1,
    "grid_question1" = A1_2,
    "grid_question2" = H1_1,
    "primary_energy" = H3_1,
   )


write_xlsx(primary_energy, path = file.path(logic_folder, "primary_energy.xlsx"))


elec_logic_check <- hfc_constr %>%
  filter( A1_2 != H1_1 | A1_3 != H2_1 | H8_6 != C3_14 ) %>%
  mutate(issue = "Electrification status & lightbulb unmatch") %>%
  select(hh_id, village, village_key, enumerator, enumerator_key, submissiondate, A1_2, H1_1, H3_1, H3_1_label, A1_3, H2_1, H8_6, C3_14,  audio) %>% 
rename(
  "offgrid_question1" = A1_3,
  "offgrid_question2" = H2_1,
  "grid_question1" = A1_2,
  "grid_question2" = H1_1,
  "primary_energy" = H3_1,
  "lightbulb1" = H8_6,
  "lightbulb2" = C3_14
)

source_folder <- "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/HFC/data/media"

# Extract the filenames (without 'media/' prefix)
audio_filenames <- basename(elec_logic_check$audio)

# Full paths to the audio files in the media folder
audio_files <- file.path(source_folder, audio_filenames)

# Check which files exist and filter them
existing_files <- audio_files[file.exists(audio_files)]

logic_folder <- "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/questionnaires/REP Backcheck survey/Audio Check/lighbulb unmatch"

dir.create(logic_folder, showWarnings = FALSE)

# Copy the filtered files to the new folder
file.copy(existing_files, logic_folder)

write_xlsx(elec_logic_check, path = file.path(logic_folder, "lightbulb mismatch.xlsx"))


#Replacement-----

##Graph----
four_scope<- read_xlsx(path ="C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/Updated scope villages& households/scope_193_0807.xlsx")
scope_villages <- read.csv("C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/questionnaires/cto attachments/scope_villages.csv")

filter <- scope_villages %>% 
  filter(!villageid_key %in% four_scope$village_id)


four_scope <- four_scope %>% 
  mutate(
    replacement = hh_head_06 -surveyed
  )

replacement <- four_scope %>% 
  select(replacement) %>% 
  mutate(
    replacement = ifelse(replacement >= 50, 50, replacement)
  ) %>% 
  mutate(
    replacement_10 = ifelse(replacement >= 10, 1, 0)
  )
# 
# Provide a table with the distributions of replacement available as a histogram 
# for all the villages in the sample. The x axis would be the number of replacement 
# households available and y axis is the number of villages. Let us add a yellow line 
# for the current cut off - 2. Let us also add a red line for 5, 8 10 and 12. 

p1 <- ggplot(replacement, aes(x = replacement)) +
  geom_bar(fill = "lightblue", color = "black") +
  geom_vline(xintercept = c(2), linetype = "dashed", color = "blue", size = 2) +
  geom_vline(xintercept = c(5), linetype = "dashed", color = "red", size = 2) +
  geom_vline(xintercept = c(8), linetype = "dashed", color = "red", size = 2) +
  geom_vline(xintercept = c(10), linetype = "dashed", color = "red", size = 2) +
  geom_vline(xintercept = c(12), linetype = "dashed", color = "red", size = 2) +
  labs(
    title = "Household Available for Replacement",
    x = "Number of Replacement Households",
    y = "Number of Villages"
  ) +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    axis.text.x = element_text(size = 28),
    axis.text.y = element_text(size = 28)
  )

p1


##For completed households----

complete_villages <- village_completion %>% 
  filter(attempt >= 18)

completed_replacement <- four_scope %>% 
  filter(
    village_id %in% complete_villages$village
  ) 

complete_villages <- complete_villages %>% 
  select(village, attempt) %>% 
  mutate(village = as.character(village))

completed_replacement <- left_join(completed_replacement, complete_villages, by = c("village_id" = "village"))

completed_replacement <- completed_replacement %>% 
  mutate(
    complete_replacement = hh_head_06 - attempt
  )


summary_df <- completed_replacement %>% 
  group_by(district) %>% 
  summarise(
    attempt = sum(attempt, na.rm = TRUE), 
    replacement = sum(complete_replacement, na.rm = TRUE)
  ) 

# Transpose the dataframe
transposed_df <- summary_df %>%
  as.data.frame() %>%                # Convert to a regular dataframe for transposition
  column_to_rownames("district") %>% # Set district as row names
  t() %>%                            # Transpose
  as.data.frame()   %>% 
  mutate(
    Total = rowSums(.))# Convert back to a dataframe

# Display the resulting dataframe
transposed_df



###HFC----

today <- hfc_constr %>% 
  filter(submissiondate == "2024-11-25") 


