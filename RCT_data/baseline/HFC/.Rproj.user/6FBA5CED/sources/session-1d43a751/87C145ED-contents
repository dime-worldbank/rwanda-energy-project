########################################################################################################
#                                                                                                      #
#                     HIGH-FREQUENCY CHECKS  -- ENUMERATOR HIGH-FREQUENCY CHECKS                       #
#                                                                                                      #
########################################################################################################

## PURPOSE      Create an enumerator high-frequency check sheet in the HFC dashboard.

## AUTHOR       Xiaoming Zhang(adapted from Adrien Ciret & Marc-Andrea Fiorina & Juliana Guerrero)

## LAST UPDATE  April 16th, 2024

########################################################################################################

# 1.Daily number ----

# For each enumerator, we want to check:
# Number of submissions
# Number of submissions per day
# Average duration of each module, average willingness to pay, average household number


hfc_constr <- hfc_constr %>% 
  mutate(
    enumerator_key = as.character(enumerator_key),
    enumerator = as.character(enumerator)
    
  )


enumerator_day <- hfc_constr %>%
  group_by(enumerator, enumerator_key, submissiondate) %>%
  summarize(
    num_surveys = n()
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from  = submissiondate,
    values_from = num_surveys
  ) %>%
  # mutate(
  #   across(
  #     enumerator,
  #     ~ case_when(
  #       is.na(.x) ~ 0,
  #       TRUE      ~ .x
  #     )
  #   )
  # ) %>%
  select(
    enumerator, enumerator_key, order(colnames(.)) # So that the days are ordered chronologically
  )



#2.Completion rate----

## Encoding Consent Variable - # Consent is imported as character type, encode Yes = 1 , No = 0
# hfc_constr <- hfc_constr %>%
#   mutate(
#     survey_complete_c=ifelse(finish ==1,'Yes','No'),
#     consent_c=ifelse(consent==1,'Yes','No')
#   )
# 
# 
# hfc_constr <- hfc_constr %>% 
#   mutate(
#     enumerator_key = ifelse(enumerator_key %in% "Other", enumerator_other, enumerator_key)
#   )

completion <- hfc_constr %>% 
  group_by(enumerator, enumerator_key) %>% 
  summarise(
    attempt = n(),
    complete = sum(finish == 1),
    no_complete = sum(finish == 0),
    consent = sum(consent == 1),
    no_consent = sum(consent == 0),
    completion_rate = paste0(round(complete / attempt, 3) * 100, "%"),
    consent_rate = paste0(round(consent / attempt, 3) * 100, "%")
  ) %>% 
  ungroup()

# enumerator_check <- left_join(enumerator_day, completion, by = c("enumerator", "enumerator_key"))




#3. Quality----

#This is for enumerator general quality

enumerator_check <- completion %>% 
  mutate(
    duplicates = ifelse(enumerator %in% duplicates$enumerator, 1, 0),
    high_survey_length = ifelse(enumerator %in% high_length_check$enumerator, 1, 0),
    low_survey_length = ifelse(enumerator %in% low_length_check$enumerator, 1, 0)
  )



hfc_sheet %>%
  sheet_write(data = enumerator_check, sheet = "enumerator_quality")

1









# enumerator_check <- enumerator_check %>% 
#   select(
#     enumerator, enumerator_key, num_surveys, starts_with(c("202")), 
#     complete, no_complete, consent, no_consent, completion_rate, consent_rate,
#     survey_duration,
#     roster_duration,
#     energy_duration,
#     wellbeing_duration,
#     willingness_duration,
#     housing_assets_duration,
#     business_duration,
#     savings_duration,
#     mobile_duration,
#     livestock_duration,
#     cleancooking_duration,
#     mental_health_duration,
#     social_desirability_duration,
#     A1_1, A1_2, A1_3, J1_final, J2_1, J3_1, J4_1, J5_1
# #   )
# 
# hfc_sheet %>%
#   sheet_write(data = enumerator_check, sheet = "enumerator_completion")
# 
# # In case googlesheets4 asks me to enter '1' to continue
# 1
# 
# 



# 
# #2. Primary data check: duration and averages
# enumerator_check <- hfc_constr %>%
#   group_by(enumerator, enumerator_key) %>%
#   summarize(
#     num_surveys = n(),
#     across(
#       c(
#         survey_duration,
#         roster_duration,
#         energy_duration,
#         wellbeing_duration,
#         willingness_duration,
#         housing_assets_duration,
#         business_duration,
#         savings_duration,
#         mobile_duration,
#         livestock_duration,
#         cleancooking_duration,
#         mental_health_duration,
#         social_desirability_duration,
#         A1_1, A1_2, A1_3, J1_final, J2_1, J3_1, J4_1, J5_1
#       ),
#       ~ round(mean(.x, na.rm = TRUE), 2), 
#       .names = "{col}_mean"
#     )
#   ) %>%
#   ungroup() %>%
#   left_join(
#     enumerator_check_by_day, by = c("enumerator")
#   )
# 
# 
# 
# #Export Data
# 
# hfc_sheet %>%
#   sheet_write(data = enumerator_check, sheet = "enum_data")
# 
# # In case googlesheets4 asks me to enter '1' to continue
# 
# 1

