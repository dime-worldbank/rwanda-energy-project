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



completion <- hfc_constr %>% 
  group_by(enumerator, enumerator_key) %>% 
  summarise(
    attempt = n(),
    complete = sum(finish == 1),
    no_complete = sum(finish == 0),
    consent = sum(consent == 1, na.rm = TRUE),
    no_consent = sum(consent == 0, na.rm = TRUE),
    completion_rate = paste0(round(complete / attempt, 3) * 100, "%"),
    consent_rate = paste0(round(consent / attempt, 3) * 100, "%")
  ) %>% 
  ungroup()

# enumerator_check <- left_join(enumerator_day, completion, by = c("enumerator", "enumerator_key"))


#3. location----

location <- hfc_constr %>% 
  select(enumerator, enumerator_key, submissiondate, hh_id, village, village_key, coordinate.Accuracy) %>% 
  filter(coordinate.Accuracy > 8)

#3. Quality----

#This is for enumerator general quality

enumerator_check <- completion %>% 
  mutate(
    duplicates = ifelse(enumerator %in% duplicates$enumerator, 1, 0),
    high_survey_length = ifelse(enumerator %in% high_length_check$enumerator, 1, 0),
    low_survey_length = ifelse(enumerator %in% low_length_check$enumerator, 1, 0), 
    coordinate_accuracy = ifelse(enumerator %in% location, 1, 0)
  )

#Daily submissions----

enumerator_check_by_day <- hfc_constr %>%
  group_by(enumerator, submissiondate) %>%
  summarize(
    num_surveys = n()
  ) %>% 
  ungroup() %>%
  pivot_wider(
    names_from = submissiondate,
    values_from = num_surveys
  ) %>%
  select(enumerator, order(colnames(.)))


enumerator_check <- left_join(enumerator_check, enumerator_check_by_day, by = c("enumerator") )


hfc_sheet %>%
  sheet_write(data = enumerator_check, sheet = "enumerator_quality")

1










