########################################################################################################
#                                                                                                      #
#                     HIGH-FREQUENCY CHECKS  -- ENUMERATOR HIGH-FREQUENCY CHECKS                       #
#                                                                                                      #
########################################################################################################

## PURPOSE      Create an enumerator high-frequency check sheet in the HFC dashboard.

## AUTHOR       Xiaoming Zhang(adapted from Adrien Ciret & Marc-Andrea Fiorina & Juliana Guerrero)

## LAST UPDATE  April 16th, 2024

########################################################################################################

# 1.Enumerator-Level Checks by day ----

# For each enumerator, we want to check:
# Number of submissions
# Number of submissions per day
# Average duration of each module, average willingness to pay, average household number


hfc_constr <- hfc_constr %>% 
  mutate(
    enumerator_key = as.character(enumerator_key),
    enumerator = as.character(enumerator)
    
  )




enumerator_check_by_day <- hfc_constr %>%
  group_by(enumerator, submissiondate) %>%
  summarize(
    num_surveys = n()
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from  = submissiondate,
    values_from = num_surveys
  ) %>%
  mutate(
    across(
      -enumerator,
      ~ case_when(
        is.na(.x) ~ 0,
        TRUE      ~ .x
      )
    )
  ) %>%
  select(
    enumerator, order(colnames(.)[-1]) # So that the days are ordered chronologically
  )

#2. Primary data check: duration and averages----
enumerator_check <- hfc_constr %>%
  group_by(enumerator, enumerator_key) %>%
  summarize(
    num_surveys = n(),
    across(
      c(ends_with("duration"), "A1_1", "A1_2", "A1_3", "J1_final", "J2_1", "J3_1", "J4_1", "J5_1"),
      ~ round(mean(.x, na.rm = TRUE), 2), .names = "{col}_mean"
    )
  ) %>%
  ungroup() %>%
  left_join(
    enumerator_check_by_day, by = c("enumerator")
  )




#Export Data

hfc_sheet %>%
  sheet_write(data = enumerator_check, sheet = "enum_data")

# In case googlesheets4 asks me to enter '1' to continue

1


#3. Completion rate of each enumerator----

## Encoding Consent Variable - # Consent is imported as character type, encode Yes = 1 , No = 0
hfc_constr <- hfc_constr %>%
  mutate(
    survey_complete_c=ifelse(finish ==1,'Yes','No'),
    consent_c=ifelse(consent==1,'Yes','No')
  )

##Completion rate of each enumerator----

hfc_constr <- hfc_constr %>% 
  mutate(
    enumerator_key = ifelse(enumerator_key %in% "Other", enumerator_other, enumerator_key)
  )

completion <- hfc_constr %>% 
  group_by(enumerator, enumerator_key, submissiondate) %>% 
  summarise(
    complete = sum(finish == 1),
    no_complete = sum(finish == 0),
    consent = sum(consent == 1),
    no_consent = sum(consent == 0),
    completion_rate = paste0(round(complete/(no_complete + complete),3)*100, "%"),
    consent_rate = paste0(round(consent/(no_consent + consent),3)*100, "%")
  ) %>% 
  ungroup() %>% 
  arrange(submissiondate)


hfc_sheet %>%
  sheet_write(data = completion, sheet = "completion_data")

# In case googlesheets4 asks me to enter '1' to continue
1



