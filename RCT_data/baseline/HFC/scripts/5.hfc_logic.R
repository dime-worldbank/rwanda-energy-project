########################################################################################################
#                                                                                                      #
#             HIGH-FREQUENCY CHECKS AND PHONE TRACKING CODE -- LOGIC HIGH-FREQUENCY CHECKS             #
#                                                                                                      #
########################################################################################################

## PURPOSE      Create a logic high-frequency check sheet in the HFC dashboard.
## AUTHOR       Xiaoming Zhang (adapted from Adrien Ciret & Marc-Andrea Fiorina & Juliana Guerrero)

## LAST UPDATE April 17th, 2023

########################################################################################################

# 1. Import Data ----

### Survey Programming Checks ----

# Purpose — Monitor survey programming issues/peculiarities that either (1) can't be programmed into the
# survey instrument or (2) don't have to be hard conditions, but we still want to avoid where possible.

# In this case:
# Situations where the enumerator used an old survey version
# Situations where the units used for produced crops and for sold crops are not the same
# Situations where the household's site is not the correct one (just to monitor)

# Process: create a small dataframe with the same structure for each check, and then bind them

#1. form version check----
formdef_check <- hfc_constr %>%
  filter(formdef_version != "2024041701") %>%
  mutate(issue = "Enumerator used old survey version") %>%
  select(hh_id, village, village_key, enumerator, enumerator_key, submissiondate,issue)

#2. Electrification check----
elec_check <- hfc_constr %>%
  filter( A1_2 != H1_1 | A1_3 != H2_1 | H8_6 != C3_14 ) %>%
  mutate(issue = "Electrification status & lightbulb unmatch") %>%
  select(hh_id, village, village_key, enumerator, enumerator_key, submissiondate,issue)



#3. Start/End/Submission Date Discrepancies----

start_end_check <- hfc_constr %>%
  filter(startdate != enddate) %>%
  mutate(issue = "End of survey date is different from start of survey date") %>%
  select(hh_id, village, village_key, enumerator, enumerator_key, submissiondate,issue) # calculate this


end_submission_check <- hfc_constr %>%
  filter(enddate != submissiondate) %>%
  mutate(issue = "Submission date is different from end of survey date") %>%
  select(hh_id, village, village_key, enumerator, enumerator_key, submissiondate,issue) # 




#4. Check for Modules with Negative Durations (i.e. end_mod was recorded before start_mod)----

negative_length_check <- hfc_constr %>%
  filter(negative_duration > 0) %>%
  mutate(issue = "This survey has one or more negative/no durations") %>%
  select(hh_id, village, village_key, enumerator, enumerator_key, submissiondate,issue) #,negative_mods

#5.High Survey Length ----

high_length_check <- hfc_constr %>%
  group_by(hh_id) %>%
  mutate(
    duration = sum(survey_duration, na.rm = TRUE),
    n_subs   = row_number()
  ) %>%
  filter(n_subs == max(n_subs, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-n_subs) %>%
  filter(survey_duration > 180) %>%
  mutate(issue = "Survey total duration is greater than 3 hours") %>%
  select(hh_id, village, village_key, enumerator, enumerator_key, submissiondate,issue) #,negative_mods

#6.Check for low Survey Length----

low_length_check <- hfc_constr%>%
  group_by(hh_id) %>%
  mutate(
    duration = sum(survey_duration, na.rm = TRUE),
    n_subs   = row_number()
  ) %>%
  filter(n_subs == max(n_subs, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-n_subs) %>%
  filter(survey_duration < 60) %>%
  mutate(issue = "Survey total duration is less than 1 hour") %>%
  select(hh_id, village, village_key, enumerator, enumerator_key, submissiondate,issue)






#7. Creat final dataset----

logic_check <- rbind(formdef_check, elec_check, start_end_check, end_submission_check, negative_length_check, low_length_check, high_length_check)


# logic_check <- logic_check %>% 
#   select(
#     hh_id, village, village_key, enumerator, startdate, enddate, submissiondate,issue, ends_with("duration"), A1_2, H1_1, A1_3, H2_1, H8_6, C3_14, formdef_version
#   ) %>% 
#   rename(
#    grid_1 = A1_2,
#    grid_2 = H1_1, 
#    offgrid_1 = A1_3,
#    offgrid_2 = H2_1,
#    lightbulb_1 = H8_6,
#    lightbulb_2 = C3_14
#   )


# Export Data ----

hfc_sheet %>%
  sheet_write(data = logic_check, sheet = "logic_data")

1
