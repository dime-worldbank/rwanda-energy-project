
pacman::p_load(knitr, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr)

getwd()

karongi <- read_xlsx(path = file.path(data_path, "Daily achievements_1201.xlsx"), sheet = "Karongi")
rutsiro <- read_xlsx(path = file.path(data_path, "Daily achievements_1201.xlsx"), sheet = "Rutsiro")
rulindo <- read_xlsx(path = file.path(data_path, "Daily achievements_1201.xlsx"), sheet = "Rulindo")
rusizi <- read_xlsx(path = file.path(data_path, "Daily achievements_1201.xlsx"), sheet = "Rusizi")


daily_achievements <- bind_rows(karongi, rutsiro, rulindo, rusizi)

day_mapping <- data.frame(
  input_value = c(paste0("Day ", 1:16), as.character(1:16)),
  date_value = rep(seq.Date(from = as.Date("2024-11-11"), 
                            to = as.Date("2024-11-26"), 
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

daily_achievements <- daily_achievements %>% 
  mutate(
    village_id = str_sub(respondent_id, 1, 8)  
  )


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


#Track on daily village----

village_track <- daily_achievements %>% 
  group_by(village_id, date) %>% 
  summarise(
    track_surveyed = n()
  )%>% 
  ungroup() %>%
  pivot_wider(
    names_from = date,
    values_from = track_surveyed
  ) %>%
  mutate(across(
    -village_id,
    ~ ifelse(is.na(.x), 0, .x)
  )) %>% 
  select(village_id, order(colnames(.)))
  


village_completion_track <- daily_achievements %>% 
  group_by(village_id) %>% 
  summarise(
    attempt = n(),
    complete = sum(successful == "Yes", na.rm = TRUE)
  ) %>%
  ungroup()


village_track <- left_join(village_track, village_completion_track, by = c("village_id")) 

village_track <- village_track %>% 
  select(village_id, attempt, complete, everything()) %>% 
  mutate(
    village_id = as.numeric(village_id)
  )

village_track <- left_join(admin_raw, village_track, by = c("villageid_key" = "village_id")) 



# saving output

hfc_sheet %>%
  
  sheet_write(data = village_track, sheet = "tracking_sheet")

1

  
##check the orders----

village_order_problem <- village_track %>%
  mutate(
    num_left = ceiling(num_to_survey * attempt / complete) # Round up instead of `round`
  )  %>% 
  select(
    villageid_key, num_left
  ) %>% 
  mutate(
    villageid_key = as.character(villageid_key)
  )


replacement_list <- read_xlsx("C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/Updated scope villages& households/household_replacement.xlsx")

replacement_list <- replacement_list %>% 
  group_by(villageid_key) %>% 
  mutate(order_replacement = row_number()) %>%
  ungroup() %>% 
  mutate(household_id = as.numeric(household_id))

hfc_replacement <- daily_achievements %>% 
  filter(respondent_id %in% replacement_list $household_id) %>% 
  filter(date >= "2024-11-21") %>% 
  distinct(respondent_id, .keep_all = TRUE) %>% 
  group_by(village) %>% 
  mutate(order_survey = row_number()) %>% 
  ungroup() %>% 
  select(respondent_id, order_survey, date) %>% 
  mutate(
    respondent_id = as.numeric(respondent_id)
  )

replacement_list <- left_join(replacement_list, hfc_replacement, by = c("household_id" = "respondent_id"))

replacement_list <- left_join(replacement_list, village_order_problem, by = c("villageid_key"))

replacement_check <- replacement_list %>% 
  filter(order_survey >= num_left) %>% 
  filter(order_replacement != order_survey) 

hfc_sheet %>%
  
  sheet_write(data = replacement_check, sheet = "replacement")

1

replacement_group <- replacement_check %>% 
  group_by(district) %>% 
  summarise(
    n = n()
  )

hfc_sheet %>%
  
  sheet_write(data = replacement_group, sheet = "replacement_district")

1
