##############
#Author: Xiaoming Zhang
#Date: 8.5.2025
#Purpose: Analysis
#############


pacman::p_load(knitr, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr, googlesheets4)
library(googlesheets4)
getwd()


# Import Data ----
dropbox <- 'C:/Users/wb614406/Dropbox'

hfc_data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/HFC/data"
)

hfc_output_path <- file.path(
  dropbox, 
  "Rwanda Energy/EAQIP/datawork/HFC/output"
)

output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/baseline analysis/output"
)


screening_data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Screening/data"
)

data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/baseline analysis/data"
)

hfc_constr_raw <- read_xlsx(file.path(hfc_output_path, "hfc_constr_0728.xlsx"))

complete_status <- read_xlsx(path = file.path(data_path_2, "vulnerable households in sample villages.xlsx")) %>% 
  mutate(hh_head_name = paste0(first_name, " ", last_name))

hfc_constr <- hfc_constr_raw %>% 
  filter(consent == 1) %>%
  filter(!is.na(A1_1)) %>%
  distinct(hh_head_name, hh_id, A1_2, A1_3, .keep_all = TRUE) %>%
  filter(village %in% village_181$villageid_key) %>%
  group_by(hh_id) %>%
  arrange(desc(hh_head_name %in% complete_status$hh_head_name)) %>% 
  slice(1) %>%
  ungroup()





#1. Binary info----



head_roster <- hfc_constr %>% select(starttime, endtime, submissiondate, enumerator, enumerator_key,
                                     district, sector, cell, village, hh_id, consent,
                                     
                                     #household head roster
                                     starts_with("A1"), hh_head_name, gender, gender_label, marital, marital_label, head_age_calculate, education, education_label, high_edu, high_edu_label, starts_with("A2"),
                                     starts_with("A3")
) %>% 
  mutate(head_female = ifelse(gender == 0, 1, 0))


age_size <- head_roster %>%
  mutate(
    head_age_calculate = ifelse(
      head_age_calculate < quantile(head_age_calculate, 0.01, na.rm = TRUE),
      quantile(head_age_calculate, 0.01, na.rm = TRUE),
      head_age_calculate
    )
  ) %>% 
  summarize(
    across(
      c(head_age_calculate, A1_1, head_female),
      list(
        mean   = ~ round(mean(.x, na.rm = TRUE),2),
        sd     = ~ round(sd(.x, na.rm = TRUE),2),
        min    = ~ min(.x, na.rm = TRUE),
        max    = ~ max(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(
    cols          = everything(),
    names_to      = c("var", "stat"),
    names_pattern = "^(.*)_(mean|sd|min|max|median)$"
  ) %>%
  pivot_wider(
    names_from  = "stat",
    values_from = "value"
  )  





household_size <- hfc_constr %>% 
  select(A1_1) %>% 
  rename(`Household size` = A1_1) %>% 
  group_by(`Household size`) %>% 
  summarise(n = n()) %>% 
  arrange(n)

# Create bar plot with continuous line for distribution
ggplot(household_size, aes(x = `Household size`, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_smooth(aes(x = `Household size`, y = n), method = "loess", se = FALSE, color = "red") +
  labs(title = "Distribution of Household Size", x = "Household Size", y = "Frequency") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),  # Increase size of x and y axis labels
    axis.text = element_text(size = 12),   # Increase size of x and y axis tick labels
    plot.title = element_text(size = 16, hjust = 0.5),  # Increase plot title size and center it
    legend.title = element_text(size = 14),  # Increase legend title size (if applicable)
    legend.text = element_text(size = 12)    # Increase legend text size (if applicable)
  )



##Member_roster----

library(tidyr)
library(dplyr)

# Select the relevant columns (all member-related columns)
member_roster <- hfc_constr %>%
  select(
    hh_id, 
    starts_with("member_name"),
    starts_with("member_gender"),
    starts_with("member_marital"),
    starts_with("member_age_calculate"),
    starts_with("member_education"),
    starts_with("member_current_school"),
    starts_with("member_high_edu"),
    starts_with("A4"),
    starts_with("A5")
  )

# Pivot the data to long format
member_roster <- member_roster %>%
  pivot_longer(
    cols = starts_with("member_") | starts_with("A4_") | starts_with("A5_"),  # Pivot all member columns
    names_to = c(".value", "member_id"),  # Extract the 'member' part and assign the 'member_id'
    names_pattern = "(.*)_(\\d+)",  # Match the pattern of variable name and member number
    values_drop_na = TRUE  # Drop NAs if there are missing values for some members
  )


##Both head and member-----

##Age-----

head_age <- head_roster %>% 
  select(hh_id, head_age_calculate) %>% 
  rename(age = head_age_calculate) 

member_age <- member_roster %>% 
  select(hh_id, member_age_calculate) %>% 
  rename(age = member_age_calculate)

age_summary <- bind_rows(head_age, member_age) %>%
  mutate(
    age_24 = ifelse(age < 24, 1, 0),
    age_65 = ifelse(age > 65, 1, 0),
    dependency = ifelse(age < 15 | age >64, 1, 0)
  ) %>%
  summarise(
    age_24_mean   = round(mean(age_24, na.rm = TRUE), 2),
    age_24_sd     = round(sd(age_24, na.rm = TRUE), 2),
    age_24_min    = min(age_24, na.rm = TRUE),
    age_24_max    = max(age_24, na.rm = TRUE),
    age_24_median = median(age_24, na.rm = TRUE),
    
    age_65_mean   = round(mean(age_65, na.rm = TRUE), 2),
    age_65_sd     = round(sd(age_65, na.rm = TRUE), 2),
    age_65_min    = min(age_65, na.rm = TRUE),
    age_65_max    = max(age_65, na.rm = TRUE),
    age_65_median = median(age_65, na.rm = TRUE),
    
    # Dependency (age < 15 or > 64)
    dependency_mean   = round(mean(dependency, na.rm = TRUE), 2),
    dependency_sd     = round(sd(dependency, na.rm = TRUE), 2),
    dependency_min    = min(dependency, na.rm = TRUE),
    dependency_max    = max(dependency, na.rm = TRUE),
    dependency_median = median(dependency, na.rm = TRUE)
  )


##education-----

head_education <- head_roster %>% 
  filter(head_age_calculate >= 6) %>% 
  select(hh_id, education)

member_education <- member_roster %>% 
  filter(member_age_calculate >= 6) %>% 
  select(hh_id, member_education) %>% 
  rename(education = member_education)

education_summary <- bind_rows(head_education, member_education) %>% 
  summarise(
    mean   = round(mean(education, na.rm = TRUE), 2),
    sd     = round(sd(education, na.rm = TRUE), 2),
    min    = min(education, na.rm = TRUE),
    max    = max(education, na.rm = TRUE),
    median = median(education, na.rm = TRUE)
  )


##work force participation----

head_work <- head_roster %>% 
  filter(head_age_calculate >= 16) %>% 
  mutate(work = ifelse(is.na(A1_4_6), 1, 0)) %>% 
  select(hh_id, work)



member_work <- member_roster %>% 
  filter(member_age_calculate >= 16) %>% 
  mutate(work = ifelse(is.na(A4_1_6), 1, 0)) %>% 
  select(hh_id, work) 

work_force <- rbind(head_work, member_work) %>% 
  summarise(
    mean   = round(mean(work, na.rm = TRUE), 2),
    sd     = round(sd(work, na.rm = TRUE), 2),
    min    = min(work, na.rm = TRUE),
    max    = max(work, na.rm = TRUE),
    median = median(work, na.rm = TRUE)
  )


##Business====

summary(hfc_constr$D1_1)
sd(hfc_constr$D1_1)


##work type-----

head_agriculture <- head_roster %>%
  mutate(
    agric_status = case_when(
      A2_1 == 1 ~ "own_farm",
      A2_1 == 2 ~ "other_farm",
      A2_1 %in% c(3, 4, 5, 6, 7, 8,-77 ) ~ "other",
      TRUE ~ "none"
    )
  ) %>%
  select(hh_id, head_age_calculate, agric_status, A2_1_label) %>% 
  rename(age = head_age_calculate,
         label = A2_1_label)


member_agriculture <- member_roster %>%
  mutate(
    agric_status = case_when(
      A4_2 == 1 ~ "own_farm",
      A4_2 == 2 ~ "other_farm",
      A4_2 %in% c(3, 4, 5, 6, 7, 8, -77) ~ "other",
      TRUE ~ "none"
    )
  )%>% select(hh_id, member_age_calculate, agric_status, A4_2_label) %>% 
  rename(age = member_age_calculate,
         label = A4_2_label)



agriculture <- rbind(head_agriculture, member_agriculture)%>% 
  filter(!is.na(label) & agric_status != "none") %>% 
  mutate(
    own_farm    = if_else(agric_status == "own_farm", 1, 0),
    other_farm  = if_else(agric_status == "other_farm", 1, 0),
    other       = if_else(agric_status == "other", 1, 0)  )

summary(agriculture$other)
sd(agriculture$other)

agriculture_work <- rbind(head_agriculture, member_agriculture)%>% 
  mutate( work = ifelse(!is.na(label) & agric_status != "none", 1, 0) )


summary(agriculture_work$work)
sd(agriculture_work$work)



##Head earning----
head_salary <- hfc_constr %>% 
  select(
    hh_id, 
    starts_with("A2_4"),
    starts_with("A2_5"),
    starts_with("A2_7"),
    starts_with("A2_8"),
    starts_with("A2_9"),
    starts_with("A3_5"),
    starts_with("A3_6"),
    starts_with("A3_8"),
    starts_with("A3_9"),
    starts_with("A3_10")
  ) %>% 
  rename(
    primary_week = A2_7,
    primary_day = A2_8,
    primary_hour = A2_9,
    secondary_week = A3_8,
    secondary_day = A3_9,
    secondary_hour = A3_10
  )

head_primary <- head_salary %>%
  filter(A2_5_label != "Other") %>%
  group_by(A2_5_label) %>%
  mutate(
    A2_4_winsor = ifelse(
      A2_4 > quantile(A2_4, 0.95, na.rm = TRUE),
      quantile(A2_4, 0.95, na.rm = TRUE),
      A2_4
    )
  ) %>%
  ungroup() %>%
  mutate(
    A2_4_week = case_when(
      A2_5_label == "Hour" ~ A2_4_winsor * primary_hour * primary_day,
      A2_5_label == "Day"  ~ A2_4_winsor * primary_week,
      A2_5_label == "Week" ~ A2_4_winsor,
      A2_5_label == "2 Weeks" ~ round(A2_4_winsor / 2, 2),
      A2_5_label == "Month" ~ round(A2_4_winsor / 4, 2),
      A2_5_label == "Quarter" ~ round(A2_4_winsor / (3 * 4), 2),
      A2_5_label == "Agricultural season" ~ round(A2_4_winsor / (4 * 4), 2),
      A2_5_label == "Half Year" ~ round(A2_4_winsor / (6 * 4), 2),
      A2_5_label == "Year" ~ round(A2_4_winsor / (12 * 4), 2)
    )
  )

# head_primary_join <- head_primary %>% 
#   select(hh_id, A2_4_week)
# 
# hfc_constr <- left_join(hfc_constr, head_primary_join)

head_primary_summary <- head_primary%>% 
  summarise(
    mean = round(mean(A2_4_week, na.rm = TRUE),2),
    sd = round(sd(A2_4_week, na.rm = TRUE),2),
    min = min(A2_4_week, na.rm = TRUE),
    max = max(A2_4_week, na.rm = TRUE),
    median = median(A2_4_week, na.rm = TRUE)
  )   %>%
  mutate(
    var_winsorized95 = "HH head primary salary(Rwf/week)"
  ) %>% 
  select(
    var_winsorized95, everything()
  )

head_secondary <- head_salary %>%
  filter(A3_6_label != "Other") %>%
  group_by(A3_6_label) %>%
  mutate(
    A3_5_winsor = ifelse(
      A3_5 > quantile(A3_5, 0.95, na.rm = TRUE),
      quantile(A3_5, 0.95, na.rm = TRUE),
      A3_5
    )
  ) %>%
  ungroup() %>%
  mutate(
    A3_5_week = case_when(
      A3_6_label == "Hour" ~ A3_5_winsor * secondary_hour * secondary_day,
      A3_6_label == "Day"  ~ A3_5_winsor * secondary_week,
      A3_6_label == "Week" ~ A3_5_winsor,
      A3_6_label == "2 Weeks" ~ round(A3_5_winsor / 2, 2),
      A3_6_label == "Month" ~ round(A3_5_winsor / 4, 2),
      A3_6_label == "Quarter" ~ round(A3_5_winsor / (3 * 4), 2),
      A3_6_label == "Agricultural season" ~ round(A3_5_winsor / (4 * 4), 2),
      A3_6_label == "Half Year" ~ round(A3_5_winsor / (6 * 4), 2),
      A3_6_label == "Year" ~ round(A3_5_winsor / (12 * 4), 2)
    )
  )





# head_secondary_join <- head_secondary %>% 
#   select(hh_id, A3_5_week)
# 
# hfc_constr <- left_join(hfc_constr, head_secondary_join)

head_secondary_summary <- head_secondary %>%
  summarise(
    mean   = round(mean(A3_5_week, na.rm = TRUE), 2),
    sd     = round(sd(A3_5_week, na.rm = TRUE), 2),
    min    = min(A3_5_week, na.rm = TRUE),
    max    = max(A3_5_week, na.rm = TRUE),
    median = median(A3_5_week, na.rm = TRUE)
  ) %>%
  mutate(
    var_winsorized95 = "HH head secondary salary (Rwf/week)"
  ) %>%
  select(var_winsorized95, everything())


##Member earning=-----


member_salary <- member_roster %>% 
  select(
    hh_id, 
    starts_with("A4_5"),
    starts_with("A4_6"),
    starts_with("A4_8"),
    starts_with("A4_9"),
    starts_with("A4_10"),
    starts_with("A5_5"),
    starts_with("A5_6"),
    starts_with("A5_8"),
    starts_with("A5_9"),
    starts_with("A5_10")
  ) %>% 
  rename(
    primary_week = A4_8,
    primary_day = A4_9,
    primary_hour = A4_10,
    secondary_week = A5_8,
    secondary_day = A5_9,
    secondary_hour = A5_10
  )


member_primary <- member_salary %>%
  filter(A4_6_label != "Other") %>%
  group_by(A4_6_label) %>%
  mutate(
    A4_5_winsor = ifelse(
      A4_5 > quantile(A4_5, 0.95, na.rm = TRUE),
      quantile(A4_5, 0.95, na.rm = TRUE),
      A4_5
    )
  ) %>%
  ungroup() %>%
  mutate(
    A4_5_week = case_when(
      A4_6_label == "Hour" ~ A4_5_winsor * primary_hour * primary_day,
      A4_6_label == "Day"  ~ A4_5_winsor * primary_week,
      A4_6_label == "Week" ~ A4_5_winsor,
      A4_6_label == "2 Weeks" ~ round(A4_5_winsor / 2, 2),
      A4_6_label == "Month" ~ round(A4_5_winsor / 4, 2),
      A4_6_label == "Quarter" ~ round(A4_5_winsor / (3 * 4), 2),
      A4_6_label == "Agricultural season" ~ round(A4_5_winsor / (4 * 4), 2),
      A4_6_label == "Half Year" ~ round(A4_5_winsor / (6 * 4), 2),
      A4_6_label == "Year" ~ round(A4_5_winsor / (12 * 4), 2)
    )
  )


member_primary_summary <- member_primary %>%
  summarise(
    mean   = round(mean(A4_5_week, na.rm = TRUE), 2),
    sd     = round(sd(A4_5_week, na.rm = TRUE), 2),
    min    = min(A4_5_week, na.rm = TRUE),
    max    = max(A4_5_week, na.rm = TRUE),
    median = median(A4_5_week, na.rm = TRUE)
  ) %>%
  mutate(
    var_winsorized95 = "HH member primary salary (Rwf/week, 95% winsorized)"
  ) %>%
  select(var_winsorized95, everything())


member_secondary <- member_salary %>%
  filter(A5_6_label != "Other") %>%
  group_by(A5_6_label) %>%
  mutate(
    A5_5_winsor = ifelse(
      A5_5 > quantile(A5_5, 0.95, na.rm = TRUE),
      quantile(A5_5, 0.95, na.rm = TRUE),
      A5_5
    )
  ) %>%
  ungroup() %>%
  mutate(
    A5_5_week = case_when(
      A5_6_label == "Hour" ~ A5_5_winsor * secondary_hour * secondary_day,
      A5_6_label == "Day"  ~ A5_5_winsor * secondary_week,
      A5_6_label == "Week" ~ A5_5_winsor,
      A5_6_label == "2 Weeks" ~ round(A5_5_winsor / 2, 2),
      A5_6_label == "Month" ~ round(A5_5_winsor / 4, 2),
      A5_6_label == "Quarter" ~ round(A5_5_winsor / (3 * 4), 2),
      A5_6_label == "Agricultural season" ~ round(A5_5_winsor / (4 * 4), 2),
      A5_6_label == "Half Year" ~ round(A5_5_winsor / (6 * 4), 2),
      A5_6_label == "Year" ~ round(A5_5_winsor / (12 * 4), 2)
    )
  )

# member_secondary_join <- member_secondary %>% 
#   select(hh_id, A5_5_week) %>% 
#   group_by(hh_id) %>% 
#   summarise(A5_5_week = sum(A5_5_week, na.rm = TRUE))
# 
# hfc_constr <- left_join(hfc_constr, member_secondary_join)

member_secondary_summary <- member_secondary %>%
  summarise(
    mean   = round(mean(A5_5_week, na.rm = TRUE), 2),
    sd     = round(sd(A5_5_week, na.rm = TRUE), 2),
    min    = min(A5_5_week, na.rm = TRUE),
    max    = max(A5_5_week, na.rm = TRUE),
    median = median(A5_5_week, na.rm = TRUE)
  ) %>%
  mutate(
    var_winsorized95 = "HH member secondary salary (Rwf/week, 95% winsorized)"
  ) %>%
  select(var_winsorized95, everything())

# week_var <- c(
#   "A2_4_week" ,
#   "A3_5_week" ,
#   "A4_5_week" ,
#   "A5_5_week"
# )
# 
# hfc_constr <- hfc_constr %>% 
#   mutate(
#     across(all_of(week_var), ~ replace(.x, is.na(.x), 0))
#   )

# 
# check <- hfc_constr %>% 
#   select(all_of(week_var))
salary_summary <- bind_rows(head_primary_summary, head_secondary_summary, member_primary_summary, member_secondary_summary)



##Total earnings households-----

# Step 1: Group member-level salary by hh_id
member_primary_hh <- member_primary %>%
  group_by(hh_id) %>%
  summarise(member_primary = sum(A4_5_week, na.rm = TRUE), .groups = "drop")

member_secondary_hh <- member_secondary %>%
  group_by(hh_id) %>%
  summarise(member_secondary = sum(A5_5_week, na.rm = TRUE), .groups = "drop")

# Step 2: Head primary and secondary (already at hh_id level)
head_primary_hh <- head_primary %>%
  select(hh_id, head_primary = A2_4_week)

head_secondary_hh <- head_secondary %>%
  select(hh_id, head_secondary = A3_5_week)

# Step 3: Combine all income components into one table
household_income <- head_primary_hh %>%
  full_join(head_secondary_hh, by = "hh_id") %>%
  full_join(member_primary_hh, by = "hh_id") %>%
  full_join(member_secondary_hh, by = "hh_id") %>%
  mutate(across(-hh_id, ~ replace_na(.x, 0))) %>%
  mutate(
    total_weekly_income = head_primary + head_secondary + member_primary + member_secondary
  )

household_income_join <- household_income %>% 
  select(total_weekly_income, hh_id) %>% 
  mutate(total_monthly_income = total_weekly_income*4)

# hfc_constr <- left_join(hfc_constr, household_income_join)

household_income_summary <- household_income %>%
  summarise(
    mean   = round(mean(total_weekly_income, na.rm = TRUE), 2),
    sd     = round(sd(total_weekly_income, na.rm = TRUE), 2),
    min    = min(total_weekly_income, na.rm = TRUE),
    max    = max(total_weekly_income, na.rm = TRUE),
    median = median(total_weekly_income, na.rm = TRUE)
  ) %>%
  mutate(
    var_winsorized95 = "Total household salary (Rwf/week)"
  ) %>%
  select(var_winsorized95, everything())


salary_summary <- bind_rows(
  salary_summary,
  household_income_summary
)




## Savings-----

savings_var <- c(
  "E1_5", #amount of savings in mobile 
  "E2_3",# amount of savings in formal
  "E3_3",#amount of savings in informal
  "E4_2" #Amount borrowed 
)



savings <- hfc_constr %>% 
  select(hh_id, formal_savings, formal_savings_label, informal_savings, informal_savings_label, "E1_5", "E2_3", "E3_3", "E4_2", "E1_1", "E4_1")

savings_check <- savings %>%
  filter(
    (formal_savings == 0 & !is.na(E2_3)) |
      (informal_savings == 0 & !is.na(E3_3)) |
      (E1_1 == "No" & !is.na(E1_5)) |
      (E4_1 == 0 & !is.na(E4_2))
  )

hfc_constr <- hfc_constr %>% 
  mutate(across(
    all_of(savings_var),
    ~ replace(.x, is.na(.x), 0)
  ))


check <- hfc_constr %>% 
  select(all_of(savings_var))
savings <- savings %>% 
  mutate(across(
    all_of(savings_var),
    ~ replace(
      pmin(.x, quantile(.x, 0.99, na.rm = TRUE), na.rm = TRUE),
      is.na(.x),
      0
    )
  )) %>%
  mutate(
    total_savings = E1_5 + E2_3 + E3_3  # Summing mobile, formal, informal savings
  ) 


savings_summary <- savings %>%
  mutate(across(
    c(E1_5, E2_3, E3_3, E4_2, total_savings),
    list(
      zero_pct = ~ round(mean(.x == 0) * 100, 1)
    ),
    .names = "{.col}_zero_pct"
  )) %>%
  summarise(
    across(
      c(E1_5, E2_3, E3_3, E4_2, total_savings),
      list(
        mean   = ~ round(mean(.x), 2),
        sd     = ~ round(sd(.x), 2),
        min    = ~ min(.x),
        max    = ~ max(.x),
        median = ~ median(.x)
      )
    ),
    across(
      ends_with("_zero_pct"),
      ~ unique(.x)
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("var", "stat"),
    names_pattern = "^(.*)_(mean|sd|min|max|median|zero_pct)$"
  ) %>%
  pivot_wider(
    names_from = "stat",
    values_from = "value"
  ) %>%
  mutate(
    var = case_when(
      var == "E1_5"          ~ "Mobile banking", 
      var == "E2_3"          ~ "Formal savings amount", 
      var == "E3_3"          ~ "Informal savings amount", 
      var == "E4_2"          ~ "Borrowed money",
      var == "total_savings" ~ "Total savings (formal + informal + mobile)"
    )
  ) %>%
  rename(var_winsorized99 = var)


total_savings <- savings %>% 
  select(hh_id, total_savings)

# hfc_constr <- left_join(hfc_constr, total_savings)


##Mobile roster----

mobile_own <- hfc_constr %>%
  mutate(
    own_mobile = ifelse(F1_1 > 0, 1, 0)
  )  


mobile <- hfc_constr %>% 
  filter(F1_1 != 0) %>% 
  select(hh_id, starts_with("F"), total_monthly_income) %>% 
  pivot_longer(
    cols = starts_with("F"),  # Pivot all member columns
    names_to = c(".value", "mobile_id"),  # Extract the 'member' part and assign the 'member_id'
    names_pattern = "(.*)_(\\d+)",  # Match the pattern of variable name and member number
    values_drop_na = TRUE  # Drop NAs if there are missing values for some members
  ) %>% 
  mutate(F2_4 = ifelse(is.na(F2_4), 0, F2_4))


mobile_var <- c(
  "F1_6", # 7-day airtime
  "F1_7", # 30-day airtime
  "F1_8", # 7-day data bundle
  "F1_9", # 30-day data bundle
  "F2_4" # phone charging
)

# Winsorize 99% and replace NAs with 0 for spending variables
mobile_winsorized <- mobile %>%
  mutate(across(
    all_of(mobile_var[1:5]),  # only numeric spendings
    ~ replace(pmin(.x, quantile(.x, 0.99, na.rm = TRUE), na.rm = TRUE), is.na(.x), 0)
  ))

# For own_mobile, keep as is but calculate share of 0s later

#Percent of income
mobile_expenditure <- mobile_winsorized%>%
  # Replace NAs with 0
  mutate(across(all_of(mobile_var), ~ replace_na(.x, 0))) %>%
  mutate(total_monthly_income = replace_na(total_monthly_income, 0)) %>%
  # Compute share of income for each spending category
  mutate(across(
    all_of(mobile_var),
    ~ ifelse(total_monthly_income == 0, 0, .x / total_monthly_income),
    .names = "perc_{.col}"
  )) %>%
  # Keep relevant variables
  select(all_of(mobile_var), total_monthly_income, starts_with("perc_"))



# Summary stats
mobile_summary <- mobile_expenditure %>%
  summarise(
    across(
      all_of(mobile_var),
      list(
        pct_zeros = ~ round(mean(.x == 0, na.rm = TRUE) * 100, 1),
        mean   = ~ round(mean(.x, na.rm = TRUE), 2),
        sd     = ~ round(sd(.x, na.rm = TRUE), 2),
        min    = ~ min(.x, na.rm = TRUE),
        max    = ~ max(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("var", "stat"),
    names_pattern = "^(.*)_(pct_zeros|mean|sd|min|max|median)$"
  ) %>%
  pivot_wider(
    names_from = "stat",
    values_from = "value"
  ) %>%
  mutate(
    var = case_when(
      var == "F1_6"       ~ "Past 7 day airtime (Rwf)",
      var == "F1_7"       ~ "Past 30 day airtime (Rwf)",
      var == "F1_8"       ~ "Past 7 day data bundle (Rwf)",
      var == "F1_9"       ~ "Past 30 day data bundle (Rwf)",
      var == "F2_4"       ~ "Phone charging (Rwf/month)"    )
  ) %>%
  rename(var_winsorized99 = var)



##Percent of household monthly income------

mobile_income_share <- mobile_expenditure %>%
  summarise(across(starts_with("perc_"), ~ mean(.x, na.rm = TRUE) * 100)) %>%
  tidyr::pivot_longer(
    everything(),
    names_to = "var",
    values_to = "mean_income_share_pct"
  ) %>%
  dplyr::mutate(
    var = sub("^perc_", "", var),
    var = dplyr::recode(
      var,
      F1_6 = "Past 7 day airtime",
      F1_7 = "Past 30 day airtime",
      F1_8 = "Past 7 day data bundle",
      F1_9 = "Past 30 day data bundle",
      F2_4 = "Phone charging"
    )
  ) %>%
  dplyr::arrange(factor(
    var,
    levels = c(
      "Past 7 day airtime",
      "Past 30 day airtime",
      "Past 7 day data bundle",
      "Past 30 day data bundle",
      "Phone charging"
    )
  ))


mean(mobile_expenditure$F2_4 >0 )

mean(mobile_expenditure$F2_4[mobile_expenditure$F2_4 > 0], na.rm = TRUE)


###Where to charge phone-----

mobile_charge <- mobile %>% 
  mutate(out_home = ifelse(F2_1_label != "Home", 1, 0))



mobile_charge <- mobile %>% 
  
  group_by(F2_1_label) %>%
  filter(!is.na(F2_1_label)) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(
    desc(n)
  ) %>% 
  mutate(percentage = paste(round((n / sum(n)),4) * 100, "%")) %>% 
  rename(
    `Where to charge mobile` = F2_1_label
  )

#2.Energy section-----

energy <- hfc_constr %>% 
  select(
    hh_id, starts_with("H")
  )

primary_energy <- hfc_constr %>% 
  group_by(H3_1_label) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = paste(round((n / sum(n)),4) * 100, "%"))

primary_energy_summary <- hfc_constr %>%
  select(hh_id, H3_1_label) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = H3_1_label, values_from = value, values_fill = 0) %>%
  select(-hh_id) %>%
  summarise(across(everything(), list(
    mean = ~ mean(.),
    sd = ~ sd(.),
    min = ~ min(.),
    max = ~ max(.),
    median = ~ median(.)
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("energy_source", "stat"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(
    mean = round(mean, 4),
    sd = round(sd, 2)
  )


member_gender <- member_roster %>% 
  select(
    hh_id, member_id, member_gender_label
  ) %>% 
  mutate(
    member_id = as.numeric(member_id)
  )

energy_gender <- left_join(energy, member_gender, by = c("hh_id" = "hh_id", "H3_3" = "member_id"))

energy_gender_group <-energy_gender %>% 
  group_by(member_gender_label) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(desc(n))




##Fuel----

# Function to summarize an energy source
summarise_energy_source <- function(df, var_label, label_name) {
  df %>%
    group_by({{ var_label }}) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(names_from = {{ var_label }}, values_from = n) %>%
    mutate(
      Yes = ifelse(is.na(Yes), 0, Yes),
      No = ifelse(is.na(No), 0, No),
      energy_source = label_name,
      percent = round(Yes / (Yes + No), 4) * 100
    ) %>%
    mutate(
      mean = round(Yes / (Yes + No), 2),
      sd = round(sqrt((percent/100)*(1 - percent/100)), 2),
      min = 0,
      max = 1,
      median = ifelse(percent >= 50, 1, 0)
    ) %>%
    select(energy_source, Yes, No, percent, mean, sd, min, max, median)
}

# Apply to each energy source
candle_summary <- summarise_energy_source(hfc_constr, H4_1_label, "Candle")
biomass_summary <- summarise_energy_source(hfc_constr, H5_1_label, "Biomass")
kerosene_summary <- summarise_energy_source(hfc_constr, H6_1_label, "Kerosene")

# Combine results
energy_source_summary <- bind_rows(candle_summary, biomass_summary, kerosene_summary)


##Expenditure----



energy_expenditure <- hfc_constr %>%
  mutate(
    # Winsorize at 99th percentile
    H4_2 = ifelse(H4_2 > quantile(H4_2, 0.99, na.rm = TRUE), quantile(H4_2, 0.99, na.rm = TRUE), H4_2),
    H5_4 = ifelse(H5_4 > quantile(H5_4, 0.99, na.rm = TRUE), quantile(H5_4, 0.99, na.rm = TRUE), H5_4),
    H6_2 = ifelse(H6_2 > quantile(H6_2, 0.99, na.rm = TRUE), quantile(H6_2, 0.99, na.rm = TRUE), H6_2),
    
    # Replace NAs with 0
    H4_2 = replace_na(H4_2, 0),
    H5_4 = replace_na(H5_4, 0),
    H6_2 = replace_na(H6_2, 0),
    total_monthly_income = replace_na(total_monthly_income, 0),
    
    # Share of total savings
    perc_candle   = ifelse(total_monthly_income == 0, 0, H4_2 / total_monthly_income),
    perc_biomass  = ifelse(total_monthly_income == 0, 0, H5_4 / total_monthly_income),
    perc_kerosene = ifelse(total_monthly_income == 0, 0, H6_2 / total_monthly_income)
  )  %>% 
  select(H4_2, H5_4, H6_2, H5_7_label, total_monthly_income, perc_candle, perc_biomass, perc_kerosene)
  



energy_expenditure_summary <- energy_expenditure %>% 
summarise(
    # Candle stats
    candle_mean    = round(mean(H4_2), 2),
    candle_sd      = round(sd(H4_2), 2),
    candle_min     = min(H4_2),
    candle_max     = max(H4_2),
    candle_median  = median(H4_2),
    candle_share   = round(mean(perc_candle) * 100, 2),
    
    # Biomass stats
    biomass_mean    = round(mean(H5_4), 2),
    biomass_sd      = round(sd(H5_4), 2),
    biomass_min     = min(H5_4),
    biomass_max     = max(H5_4),
    biomass_median  = median(H5_4),
    biomass_share   = round(mean(perc_biomass) * 100, 2),
    
    # Kerosene stats
    kerosene_mean    = round(mean(H6_2), 2),
    kerosene_sd      = round(sd(H6_2), 2),
    kerosene_min     = min(H6_2),
    kerosene_max     = max(H6_2),
    kerosene_median  = median(H6_2),
    kerosene_share   = round(mean(perc_kerosene) * 100, 2)
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("fuel", ".value"),
    names_sep = "_"
  ) %>%
  mutate(
    var_winsorized99 = case_when(
      fuel == "candle" ~ "Candle (Rwf/month)",
      fuel == "biomass" ~ "Biomass (Rwf/month)",
      fuel == "kerosene" ~ "Kerosene (Rwf/month)"
    )
  ) %>%
  select(var_winsorized99, mean, sd, min, max, median, share) %>%
  rename(
    mean_expenditure = mean,
    sd_expenditure = sd,
    min_expenditure = min,
    max_expenditure = max,
    median_expenditure = median,
    mean_share_percent = share
  )





biomass_cooking <- energy_expenditure %>% 
  filter(str_detect(H5_7_label, "Cooking"))  %>% 
  summarise(
    # Candle stats
    candle_mean    = round(mean(H4_2), 2),
    candle_sd      = round(sd(H4_2), 2),
    candle_min     = min(H4_2),
    candle_max     = max(H4_2),
    candle_median  = median(H4_2),
    candle_share   = round(mean(perc_candle) * 100, 2),
    
    # Biomass stats
    biomass_mean    = round(mean(H5_4), 2),
    biomass_sd      = round(sd(H5_4), 2),
    biomass_min     = min(H5_4),
    biomass_max     = max(H5_4),
    biomass_median  = median(H5_4),
    biomass_share   = round(mean(perc_biomass) * 100, 2),
    
    # Kerosene stats
    kerosene_mean    = round(mean(H6_2), 2),
    kerosene_sd      = round(sd(H6_2), 2),
    kerosene_min     = min(H6_2),
    kerosene_max     = max(H6_2),
    kerosene_median  = median(H6_2),
    kerosene_share   = round(mean(perc_kerosene) * 100, 2)
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("fuel", ".value"),
    names_sep = "_"
  ) %>%
  mutate(
    var_winsorized99 = case_when(
      fuel == "candle" ~ "Candle (Rwf/month)",
      fuel == "biomass" ~ "Biomass (Rwf/month)",
      fuel == "kerosene" ~ "Kerosene (Rwf/month)"
    )
  ) %>%
  select(var_winsorized99, mean, sd, min, max, median, share) %>%
  rename(
    mean_expenditure = mean,
    sd_expenditure = sd,
    min_expenditure = min,
    max_expenditure = max,
    median_expenditure = median,
    mean_share_percent = share
  )

cooking_share <- hfc_constr %>%
  summarise(
    cooking_users = sum(str_detect(H5_7_label, "Cooking"), na.rm = TRUE),
    total = n(),
    percent_cooking = round(cooking_users / total * 100, 2)
  )


##Lighting----

###lightbulb


summary(hfc_constr$C3_14)

sd(hfc_constr$C3_14)







primary_light_energy <- hfc_constr %>% 
  group_by(H7_1_label) %>% 
  summarise(
    n = n(),
    .groups = "drop"
  ) %>% 
  arrange(desc(n)) %>% 
  rename(primary_lighting_energy = H7_1_label) %>% 
  mutate(
    percentage = round((n / sum(n)) * 100, 2)  # 2 decimal places
  )


library(dplyr)
library(ggplot2)

# Create grouped dataset
plot_df <- primary_light_energy %>%
  mutate(
    category = case_when(
      primary_lighting_energy == "Dry cell batteries/Electric Torch" ~ "Torch",
      primary_lighting_energy == "Biomass(Charcoal/Wood）" ~ "Biomass",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(category) %>%
  summarise(
    n = sum(n),
    percentage = round(sum(percentage), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

# Plot
primary_lighting_plot <- ggplot(plot_df, aes(x = reorder(category, -percentage), y = percentage)) +
  geom_col(fill = "steelblue", width = 0.3) +
  geom_text(aes(label = paste0(percentage, "%")),
            vjust = -0.3, size = 4) +
  labs(
    title = "Primary Lighting Energy Source",
    x = "",
    y = "Percentage of Households"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text( hjust = 0.5),
    axis.text = element_text(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10))
  )

primary_lighting_plot 

ggsave(
  file.path(output_path, "figures", "primary_lighting_bar.png"),   # file name
  plot = primary_lighting_plot,                      # which plot to save
  width = 6,                                         # width in inches
  height = 4,                                        # height in inches
  dpi = 300,                                         # high quality
  scale = 0.6                                        # scale down by 60%
)

light_hour <- hfc_constr %>%
  select(H8_1, H8_2) %>%
  mutate(across(
    everything(),
    ~ case_when(
      .x == 0 ~ 24,                                # replace 0 with 24
      .x > 100 ~ as.numeric(substr(.x, 1, 2)),     # take first two digits
      TRUE ~ .x                                    # otherwise keep as is
    ) 
    %>% pmin(24)                                 # cap values at 24
  )) %>% 
  mutate(lighting_hours = H8_2 - H8_1) %>% 
  filter(lighting_hours > 0) %>% 
  summarize(
    across(
      c("H8_1", "H8_2", lighting_hours),
      list(
        mean   = ~ round(mean(.x, na.rm = TRUE), 2),
        sd     = ~ round(sd(.x, na.rm = TRUE), 2),
        min    = ~ min(.x, na.rm = TRUE),
        max    = ~ max(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(
    cols          = everything(),
    names_to      = c("var", "stat"),
    names_pattern = "^(.*)_(mean|sd|min|max|median)$"
  ) %>%
  pivot_wider(
    names_from  = "stat",
    values_from = "value"
  ) %>%
  mutate(
    var = case_when(
      var == "H8_1" ~ "Evening Begin using light",
      var == "H8_2" ~ "Evening stop using light",
      var == "lighting_hours" ~"lighting_hours"
    )
  )





#3. Energy Well-being ------


energy_well <- hfc_constr %>% 
  summarize(
    across(
      c( "B4_1", #energy reliability
         "B4_2", #energy efficiency
         "B4_3", #energy accessibility
         "B4_4", #light accessbility
         "B4_5", #energy service satisfaction
         "B4_6", #anxiety related to energy
         "B4_7", #energy challenges
         "B4_8"
      ),
      list(
        mean   = ~ round(mean(.x, na.rm = TRUE),2),
        sd     = ~ round(sd(.x, na.rm = TRUE),2),
        min    = ~ min(.x, na.rm = TRUE),
        max    = ~ max(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(
    cols          = everything(),
    names_to      = c("var", "stat"),
    names_pattern = "^(.*)_(mean|sd|min|max|median)$"
  ) %>%
  pivot_wider(
    names_from  = "stat",
    values_from = "value"
  )  %>% 
  mutate(
    var = case_when(
      
      var == "B4_1" ~ "Not reliable energy",
      var == "B4_2" ~ "Inefficient_energy",
      var == "B4_3" ~ "Energy inaccessibility",
      var == "B4_4" ~ "Light inaccessibility",
      var == "B4_5" ~ "Local energy service not satisfied",
      var == "B4_6" ~ "Energy anxiety",
      var == "B4_7" ~ "Energy challenge",
      var == "B4_8" ~ "Lack of energy peace of mind"
      
    )
  )





#4. General well being----

general_well <- hfc_constr %>%
  mutate(
    across(c(B5_1, B5_2, B5_3, B5_4, B5_5, B5_6, B5_7, B5_8),
           ~ na_if(.x, -66))
  ) %>%
  summarize(
    across(
      c(B5_1, B5_2, B5_3, B5_4, B5_5, B5_6, B5_7, B5_8),
      list(
        mean   = ~ round(mean(.x, na.rm = TRUE), 2),
        sd     = ~ round(sd(.x, na.rm = TRUE), 2),
        min    = ~ min(.x, na.rm = TRUE),
        max    = ~ max(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("var", "stat"),
    names_pattern = "^(.*)_(mean|sd|min|max|median)$"
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(
    var = case_when(
      var == "B5_1" ~ "Current economic status",
      var == "B5_2" ~ "Economic status one year ago",
      var == "B5_3" ~ "Financial situation",
      var == "B5_4" ~ "Energy use in cooking",
      var == "B5_5" ~ "Energy use in mobile phone charging",
      var == "B5_6" ~ "Energy use in lighting",
      var == "B5_7" ~ "Subjective energy status",
      var == "B5_8" ~ "Current energy status"
    )
  )


#5. Willingness to pay----

hfc_constr <- hfc_constr %>%
  mutate(
    wtp_12 = J4_2*12,
    wtp_24 = J5_2*24
  )

wtp <- hfc_constr %>%
  mutate(
    wtp_12 = J4_2*12,
    wtp_24 = J5_2*24
  )

wtp_var <- c(
  "J1_final", # wtp_fixed
  "J2_1",     # wtp_fixed_appliance
  "J3_1",     # wtp_fixed_low_reliability
  "J4_2",     # wtp_paygo_12
  "J5_2",     # wtp_paygo_24
  "wtp_12",
  "wtp_24",
  "J6_1")

sum(hfc_constr$J5_2 > 2500)

wtp <- wtp %>% 
  mutate(across(
    wtp_var, 
    ~ pmin(.x, quantile(.x, 0.99, na.rm = TRUE), na.rm = TRUE)  # Applying pmin for each column
  ))

wtp_summary <- wtp %>% 
  summarise(
    across(
      wtp_var,
      list(
        mean = ~ round(mean(.x, na.rm = TRUE), 2),
        sd = ~ round(sd(.x, na.rm = TRUE), 2),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("var", "stat"),
    names_pattern = "^(.*)_(mean|sd|min|max|median)$"
  ) %>%
  pivot_wider(
    names_from = "stat",
    values_from = "value"
  ) %>%
  mutate(
    var = case_when(
      var == "J1_final" ~ "wtp_fixed_system (Rwf)",
      var == "J2_1" ~ "wtp_fixed_appliance(Rwf)",
      var == "J3_1" ~ "wtp_fixed_low_reliability(Rwf)",
      var == "J4_2" ~ "wtp_fixed_paygo_12(Rwf/month)",
      var == "J5_2" ~ "wtp_fixed_paygo_24(Rwf/month)",
      var == "wtp_12" ~ "wtp_paygo12_total(Rwf)", 
      var == "wtp_24" ~ "wtp_paygo24_total(Rwf)",
      var == "J6_1" ~ "wtp_lightbulb(Rwf/month)"
    )
  ) %>%
  rename(
    var_winsorized99 = var
  )

View(wtp_summary)



#6.Cookstove----

cookstove <- hfc_constr %>% 
  select(hh_id, starts_with("I"))



main_cookstove <- cookstove %>% 
  group_by(I1_1_label) %>%
  filter(!is.na(I1_1_label)) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(
    desc(n)
  ) %>% 
  mutate(percentage = paste(round((n / sum(n)),4) * 100, "%")) %>% 
  rename(
    `Main cookstove` = I1_1_label
  )



second_cookstove <- cookstove %>% 
  group_by(I1_2_label) %>%
  filter(!is.na(I1_2_label)) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(
    desc(n)
  ) %>% 
  mutate(percentage = paste(round((n / sum(n)),4) * 100, "%")) %>% 
  rename(
    `Secondary cookstove` = I1_2_label
  )









sum(!is.na(hfc_constr$I4_1))
sum(!is.na(hfc_constr$I4_2))

fuel_prep <- cookstove%>% 
  summarise(
    across(
      c("I4_1", "I4_2"),
      list(
        mean = ~ round(mean(.x, na.rm = TRUE), 2),
        sd = ~ round(sd(.x, na.rm = TRUE), 2),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("var", "stat"),
    names_pattern = "^(.*)_(mean|sd|min|max|median)$"
  ) %>%
  pivot_wider(
    names_from = "stat",
    values_from = "value"
  ) %>%
  mutate(
    var = case_when(
      var == "I4_1"~"Fuel prep women(minute per day)", #7day airtime
      var == "I4_2" ~"Fuel prep men(minute per day" 
    )
  ) 


#Write final hfc_constr----


write_xlsx(hfc_constr, path = file.path(output_path, "hfc_constr_files",   paste0("hfc_constr_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")))





# 
# 
# #7. Map of location---------
# 
# #Distance to the household and the surveyed poles
# 
# baseline_data <- file.path(dropbox,  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/data")
# rwa_villages <- st_read(dsn = file.path(baseline_data, "rwa_villages", "Village.shp"))
# rwa_villages <- st_make_valid(rwa_villages)
# rwa_villages <- st_transform(rwa_villages, crs = 4326)
# 
# rwa_district <- st_read(dsn = file.path(baseline_data, "rwa_district", "District.shp"))
# rwa_district <- st_make_valid(rwa_district)
# rwa_district <- st_transform(rwa_district, crs = 4326)
# 
# sample_villages <- rwa_villages %>%
#   mutate(District = str_to_title(District)) %>% 
#   filter(District %in% c("Karongi", "Rutsiro", "Rulindo", "Rusizi"))
# 
# hfc_sf <- st_as_sf(hfc_constr, coords = c("coordinate.Longitude", "coordinate.Latitude"), crs = 4326)
# hfc_sf <- st_make_valid(hfc_sf)
# 
# hfc_sf_kml <- hfc_sf %>%
#   janitor::clean_names()  # optional, ensures safe field names
# 
# 
# # Write to KML
# st_write(hfc_sf, dsn = file.path(output_path, "hfc_points.kml"), driver = "KML", delete_dsn = TRUE)
# 
# 
# 
# p_map <- ggplot(data = sample_villages) +
#   geom_sf(fill = NA, color = "lightgrey") +  
#   geom_sf(data = rwa_district, fill = NA, color = "black") +
#   geom_sf(data = hfc_sf, color = "red", size = 0.1) +
#   theme_void()
# 
# # Save to your figures folder
# ggsave(
#   filename = file.path(output_path,  "figures", "sample_villages_map.png"),
#   plot = p_map,
#   width = 8, height = 6, dpi = 300
# )
# 
