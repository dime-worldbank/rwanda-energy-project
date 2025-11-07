

####################################################

#Purpose: Clean HFC constr as a whole-
#Author: Xiaoming
#Date 9.26.2025

##################################################################




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

# find the latest hfc_constr file
latest_file <- list.files(
  path = file.path(output_path, "hfc_constr_files"),
  pattern = "^hfc_constr_\\d{8}_\\d{6}\\.xlsx$",
  full.names = TRUE
) %>%
  sort(decreasing = TRUE) %>%
  .[1]

# read it
hfc_constr <- read_xlsx(latest_file)

cat("Loaded:", latest_file, "\n")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HFC_CONSTR DATA CLEANING PIPELINE
# Author: Xiaoming Zhang
# Date: 2025-09-25
# Purpose: Construct clean HFC dataset across modules
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(dplyr)
library(tidyr)

#------------------------------------------------------------------------------
# Helper Functions
#------------------------------------------------------------------------------
remove_outliers_3sd <- function(x) {
  if (is.numeric(x)) {
    mu <- mean(x, na.rm = TRUE)
    sigma <- sd(x, na.rm = TRUE)
    x[abs(x - mu) > 3 * sigma] <- NA
  }
  x
}

upper_winsor <- function(x, p = 0.99) {
  q <- quantile(x, p, na.rm = TRUE)
  x <- ifelse(x > q, q, x)
  return(x)
}

# 0. Global Cleaning------

hfc_constr_clean <- hfc_constr %>%
  mutate(across(where(is.numeric), ~ na_if(.x, -77))) %>%
  mutate(across(where(is.numeric), ~ na_if(.x, -88))) %>%
  mutate(across(where(is.numeric), ~ na_if(.x, -99)))


# 1. Age and Demographics------

hfc_constr_clean <- hfc_constr_clean %>%
  mutate(across(contains("age"), ~ ifelse(.x < 0, NA, .x))) %>%
  mutate(across(contains("age"), remove_outliers_3sd))


# 2. Well-being Indicators------

hfc_constr_clean.1 <- hfc_constr_clean %>%
  mutate(across(starts_with("B4_"), ~ ifelse(.x < 0, NA, .x))) %>%
  mutate(across(starts_with("B5_"), ~ ifelse(.x < 0, NA, .x))) 


# 3. Savings and Borrowing-------

savings_vars <- c("E1_5", "E2_3", "E3_3", "E4_2")

hfc_constr_clean <- hfc_constr_clean.1 %>%
  # mutate(across(all_of(savings_vars), ~ replace_na(.x, 0))) %>%
  mutate(across(all_of(savings_vars), remove_outliers_3sd)) %>%
  mutate(across(all_of(savings_vars), ~ pmin(.x, quantile(.x, 0.95, na.rm = TRUE)))) %>%
  # mutate(across(all_of(savings_vars), ~ pmax(.x, quantile(.x, 0.10, na.rm = TRUE)))) %>%
  
  mutate(total_savings = E1_5 + E2_3 + E3_3) 


# 4. Energy Expenditure-----

energy_vars <- c("H4_2", "H5_4", "H6_2")

hfc_constr_clean <- hfc_constr_clean %>%
  mutate(across(all_of(energy_vars), ~ replace_na(.x, 0))) %>%
  mutate(across(all_of(energy_vars), remove_outliers_3sd)) 
# %>% 
#   mutate(across(all_of(energy_vars), ~ pmin(.x, quantile(.x, 0.90, na.rm = TRUE))))
  


lighting_vars <- c("H8_1", "H8_2")

hfc_constr_clean <- hfc_constr_clean %>% 
  mutate(across(
    all_of(lighting_vars),
    ~ case_when(
      .x == 0 ~ 24,                                # replace 0 with 24
      .x > 100 ~ as.numeric(substr(.x, 1, 2)),     # take first two digits
      TRUE ~ .x                                    # otherwise keep as is
    )
  ))



# 5. Willingness to Pay--------

wtp_vars <- c("J1_final", "J2_1", "J3_1", "J4_2", "J5_2", "J6_1", "wtp_12", "wtp_24")

hfc_constr_clean <- hfc_constr_clean %>%
  mutate(
    wtp_12 = J4_2 * 12,
    wtp_24 = J5_2 * 24
  ) %>%
  mutate(across(
    wtp_var, 
    ~ pmin(.x, quantile(.x, 0.99, na.rm = TRUE), na.rm = TRUE)  # Applying pmin for each column
  ))


# 6. Household Income Construction------
# (no winsorizing, just outlier cleaning at end)



## Head Primary
head_primary <- hfc_constr_clean %>%
  filter(A2_5_label != "Other") %>%
  mutate(A2_4_week = case_when(
    A2_5_label == "Hour" ~ A2_4 * A2_9 * A2_8,
    A2_5_label == "Day"  ~ A2_4 * A2_7,
    A2_5_label == "Week" ~ A2_4,
    A2_5_label == "2 Weeks" ~ A2_4 / 2,
    A2_5_label == "Month" ~ A2_4 / 4,
    A2_5_label == "Quarter" ~ A2_4 / 12,
    A2_5_label == "Agricultural season" ~ A2_4 / 16,
    A2_5_label == "Half Year" ~ A2_4 / 24,
    A2_5_label == "Year" ~ A2_4 / 48
  )) %>%
  select(hh_id, head_primary = A2_4_week)

## Head Secondary
head_secondary <- hfc_constr_clean %>%
  filter(A3_6_label != "Other") %>%
  mutate(A3_5_week = case_when(
    A3_6_label == "Hour" ~ A3_5 * A3_10 * A3_9,
    A3_6_label == "Day"  ~ A3_5 * A3_8,
    A3_6_label == "Week" ~ A3_5,
    A3_6_label == "2 Weeks" ~ A3_5 / 2,
    A3_6_label == "Month" ~ A3_5 / 4,
    A3_6_label == "Quarter" ~ A3_5 / 12,
    A3_6_label == "Agricultural season" ~ A3_5 / 16,
    A3_6_label == "Half Year" ~ A3_5 / 24,
    A3_6_label == "Year" ~ A3_5 / 48
  )) %>%
  select(hh_id, head_secondary = A3_5_week)

## Member Primary


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


member_primary <- member_roster %>%
  filter(A4_6_label != "Other") %>%
  mutate(A4_5_week = case_when(
    A4_6_label == "Hour" ~ A4_5 * A4_10 * A4_9,
    A4_6_label == "Day"  ~ A4_5 * A4_8,
    A4_6_label == "Week" ~ A4_5,
    A4_6_label == "2 Weeks" ~ A4_5 / 2,
    A4_6_label == "Month" ~ A4_5 / 4,
    A4_6_label == "Quarter" ~ A4_5 / 12,
    A4_6_label == "Agricultural season" ~ A4_5 / 16,
    A4_6_label == "Half Year" ~ A4_5 / 24,
    A4_6_label == "Year" ~ A4_5 / 48
  )) %>%
  group_by(hh_id) %>%
  summarise(member_primary = sum(A4_5_week, na.rm = TRUE), .groups = "drop")

## Member Secondary
member_secondary <- member_roster %>%
  filter(A5_6_label != "Other") %>%
  mutate(A5_5_week = case_when(
    A5_6_label == "Hour" ~ A5_5 * A5_10 * A5_9,
    A5_6_label == "Day"  ~ A5_5 * A5_8,
    A5_6_label == "Week" ~ A5_5,
    A5_6_label == "2 Weeks" ~ A5_5 / 2,
    A5_6_label == "Month" ~ A5_5 / 4,
    A5_6_label == "Quarter" ~ A5_5 / 12,
    A5_6_label == "Agricultural season" ~ A5_5 / 16,
    A5_6_label == "Half Year" ~ A5_5 / 24,
    A5_6_label == "Year" ~ A5_5 / 48
  )) %>%
  group_by(hh_id) %>%
  summarise(member_secondary = sum(A5_5_week, na.rm = TRUE), .groups = "drop")

## Household Total Income ----
household_income <- head_primary %>%
  full_join(head_secondary, by = "hh_id") %>%
  full_join(member_primary, by = "hh_id") %>%
  full_join(member_secondary, by = "hh_id") %>%
  mutate(across(-hh_id, ~ replace_na(.x, 0))) %>%   # Step 1: fill missing with 0 before sum
  mutate(across(-hh_id, ~ replace_na(.x, 0)))   %>%     # Step 3: replace NAs back with 0

  mutate(
    total_weekly_income  = head_primary + head_secondary + member_primary + member_secondary,
    total_monthly_income = total_weekly_income * 4
  ) %>%
  mutate(across(-hh_id, remove_outliers_3sd))

hfc_constr_clean <- hfc_constr_clean %>%
  left_join(household_income, by = "hh_id")

# # 7. Final Pass: Remove outliers globally on remaining numeric vars-----

# hfc_constr_clean <- hfc_constr_clean %>%
#   mutate(across(where(is.numeric), remove_outliers_3sd))
