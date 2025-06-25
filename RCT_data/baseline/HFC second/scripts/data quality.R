# Managing Successful Field Research 2023 — R Labs
## Session 6 — Ensuring Data Quality

##################################################

## 1. Setup ----

options(scipen = 999)
options(max.print = 5000)
options(tibble.width = Inf)

# Run the install.packages() lines if you haven't installed there packages yet

# install.packages("tidyverse")
# install.packages("janitor")
# install.packages("openxlsx")
# install.packages("here")

library(here)
library(readr) # from tidyverse
library(dplyr) # from tidyverse
library(tidyr) # from tidyverse
library(purrr) # from tidyverse
library(tidyselect) # from tidyverse
library(janitor)
library(openxlsx)

## 2. Data Import ----

survey_raw <- read_csv(
  here("DataWork", "data", "raw", "LWH_FUP2_raw_data.csv"), na = ""
)

admin_raw <- read_csv(
  here("DataWork", "data", "raw", "village_data.csv"), na = ""
)

## 3. Basic Cleaning ----

survey_data <- survey_raw %>%
  mutate(
    across(
      where(is.numeric),
      ~ case_when(
        .x %in% c(-66, -88, -99) ~ NA_real_,
        TRUE                     ~ .x
      )
    )
  )

## 4. Data Quality Check Dataframes Creation ----

### Duplicate IDs ----

# Produce a dataset that has the households with duplicate IDs, with variables that will help solve them

duplicate_check <- survey_data %>%
  group_by(hhid) %>% # If the dataset was uniquely identified, each ID would only appear once
  filter(n() > 1) %>% # n() counts observations relative to a grouped-by column
  ungroup() %>%
  select(hhid, enumerator, province:village, inc_01, a_crop_c1_p1, crp09qa_c1_p1)

# There are three sets of duplicate ID s that we need to deal with. For now, we still want to include any other
# issues we find with these duplicates. So we're going to modify their IDs to keep going

survey_data <- survey_data %>%
  group_by(hhid) %>%
  mutate(
    hhid = case_when(
      n() > 1 ~ paste0(hhid, "_", row_number()),
      TRUE    ~ as.character(hhid)
    )
  ) %>%
  ungroup()

### Outliers ----

# In our case, want to identify values that are more than 3 standard deviations away from the variable's mean

# Step 1 — Identify variables to check

outlier_variables <- c(
  "inc_01",
  "crp10a_c1_p1",
  "crp10a_c1_p2"
)

# Note — This can also be done by running:
#   outlier_variables <- survey_data %>%
#       select(
#           starts_with("inc_"), starts_with("crp10a_c1_")
#       ) %>%
#       names()

# Step 2 — Check for variables outside of expected range

outlier_check <- outlier_variables %>%
  map_dfr(
    ~ survey_data %>%
      mutate(
        across(
          matches(.x), ~ mean(.x, na.rm = TRUE), .names = "mean"
        ),
        across(
          matches(.x), ~ sd(.x, na.rm = TRUE), .names = "sd"
        ),
        low_limit  = mean - 3 * sd,
        high_limit = mean + 3 * sd
      ) %>%
      filter(
        !!sym(.x) < low_limit | !!sym(.x) > high_limit
      ) %>%
      mutate(
        issue_var = .x,
        across(
          mean:high_limit, ~ round(.x, digits = 0)
        )
      ) %>%
      select(
        hhid, enumerator, issue_var, value = matches(.x), mean, sd, low_limit, high_limit
      )
  )

# Note — Ideally, we would want one mean and one standard deviation for income across all household
# members. Try thinking about how we could implement that into the above.

### Descriptive Statistics ----

# For each variable of interest, we want to output its:
# mean
# standard deviation
# mininum
# maximum
# median

desc_stats_variables <- c(
  "inc_01",
  "exp_25_1",
  "exp_25_2",
  "crp10a_c1_p1",
  "crp10a_c1_p2"
)

desc_stats_labels <- c(
  "inc_01"       = "First household member income",
  "exp_25_1"     = "Days household has consumed flour in past week",
  "exp_25_2"     = "Days household has consumed bread in past week",
  "crp10a_c1_p1" = "Crop 1 profit",
  "crp10a_c1_p2" = "Crop 2 profit"
)

desc_stats_check <- survey_data %>%
  summarize(
    across(
      all_of(desc_stats_variables),
      list(
        mean     = ~ mean(.x, na.rm = TRUE),
        sd       = ~ sd(.x, na.rm = TRUE),
        min      = ~ min(.x, na.rm = TRUE),
        max      = ~ max(.x, na.rm = TRUE),
        median   = ~ median(.x, na.rm = TRUE)
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
    var = desc_stats_labels[var],
    across(
      mean:median, ~ round(.x, digits = 3)
    )
  )

### Enumerator-Level Checks ----

# For each enumerator, we want to check:
# Number of submissions
# Number of submissions per day
# Average value of income and revenue

enumerator_check_by_day <- survey_data %>%
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
    enumerator, sort(peek_vars()) # So that the days are ordered chronologically
  )

enumerator_check <- survey_data %>%
  group_by(enumerator) %>%
  summarize(
    num_surveys = n(),
    across(
      c("inc_01", "crp10a_c1_p1", "crp10a_c1_p2"),
      ~ round(mean(.x, na.rm = TRUE), digits = 0), .names = "{col}_mean"
    )
  ) %>%
  ungroup() %>%
  left_join(
    enumerator_check_by_day
  )

### Geographic/Administrative Unit-Level Checks ----

# For each geographic/administrative unit (in this case, village), we want to check:
# Number of submissions
# Number of submissions per day
# Progress (%age of expected surveys completed)

village_check_by_day <- survey_data %>%
  group_by(village, submissiondate) %>%
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
      -village,
      ~ case_when(
        is.na(.x) ~ 0,
        TRUE      ~ .x
      )
    )
  ) %>%
  select(
    village, sort(peek_vars()) # So that the days are ordered chronologically
  )

village_check <- survey_data %>%
  group_by(province, district, sector, cell, village) %>% # Can't just use "village" in case there are
  # multiple villages with same name
  summarize(
    num_surveys = n()
  ) %>%
  left_join(
    admin_raw %>% select(province:village, num_to_survey) # To get the expected number of survey by village
  ) %>%
  mutate(
    progress = paste0(round(num_surveys / num_to_survey, 3) * 100, "%")
  ) %>%
  left_join(village_check_by_day)

### Survey Programming Checks ----

# Purpose — Monitor survey programming issues/peculiarities that either (1) can't be programmed into the
# survey instrument or (2) don't have to be hard conditions, but we still want to avoid where possible.

# In this case:
# Situations where the enumerator used an old survey version
# Situations where the units used for produced crops and for sold crops are not the same
# Situations where the household's site is not the correct one (just to monitor)

# Process: create a small dataframe with the same structure for each check, and then bind them

formdef_check <- survey_data %>%
  filter(formdef_version != "2305021814") %>%
  mutate(issue = "Enumerator used old survey version") %>%
  select(hhid, enumerator, issue)

unit_check <- survey_data %>%
  filter(crp08ua_c1_p1 != crp09ua_c1_p1 | crp08ua_c1_p2 != crp09ua_c1_p2) %>%
  mutate(issue = "Enumerator used different unit of measure for crops produced and crops sold") %>%
  select(hhid, enumerator, issue)

wrong_site_check <- survey_data %>%
  filter(id_10_confirm == "No") %>%
  mutate(issue = "Household had the wrong site originally recorded") %>%
  select(hhid, enumerator, issue)

survey_programming_check <- formdef_check %>%
  bind_rows(unit_check) %>%
  bind_rows(wrong_site_check) %>%
  left_join( # Let's get the variables that'll help us address these programming issues
    survey_data %>%
      select(
        hhid,
        formdef_version,                                            # for formdef_check
        crp08ua_c1_p1, crp09ua_c1_p1, crp08ua_c1_p2, crp09ua_c1_p2, # for unit_check
        id_10, id_10_corrected                                      # for wrong_site_check
      )
  )

## 5. Data Quality Checks Export ----

# Multiple options! Today we'll show how to export to Excel

hfc_excel <- createWorkbook()

hfc_sheets <- list(
  "duplicate_data"          = duplicate_check,
  "outlier_data"            = outlier_check,
  "desc_stats_data"         = desc_stats_check,
  "enum_data"               = enumerator_check,
  "village_data"            = village_check,
  "survey_programming_data" = survey_programming_check
)

map(
  names(hfc_sheets),
  .f = function(x){
    addWorksheet(hfc_excel, x)
  }
)


map2(
  hfc_sheets,
  names(hfc_sheets),
  ~ writeData(
    hfc_excel, sheet = .y, x = .x
  )
)

map(
  1:5, # First six sheets
  .f = function(x) {
    setRowHeights(hfc_excel, sheet = x, rows = 1:1000, heights = 40)
    setColWidths(hfc_excel,  sheet = x, cols = 1:1000, widths  = 20)
  }
)

# One exception
setColWidths(hfc_excel, sheet = 6, cols = 4, widths = 75)

saveWorkbook(
  hfc_excel,
  here("DataWork", "outputs", "hfc_output.xlsx"), overwrite = TRUE
)
