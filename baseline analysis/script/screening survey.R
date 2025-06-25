##############
#Author: Xiaoming Zhang
#Date: 6.13.2025
#Purpose: Compare EDCL list
#############


pacman::p_load(knitr, stargazer, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr, lfe)
library(googlesheets4)
getwd()


# Import Data ----
dropbox <- 'C:/Users/wb614406/Dropbox'

output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/baseline analysis/output"
)

data_path_1 <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data"
)

hfc_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/HFC/data"
)


data_path_2 <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/Updated scope villages& households"
)


#Read files------

deployment_households <- read_xlsx(path = file.path(data_path_2, "Household and village list for deployment.xlsx"), sheet = "household list")
scope_village <- read_xlsx(path = file.path(data_path_2, "scope_193_0807.xlsx"))


scope_village <- scope_village %>%
  mutate(
    village_id = as.numeric(village_id)
  ) %>%
  select(village_id, name, cell, sector, district) 




screening_survey <- read.csv(file.path(hfc_path, "REP_screening_WIDE.csv"))

screening_survey <- screening_survey %>%
  mutate(
    across(everything(), ~ ifelse(.x == "", NA, .x))
  ) %>% 
  left_join(
    scope_village, 
    by = c("village" = "village_id")
  )


complete  <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","vulnerable households in sample villages.xlsx"))

complete_report_join <- complete %>% 
  filter(
    `In Fully Completed village` == "No" &`Approached by Lattanzio` == "No" & `Dropped from scope due to 15kv` == "No"
  ) 

complete_report_village <- complete %>% 
  filter(
    `In Fully Completed village` == "No" &`Approached by Lattanzio` == "No" & `Dropped from scope due to 15kv` == "No"
  ) %>% 
  select(villageid_key, village, cell, sector, district) %>% 
  distinct(villageid_key, .keep_all = TRUE)


write_xlsx(
  list(
    "Household_List" = complete_report_join,
    "Village_List" = complete_report_village
  ),
    path = file.path(data_path_2, "Household and village list for deployment.xlsx")
)


screening_check_village <- screening_survey %>% 
  filter(!village %in% complete_report_village$villageid_key)

screening_check <- screening_survey %>%
  filter(
    !village %in% complete_report_join$villageid_key | 
      !hh_id %in% complete_report_join$household_id
  )

write_xlsx(screening_check, path = file.path(data_path_2, "Screening survey not in scope.xlsx"))


screening_in <- screening_survey %>%
  filter(
    village %in% complete_report_join$villageid_key & 
      hh_id %in% complete_report_join$household_id
  )

sum(!is.na(screening_in$eligible))




