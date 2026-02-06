##################################
#Updated scope
#2026-01-19
#Xiaoming



pacman::p_load(knitr, stargazer, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr, lfe, install = TRUE)
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


#Village treatment----


village_treatment <- read_xlsx(path = file.path(data_path_2, "scope_193_0807.xlsx")) %>% 
  select(village_id, name, cell, sector, district, lot, treat) %>% 
  mutate(
    treat_name = case_when(
      treat == "C" ~ "Control",
      treat == "T1" ~ "Readyboard",
      treat == "T2" ~ "Solar off grid",
      treat == "T3" ~ "Readyboard & Solar off grid"
    )
  )

scope_updated <- read_xlsx(path = file.path(data_path_2, "updated scope 202601.xlsx")) %>% 
  clean_names() %>% 
  mutate(village_id = as.character(village_id))

village_treatment <- left_join(village_treatment, scope_updated, by = c("village_id"))

village_treatment <- village_treatment %>% 
  mutate(scope = ifelse(is.na(scope), "No", scope))

off_grid_village <- village_treatment %>% 
  filter(scope == "Yes") %>% 
  filter(treat == "T2" | treat == "T3")

write_xlsx(village_treatment, path = file.path(data_path_2, "village treatment(updated scope).xlsx"))
write_xlsx(off_grid_village, path = file.path(data_path_2, "off-grid villages(updated scope).xlsx"))


write_xlsx(village_treatment, path = file.path(data_path_2,"Latest Scope", "village treatment(updated scope).xlsx"))
write_xlsx(off_grid_village, path = file.path(data_path_2,"Latest Scope", "off-grid villages(updated scope).xlsx"))

#Household treatment====


complete  <- read_xlsx(path = file.path(data_path_2, "survey status of vulnerable households in sample villages_final(enumerator).xlsx"))

village_scope_join <- village_treatment %>% 
  select(village_id, scope, treat, treat_name)

complete_scope <- left_join(complete, village_scope_join, by = c("villageid_key" = "village_id"))

complete_scope <- complete_scope %>% 
  select(villageid_key, village, cell, sector, district, household_id, first_name, last_name, gender, nid, treat, treat_name, everything())

View(complete_scope)

write_xlsx(complete_scope, path = file.path(data_path_2, "Latest Scope", "household_list(survey status & scope & treatment).xlsx"))
