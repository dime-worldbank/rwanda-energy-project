##############
#Author: Xiaoming Zhang
#Date: 7.30.2024
#Reproduced Date: 2026-09-03
#Reviewer: Yeji Kim 
#Purpose: Randomization primary construction
#Reproducibility Purpose: Check strata of the randomization 
#############

# Empty Environment 
rm(list = ls())

# Dependencies
pacman::p_load(here, knitr, tidyverse, dplyr, here, sf, ggplot2, readxl, 
               writexl, janitor, randomizr, RCT, purrr)

# Paths
source(here("PATHS.R"))
getwd()

#Dropbox----


if (Sys.getenv("USERNAME") == "wb636130"){
  DROPBOX <- file.path("C:/Users/wb636130/Dropbox")
}


path <- file.path(
  DROPBOX,
  "Rwanda Energy/EAQIP/datawork/RCT_data",
  "baseline/data/data/four_district_2402.xlsx"
)


data_path <- file.path(
  DROPBOX,
  "Rwanda Energy/EAQIP/datawork/RCT_data",
  "baseline/data/data"
)

scope_path <- file.path(
  DROPBOX,
  "Rwanda Energy/EAQIP/datawork/RCT_data",
  "baseline/data/Updated scope villages& households"
)

output_path <- file.path(
  DROPBOX,
  "Rwanda Energy/EAQIP/datawork/RCT_data",
  "baseline/outputs"
)

#--------------------
# Import Data
#-------------------

# Most Recent District Data 
four_district <- read_xlsx(
  # 2111 unique villages 
  path = file.path(DATA_RCT, "four_district_2408.xlsx"))
household_head <- read_xlsx(path = file.path(DATA_RCT, "household_head_clean.xlsx"))

#---------------------
# 1. Filters to scoped villages only
#---------------------

four_scope_newly<- four_district %>% 
  filter(scope_2407 == 1 & status == "newly") # 193 obs, 193 unique villages 

four_scope <- four_district %>% 
  filter(scope_2407 == 1) #714 obs, 714 unique villages

#---------------------
# Descriptive
#---------------------

# Q How many observations per lot? 
table(four_scope_newly$lot)
# Karongi  Rulindo Rusizi-1 Rusizi-2  Rutsiro 
#      29       44       48       20       52 

# Q How many customers per each district under new scope?
four_scope_newly %>% 
  group_by(district) %>% 
  summarise(
    customer = sum(customer)
  )

# # A tibble: 4 × 2
# district customer
# <chr>       <dbl>
#  1 Karongi      3316
# 2 Rulindo      5829
# 3 Rusizi       8043
# 4 Rutsiro      6939

#--------------------
# 2. Groups by stratification blocks 
#--------------------

four_scope_newly <- four_scope_newly %>%
  group_by(lot) %>%
  arrange(lot, hh_head_06) %>% 
  mutate(
    median_position = ceiling(n() / 2),  # Calculate median position within each group
    hh_head_06_order = order(hh_head_06),  # Get the order of hh_head_06 values
    median = ifelse(rank(hh_head_06_order) <= median_position, "below", "above")
  ) %>%
  ungroup() 

four_scope_newly %>% 
  group_by(lot) %>% 
  summarise(
    n = n(),
    survey = n*20, #Q why multiply by 20? 
    # Is this a standard way to calculate how many households should be surveyed?
    vulnerable = sum(hh_head_06) #does hh_head_06 is a number of vulnerable households in a village? 
  )

four_scope_newly <- four_scope_newly %>% 
  mutate(
    strata = paste0(lot, "_", median)
  )

table(four_scope_newly$strata)

# # OR
# four_scope_newly %>% 
#   group_by(lot, median) %>% 
#   summarise(n = n())

#misfit global----

#------------------------
# 3. Uses randomizr for block randomization
#-----------------------

treatment <- treatment_assign(
  data = four_scope_newly,
  share_control = 0.25,
  share_ti = c(0.25, 0.25, 0.25),
  n_t = 3,
  strata_varlist = dplyr::vars(lot, median),
  missfits = "global", # 4. Assigns equal numbers to each arm
  key = "village_id",
  seed = 080501
)

names(treatment)

#----------------------------------------
# # Randomization Method Interpretation 
#----------------------------------------

# 193 scope villages were assigned to treatment using two-level stratification. 
# The first level is the lot: each district constitutes one lot, except Ruisizi, 
# which is split into two, giving five lots in total. 
# The second level splits each lot into roughly equal halves by hh_head_06 
# (later gets renamed to vulnerable, so most likely the count of vulnerable households in the village): 
# villages are ranked within their lot from the fewest hh_head_06 (ranking 1) upward, 
# and the lower-ranked half is labeled "below" while the upper half "above''.  
# This creates 10 strata (five lots * two halves), each an approximately equal sized "below" OR "above" group. 
# Within each stratum, 25% of villages are assigned to control and 25% to each of the three treatment arms.
# 
# In case a strata is not exactly a multiple of four, 
# the misfits will be pooled and randomly assigned to one of four arms to ensure equal sizes across arms (~48). 
# In our sample of 193 villages, there are 17 misfit villages and 
# final assignment is 48 villages for control, T1, T2 and 49 villages for T3.

treatment <- as.data.frame(treatment$data) %>% 
  select(
    village_id, treat
  ) 

table(treatment$treat)

#----------------------------
# 5. Creates treatment indicators and labels
#----------------------------

four_scope_newly <- left_join(four_scope_newly, treatment, by = c("village_id"))

table(four_scope_newly$treat)


rand_newly_sum <- four_scope_newly %>% 
  group_by(strata) %>% 
  summarise(
    C = sum(treat == 0),
    T1 = sum(treat == 1),
    T2 = sum(treat == 2),
    T3 = sum(treat == 3),
    sum = n()
  ) 


summarise_row <-rand_newly_sum %>% 
  summarise(
    strata = "Total",
    C = sum(rand_newly_sum$C),
    T1 = sum(rand_newly_sum$T1),
    T2 = sum(rand_newly_sum$T2),
    T3 = sum(rand_newly_sum$T3),
    sum = sum(rand_newly_sum$sum)
  )

rand_newly_sum <- bind_rows(rand_newly_sum, summarise_row)

View(rand_newly_sum)

kable(rand_newly_sum, format = "latex", booktabs = TRUE)

#-------------------------
# Replication Ends here
#-------------------------

#------------------------
# 6. HH_id construct
#------------------------

get_unit_digit <- function(id) {
  digits_sum <- sum(as.numeric(unlist(strsplit(as.character(id), ""))))
  unit_digit <- digits_sum %% 10
  return(unit_digit)
}

household_head <- household_head %>%
  group_by(villageid_key) %>%
  mutate(
    villageid_key = as.numeric(villageid_key),
    household = row_number(),
    household_id = villageid_key*10000 + household*10,
    unit_digit = sapply(household_id, get_unit_digit),
    household_id = household_id + unit_digit
  ) %>%
  #   unit_digit = sapply(household_id, get_unit_digit),
  ungroup() %>% 
  mutate(
    villageid_key = as.character(villageid_key),
    household_id = as.character(household_id)
  )


write_xlsx(household_head, path = file.path(data_path, "household_head_clean.xlsx"))


##Sample hh----
household_head <- read_xlsx(path = file.path(data_path, "household_head_clean.xlsx"))

household_head_scope <- household_head %>% 
  filter(villageid_key %in% four_scope_newly$village_id) 


household_select <- household_head_scope %>%
  group_by(villageid_key) %>%
  nest() %>%
  mutate(sampled = map(data, ~ if (nrow(.x) <= 20) .x else sample_n(.x, size = 20))) %>% 
  select(villageid_key, sampled) %>%
  unnest(sampled)


household_backup <- household_head_scope %>% 
  (!household_id %in% household_select$household_id)


household_backup <- household_backup %>%
  group_by(villageid_key) %>%
  nest() %>%
  mutate(sampled = map(data, ~ if (nrow(.x) <= 1) sample_n(.x, size = 1) else sample_n(.x, size = 2)))  %>% 
  select(villageid_key, sampled) %>%
  unnest(sampled)


check <- household_backup %>% 
  group_by(villageid_key) %>% 
  summarise(
    n = n()
  )


# household_select <- anti_join(household_select, household_backup, by = c("household_id"))


household <- list( `selected` = household_select, `backup` = household_backup)


write_xlsx(
  household, path = file.path(scope_path, "scope_households_0807.xlsx")
)

write_xlsx(
  household_select, path = file.path(scope_path, "household_select_0807.xlsx")
)

write_xlsx(
  household_backup, path = file.path(scope_path, "household_backup_0807.xlsx")
)

#household situation-----

household_backup <- read_xlsx(path = file.path(scope_path, "household_backup_0807.xlsx"))

household_select <- read_xlsx(path = file.path(scope_path, "household_select_0807.xlsx"))


household_join <- bind_rows(household_backup, household_select)

write_xlsx(household_join, path = file.path(scope_path, "household_join_1111.xlsx"))






#Join back to get summary----


join <- household_select %>% 
  group_by(villageid_key) %>%
  summarise(surveyed = n()) %>% 
  mutate(villageid_key = as.character(villageid_key))


four_scope_newly <- left_join(four_scope_newly, join, by = c("village_id" = "villageid_key"))

# four_scope_newly <- four_scope_newly %>% 
#   select(-lot_rusizi)

summarise <- four_scope_newly %>%
  group_by(lot) %>%
  summarise(
    n = n(),
    survey_hh = sum(surveyed),
    vulnerable = sum(hh_head_06),
    surveyed = sum(customer)
  )


View(summarise)

four_scope_newly <- four_scope_newly %>% 
  mutate(
    treat = case_when(
      treat == 0  ~ "C",
      treat == 1  ~ "T1",
      treat == 2  ~ "T2",
      treat == 3 ~  "T3"
    )
  )

write_xlsx(four_scope_newly, path = file.path(scope_path, "scope_193_0807.xlsx"))

four_scope_newly %>% 
  group_by(lot) %>% 
  summarise(
    vulnerable_household = sum(hh_head_06),
    readyboard_request = sum(case_when(
      treat %in% c("T1", "T3") ~ hh_head_06,
      TRUE ~ 0
    ))
  ) %>% 
  ungroup()


