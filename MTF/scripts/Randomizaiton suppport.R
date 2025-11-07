############
#Author: Xiaoming Zhang
#Date: 1.24.2024
#Purpose: Help with understanding the scope for randomization
############


pacman::p_load(tidyverse, dplyr, here, sf, 
               stringr, haven, lmtest, fixest, kableExtra,
               ggplot2, readxl, writexl, janitor, randomizr,
               stargazer, modelsummary)

getwd()

#read the clean data----

mtf_join <- read.csv(here("outputs", "MTF_join_clean_fuel_0823.csv"))

#check for unique hhid----
is_unique <- length(unique(mtf_join$HHID)) == nrow(mtf_join)

# Print the result
cat("Is HHID column unique:", is_unique, "\n")


#MTF_Join
mtf_analysis <- mtf_join %>% 
  filter(district %in% c("Karongi", "Rulindo", "Rutsiro")) %>% 
  group_by(cluster) %>% 
  summarise(
    hh_number = n(),
    hh_electrified = sum(electricity == 1),
    percent_electrified = round(hh_electrified/hh_number,2),
    ubudehe_1 = sum(ubudehe == 1),
    ubu1_electrified = sum(ubudehe == 1 & electricity == 1),
    percent_ubu1elec = round(ubu1_electrified/ubudehe_1, 2)
  )


mtf_cluster <- mtf_join %>% 
  select(cluster, district, province)

mtf_analysis <- left_join(mtf_analysis, mtf_cluster, by = c("cluster"))

mtf_analysis <- mtf_analysis %>% 
  select(province, district, cluster, everything()) %>% 
  mutate(percent_ubu1elec = ifelse(is.na(percent_ubu1elec), 0, percent_ubu1elec))

mtf_ubu1_no <- mtf_analysis %>% 
  summarise(
    no_ubu1 = sum(ubu1_electrified == 0),
    less_25 = sum(percent_electrified <= 0.25),
    less_25_ubu = sum(ubudehe_1[percent_electrified <= 0.25])
  )


mtf_25<- mtf_analysis %>% 
  filter(percent_electrified <= 0.25)
