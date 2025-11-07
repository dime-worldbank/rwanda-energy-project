#######################################
#Purpose: Utility as outcome
#Author: XIAOMING ZHANG
#Date: September 10th 2025
######################################################
pacman::p_load(knitr, lfe,fixest, modelsummary, stargazer, tidyverse, dplyr, here, sf, haven, ggplot2, readxl,  writexl, janitor, randomizr, RCT, purrr, RODBC, DBI)

getwd()

dropbox <- 'C:/Users/wb614406/Dropbox'

data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Historical Expansion/data"
)
  
output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Historical Expansion/outputs"
)

#Read and construct data-----
utility <- read_xlsx(path = file.path(data_path, "usage_id_0416.xlsx"))


utility_long  <- utility %>% 
  pivot_longer(
    cols = matches("^\\d{4}_usage$"),
    names_to = "year",
    names_pattern = "(\\d{4})_usage",
    values_to = "usage"
  ) %>% 
  group_by(village_id, year) %>% 
  summarise(usage = sum(usage, na.rm = TRUE), .groups = "drop") %>% 
  select(village_id, year, usage) %>% 
  mutate(year = as.numeric(year))

utility_long_join <- utility_long %>% 
  filter(year == 2011 | year == 2014 | year == 2017 | year == 2020) 




expansion_utility_join <- expansion_join %>% 
  mutate(
    `elec15_17` = ifelse(electrified_year %in% c("2015", "2016", "2017"), 1, 0),
    `elec12_14` = ifelse(electrified_year %in% c("2012", "2013", "2014"), 1, 0),
    `EARP` = ifelse(earp_lv == 1 | earp_mv == 1, 1, 0)
  ) %>% 
  select(District, village_id, cell_id, primary_school, cell_office, health_center, elec15_17, elec12_14, EARP, electrified_year)



wanted_years <- c(2011, 2014, 2017, 2020)

utility_reg <- expansion_utility_join %>% 
  full_join(utility_long_join, by = "village_id") %>%
  group_by(village_id) %>%
  complete(year = wanted_years) %>%
  fill(District, cell_id, primary_school, cell_office, health_center,
       elec15_17, elec12_14, EARP, electrified_year, .direction = "downup") %>% 
  filter(!is.na(year)) %>%
  mutate(usage = ifelse(is.na(usage), 0, usage)) %>%
  ungroup()




utility_reg <- utility_reg %>%   
  filter(! District %in% c("Nyabihu", "Ngororero", "Nyamasheke", "Rubavu")) %>% 
  anti_join(earp_existing_mv, by = c("village_id" = "Village_ID")) %>% 
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    cell_id    = substr(village_id, 1, 6),
    sector_id  = substr(village_id, 1, 4),
    district_id= substr(village_id, 1, 2)
  ) %>% 
  anti_join(earp_existing_mv, by = c("village_id" = "Village_ID")) %>% 
  mutate(
    usage = ifelse(is.na(usage), 0, usage),
    usage = usage/ (100*365)
  ) 

# %>% 
#   mutate(usage = ifelse(electrified_year > year, 0, usage))


#EARP----

earp_utility_reg <- utility_reg %>% 
  filter(electrified_year > 2013)

# 2011
earp_utility_reg_2011 <- earp_utility_reg %>% filter(year == 2011)
earp_2011_utility <- felm(usage ~ EARP | cell_id + primary_school + health_center + cell_office | 0 | sector_id,
                          data = earp_utility_reg_2011)

# 2014
earp_utility_reg_2014 <- earp_utility_reg %>% filter(year == 2014)
earp_2014_utility <- felm(usage ~ EARP | cell_id + primary_school + health_center + cell_office | 0 | sector_id,
                          data = earp_utility_reg_2014)

# 2017
earp_utility_reg_2017 <- earp_utility_reg %>% filter(year == 2017)
earp_2017_utility <- felm(usage ~ EARP | cell_id + primary_school + health_center + cell_office | 0 | sector_id,
                          data = earp_utility_reg_2017)

# 2020
earp_utility_reg_2020 <- earp_utility_reg %>% filter(year == 2020)
earp_2020_utility <- felm(usage ~ EARP | cell_id + primary_school + health_center + cell_office | 0 | sector_id,
                          data = earp_utility_reg_2020)


# Summaries
summary(earp_2011_utility)
summary(earp_2014_utility)
summary(earp_2017_utility)
summary(earp_2020_utility)


#Elec12-14----


elec12_14_utility_reg <- utility_reg %>% 
  filter(electrified_year %in% c("2012", "2013", "2014") | electrified_year >2020)

# 2011
elec12_14_utility_reg_2011 <- elec12_14_utility_reg %>% filter(year == 2011)
elec12_14_2011_utility <- felm(usage ~ elec12_14 | cell_id + primary_school + health_center + cell_office | 0 | sector_id,
                          data = elec12_14_utility_reg_2011)

# 2014
elec12_14_utility_reg_2014 <- elec12_14_utility_reg %>% filter(year == 2014)
elec12_14_2014_utility <- felm(usage ~ elec12_14 | cell_id + primary_school + health_center + cell_office | 0 | sector_id,
                          data = elec12_14_utility_reg_2014)

# 2017
elec12_14_utility_reg_2017 <- elec12_14_utility_reg %>% filter(year == 2017)
elec12_14_2017_utility <- felm(usage ~ elec12_14 | cell_id + primary_school + health_center + cell_office | 0 | sector_id,
                          data = elec12_14_utility_reg_2017)

# 2020
elec12_14_utility_reg_2020 <- elec12_14_utility_reg %>% filter(year == 2020)
elec12_14_2020_utility <- felm(usage ~ elec12_14 | cell_id + primary_school + health_center + cell_office | 0 | sector_id,
                          data = elec12_14_utility_reg_2020)

# Summaries
summary(elec12_14_2011_utility)
summary(elec12_14_2014_utility)
summary(elec12_14_2017_utility)
summary(elec12_14_2020_utility)


#Elec15-17----


elec15_17_utility_reg <- utility_reg %>% 
  filter(electrified_year %in% c("2015", "2016", "2017") | electrified_year >2020)


# 2014
elec15_17_utility_reg_2014 <- elec15_17_utility_reg %>% filter(year == 2014)
elec15_17_2014_utility <- felm(usage ~ elec15_17 | cell_id + primary_school + health_center + cell_office | 0 | sector_id,
                               data = elec15_17_utility_reg_2014)

# 2017
elec15_17_utility_reg_2017 <- elec15_17_utility_reg %>% filter(year == 2017)

elec15_17_2017_utility <- felm(usage ~ elec15_17 | cell_id + primary_school + health_center + cell_office | 0 | sector_id,
                               data = elec15_17_utility_reg_2017)

# 2020
elec15_17_utility_reg_2020 <- elec15_17_utility_reg %>% filter(year == 2020)
elec15_17_2020_utility <- felm(usage ~ elec15_17 | cell_id + primary_school + health_center + cell_office | 0 | sector_id,
                               data = elec15_17_utility_reg_2020)

# Summaries
summary(elec15_17_2014_utility)
summary(elec15_17_2017_utility)
summary(elec15_17_2020_utility)

#Output to latex-------
library(stargazer)

# -------------------------
# 1) EARP group (4 models)
# -------------------------
regs_earp <- list(
  earp_2011_utility,
  earp_2014_utility,
  earp_2017_utility,
  earp_2020_utility
)

stargazer(
  regs_earp,
  output = file.path(output_path, "utility_earp.tex"),
  title  = "Regression Results: EARP"
)


# ---------------------------------------------
# 2) elec12_14 group (4 models, 2011/2014/2017/2020)
# ---------------------------------------------
regs_elec12_14 <- list(
  elec12_14_2011_utility,
  elec12_14_2014_utility,
  elec12_14_2017_utility,
  elec12_14_2020_utility
)

stargazer(
  regs_elec12_14,
  output = file.path(output_path, "utility_elec12_14.tex"),
  title  = "Regression Results: Electrified 2012–2014 vs Post-2020"
)


# ---------------------------------------------
# 3) elec15_17 group (3 models, 2014/2017/2020)
# ---------------------------------------------
regs_elec15_17 <- list(
  elec15_17_2014_utility,
  elec15_17_2017_utility,
  elec15_17_2020_utility
)

stargazer(
  regs_elec15_17,
  output = file.path(output_path, "utility_elec15_17.tex"),
  title  = "Regression Results: Electrified 2015–2017 vs Post-2020"
)
