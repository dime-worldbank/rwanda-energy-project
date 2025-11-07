#######################################
#Purpose: Group by working place
#Author: XIAOMING ZHANG
#Date: November 4th
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


#Sample restriction----

expansion_join_drop12_17 <- expansion_join%>% 
  filter(electrified_year %in% c("2012", "2013", "2014","2015", "2016", "2017") |electrified_year == "9999" ) %>% 
  mutate(
    `elec12_14` = ifelse(electrified_year %in% c("2012", "2013", "2014"), 1, 0),
    `elec15_17` = ifelse(electrified_year %in% c("2015", "2016", "2017"), 1, 0)
    
  ) %>% 
  anti_join(earp_existing_mv, by = c("village_id" = "Village_ID")) %>% 
  filter(! District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu")) %>% 
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0)
  )



#Event study with ISIC restriction----


##ISIC selection------

#utility_long join



ec_2011 <- read_xlsx(path = file.path(data_path, "2011", "group_long_2011(isic).xlsx"), sheet ="working_place")

ec_2011 <- ec_2011 %>% 
  mutate(village_id = as.character(village_id)) %>% 
  rename(num_establishment = n) %>% 
  group_by(village_id, isic_level1) %>% 
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE)) %>% 
  select(village_id, num_establishment, total_employee, isic_level1) %>% 
  mutate(year = 2011) %>% 
  ungroup()

ec_2014 <- read_xlsx(path = file.path(data_path, "2014", "group_long_2014(isic).xlsx"), sheet ="working_place")

ec_2014 <- ec_2014 %>% 
  mutate(village_id = as.character(village_id)) %>% 
  rename(num_establishment = n,
         isic_level1 = isic_level1_main) %>% 
  group_by(village_id, isic_level1) %>% 
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE)) %>% 
  select(village_id, num_establishment, total_employee, isic_level1) %>% 
  mutate(year = 2014) %>% 
  ungroup()


ec_2017 <- read_xlsx(path = file.path(data_path, "2017", "group_long_2017(isic).xlsx"), sheet ="working_place")

ec_2017 <- ec_2017 %>% 
  mutate(village_id = as.character(village_id)) %>% 
  rename(num_establishment = n,
         isic_level1 = isic_1_digit) %>% 
  group_by(village_id, isic_level1) %>% 
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE)) %>% 
  select(village_id, num_establishment, total_employee, isic_level1) %>% 
  mutate(year = 2017) %>% 
  ungroup()

ec_2020 <- read_xlsx(path = file.path(data_path, "2020", "group_long_2020(isic).xlsx"), sheet ="working_place")

ec_2020 <- ec_2020 %>% 
  mutate(village_id = as.character(village_id)) %>% 
  rename(num_establishment = n) %>% 
  group_by(village_id, isic_level1) %>% 
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE)) %>% 
  select(village_id, num_establishment, total_employee, isic_level1) %>% 
  mutate(year = 2020) %>% 
  ungroup()


ec_all <- rbind(ec_2011, ec_2014, ec_2017, ec_2020)



elec12_17_join <- expansion_join_drop12_17%>% 
  select(village_id,elec12_14, elec15_17, cell_id, sector_id, district_id) 

elec12_17_did_isic<- left_join( elec12_17_join, ec_all, by = c("village_id"))


elec12_17_did_isic <- elec12_17_did_isic %>%
  complete(year, village_id, isic_level1,
           fill = list(num_establishment = 0, total_employee = 0)) %>% 
  filter(!is.na(year))   %>% 
  filter(!is.na(isic_level1)) %>% 
  select(year, village_id, num_establishment, total_employee, isic_level1)


elec12_17 <- expansion_join_drop12_17%>%
  select(village_id, elec12_14, elec15_17, earp_existing_mv, health_center, primary_school, cell_office, secondary_school, sector_district_office,
         industry, market, residential_consumer, non_residential_consumer, imidugudu) 


elec12_17_did_isic <- left_join(elec12_17_did_isic, elec12_17, by = c("village_id"))



elec12_17_did_isic <- elec12_17_did_isic %>% 
  mutate(
    cell_id    = substr(village_id, 1, 6),
    sector_id  = substr(village_id, 1, 4),
    district_id= substr(village_id, 1, 2)
  ) %>% 
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  ) %>%
  filter(!is.na(isic_level1)) %>%
  mutate(
    isic_level1 = as.character(isic_level1)  ) %>% 
  rename(
    isic = isic_level1
  )  





# Private sector data clean-------


join_drop12_17 <- expansion_join_drop12_17 %>% 
  select(`elec12_14`, elec15_17, village_id, cell_id, sector_id, cell_office, health_center, primary_school, secondary_school, 
         sector_district_office, market, industry, residential_consumer,  non_residential_consumer, imidugudu, electrified_year, population, dist_na_rd)

elec12_17_p <- elec12_17_did_isic %>%
  mutate(private_sector = ifelse(isic %in% c(9,7,3,19), 1, 0)) %>%
  filter(private_sector == 1) %>%
  group_by(year, village_id) %>%
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE), .groups="drop") %>%
  mutate(total_employee = pmin(total_employee, 100)) %>% 
  left_join(join_drop12_17, by = c("village_id") )  %>%
  mutate(
    cell_id = as.factor(cell_id),
    cell_office = as.factor(cell_office),
    health_center = as.factor(health_center),
    primary_school = as.factor(primary_school),
    secondary_school = as.factor(secondary_school),
    industry = as.factor(industry),
    market = as.factor(market),
    imidugudu = as.factor(imidugudu),
    population = as.factor(population)
  ) %>% 
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0)
  ) %>% 
  mutate(
    log1_num_establishment = log(num_establishment + 1),
    log1_total_employee = log(total_employee + 1)
  ) 

elec12_17_p <- elec12_17_p %>%
  mutate(
    treatment = ifelse(elec12_14 == 1 | elec15_17 == 1, 1, 0),
    
    period = case_when(
      treatment == 0 ~ "1-3 years before electrification",
      
      elec12_14 == 1 & year == 2011 ~ "1-3 years before electrification",
      elec12_14 == 1 & year == 2014 ~ "electrified 0-2 years",
      elec12_14 == 1 & year == 2017 ~ "electrified 3-5 years",
      elec12_14 == 1 & year == 2020 ~ "electrified 6-8 years",
      
      elec15_17 == 1 & year == 2011 ~ "4-6 years before electrification",
      elec15_17 == 1 & year == 2014 ~ "1-3 years before electrification",
      elec15_17 == 1 & year == 2017 ~ "electrified 0-2 years",
      elec15_17 == 1 & year == 2020 ~ "electrified 3-5 years",
      
      # Fallback (if no match)
      TRUE ~ NA_character_
    ),
    
    # Ensure consistent factor ordering
    period = factor(
      period,
      levels = c(
        "1-3 years before electrification",
        "4-6 years before electrification",
        "electrified 0-2 years",
        "electrified 3-5 years",
        "electrified 6-8 years"
      )
    )
  ) 
