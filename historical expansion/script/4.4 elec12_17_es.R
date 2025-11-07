#######################################
#Purpose: Using EARP as a control for analysis
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


#Sample restriction----

expansion_join_drop12_17 <- expansion_join%>% 
  filter(electrified_year %in% c("2012", "2013", "2014","2015", "2016", "2017") |electrified_year == "9999" ) %>% 
  mutate(
    `elec12_14` = ifelse(electrified_year %in% c("2012", "2013", "2014"), 1, 0),
    `elec15_17` = ifelse(electrified_year %in% c("2015", "2016", "2017"), 1, 0)
    
  ) %>% 
  anti_join(earp_existing_mv, by = c("village_id" = "Village_ID")) %>% 
  filter(! District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu")) 

#Event study with ISIC restriction----


##ISIC selection------

#utility_long join



ec_2011 <- read_xlsx(path = file.path(data_path, "2011", "group_long_2011(isic).xlsx"))

ec_2011 <- ec_2011 %>% 
  mutate(village_id = as.character(village_id)) %>% 
  rename(num_establishment = n) %>% 
  group_by(village_id, isic_level1) %>% 
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE)) %>% 
  select(village_id, num_establishment, total_employee, isic_level1) %>% 
  mutate(year = 2011) %>% 
  ungroup()

ec_2014 <- read_xlsx(path = file.path(data_path, "2014", "group_long_2014(isic).xlsx"))

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


ec_2017 <- read_xlsx(path = file.path(data_path, "2017", "group_long_2017(isic).xlsx"))

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

ec_2020 <- read_xlsx(path = file.path(data_path, "2020", "group_long_2020(isic).xlsx"))

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
  filter(!is.na(isic_level1)) %>%
  mutate(
    isic_level1 = as.character(isic_level1)  ) %>% 
  rename(
    isic = isic_level1
  )  





# Private sector data clean-------


join_drop12_17 <- expansion_join_drop12_17 %>% 
  select(`elec12_14`, elec15_17, village_id, cell_id, sector_id, cell_office, health_center, primary_school, secondary_school, 
         sector_district_office, market, industry, residential_consumer,  non_residential_consumer, imidugudu, dist_na_rd, population)

elec12_17_p <- elec12_17_did_isic %>%
  mutate(private_sector = ifelse(isic %in% c(9,7,3,19), 1, 0)) %>%
  filter(private_sector == 1) %>%
  group_by(year, village_id) %>%
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE), .groups="drop") %>%
  mutate(total_employee = pmin(total_employee, 100)) %>% 
  left_join(join_drop12_17, by = c("village_id") )  %>%
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    imidugudu_year = paste0(imidugudu, "_", year)
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
  ) %>% 
 mutate(
   dist_abv = ifelse(dist_na_rd < 1, 1, 0),
   pop_abv = ifelse(population > median(population, na.rm = TRUE), 1, 0)
 ) %>% 
  clean_names() %>% 
  mutate(
    year = as.factor(year),
    cell_id = as.factor(cell_id)
  )

##regression --------

###Base-----
reg_est <- felm(
  num_establishment ~ 
    period +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p 
)

summary(reg_est)



reg_emp <- felm(
  total_employee ~ 
    period +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p
)

summary(reg_emp)


### road----------


reg_est_rd <- felm(
  num_establishment ~ 
    period +
    period * dist_abv + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * dist_abv|
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p 
)

summary(reg_est_rd)



reg_emp_rd <- felm(
  total_employee ~ 
    period +
    period * dist_abv + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * dist_abv|
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p 
)

summary(reg_emp_rd)


### population-------

reg_est_pop <- felm(
  num_establishment ~ 
    period +
    period * pop_abv + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * pop_abv|
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p 
)

summary(reg_est_pop)



reg_emp_pop <- felm(
  total_employee ~ 
    period +
    period * pop_abv + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * pop_abv|
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p 
)

summary(reg_emp_pop)



###residential consumer------


reg_est_consumer <- felm(
  num_establishment ~ 
    period +
    period * any_non_residential_consumer + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * any_non_residential_consumer|
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p 
)

summary(reg_est_consumer)



reg_emp_consumer <- felm(
  total_employee ~ 
    period +
    period * any_non_residential_consumer + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * any_non_residential_consumer|
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p 
)

summary(reg_emp_consumer)


###Outcome only on covariate======

road_est <- felm(num_establishment ~ dist_abv + year + year*dist_abv|cell_year , data = elec12_17_p)

summary(road_est)





# Manufacture -------

elec12_17_p.3 <- elec12_17_did_isic %>%
  mutate(private_sector = ifelse(isic %in% c(3), 1, 0)) %>%
  filter(private_sector == 1) %>%
  group_by(year, village_id) %>%
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE), .groups="drop") %>%
  mutate(total_employee = pmin(total_employee, 100)) %>% 
  left_join(join_drop12_17, by = c("village_id") )  %>%
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    imidugudu_year = paste0(imidugudu, "_", year), 
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
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

elec12_17_p.3 <- elec12_17_p.3 %>%
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
      
      TRUE ~ NA_character_
    ),
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
  ) %>% 
  mutate(
    dist_abv = ifelse(dist_na_rd > median(dist_na_rd, na.rm = TRUE), 1, 0),
    pop_abv = ifelse(population > median(population, na.rm = TRUE), 1, 0)
  ) %>% 
  clean_names() %>% 
  mutate(
    year = as.factor(year)
  )

## regression --------

### BASE -------------------------------------------------------------

reg_est.3 <- felm(
  num_establishment ~ 
    period +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.3
)

summary(reg_est.3)

reg_emp.3 <- felm(
  total_employee ~ 
    period +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.3
)

summary(reg_emp.3)


### ROAD -------------------------------------------------------------

reg_est_rd.3 <- felm(
  num_establishment ~ 
    period +
    period * dist_abv + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * dist_abv |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.3
)

summary(reg_est_rd.3)

reg_emp_rd.3 <- felm(
  total_employee ~ 
    period +
    period * dist_abv + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * dist_abv |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.3
)

summary(reg_emp_rd.3)


### POPULATION -------------------------------------------------------------

reg_est_pop.3 <- felm(
  num_establishment ~ 
    period +
    period * pop_abv + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * pop_abv |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.3
)

summary(reg_est_pop.3)

reg_emp_pop.3 <- felm(
  total_employee ~ 
    period +
    period * pop_abv + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * pop_abv |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.3
)

summary(reg_emp_pop.3)


### CONSUMER -------------------------------------------------------------

reg_est_consumer.3 <- felm(
  num_establishment ~ 
    period +
    period * any_non_residential_consumer + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * any_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.3
)

summary(reg_est_consumer.3)

reg_emp_consumer.3 <- felm(
  total_employee ~ 
    period +
    period * any_non_residential_consumer + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * any_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.3
)

summary(reg_emp_consumer.3)




# wholesale -------

elec12_17_p.7 <- elec12_17_did_isic %>%
  mutate(private_sector = ifelse(isic %in% c(7), 1, 0)) %>%
  filter(private_sector == 1) %>%
  group_by(year, village_id) %>%
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE), .groups="drop") %>%
  mutate(total_employee = pmin(total_employee, 100)) %>% 
  left_join(join_drop12_17, by = c("village_id") )  %>%
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    imidugudu_year = paste0(imidugudu, "_", year), 
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
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

elec12_17_p.7 <- elec12_17_p.7 %>%
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
      
      TRUE ~ NA_character_
    ),
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
  ) %>% 
  mutate(
    dist_abv = ifelse(dist_na_rd > median(dist_na_rd, na.rm = TRUE), 1, 0),
    pop_abv = ifelse(population > median(population, na.rm = TRUE), 1, 0)
  ) %>%
  clean_names() %>% 
  mutate(
    year = as.factor(year)
  )

## regression --------

### BASE -------------------------------------------------------------

reg_est.7 <- felm(
  num_establishment ~ 
    period +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.7
)

summary(reg_est.7)

reg_emp.7 <- felm(
  total_employee ~ 
    period +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.7
)

summary(reg_emp.7)


### ROAD -------------------------------------------------------------

reg_est_rd.7 <- felm(
  num_establishment ~ 
    period +
    period * dist_abv + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * dist_abv |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.7
)

summary(reg_est_rd.7)

reg_emp_rd.7 <- felm(
  total_employee ~ 
    period +
    period * dist_abv + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * dist_abv |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.7
)

summary(reg_emp_rd.7)


### POPULATION -------------------------------------------------------------

reg_est_pop.7 <- felm(
  num_establishment ~ 
    period +
    period * pop_abv + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * pop_abv |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.7
)

summary(reg_est_pop.7)

reg_emp_pop.7 <- felm(
  total_employee ~ 
    period +
    period * pop_abv + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * pop_abv |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.7
)

summary(reg_emp_pop.7)


### CONSUMER -------------------------------------------------------------

reg_est_consumer.7 <- felm(
  num_establishment ~ 
    period +
    period * any_non_residential_consumer + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * any_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.7
)

summary(reg_est_consumer.7)

reg_emp_consumer.7 <- felm(
  total_employee ~ 
    period +
    period * any_non_residential_consumer + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * any_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.7
)

summary(reg_emp_consumer.7)





# food&accomodation -------

elec12_17_p.9 <- elec12_17_did_isic %>%
  mutate(private_sector = ifelse(isic %in% c(9), 1, 0)) %>%
  filter(private_sector == 1) %>%
  group_by(year, village_id) %>%
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE), .groups="drop") %>%
  mutate(total_employee = pmin(total_employee, 100)) %>% 
  left_join(join_drop12_17, by = c("village_id") )  %>%
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    imidugudu_year = paste0(imidugudu, "_", year), 
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
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

elec12_17_p.9 <- elec12_17_p.9 %>%
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
      
      TRUE ~ NA_character_
    ),
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
  ) %>% 
  mutate(
    dist_abv = ifelse(dist_na_rd > median(dist_na_rd, na.rm = TRUE), 1, 0),
    pop_abv = ifelse(population > median(population, na.rm = TRUE), 1, 0)
  ) %>%
  clean_names() %>% 
  mutate(
    year = as.factor(year)
  )

## regression --------

### BASE -------------------------------------------------------------

reg_est.9 <- felm(
  num_establishment ~ 
    period +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.9
)

summary(reg_est.9)

reg_emp.9 <- felm(
  total_employee ~ 
    period +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.9
)

summary(reg_emp.9)


### ROAD -------------------------------------------------------------

reg_est_rd.9 <- felm(
  num_establishment ~ 
    period +
    period * dist_abv + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * dist_abv |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.9
)

summary(reg_est_rd.9)

reg_emp_rd.9 <- felm(
  total_employee ~ 
    period +
    period * dist_abv + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * dist_abv |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.9
)

summary(reg_emp_rd.9)


### POPULATION -------------------------------------------------------------

reg_est_pop.9 <- felm(
  num_establishment ~ 
    period +
    period * pop_abv + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * pop_abv |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.9
)

summary(reg_est_pop.9)

reg_emp_pop.9 <- felm(
  total_employee ~ 
    period +
    period * pop_abv + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * pop_abv |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.9
)

summary(reg_emp_pop.9)


### CONSUMER -------------------------------------------------------------

reg_est_consumer.9 <- felm(
  num_establishment ~ 
    period +
    period * any_non_residential_consumer + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * any_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.9
)

summary(reg_est_consumer.9)

reg_emp_consumer.9 <- felm(
  total_employee ~ 
    period +
    period * any_non_residential_consumer + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * any_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.9
)

summary(reg_emp_consumer.9)








# other -------

elec12_17_p.19 <- elec12_17_did_isic %>%
  mutate(private_sector = ifelse(isic %in% c(19), 1, 0)) %>%
  filter(private_sector == 1) %>%
  group_by(year, village_id) %>%
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE), .groups="drop") %>%
  mutate(total_employee = pmin(total_employee, 100)) %>% 
  left_join(join_drop12_17, by = c("village_id") )  %>%
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    imidugudu_year = paste0(imidugudu, "_", year), 
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
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

elec12_17_p.19 <- elec12_17_p.19 %>%
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
      
      TRUE ~ NA_character_
    ),
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
  ) %>% 
  mutate(
    dist_abv = ifelse(dist_na_rd > median(dist_na_rd, na.rm = TRUE), 1, 0),
    pop_abv = ifelse(population > median(population, na.rm = TRUE), 1, 0)
  ) %>%
  clean_names() %>% 
  mutate(
    year = as.factor(year)
  )

## regression --------

### BASE -------------------------------------------------------------

reg_est.19 <- felm(
  num_establishment ~ 
    period +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.19
)

summary(reg_est.19)

reg_emp.19 <- felm(
  total_employee ~ 
    period +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.19
)

summary(reg_emp.19)


### ROAD -------------------------------------------------------------

reg_est_rd.19 <- felm(
  num_establishment ~ 
    period +
    period * dist_abv + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * dist_abv |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.19
)

summary(reg_est_rd.19)

reg_emp_rd.19 <- felm(
  total_employee ~ 
    period +
    period * dist_abv + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * dist_abv |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.19
)

summary(reg_emp_rd.19)


### POPULATION -------------------------------------------------------------

reg_est_pop.19 <- felm(
  num_establishment ~ 
    period +
    period * pop_abv + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * pop_abv |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.19
)

summary(reg_est_pop.19)

reg_emp_pop.19 <- felm(
  total_employee ~ 
    period +
    period * pop_abv + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * pop_abv |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.19
)

summary(reg_emp_pop.19)


### CONSUMER -------------------------------------------------------------

reg_est_consumer.19 <- felm(
  num_establishment ~ 
    period +
    period * any_non_residential_consumer + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * any_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.19
)

summary(reg_est_consumer.19)

reg_emp_consumer.19 <- felm(
  total_employee ~ 
    period +
    period * any_non_residential_consumer + 
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer +
    year * any_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.19
)

summary(reg_emp_consumer.19)


#Stargazer--------



# === Helper: Extract coefficient terms from LaTeX ==============
extract_terms_period <- function(tex_file_path, covariate = NULL) {
  # Read LaTeX file
  lines <- readLines(tex_file_path)
  
  # Helper to grab coefficient + SE + blank line
  extract_chunk <- function(indexes) {
    chunks <- lapply(indexes, function(idx) {
      end <- min(idx + 2, length(lines))
      lines[idx:end]
    })
    unlist(chunks)
  }
  
  # ----  Extract "clean" period rows (exclude underscored versions) 
  period_idx <- grep("^\\s*period(?!_)", lines, perl = TRUE)
  period_terms <- extract_chunk(period_idx)
  
  # ----  Extract covariate × year interactions (separate) 
  covariate_idx <- grep(":year", lines, fixed = TRUE)
  covariate_terms <- extract_chunk(covariate_idx)
  
  
  # ---- Extract Observations and R² lines 
  obs_line <- lines[grep("^Observations\\s*&", lines)]
  r2_line  <- lines[grep("^R\\$\\^\\{2\\}\\$\\s*&", lines)]
  
  # ----  Return as separate elements 
  list(
    period = period_terms,
    covariate = covariate_terms,
    obs = obs_line,
    r2 = r2_line
  )
}



# === Helper: Combine and write sector-level tables by spec =====


make_sector_table <- function(
    reg_list, output_name, title, label, covariate = NULL
) {
  # 1. Export combined LaTeX
  raw_path <- file.path(output_path, "regressions", "raw", paste0(output_name, "_raw.tex"))
  
  stargazer(
    reg_list,
    type = "latex",
    out = raw_path,
    title = title,
    label = label,
    column.labels = rep(c("Num. Establishments", "Total Employees"), 5),
    dep.var.labels.include = FALSE,
    keep.stat = c("n", "rsq"),
    omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
    header = FALSE,
    digits = 3,
    float = TRUE,
    float.env = "table",
    table.placement = "!htbp"
  )
  
  # 2. Extract coefficients
  extracted <- extract_terms_period(raw_path, covariate)
  
  # 3. Write cleaned LaTeX summary
  writeLines(c(
    "\\begin{table}[hbt!]",
    "\\centering",
    paste0("\\caption{", title, "}"),
    paste0("\\label{tab:", label, "}"),
    "\\small",
    "\\resizebox{\\textwidth}{!}{%",
    "\\begin{tabular}{@{\\extracolsep{5pt}}lcccccccccc}",
    "\\\\[-1.8ex]\\hline",
    "\\hline \\\\[-1.8ex]",
    " & \\multicolumn{10}{c}{\\textit{Dependent variable:}} \\\\",
    "\\cline{2-11}",
    "\\\\[-1.8ex] & \\multicolumn{2}{c}{Private Sector} & \\multicolumn{2}{c}{Manufacturing} & \\multicolumn{2}{c}{Wholesale} & \\multicolumn{2}{c}{Food \\& Accom.} & \\multicolumn{2}{c}{Barbershop} \\\\",
    "\\\\[-1.8ex] & num\\_establishment & total\\_employee & num\\_establishment & total\\_employee & num\\_establishment & total\\_employee & num\\_establishment & total\\_employee & num\\_establishment & total\\_employee \\\\",
    " & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) & (9) & (10) \\\\",
    "\\hline \\\\[-1.8ex]",
    extracted$period,
    extracted$covariate,
  
    "\\hline \\\\[-1.8ex]",
    extracted$obs,
    extracted$r2,
    "FE: Village & X & X & X & X & X & X & X & X & X & X \\\\",
    "FE: Cell-Year & X & X & X & X & X & X & X & X & X & X \\\\",
    "FE: Cell Office-Year & X & X & X & X & X & X & X & X & X & X \\\\",
    "FE: Health Center-Year & X & X & X & X & X & X & X & X & X & X \\\\",
    "FE: Primary School-Year & X & X & X & X & X & X & X & X & X & X \\\\",
    "FE: Secondary School-Year & X & X & X & X & X & X & X & X & X & X \\\\",
    "FE: Sector/District Office-Year & X & X & X & X & X & X & X & X & X & X \\\\",
    "FE: Industry-Year & X & X & X & X & X & X & X & X & X & X \\\\",
    "FE: Market-Year & X & X & X & X & X & X & X & X & X & X \\\\",
    "FE: Imidugudu-Year & X & X & X & X & X & X & X & X & X & X \\\\",
    "\\hline",
    "\\end{tabular}",
    "}%",
    "\\end{table}"
  ),
  file.path(output_path, "regressions", paste0(output_name, ".tex"))
  )
}


#  Generate all 4 main tables ================================

 #--- 1. Base Specification
reg_list_base <- list(
  reg_est, reg_emp,
  reg_est.3, reg_emp.3,
  reg_est.7, reg_emp.7,
  reg_est.9, reg_emp.9,
  reg_est.19, reg_emp.19
)

make_sector_table(
  reg_list_base,
  "eventstudy_sector_period_base",
  "Event Study: Electrification Impacts by Sector (Base Specification)",
  "eventstudy_sector_period_base"
)

 #--- 2. Road (RD)
reg_list_rd <- list(
  reg_est_rd, reg_emp_rd,
  reg_est_rd.3, reg_emp_rd.3,
  reg_est_rd.7, reg_emp_rd.7,
  reg_est_rd.9, reg_emp_rd.9,
  reg_est_rd.19, reg_emp_rd.19
)

make_sector_table(
  reg_list_rd,
  "eventstudy_sector_period_rd",
  "Event Study: Electrification Impacts by Sector (Road Distance Interaction)",
  "eventstudy_sector_period_rd",
  covariate = "dist_abv"
)

 #--- 3. Population (POP)
reg_list_pop <- list(
  reg_est_pop, reg_emp_pop,
  reg_est_pop.3, reg_emp_pop.3,
  reg_est_pop.7, reg_emp_pop.7,
  reg_est_pop.9, reg_emp_pop.9,
  reg_est_pop.19, reg_emp_pop.19
)

make_sector_table(
  reg_list_pop,
  "eventstudy_sector_period_pop",
  "Event Study: Electrification Impacts by Sector (Population Interaction)",
  "eventstudy_sector_period_pop",
  covariate = "pop_abv"
)

 #--- 4. Consumer (CONS) 
reg_list_cons <- list(
  reg_est_consumer, reg_emp_consumer,
  reg_est_consumer.3, reg_emp_consumer.3,
  reg_est_consumer.7, reg_emp_consumer.7,
  reg_est_consumer.9, reg_emp_consumer.9,
  reg_est_consumer.19, reg_emp_consumer.19
)

make_sector_table(
  reg_list_cons,
  "eventstudy_sector_period_cons",
  "Event Study: Electrification Impacts by Sector (Consumer Interaction)",
  "eventstudy_sector_period_cons",
  covariate = "any_non_residential_consumer"
)



#Plot------


extract_eventstudy <- function(model, outcome_label) {
  tidy(model) %>%
    filter(grepl("^period", term)) %>%
    mutate(
      outcome = outcome_label,
      period_label = term %>%
        gsub("^period", "", .) %>%
        trimws()
    ) %>%
    select(period_label, estimate, std.error, p.value, outcome)
}




#Road========

## --- 2. Extract for both outcomes 
est_df <- extract_eventstudy(reg_est_rd, "Establishments")
emp_df <- extract_eventstudy(reg_emp_rd, "Employees")

## --- 3. Combine and compute CI, stars, labels 

#  Identify main period and interaction terms separately
period_df <- bind_rows(est_df, emp_df) %>%
  filter(!str_detect(period_label, ":")) %>%
  select(period_label, estimate, std.error, p.value, outcome)

interaction_df <- bind_rows(est_df, emp_df) %>%
  filter(str_detect(period_label, ":")) %>%
  select(period_label, estimate, std.error, p.value, outcome)

#Extract pure period label from the interaction term (before the ":")
interaction_df <- interaction_df %>%
  mutate(
    base_label = str_extract(period_label, "^[^:]+")
  )

#  Join with base period effects and sum the coefficients
merged_df <- interaction_df %>%
  left_join(
    period_df %>%
      rename(base_est = estimate,
             base_se = std.error,
             base_p = p.value,
             base_label = period_label),
    by = c("base_label", "outcome")
  ) %>%
  mutate(
    estimate = estimate + base_est,       # add base + interacted
    # keep SE from the interacted term only
    period_label = base_label,
    group = "period*covariate"
  ) %>%
  select(period_label, estimate, std.error, p.value, outcome, group)

#  Combine back with main period terms
base_df <- period_df %>%
  mutate(group = "period")

plot_df <- bind_rows(base_df, merged_df) %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    stars = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.1  ~ "*",
      TRUE ~ ""
    ),
    label = paste0(round(estimate, 2), stars),
    # --- recode period labels based on partial string match ---
    time_since = case_when(
      str_detect(period_label, "electrified 0-2 years") ~ "0 to 2",
      str_detect(period_label, "electrified 3-5 years") ~ "3 to 5",
      str_detect(period_label, "electrified 6-8 years") ~ "6 to 8",
      str_detect(period_label, "4-6 years before electrification") ~ "-6 to -4",
      TRUE ~ NA_character_
    )
  ) %>%
  # --- add zeros for both groups for dodge separation ---
  add_row(time_since = "-3 to -1", outcome = "Establishments",
          estimate = 0, conf.low = NA, conf.high = NA, label = "0",
          group = "period") %>%
  add_row(time_since = "-3 to -1", outcome = "Establishments",
          estimate = 0, conf.low = NA, conf.high = NA, label = "0",
          group = "period*covariate") %>%
  add_row(time_since = "-3 to -1", outcome = "Employees",
          estimate = 0, conf.low = NA, conf.high = NA, label = "0",
          group = "period") %>%
  add_row(time_since = "-3 to -1", outcome = "Employees",
          estimate = 0, conf.low = NA, conf.high = NA, label = "0",
          group = "period*covariate") %>%
  mutate(
    time_since = factor(
      time_since,
      levels = c("-6 to -4", "-3 to -1", "0 to 2", "3 to 5", "6 to 8")
    )
  ) %>% 
  mutate(
    conf.low = ifelse(group == "period", NA, conf.low),
    conf.high = ifelse(group == "period", NA, conf.high)
  )


## --- 4. plot: Establishments
plot_est <- plot_df %>%
  filter(outcome == "Establishments")

# Create dodge object
pd <- position_dodge(width = 0.4)  # increase or decrease width to adjust spacing

p_est_rd <- ggplot(plot_est, aes(x = time_since, y = estimate, color = group, shape = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  
  # CI (both groups handled via dodge)
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.1,
    linewidth = 0.6,
    position = pd
  ) +
  
  # Points (both groups handled via dodge)
  geom_point(
    size = 2.5,
    position = pd
  ) +
  
  scale_color_manual(
    values = c("period" = "black", "period*covariate" = "#1f78b4"),
    labels = c("period" = "Far from road", "period*covariate" = "Close to road")
  ) +
  scale_shape_manual(
    values = c("period" = 16, "period*covariate" = 17),
    labels = c("period" = "Far from road", "period*covariate" = "Close to road")
  ) +
  
  labs(
    x = "Years Since Electrification",
    y = "Number of Establishments\n (Private Sector)",
    color = NULL,
    shape = NULL
  ) +
  
  theme_classic(base_size = 13) +
  theme(
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 12)
  )





## --- 2. Prepare Employees plot Data 
# plot: Employees
plot_emp <- plot_df %>%
  filter(outcome == "Employees")


p_emp_rd <- ggplot(plot_emp, aes(x = time_since, y = estimate, color = group, shape = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  
  # CI (both groups handled via dodge)
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.1,
    linewidth = 0.6,
    position = pd
  ) +
  
  # Points (both groups handled via dodge)
  geom_point(
    size = 2.5,
    position = pd
  ) +
  
  scale_color_manual(
    values = c("period" = "black", "period*covariate" = "#1f78b4"),
    labels = c("period" = "Far from road", "period*covariate" = "Close to road")
  ) +
  scale_shape_manual(
    values = c("period" = 16, "period*covariate" = 17),
    labels = c("period" = "Far from road", "period*covariate" = "Close to road")
  ) +
  
  labs(
    x = "Years Since Electrification",
    y = "Number of Total Employee \n (Private Sector)",
    color = NULL,
    shape = NULL
  ) +
  
  theme_classic(base_size = 13) +
  theme(
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 12)
  )







## --- 6. Display
p_est_rd
p_emp_rd

## --- 7. Save 
ggsave(file.path(output_path, "plot", "eventstudy_establishments_road.png"),
       p_est_rd, width = 8, height = 6, dpi = 400, bg = "white", scale = 0.6)

ggsave(file.path(output_path, "plot", "eventstudy_employees_road.png"),
       p_emp_rd, width = 8, height = 6, dpi = 400, bg = "white", scale = 0.6)








#population========

## --- 2. Extract for both outcomes 
est_df <- extract_eventstudy(reg_est_pop, "Establishments")
emp_df <- extract_eventstudy(reg_emp_pop, "Employees")

## --- 3. Combine and compute CI, stars, labels 

#  Identify main period and interaction terms separately
period_df <- bind_rows(est_df, emp_df) %>%
  filter(!str_detect(period_label, ":")) %>%
  select(period_label, estimate, std.error, p.value, outcome)

interaction_df <- bind_rows(est_df, emp_df) %>%
  filter(str_detect(period_label, ":")) %>%
  select(period_label, estimate, std.error, p.value, outcome)

#Extract pure period label from the interaction term (before the ":")
interaction_df <- interaction_df %>%
  mutate(
    base_label = str_extract(period_label, "^[^:]+")
  )

#  Join with base period effects and sum the coefficients
merged_df <- interaction_df %>%
  left_join(
    period_df %>%
      rename(base_est = estimate,
             base_se = std.error,
             base_p = p.value,
             base_label = period_label),
    by = c("base_label", "outcome")
  ) %>%
  mutate(
    estimate = estimate + base_est,       # add base + interacted
    # keep SE from the interacted term only
    period_label = base_label,
    group = "period*covariate"
  ) %>%
  select(period_label, estimate, std.error, p.value, outcome, group)

#  Combine back with main period terms
base_df <- period_df %>%
  mutate(group = "period")

plot_df <- bind_rows(base_df, merged_df) %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    stars = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.1  ~ "*",
      TRUE ~ ""
    ),
    label = paste0(round(estimate, 2), stars),
    # --- recode period labels based on partial string match ---
    time_since = case_when(
      str_detect(period_label, "electrified 0-2 years") ~ "0 to 2",
      str_detect(period_label, "electrified 3-5 years") ~ "3 to 5",
      str_detect(period_label, "electrified 6-8 years") ~ "6 to 8",
      str_detect(period_label, "4-6 years before electrification") ~ "-6 to -4",
      TRUE ~ NA_character_
    )
  ) %>%
  # --- add zeros for both groups for dodge separation ---
  add_row(time_since = "-3 to -1", outcome = "Establishments",
          estimate = 0, conf.low = NA, conf.high = NA, label = "0",
          group = "period") %>%
  add_row(time_since = "-3 to -1", outcome = "Establishments",
          estimate = 0, conf.low = NA, conf.high = NA, label = "0",
          group = "period*covariate") %>%
  add_row(time_since = "-3 to -1", outcome = "Employees",
          estimate = 0, conf.low = NA, conf.high = NA, label = "0",
          group = "period") %>%
  add_row(time_since = "-3 to -1", outcome = "Employees",
          estimate = 0, conf.low = NA, conf.high = NA, label = "0",
          group = "period*covariate") %>%
  mutate(
    time_since = factor(
      time_since,
      levels = c("-6 to -4", "-3 to -1", "0 to 2", "3 to 5", "6 to 8")
    )
  ) %>% 
  mutate(
    conf.low = ifelse(group == "period", NA, conf.low),
    conf.high = ifelse(group == "period", NA, conf.high)
  )


## --- 4. plot: Establishments
plot_est <- plot_df %>%
  filter(outcome == "Establishments")

# Create dodge object
pd <- position_dodge(width = 0.4)  # increase or decrease width to adjust spacing

p_est_pop <- ggplot(plot_est, aes(x = time_since, y = estimate, color = group, shape = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  
  # CI (both groups handled via dodge)
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.1,
    linewidth = 0.6,
    position = pd
  ) +
  
  # Points (both groups handled via dodge)
  geom_point(
    size = 2.5,
    position = pd
  ) +
  
  scale_color_manual(
    values = c("period" = "black", "period*covariate" = "#1f78b4"),
    labels = c("period" = "Less population", "period*covariate" = "More population")
  ) +
  scale_shape_manual(
    values = c("period" = 16, "period*covariate" = 17),
    labels = c("period" = "Less population", "period*covariate" = "More population")
  ) +
  
  labs(
    x = "Years Since Electrification",
    y = "Number of Establishments\n (Private Sector)",
    color = NULL,
    shape = NULL
  ) +
  
  theme_classic(base_size = 13) +
  theme(
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 12)
  )





## --- 2. Prepare Employees plot Data 
# plot: Employees
plot_emp <- plot_df %>%
  filter(outcome == "Employees")


p_emp_pop <- ggplot(plot_emp, aes(x = time_since, y = estimate, color = group, shape = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  
  # CI (both groups handled via dodge)
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.1,
    linewidth = 0.6,
    position = pd
  ) +
  
  # Points (both groups handled via dodge)
  geom_point(
    size = 2.5,
    position = pd
  ) +
  
  scale_color_manual(
    values = c("period" = "black", "period*covariate" = "#1f78b4"),
    labels = c("period" = "Less population", "period*covariate" = "More population")
  ) +
  scale_shape_manual(
    values = c("period" = 16, "period*covariate" = 17),
    labels = c("period" = "Less population", "period*covariate" = "More population")
  ) +
  
  labs(
    x = "Years Since Electrification",
    y = "Number of Total Employee \n (Private Sector)",
    color = NULL,
    shape = NULL
  ) +
  
  theme_classic(base_size = 13) +
  theme(
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 12)
  )







## --- 6. Display
p_est_pop
p_emp_pop

## --- 7. Save 
ggsave(file.path(output_path, "plot", "eventstudy_establishments_population.png"),
       p_est_pop, width = 8, height = 6, dpi = 400, bg = "white", scale = 0.6)

ggsave(file.path(output_path, "plot", "eventstudy_employees_population.png"),
       p_emp_pop, width = 8, height = 6, dpi = 400, bg = "white", scale = 0.6)






#Consumer========

## --- 2. Extract for both outcomes 
est_df <- extract_eventstudy(reg_est_consumer, "Establishments")
emp_df <- extract_eventstudy(reg_emp_consumer, "Employees")

## --- 3. Combine and compute CI, stars, labels 

#  Identify main period and interaction terms separately
period_df <- bind_rows(est_df, emp_df) %>%
  filter(!str_detect(period_label, ":")) %>%
  select(period_label, estimate, std.error, p.value, outcome)

interaction_df <- bind_rows(est_df, emp_df) %>%
  filter(str_detect(period_label, ":")) %>%
  select(period_label, estimate, std.error, p.value, outcome)

#Extract pure period label from the interaction term (before the ":")
interaction_df <- interaction_df %>%
  mutate(
    base_label = str_extract(period_label, "^[^:]+")
  )

#  Join with base period effects and sum the coefficients
merged_df <- interaction_df %>%
  left_join(
    period_df %>%
      rename(base_est = estimate,
             base_se = std.error,
             base_p = p.value,
             base_label = period_label),
    by = c("base_label", "outcome")
  ) %>%
  mutate(
    estimate = estimate + base_est,       # add base + interacted
    # keep SE from the interacted term only
    period_label = base_label,
    group = "period*covariate"
  ) %>%
  select(period_label, estimate, std.error, p.value, outcome, group)

#  Combine back with main period terms
base_df <- period_df %>%
  mutate(group = "period")

plot_df <- bind_rows(base_df, merged_df) %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    stars = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.1  ~ "*",
      TRUE ~ ""
    ),
    label = paste0(round(estimate, 2), stars),
    # --- recode period labels based on partial string match ---
    time_since = case_when(
      str_detect(period_label, "electrified 0-2 years") ~ "0 to 2",
      str_detect(period_label, "electrified 3-5 years") ~ "3 to 5",
      str_detect(period_label, "electrified 6-8 years") ~ "6 to 8",
      str_detect(period_label, "4-6 years before electrification") ~ "-6 to -4",
      TRUE ~ NA_character_
    )
  ) %>%
  # --- add zeros for both groups for dodge separation ---
  add_row(time_since = "-3 to -1", outcome = "Establishments",
          estimate = 0, conf.low = NA, conf.high = NA, label = "0",
          group = "period") %>%
  add_row(time_since = "-3 to -1", outcome = "Establishments",
          estimate = 0, conf.low = NA, conf.high = NA, label = "0",
          group = "period*covariate") %>%
  add_row(time_since = "-3 to -1", outcome = "Employees",
          estimate = 0, conf.low = NA, conf.high = NA, label = "0",
          group = "period") %>%
  add_row(time_since = "-3 to -1", outcome = "Employees",
          estimate = 0, conf.low = NA, conf.high = NA, label = "0",
          group = "period*covariate") %>%
  mutate(
    time_since = factor(
      time_since,
      levels = c("-6 to -4", "-3 to -1", "0 to 2", "3 to 5", "6 to 8")
    )
  ) %>% 
  mutate(
    conf.low = ifelse(group == "period", NA, conf.low),
    conf.high = ifelse(group == "period", NA, conf.high)
  )



## --- 4. plot: Establishments
plot_est <- plot_df %>%
  filter(outcome == "Establishments")

# Create dodge object
pd <- position_dodge(width = 0.4)  # increase or decrease width to adjust spacing

p_est_con <- ggplot(plot_est, aes(x = time_since, y = estimate, color = group, shape = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  
  # CI (both groups handled via dodge)
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.1,
    linewidth = 0.6,
    position = pd
  ) +
  
  # Points (both groups handled via dodge)
  geom_point(
    size = 2.5,
    position = pd
  ) +
  
  scale_color_manual(
    values = c("period" = "black", "period*covariate" = "#1f78b4"),
    labels = c("period" = "No non-residential \n consumer", "period*covariate" = "Any non-residential \n consumer")
  ) +
  scale_shape_manual(
    values = c("period" = 16, "period*covariate" = 17),
    labels = c("period" = "No non-residential \n consumer", "period*covariate" = "Any non-residential \n consumer")
  ) +
  
  labs(
    x = "Years Since Electrification",
    y = "Number of Establishments\n (Private Sector)",
    color = NULL,
    shape = NULL
  ) +
  
  theme_classic(base_size = 13) +
  theme(
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 12)
  )





## --- 2. Prepare Employees plot Data 
# plot: Employees
plot_emp <- plot_df %>%
  filter(outcome == "Employees")


p_emp_con <- ggplot(plot_emp, aes(x = time_since, y = estimate, color = group, shape = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  
  # CI (both groups handled via dodge)
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.1,
    linewidth = 0.6,
    position = pd
  ) +
  
  # Points (both groups handled via dodge)
  geom_point(
    size = 2.5,
    position = pd
  ) +
  
  scale_color_manual(
    values = c("period" = "black", "period*covariate" = "#1f78b4"),
    labels = c("period" = "No non-residential \n consumer", "period*covariate" = "Any non-residential \n consumer")
  ) +
  scale_shape_manual(
    values = c("period" = 16, "period*covariate" = 17),
    labels = c("period" = "No non-residential \n consumer", "period*covariate" = "Any non-residential \n consumer")
  ) +
  
  labs(
    x = "Years Since Electrification",
    y = "Number of Total Employee \n (Private Sector)",
    color = NULL,
    shape = NULL
  ) +
  
  theme_classic(base_size = 13) +
  theme(
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 12)
  )







## --- 6. Display
p_est_con
p_emp_con

## --- 7. Save 
ggsave(file.path(output_path, "plot", "eventstudy_establishments_consumer.png"),
       p_est_con, width = 8, height = 6, dpi = 400, bg = "white", scale = 0.6)

ggsave(file.path(output_path, "plot", "eventstudy_employees_consumer.png"),
       p_emp_con, width = 8, height = 6, dpi = 400, bg = "white", scale = 0.6)








































