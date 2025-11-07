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


#Above below median for distance to rd-------

elec12_17.p_rd <- elec12_17_p %>%
  mutate(
    dist_abv = ifelse(dist_na_rd > median(dist_na_rd, na.rm = TRUE), 1, 0)
  ) %>% 
  mutate(
    period = factor(period, levels = c(
      "1-3 years before electrification",
      "4-6 years before electrification",
      "electrified 0-2 years",
      "electrified 3-5 years",
      "electrified 6-8 years"
    )),
    year = as.factor(year),
    cell_id = as.factor(cell_id),
    cell_office = as.factor(cell_office),
    health_center = as.factor(health_center),
    primary_school = as.factor(primary_school),
    secondary_school = as.factor(secondary_school),
    industry = as.factor(industry),
    market = as.factor(market),
    imidugudu = as.factor(imidugudu)
  )

elec12_17.p_rd1 <- elec12_17.p_rd %>% 
  filter(dist_abv == 1)

est_rd.1 <- felm(
  num_establishment ~ period |  
    village_id +
    cell_office * year+
    health_center * year  +
    primary_school * year  +
    secondary_school * year +
    industry * year  +
    market * year  +
    imidugudu * year + population * year |
    0 | sector_id,
  data = elec12_17.p_rd1
)


summary(est_rd.1)


emp_rd.1 <- felm(
  total_employee ~ period |  
    village_id +
    cell_office * year+
    health_center * year  +
    primary_school * year  +
    secondary_school * year +
    industry * year  +
    market * year  +
    imidugudu * year + population * year |
    0 | sector_id,
  data = elec12_17.p_rd1
)


summary(emp_rd.1)



elec12_17.p_rd0 <- elec12_17.p_rd %>% 
  filter(dist_abv == 0)

est_rd.0 <- felm(
  num_establishment ~ period |  
    village_id +
    cell_office * year+
    health_center * year  +
    primary_school * year  +
    secondary_school * year +
    industry * year  +
    market * year  +
    imidugudu * year + population * year |
    0 | sector_id,
  data = elec12_17.p_rd0
)


summary(est_rd.0)


emp_rd.0 <- felm(
  total_employee ~ period |  
    village_id +
    cell_office * year+
    health_center * year  +
    primary_school * year  +
    secondary_school * year +
    industry * year  +
    market * year  +
    imidugudu * year + population * year |
    0 | sector_id,
  data = elec12_17.p_rd0
)


summary(emp_rd.0)






stargazer(
  est_rd.0, est_rd.1, emp_rd.0, emp_rd.1,
  type = "latex",
  out = file.path(output_path, "regressions", "eventstudy_dist_above_below.tex"),
  title = "Impact of Electrification by Distance to Road Median",
  label = "tab:eventstudy_dist_above_below",
  column.labels = c("Below Median", "Above Median", "Below Median", "Above Median"),
  dep.var.labels = c("Number of Establishments", "Total Employees"),
  keep.stat = c("n", "rsq"),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  digits = 3,
  align = TRUE,
  no.space = TRUE,
  float.env = "table"
)











#Above below median for baseline=========

#Below median --------
median_s11.1 <- elec12_17_p %>%
  filter(year == 2011) %>% 
  mutate(
    est_median.1 = median(num_establishment, na.rm = TRUE),
    emp_median.1 = median(total_employee, na.rm = TRUE)
  ) %>%
  mutate(
    est_s11.1 = ifelse(num_establishment < est_median.1, 1, 0),
    emp_s11.1 = ifelse(total_employee < emp_median.1, 1, 0)
  ) %>% 
  select(village_id, est_s11.1, emp_s11.1)

median_s14.1 <- elec12_17_p %>%
  filter(year == 2014) %>% 
  mutate(
    est_median.1 = median(num_establishment, na.rm = TRUE),
    emp_median.1 = median(total_employee, na.rm = TRUE)
  ) %>%
  mutate(
    est_s14.1 = ifelse(num_establishment < est_median.1, 1, 0),
    emp_s14.1 = ifelse(total_employee < emp_median.1, 1, 0)
  )  %>% 
  select(village_id, est_s14.1, emp_s14.1)

elec12_17_p_median.1 <- left_join(elec12_17_p, median_s11.1)
elec12_17_p_median.1 <- left_join(elec12_17_p_median.1, median_s14.1)

## Num-establishment-------
c_2011_est.1 <- elec12_17_p_median.1 %>% 
  filter(est_s11.1 == 1) %>% 
  filter(electrified_year == 9999 | elec12_14 == 1) %>% 
  mutate(c = 2011)

c_2014_est.1 <- elec12_17_p_median.1 %>% 
  filter(est_s14.1 == 1) %>% 
  filter(electrified_year == 9999 | elec15_17 == 1) %>% 
  mutate(c = 2014)

stacked_est.1 <- rbind(c_2011_est.1, c_2014_est.1) %>% 
  select(est_s11.1, est_s14.1, c, everything()) %>% 
  mutate(
    period = factor(period, levels = c(
      "1-3 years before electrification",
      "4-6 years before electrification",
      "electrified 0-2 years",
      "electrified 3-5 years",
      "electrified 6-8 years"
    )),
    year = as.factor(year),
    c = as.factor(c),
    cell_office = as.factor(cell_office),
    health_center = as.factor(health_center),
    primary_school = as.factor(primary_school),
    secondary_school = as.factor(secondary_school),
    industry = as.factor(industry),
    market = as.factor(market),
    imidugudu = as.factor(imidugudu)
  )

est_stack_est.1 <- felm(
  num_establishment ~ period |  
    village_id + 
    cell_office * year * c +
    health_center * year * c +
    primary_school * year * c +
    secondary_school * year * c +
    industry * year * c +
    market * year * c +
    imidugudu * year * c |
    0 | sector_id,
  data = stacked_est.1
)

est_stack_emp.1 <- felm(
  total_employee ~ period |  
    village_id + 
    cell_office * year * c +
    health_center * year * c +
    primary_school * year * c +
    secondary_school * year * c +
    industry * year * c +
    market * year * c +
    imidugudu * year * c |
    0 | sector_id,
  data = stacked_est.1
)

## Total-employee-------
c_2011_emp.1 <- elec12_17_p_median.1 %>% 
  filter(emp_s11.1 == 1) %>% 
  filter(electrified_year == 9999 | elec12_14 == 1) %>% 
  mutate(c = 2011)

c_2014_emp.1 <- elec12_17_p_median.1 %>% 
  filter(emp_s14.1 == 1) %>% 
  filter(electrified_year == 9999 | elec15_17 == 1) %>% 
  mutate(c = 2014)

stacked_emp.1 <- rbind(c_2011_emp.1, c_2014_emp.1) %>% 
  select(emp_s11.1, emp_s14.1, c, everything()) %>% 
  mutate(
    period = factor(period, levels = c(
      "1-3 years before electrification",
      "4-6 years before electrification",
      "electrified 0-2 years",
      "electrified 3-5 years",
      "electrified 6-8 years"
    )),
    year = as.factor(year),
    c = as.factor(c),
    cell_office = as.factor(cell_office),
    health_center = as.factor(health_center),
    primary_school = as.factor(primary_school),
    secondary_school = as.factor(secondary_school),
    industry = as.factor(industry),
    market = as.factor(market),
    imidugudu = as.factor(imidugudu)
  )

emp_stack_emp.1 <- felm(
  total_employee ~ period |  
    village_id + 
    cell_office * year * c +
    health_center * year * c +
    primary_school * year * c +
    secondary_school * year * c +
    industry * year * c +
    market * year * c +
    imidugudu * year * c | 0 | sector_id,
  data = stacked_emp.1
)

emp_stack_est.1 <- felm(
  num_establishment ~ period |  
    village_id + 
    cell_office * year * c +
    health_center * year * c +
    primary_school * year * c +
    secondary_school * year * c +
    industry * year * c +
    market * year * c +
    imidugudu * year * c | 0 | sector_id,
  data = stacked_emp.1
)

## Cohort: Num_establishment------
cohort_est.1 <- elec12_17_p_median.1 %>% 
  filter(electrified_year == 9999 | elec12_14 == 1 | elec15_17 == 1) %>% 
  filter(est_s11.1 == 1 | est_s14.1 == 1) %>% 
  mutate(
    year = as.factor(year),
    est_s11.1 = as.factor(est_s11.1),
    est_s14.1 = as.factor(est_s14.1), 
    cell_office = as.factor(cell_office),
    health_center = as.factor(health_center),
    primary_school = as.factor(primary_school),
    secondary_school = as.factor(secondary_school),
    industry = as.factor(industry),
    market = as.factor(market),
    imidugudu = as.factor(imidugudu),
    period = factor(period, levels = c(
      "1-3 years before electrification",
      "4-6 years before electrification",
      "electrified 0-2 years",
      "electrified 3-5 years",
      "electrified 6-8 years"
    ))
  )

est_cohort_est.1 <- felm(
  num_establishment ~ period |  
    village_id + 
    cell_office * year * est_s11.1 + cell_office * year * est_s14.1 +
    health_center * year * est_s11.1 + health_center * year * est_s14.1 +
    primary_school * year * est_s11.1 + primary_school * year * est_s14.1 +
    secondary_school * year * est_s11.1 + secondary_school * year * est_s14.1 +
    industry * year * est_s11.1 + industry * year * est_s14.1 +
    market * year * est_s11.1 + market * year * est_s14.1 +
    imidugudu * year * est_s11.1 + imidugudu * year * est_s14.1 | 0 | sector_id,
  data = cohort_est.1
)

est_cohort_emp.1 <- felm(
  total_employee ~ period |  
    village_id + 
    cell_office * year * est_s11.1 + cell_office * year * est_s14.1 +
    health_center * year * est_s11.1 + health_center * year * est_s14.1 +
    primary_school * year * est_s11.1 + primary_school * year * est_s14.1 +
    secondary_school * year * est_s11.1 + secondary_school * year * est_s14.1 +
    industry * year * est_s11.1 + industry * year * est_s14.1 +
    market * year * est_s11.1 + market * year * est_s14.1 +
    imidugudu * year * est_s11.1 + imidugudu * year * est_s14.1 | 0 | sector_id,
  data = cohort_est.1
)

## Cohort: Total_employee-------
cohort_emp.1 <- elec12_17_p_median.1 %>% 
  filter(electrified_year == 9999 | elec12_14 == 1 | elec15_17 == 1) %>% 
  filter(emp_s11.1 == 1 | emp_s14.1 == 1) %>% 
  mutate(
    year = as.factor(year),
    emp_s11.1 = as.factor(emp_s11.1),
    emp_s14.1 = as.factor(emp_s14.1), 
    cell_office = as.factor(cell_office),
    health_center = as.factor(health_center),
    primary_school = as.factor(primary_school),
    secondary_school = as.factor(secondary_school),
    industry = as.factor(industry),
    market = as.factor(market),
    imidugudu = as.factor(imidugudu),
    period = factor(period, levels = c(
      "1-3 years before electrification",
      "4-6 years before electrification",
      "electrified 0-2 years",
      "electrified 3-5 years",
      "electrified 6-8 years"
    ))
  )

emp_cohort_emp.1 <- felm(
  total_employee ~ period |  
    village_id + 
    cell_office * year * emp_s11.1 + cell_office * year * emp_s14.1 +
    health_center * year * emp_s11.1 + health_center * year * emp_s14.1 +
    primary_school * year * emp_s11.1 + primary_school * year * emp_s14.1 +
    secondary_school * year * emp_s11.1 + secondary_school * year * emp_s14.1 +
    industry * year * emp_s11.1 + industry * year * emp_s14.1 +
    market * year * emp_s11.1 + market * year * emp_s14.1 +
    imidugudu * year * emp_s11.1 + imidugudu * year * emp_s14.1 | 0 | sector_id,
  data = cohort_emp.1
)

emp_cohort_est.1 <- felm(
  num_establishment ~ period |  
    village_id + 
    cell_office * year * emp_s11.1 + cell_office * year * emp_s14.1 +
    health_center * year * emp_s11.1 + health_center * year * emp_s14.1 +
    primary_school * year * emp_s11.1 + primary_school * year * emp_s14.1 +
    secondary_school * year * emp_s11.1 + secondary_school * year * emp_s14.1 +
    industry * year * emp_s11.1 + industry * year * emp_s14.1 +
    market * year * emp_s11.1 + market * year * emp_s14.1 +
    imidugudu * year * emp_s11.1 + imidugudu * year * emp_s14.1 | 0 | sector_id,
  data = cohort_emp.1
)






#Above median --------
median_s11.2 <- elec12_17_p %>%
  filter(year == 2011) %>% 
  mutate(
    est_median.2 = median(num_establishment, na.rm = TRUE),
    emp_median.2 = median(total_employee, na.rm = TRUE)
  ) %>%
  mutate(
    est_s11.2 = ifelse(num_establishment > est_median.2, 1, 0),
    emp_s11.2 = ifelse(total_employee >  emp_median.2, 1, 0)
  ) %>% 
  select(village_id, est_s11.2, emp_s11.2)

median_s14.2 <- elec12_17_p %>%
  filter(year == 2014) %>% 
  mutate(
    est_median.2 = median(num_establishment, na.rm = TRUE),
    emp_median.2 = median(total_employee, na.rm = TRUE)
  ) %>%
  mutate(
    est_s14.2 = ifelse(num_establishment > est_median.2, 1, 0),
    emp_s14.2 = ifelse(total_employee > emp_median.2, 1, 0)
  )  %>% 
  select(village_id, est_s14.2, emp_s14.2)

elec12_17_p_median.2 <- left_join(elec12_17_p, median_s11.2)
elec12_17_p_median.2 <- left_join(elec12_17_p_median.2, median_s14.2)

## Num-establishment-------
c_2011_est.2 <- elec12_17_p_median.2 %>% 
  filter(est_s11.2 == 1) %>% 
  filter(electrified_year == 9999 | elec12_14 == 1) %>% 
  mutate(c = 2011)

c_2014_est.2 <- elec12_17_p_median.2 %>% 
  filter(est_s14.2 == 1) %>% 
  filter(electrified_year == 9999 | elec15_17 == 1) %>% 
  mutate(c = 2014)

stacked_est.2 <- rbind(c_2011_est.2, c_2014_est.2) %>% 
  select(est_s11.2, est_s14.2, c, everything()) %>% 
  mutate(
    period = factor(period, levels = c(
      "1-3 years before electrification",
      "4-6 years before electrification",
      "electrified 0-2 years",
      "electrified 3-5 years",
      "electrified 6-8 years"
    )),
    year = as.factor(year),
    c = as.factor(c),
    cell_office = as.factor(cell_office),
    health_center = as.factor(health_center),
    primary_school = as.factor(primary_school),
    secondary_school = as.factor(secondary_school),
    industry = as.factor(industry),
    market = as.factor(market),
    imidugudu = as.factor(imidugudu)
  )

est_stack_est.2 <- felm(
  num_establishment ~ period |  
    village_id + 
    cell_office * year * c +
    health_center * year * c +
    primary_school * year * c +
    secondary_school * year * c +
    industry * year * c +
    market * year * c +
    imidugudu * year * c |
    0 | sector_id,
  data = stacked_est.2
)

est_stack_emp.2 <- felm(
  total_employee ~ period |  
    village_id + 
    cell_office * year * c +
    health_center * year * c +
    primary_school * year * c +
    secondary_school * year * c +
    industry * year * c +
    market * year * c +
    imidugudu * year * c |
    0 | sector_id,
  data = stacked_est.2
)

## Total-employee-------
c_2011_emp.2 <- elec12_17_p_median.2 %>% 
  filter(emp_s11.2 == 1) %>% 
  filter(electrified_year == 9999 | elec12_14 == 1) %>% 
  mutate(c = 2011)

c_2014_emp.2 <- elec12_17_p_median.2 %>% 
  filter(emp_s14.2 == 1) %>% 
  filter(electrified_year == 9999 | elec15_17 == 1) %>% 
  mutate(c = 2014)

stacked_emp.2 <- rbind(c_2011_emp.2, c_2014_emp.2) %>% 
  select(emp_s11.2, emp_s14.2, c, everything()) %>% 
  mutate(
    period = factor(period, levels = c(
      "1-3 years before electrification",
      "4-6 years before electrification",
      "electrified 0-2 years",
      "electrified 3-5 years",
      "electrified 6-8 years"
    )),
    year = as.factor(year),
    c = as.factor(c),
    cell_office = as.factor(cell_office),
    health_center = as.factor(health_center),
    primary_school = as.factor(primary_school),
    secondary_school = as.factor(secondary_school),
    industry = as.factor(industry),
    market = as.factor(market),
    imidugudu = as.factor(imidugudu)
  )

emp_stack_emp.2 <- felm(
  total_employee ~ period |  
    village_id + 
    cell_office * year * c +
    health_center * year * c +
    primary_school * year * c +
    secondary_school * year * c +
    industry * year * c +
    market * year * c +
    imidugudu * year * c | 0 | sector_id,
  data = stacked_emp.2
)

emp_stack_est.2 <- felm(
  num_establishment ~ period |  
    village_id + 
    cell_office * year * c +
    health_center * year * c +
    primary_school * year * c +
    secondary_school * year * c +
    industry * year * c +
    market * year * c +
    imidugudu * year * c | 0 | sector_id,
  data = stacked_emp.2
)

## Cohort: Num_establishment------
cohort_est.2 <- elec12_17_p_median.2 %>% 
  filter(electrified_year == 9999 | elec12_14 == 1 | elec15_17 == 1) %>% 
  filter(est_s11.2 == 1 | est_s14.2 == 1) %>% 
  mutate(
    year = as.factor(year),
    est_s11.2 = as.factor(est_s11.2),
    est_s14.2 = as.factor(est_s14.2), 
    cell_office = as.factor(cell_office),
    health_center = as.factor(health_center),
    primary_school = as.factor(primary_school),
    secondary_school = as.factor(secondary_school),
    industry = as.factor(industry),
    market = as.factor(market),
    imidugudu = as.factor(imidugudu),
    period = factor(period, levels = c(
      "1-3 years before electrification",
      "4-6 years before electrification",
      "electrified 0-2 years",
      "electrified 3-5 years",
      "electrified 6-8 years"
    ))
  )

est_cohort_est.2 <- felm(
  num_establishment ~ period |  
    village_id + 
    cell_office * year * est_s11.2 + cell_office * year * est_s14.2 +
    health_center * year * est_s11.2 + health_center * year * est_s14.2 +
    primary_school * year * est_s11.2 + primary_school * year * est_s14.2 +
    secondary_school * year * est_s11.2 + secondary_school * year * est_s14.2 +
    industry * year * est_s11.2 + industry * year * est_s14.2 +
    market * year * est_s11.2 + market * year * est_s14.2 +
    imidugudu * year * est_s11.2 + imidugudu * year * est_s14.2 | 0 | sector_id,
  data = cohort_est.2
)

est_cohort_emp.2 <- felm(
  total_employee ~ period |  
    village_id + 
    cell_office * year * est_s11.2 + cell_office * year * est_s14.2 +
    health_center * year * est_s11.2 + health_center * year * est_s14.2 +
    primary_school * year * est_s11.2 + primary_school * year * est_s14.2 +
    secondary_school * year * est_s11.2 + secondary_school * year * est_s14.2 +
    industry * year * est_s11.2 + industry * year * est_s14.2 +
    market * year * est_s11.2 + market * year * est_s14.2 +
    imidugudu * year * est_s11.2 + imidugudu * year * est_s14.2 | 0 | sector_id,
  data = cohort_est.2
)

## Cohort: Total_employee-------
cohort_emp.2 <- elec12_17_p_median.2 %>% 
  filter(electrified_year == 9999 | elec12_14 == 1 | elec15_17 == 1) %>% 
  filter(emp_s11.2 == 1 | emp_s14.2 == 1) %>% 
  mutate(
    year = as.factor(year),
    emp_s11.2 = as.factor(emp_s11.2),
    emp_s14.2 = as.factor(emp_s14.2), 
    cell_office = as.factor(cell_office),
    health_center = as.factor(health_center),
    primary_school = as.factor(primary_school),
    secondary_school = as.factor(secondary_school),
    industry = as.factor(industry),
    market = as.factor(market),
    imidugudu = as.factor(imidugudu),
    period = factor(period, levels = c(
      "1-3 years before electrification",
      "4-6 years before electrification",
      "electrified 0-2 years",
      "electrified 3-5 years",
      "electrified 6-8 years"
    ))
  )

emp_cohort_emp.2 <- felm(
  total_employee ~ period |  
    village_id + 
    cell_office * year * emp_s11.2 + cell_office * year * emp_s14.2 +
    health_center * year * emp_s11.2 + health_center * year * emp_s14.2 +
    primary_school * year * emp_s11.2 + primary_school * year * emp_s14.2 +
    secondary_school * year * emp_s11.2 + secondary_school * year * emp_s14.2 +
    industry * year * emp_s11.2 + industry * year * emp_s14.2 +
    market * year * emp_s11.2 + market * year * emp_s14.2 +
    imidugudu * year * emp_s11.2 + imidugudu * year * emp_s14.2 | 0 | sector_id,
  data = cohort_emp.2
)

emp_cohort_est.2 <- felm(
  num_establishment ~ period |  
    village_id + 
    cell_office * year * emp_s11.2 + cell_office * year * emp_s14.2 +
    health_center * year * emp_s11.2 + health_center * year * emp_s14.2 +
    primary_school * year * emp_s11.2 + primary_school * year * emp_s14.2 +
    secondary_school * year * emp_s11.2 + secondary_school * year * emp_s14.2 +
    industry * year * emp_s11.2 + industry * year * emp_s14.2 +
    market * year * emp_s11.2 + market * year * emp_s14.2 +
    imidugudu * year * emp_s11.2 + imidugudu * year * emp_s14.2 | 0 | sector_id,
  data = cohort_emp.2
)





# Stargazer output-------

# ---- Below Median: Number of Establishments ----
mean_est_below_est <- round(mean(stacked_est.1$num_establishment[stacked_est.1$electrified_year == 9999], na.rm = TRUE), 2)
mean_emp_below_est <- round(mean(stacked_est.1$total_employee[stacked_est.1$electrified_year == 9999], na.rm = TRUE), 2)

stargazer(
  est_cohort_est.1, est_stack_est.1, est_cohort_emp.1, est_stack_emp.1,
  type = "latex",
  out = file.path(output_path, "regressions", "eventstudy_est_below_median.tex"),
  title = "Event Study Number of Establishments Below Median in Baseline",
  label = "tab:eventstudy_est_below",
  column.labels = c("Cohort", "Stacked", "Cohort", "Stacked"),
  dep.var.labels = c("Number of Establishments", "Total Employees"),
  keep.stat = c("n", "rsq"),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  notes = c(
    paste0("Mean (Never Electrified): establishments = ", mean_est_below_est,
           ", employees = ", mean_emp_below_est)
  ),
  float.env = "table"
)


# ---- Above Median: Number of Establishments ----
mean_est_above_est <- round(mean(stacked_est.2$num_establishment[stacked_est.2$electrified_year == 9999], na.rm = TRUE), 2)
mean_emp_above_est <- round(mean(stacked_est.2$total_employee[stacked_est.2$electrified_year == 9999], na.rm = TRUE), 2)

stargazer(
  est_cohort_est.2, est_stack_est.2, est_cohort_emp.2, est_stack_emp.2,
  type = "latex",
  out = file.path(output_path, "regressions", "eventstudy_est_above_median.tex"),
  title = "Event Study Number of Establishments Above Median in Baseline",
  label = "tab:eventstudy_est_above",
  column.labels = c("Cohort", "Stacked", "Cohort", "Stacked"),
  dep.var.labels = c("Number of Establishments", "Total Employees"),
  keep.stat = c("n", "rsq"),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  notes = c(
    paste0("Mean (Never Electrified): establishments = ", mean_est_above_est,
           ", employees = ", mean_emp_above_est)
  ),
  float.env = "table"
)


# ---- Below Median: Total Employees ----
mean_est_below_emp <- round(mean(stacked_emp.1$num_establishment[stacked_emp.1$electrified_year == 9999], na.rm = TRUE), 2)
mean_emp_below_emp <- round(mean(stacked_emp.1$total_employee[stacked_emp.1$electrified_year == 9999], na.rm = TRUE), 2)

stargazer(
  emp_cohort_est.1, emp_stack_est.1, emp_cohort_emp.1, emp_stack_emp.1,
  type = "latex",
  out = file.path(output_path, "regressions", "eventstudy_emp_below_median.tex"),
  title = "Event Study Number of Employees Below Median in Baseline",
  label = "tab:eventstudy_emp_below",
  column.labels = c("Cohort", "Stacked", "Cohort", "Stacked"),
  dep.var.labels = c("Number of Establishments", "Total Employees"),
  keep.stat = c("n", "rsq"),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  notes = c(
    paste0("Mean (Never Electrified): establishments = ", mean_est_below_emp,
           ", employees = ", mean_emp_below_emp)
  ),
  float.env = "table"
)


# ---- Above Median: Total Employees ----
mean_est_above_emp <- round(mean(stacked_emp.2$num_establishment[stacked_emp.2$electrified_year == 9999], na.rm = TRUE), 2)
mean_emp_above_emp <- round(mean(stacked_emp.2$total_employee[stacked_emp.2$electrified_year == 9999], na.rm = TRUE), 2)

stargazer(
  emp_cohort_est.2, emp_stack_est.2, emp_cohort_emp.2, emp_stack_emp.2,
  type = "latex",
  out = file.path(output_path, "regressions", "eventstudy_emp_above_median.tex"),
  title = "Event Study Number of Employees Above Median in Baseline",
  label = "tab:eventstudy_emp_above",
  column.labels = c("Cohort", "Stacked", "Cohort", "Stacked"),
  dep.var.labels = c("Number of Establishments", "Total Employees"),
  keep.stat = c("n", "rsq"),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  notes = c(
    paste0("Mean (Never Electrified): establishments = ", mean_est_above_emp,
           ", employees = ", mean_emp_above_emp)
  ),
  float.env = "table"
)