
##########################
#Author: Xiaoming Zhang
#Date: 02132024
#purpose:establishment census analysis
############################


#library----

pacman::p_load(tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, haven, stringr)


#read file----

if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX, "Rwanda Energy/datawork/Historical data"
)

nisr_2011 <- read_sav(file.path(data_path, "2011", "rec-2011-data-v2.sav"))
nisr_2014 <- read_sav(file.path(data_path, "2014", "rec-2014-data-v2.sav"))
nisr_2017 <- read_dta(file.path(data_path, "2017", "data-rwa-nisr-ec-2017_stata.dta"))
nisr_2020 <- read_dta(file.path(data_path, "2020", "Data-rwa-nisr-ec-2020_stata.dta"))

nisr14_sector<- read_xlsx(file.path(data_path, "Sector2014.xlsx"))
nisr17_sector <- read_xlsx(file.path(data_path, "Sector2017.xlsx"))
nisr20_sector <- read_xlsx(file.path(data_path, "Sector2020.xlsx"))

#Village_id for establishment 2014

nisr_2014$Village_ID <- substr(nisr_2014$key, 1, 8)

nisr_2014 <- nisr_2014 %>% 
  select(Village_ID, everything())


nisr_2011 <- nisr_2011 %>% 
  mutate(Village_ID = ID1*10000000 + ID2*1000000 + ID3*10000 + ID4*100 +ID5) %>% 
  select(Village_ID, everything())

n_distinct(nisr_2011$Village_ID)

nisr_2011 <- nisr_2011 %>% 
  mutate(sector_ID = ID1*1000 + ID2*100 + ID3 ) %>% 
  select(sector_ID, everything())


analysis_2011_sector <- nisr_2011 %>% 
  group_by(sector_ID) %>% 
  summarize(
    num_establishments = n(),
    total_worker = sum(ifelse(Total_emp2 != 999999, Total_emp2, 0)), 
    within_market = sum(S03 == 1, na.rm = TRUE),
    outside_market = sum(S03 == 2, na.rm = TRUE),
    status_working = sum(S04 == 1, na.rm = TRUE),
    status_temp_closed = sum(S04 == 2, na.rm = TRUE),
    status_perm_closed = sum(S04 == 3, na.rm = TRUE),
    household_sector = sum(S05 == 1, na.rm = TRUE),
    private_sector = sum(S05 == 2, na.rm = TRUE),
    public_sector = sum(S05 == 3, na.rm = TRUE),
    mixed_sector = sum(S05 == 4, na.rm = TRUE),
    cooperative_sector = sum(S05 == 5, na.rm = TRUE),
    private_education_institution = sum(S05 == 6, na.rm = TRUE),
    private_health_institution = sum(S05 == 7, na.rm = TRUE),  # Fixed this line
    local_npo = sum(S05 == 8, na.rm = TRUE),  # Fixed this line
    IO = sum(S05 == 9, na.rm = TRUE),
    head_office = sum(S15 == 1, na.rm = TRUE),
    single_unit_establishment = sum(S15 == 2, na.rm = TRUE),
    branch_national_enterprise = sum(S15 == 3, na.rm = TRUE),
    branch_international_enterprise = sum(S15 == 4, na.rm = TRUE)
  )

analysis_2011_village <- nisr_2011 %>% 
  group_by(Village_ID) %>% 
  summarize(
    num_establishments = n(),
    total_worker = sum(ifelse(Total_emp2 != 999999, Total_emp2, 0)), 
    within_market = sum(S03 == 1, na.rm = TRUE),
    outside_market = sum(S03 == 2, na.rm = TRUE),
    status_working = sum(S04 == 1, na.rm = TRUE),
    status_temp_closed = sum(S04 == 2, na.rm = TRUE),
    status_perm_closed = sum(S04 == 3, na.rm = TRUE),
    household_sector = sum(S05 == 1, na.rm = TRUE),
    private_sector = sum(S05 == 2, na.rm = TRUE),
    public_sector = sum(S05 == 3, na.rm = TRUE),
    mixed_sector = sum(S05 == 4, na.rm = TRUE),
    cooperative_sector = sum(S05 == 5, na.rm = TRUE),
    private_education_institution = sum(S05 == 6, na.rm = TRUE),
    private_health_institution = sum(S05 == 7, na.rm = TRUE),  
    local_npo = sum(S05 == 8, na.rm = TRUE),  
    IO = sum(S05 == 9, na.rm = TRUE),
    head_office = sum(S15 == 1, na.rm = TRUE),
    single_unit_establishment = sum(S15 == 2, na.rm = TRUE),
    branch_national_enterprise = sum(S15 == 3, na.rm = TRUE),
    branch_international_enterprise = sum(S15 == 4, na.rm = TRUE)
  )





#icc function----

library(dplyr)

icc <- function(data, id_column, factor_column) {
  factor_col <- ensym(factor_column)
  id_col <- ensym(id_column)
  
  var_within_calc <- data %>%
    group_by(!!id_col) %>%
    summarise(var = var(!!factor_col)) %>%
    filter(!is.na(var))
  
  var_within <- mean(var_within_calc$var)
  
  var_between <- var(data[[as.character(factor_col)]], na.rm = TRUE)
  
  icc <- var_between / (var_between + var_within)
  
  return(icc)
}

#clean dataset to fit----

nisr_2011_analysis <- nisr_2011 %>% 
  rename(workers = Total_emp2) %>% 
  mutate(workers = ifelse(workers== 999999 | is.na(workers), 0, workers))

analysis_2011_village.1 <- analysis_2011_village

analysis_2011_village.1$Sector_ID <-substr(analysis_2011_village.1$Village_ID, 1, 4)

analysis_2011_village.1 <- analysis_2011_village.1 %>% 
  mutate(total_worker = ifelse(is.na(total_worker), 0, total_worker))

#For village
icc(nisr_2011_analysis, Village_ID, workers)

#For sector
icc(analysis_2011_village.1, Sector_ID, status_working)



#Just the variance----

var(analysis_2011_sector$total_worker)
var(analysis_2011_village$total_worker)

sector_variance <- analysis_2011_sector %>%
  summarise(across(everything(), var, na.rm = TRUE))


village_variance <- analysis_2011_village %>%
  summarise(across(everything(), var, na.rm = TRUE))

var(analysis_2011_sector$total_worker)

sector_variance <- sector_variance %>% 
  pivot_longer(
    cols = everything(),
    names_to = "category",
    values_to = "sector_variance"
  )

village_variance <- village_variance %>% 
  pivot_longer(
    cols = everything(),
    names_to = "category",
    values_to = "village_variance"
  )

variance <- left_join(sector_variance, village_variance, by = c("category"))

variance <- variance %>% 
  filter(category != "sector_ID")

write_xlsx(variance, path = file.path(data_path, "nisr_2011_variance.xlsx"))





#Just the sdiance----



sector_sd <- analysis_2011_sector %>%
  summarise(across(everything(), sd, na.rm = TRUE))


village_sd <- analysis_2011_village %>%
  summarise(across(everything(), sd, na.rm = TRUE))


sector_cv <- analysis_2011_sector %>%
  summarise(across(everything(), ~ (sd(.)/mean(., na.rm = TRUE)) * 100))

village_cv <- analysis_2011_village %>%
  summarise(across(everything(), ~ (sd(.)/mean(., na.rm = TRUE)) * 100))


sector_sd <- sector_sd %>% 
  pivot_longer(
    cols = everything(),
    names_to = "category",
    values_to = "sector_sd"
  )

village_sd <- village_sd %>% 
  pivot_longer(
    cols = everything(),
    names_to = "category",
    values_to = "village_sd"
  )


sector_cv <- sector_cv %>% 
  pivot_longer(
    cols = everything(),
    names_to = "category",
    values_to = "sector_cv"
  )

village_cv <- village_cv %>% 
  pivot_longer(
    cols = everything(),
    names_to = "category",
    values_to = "village_cv"
  )

sd <- left_join(sector_sd, village_sd, by = c("category"))
sd <- left_join(sd, sector_cv, by = c("category"))
sd <- left_join(sd, village_cv, by = c("category"))

sd <- sd %>% 
  filter(category != "sector_ID")

write_xlsx(sd, path = file.path(data_path, "nisr_2011_sd&cv.xlsx"))





