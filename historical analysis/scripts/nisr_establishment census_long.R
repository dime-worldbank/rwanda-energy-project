
##########################
#Author: Xiaoming Zhang
#Date of last modification: 07222024
#purpose:NISR establishment census analysis long dataset, output 0605
############################


#library----

pacman::p_load(fixest, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, plm, haven, stringr, modelsummary, kableExtra, stargazer, lfe)


#read file----

if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX, "Rwanda Energy/datawork/Historical data"
)


#Dataset preparation----

##Read capital level----

capital_2011 <- read_xlsx(path = file.path(data_path, "Establishment census", "outputs0605", "2011", "group_long_2011.xlsx"), sheet = "capital_group")
capital_2014 <- read_xlsx(path = file.path(data_path, "Establishment census", "outputs0605", "2014", "group_long_2014.xlsx"), sheet = "capital_group")
capital_2017 <- read_xlsx(path = file.path(data_path, "Establishment census", "outputs0605", "2017", "group_long_2017.xlsx"), sheet = "capital_group")
capital_2020 <- read_xlsx(path = file.path(data_path, "Establishment census", "outputs0605", "2020", "group_long_2020.xlsx"), sheet = "capital_group")
rwa_long <- read_xlsx( path = file.path(data_path, "nisr stata", "Attachment data", "rwa_long.xlsx"))


#Capital-----
capital_2011 <- capital_2011%>% 
  complete(village_id, employed_capital_group, fill = list(n = 0, total_employee = 0, total_manager = 0, total_owner = 0)) %>% 
  mutate(year = 2011) %>% 
  mutate(village_id = as.character(village_id))


capital_2014 <- capital_2014%>% 
  complete(village_id, employed_capital_group, fill = list(n = 0, total_employee = 0, total_manager = 0, total_owner = 0)) %>% 
  mutate(year = 2014) %>% 
  mutate(village_id = as.character(village_id))


capital_2017 <- capital_2017%>% 
  complete(village_id, employed_capital_group, fill = list(n = 0, total_employee = 0, total_manager = 0, total_owner = 0)) %>% 
  mutate(year = 2017) %>% 
  mutate(village_id = as.character(village_id))


capital_2020 <- capital_2020%>% 
  complete(village_id, employed_capital_group, fill = list(n = 0, total_employee = 0, total_manager = 0, total_owner = 0)) %>% 
  mutate(year = 2020) %>% 
  mutate(village_id = as.character(village_id))



capital <- bind_rows(capital_2011, capital_2014, capital_2017, capital_2020)


capital <- capital %>% 
  complete(village_id, employed_capital_group, year, fill = list(n = 0, total_employee = 0, total_manager = 0, total_owner = 0) )



capital_2011 <- capital_2011 %>% 
  mutate(employed_capital_group = ifelse(is.na(employed_capital_group), 0, employed_capital_group))

capital_2014 <- capital_2014 %>% 
  mutate(employed_capital_group = ifelse(is.na(employed_capital_group), 0, employed_capital_group))


capital_2017 <- capital_2017 %>% 
  mutate(employed_capital_group = ifelse(is.na(employed_capital_group), 0, employed_capital_group))


capital_2020 <- capital_2020 %>% 
  mutate(employed_capital_group = ifelse(is.na(employed_capital_group), 0, employed_capital_group))

##Capital group 1----
capital_2011_1 <- capital_2011 %>% 
  filter(employed_capital_group == 1 | employed_capital_group == 0)  %>% 
  mutate(year = 2011)

capital_2014_1 <- capital_2014 %>% 
  filter(employed_capital_group == 1  | employed_capital_group == 0)  %>% 
  mutate(year = 2014)

capital_2017_1 <- capital_2017 %>% 
  filter(employed_capital_group == 1  | employed_capital_group == 0)  %>% 
  mutate(year = 2017)

capital_2020_1 <- capital_2020 %>% 
  filter(employed_capital_group == 1  | employed_capital_group == 0)  %>% 
  mutate(year = 2020)


capital_1 <- bind_rows(capital_2011_1, capital_2014_1, capital_2017_1, capital_2020_1)

capital_1 <- capital_1 %>% 
  mutate(village_id = as.character(village_id)) %>% 
  rename(num_establishment = n)
##Capital group 2----

capital_2011_2 <- capital_2011 %>% 
  filter(employed_capital_group == 2  | employed_capital_group == 0)  %>% 
  mutate(year = 2011)

capital_2014_2 <- capital_2014 %>% 
  filter(employed_capital_group == 2  | employed_capital_group == 0)  %>% 
  mutate(year = 2014)

capital_2017_2 <- capital_2017 %>% 
  filter(employed_capital_group == 2  | employed_capital_group == 0)  %>% 
  mutate(year = 2017)

capital_2020_2 <- capital_2020 %>% 
  filter(employed_capital_group == 2 | employed_capital_group == 0)  %>% 
  mutate(year = 2020)


capital_2 <- bind_rows(capital_2011_2, capital_2014_2, capital_2017_2, capital_2020_2)

capital_2 <- capital_2 %>% 
  mutate(village_id = as.character(village_id)) %>% 
  rename(num_establishment = n)



##Capital group 3----

capital_2011_3 <- capital_2011 %>% 
  filter(employed_capital_group == 3  | employed_capital_group == 0)  %>% 
  mutate(year = 2011)

capital_2014_3 <- capital_2014 %>% 
  filter(employed_capital_group == 3  | employed_capital_group == 0)  %>% 
  mutate(year = 2014)

capital_2017_3 <- capital_2017 %>% 
  filter(employed_capital_group == 3  | employed_capital_group == 0)  %>% 
  mutate(year = 2017)

capital_2020_3 <- capital_2020 %>% 
  filter(employed_capital_group == 3  | employed_capital_group == 0)  %>% 
  mutate(year = 2020)


capital_3 <- bind_rows(capital_2011_3, capital_2014_3, capital_2017_3, capital_2020_3)

capital_3 <- capital_3 %>% 
  mutate(village_id = as.character(village_id)) %>% 
  rename(num_establishment = n)

##Capital group 4----

capital_2011_4 <- capital_2011 %>% 
  filter(employed_capital_group == 4 | employed_capital_group == 0)  %>% 
  mutate(year = 2011)

capital_2014_4 <- capital_2014 %>% 
  filter(employed_capital_group == 4  | employed_capital_group == 0)  %>% 
  mutate(year = 2014)

capital_2017_4 <- capital_2017 %>% 
  filter(employed_capital_group == 4  | employed_capital_group == 0)  %>% 
  mutate(year = 2017)

capital_2020_4 <- capital_2020 %>% 
  filter(employed_capital_group == 4  | employed_capital_group == 0)  %>% 
  mutate(year = 2020)


capital_4 <- bind_rows(capital_2011_4, capital_2014_4, capital_2017_4, capital_2020_4)

capital_4 <- capital_4 %>% 
  mutate(village_id = as.character(village_id)) %>% 
  rename(num_establishment = n)



##Capital group NA----

# capital_2011_na <- capital_2011 %>% 
#   filter(employed_capital_group == 0 )  %>% 
#   mutate(year = 2011)
# 
# capital_2014_na <- capital_2014 %>% 
#   filter(employed_capital_group == 0  ) %>% 
#   mutate(year = 2014)
# 
# capital_2017_na <- capital_2017 %>% 
#   filter(employed_capital_group == 0  ) %>% 
#   mutate(year = 2017)
# 
# capital_2020_na <- capital_2020 %>% 
#   filter(employed_capital_group == 0 )  %>% 
#   mutate(year = 2020)
# 
# 
# capital_na <- bind_rows(capital_2011_na, capital_2014_na, capital_2017_na, capital_2020_na)
# 
# capital_na <- capital_na %>% 
#   mutate(village_id = as.character(village_id))  %>% 
#   rename(num_establishment = n)
# 
# 
# 






#Descriptive statistics
capital <- bind_rows(capital_1, capital_2, capital_3, capital_4, capital_na)


capital_descriptive <- capital %>% 
  group_by(year, employed_capital_group) %>% 
  summarise(
    num_establishment = sum(num_establishment, na.rm = TRUE),
    total_employee = sum(total_employee, na.rm = TRUE)
  ) 



#rwa_long cleaning----

rwa_long <- rwa_long %>% 
  select(village_id, name, year, year_first, value, connect_vt, district, Distr_ID, sector, Sector_ID, cell, Cell_ID)


rwa_regress <- left_join(capital, rwa_long, by = c("village_id", "year"))

rwa_regress <- rwa_regress %>% 
  rename(
    num_establishment = n
  ) %>% 
  mutate(
    value = ifelse(is.na(value), 0, value)
  )
  


#Capital_1 analysis----

rwa_regress_1 <- rwa_regress %>% 
  filter(employed_capital_group == 1)


rwa_regress_1 <- rwa_regress_1 %>% 
  mutate(
    always_elec = ifelse(year_first <= 2011, 1, 0),
    elec12_14 = ifelse( year_first <= 2014 & year_first >= 2012, 1, 0),
    elec15_17 = ifelse(year_first <= 2017 & year_first >= 2015 , 1, 0),
    elec18_20 = ifelse( year_first <= 2020 & year_first >= 2018, 1,0),
    elec21_22 = ifelse(year_first <= 2022 & year_first >= 2021, 1,0),
    never_elec = ifelse(year_first == 2300 | is.na(year_first), 1, 0)
  ) %>% 
  mutate(
    status = case_when(
      year_first <= 2011 ~ "always_elec",
      year_first <= 2014 & year_first >= 2012 ~ "elec12_14",
      year_first <= 2017 & year_first >= 2015 ~ "elec15_17",
      year_first <= 2020 & year_first >= 2018 ~ "elec18_20",
      year_first <= 2022 & year_first >= 2021 ~ "elec21_22",
      year_first == 2300 | is.na(year_first) ~ "never_elec")
  ) %>% 
  mutate(
    num_establishment = ifelse(is.na(num_establishment), 0, num_establishment),
    total_employee = ifelse(is.na(total_employee), 0, total_employee)
  ) %>% 
  filter(!district %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu") ) %>% 
  filter(year %in% c("2011", "2014", "2017", "2020"))


##diff in diff


rwa_did_1 <- rwa_regress_1 %>% 
  filter(year_first > 2011) %>%
  filter(!is.na(employed_capital_group)) %>% 
  mutate(
    electrified = case_when(
      year_first > year ~ 0, 
      year_first <= year ~ 1
    )
  ) %>% 
  mutate(electrified = ifelse(is.na(electrified), 0, electrified)) 


###NTL Value

ntl_ec_model <- felm(value ~  electrified  | village_id + year, cluster = "village_id", data = rwa_did_1)

summary(ntl_ec_model)

###Num Estabilsment 


num_ec_model <- felm( num_establishment ~  electrified  | village_id + year, cluster = "village_id", data = rwa_did_1) 

summary(num_ec_model)


###Num Employee

employee_ec_model <-  felm(total_employee ~  electrified  | village_id + year, cluster = "village_id", data = rwa_did_1) 

summary(employee_ec_model)


###Poison establishment

rwa_ec_poison <- rwa_did_1 %>% 
  filter(num_establishment != 0)

num_ec_log <-  felm(log(num_establishment) ~  electrified  | village_id + year, cluster = "village_id", data = rwa_ec_poison) 

summary(num_ec_log)


###Poison  employee

rwa_ec_poison <- rwa_did_1 %>% 
  filter(total_employee != 0)

employee_ec_log <- felm( log(total_employee) ~  electrified  | village_id + year, cluster = "village_id", data = rwa_ec_poison) 

summary(employee_ec_log)



village_list <- list(
  `nighttime_light` = ntl_ec_model,
  `num_establishment` = num_ec_model,
  `total_employee` = employee_ec_model,
  `log_num_establishment` = num_ec_log,
  `log_total_employee` = employee_ec_log
)


stargazer(
  village_list,
  title = "Capital 1 Difference in differences (2011,2014,2017,2020)",
  stars = TRUE,
  notes = "Exclude villages that are electrified on or before 2011, \n exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu")






#capital_2 analysis----

rwa_regress_2 <- rwa_regress %>% 
  filter(employed_capital_group == 2)


rwa_regress_2 <- rwa_regress_2 %>% 
  mutate(
    always_elec = ifelse(year_first <= 2011, 1, 0),
    elec12_14 = ifelse( year_first <= 2014 & year_first >= 2012, 1, 0),
    elec15_17 = ifelse(year_first <= 2017 & year_first >= 2015 , 1, 0),
    elec18_20 = ifelse( year_first <= 2020 & year_first >= 2018, 1,0),
    elec21_22 = ifelse(year_first <= 2022 & year_first >= 2021, 1,0),
    never_elec = ifelse(year_first == 2300 | is.na(year_first), 1, 0)
  ) %>% 
  mutate(
    status = case_when(
      year_first <= 2011 ~ "always_elec",
      year_first <= 2014 & year_first >= 2012 ~ "elec12_14",
      year_first <= 2017 & year_first >= 2015 ~ "elec15_17",
      year_first <= 2020 & year_first >= 2018 ~ "elec18_20",
      year_first <= 2022 & year_first >= 2021 ~ "elec21_22",
      year_first == 2300 | is.na(year_first) ~ "never_elec")
  ) %>% 
  mutate(
    num_establishment = ifelse(is.na(num_establishment), 0, num_establishment),
    total_employee = ifelse(is.na(total_employee), 0, total_employee)
  ) %>% 
  filter(!district %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu") ) %>% 
  filter(year %in% c("2011", "2014", "2017", "2020"))


##diff in diff


rwa_did_2 <- rwa_regress_2 %>% 
  filter(year_first > 2011) %>% 
  filter(!is.na(employed_capital_group)) %>% 
  mutate(
    electrified = case_when(
      year_first > year ~ 0, 
      year_first <= year ~ 1
    )
  ) %>% 
  mutate(electrified = ifelse(is.na(electrified), 0, electrified)) 


###NTL Value

ntl_ec_model <- felm(value ~  electrified  | village_id + year, cluster = "village_id", data = rwa_did_2)

summary(ntl_ec_model)

###Num Estabilsment 


num_ec_model <- felm( num_establishment ~  electrified  | village_id + year, cluster = "village_id", data = rwa_did_2) 

summary(num_ec_model)


###Num Employee

employee_ec_model <-  felm(total_employee ~  electrified  | village_id + year, cluster = "village_id", data = rwa_did_2) 

summary(employee_ec_model)


###Poison establishment

rwa_ec_poison <- rwa_did_2 %>% 
  filter(num_establishment != 0)

num_ec_log <-  felm(log(num_establishment) ~  electrified  | village_id + year, cluster = "village_id", data = rwa_ec_poison) 

summary(num_ec_log)


###Poison  employee

rwa_ec_poison <- rwa_did_2 %>% 
  filter(total_employee != 0)

employee_ec_log <- felm( log(total_employee) ~  electrified  | village_id + year, cluster = "village_id", data = rwa_ec_poison) 

summary(employee_ec_log)



village_list <- list(
  `nighttime_light` = ntl_ec_model,
  `num_establishment` = num_ec_model,
  `total_employee` = employee_ec_model,
  `log_num_establishment` = num_ec_log,
  `log_total_employee` = employee_ec_log
)


stargazer(
  village_list,
  title = "Capital 2 Difference in differences (2011,2014,2017,2020)",
  stars = TRUE,
  notes = "Exclude villages that are electrified on or before 2011, \n exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu")











#Capital_3 analysis----


rwa_regress_3 <-  rwa_regress %>% 
  filter(employed_capital_group == 3)

rwa_regress_3 <- rwa_regress_3 %>% 
  mutate(
    always_elec = ifelse(year_first <= 2011, 1, 0),
    elec12_14 = ifelse( year_first <= 2014 & year_first >= 2012, 1, 0),
    elec15_17 = ifelse(year_first <= 2017 & year_first >= 2015 , 1, 0),
    elec18_20 = ifelse( year_first <= 2020 & year_first >= 2018, 1,0),
    elec21_22 = ifelse(year_first <= 2022 & year_first >= 2021, 1,0),
    never_elec = ifelse(year_first == 2300 | is.na(year_first), 1, 0)
  ) %>% 
  mutate(
    status = case_when(
      year_first <= 2011 ~ "always_elec",
      year_first <= 2014 & year_first >= 2012 ~ "elec12_14",
      year_first <= 2017 & year_first >= 2015 ~ "elec15_17",
      year_first <= 2020 & year_first >= 2018 ~ "elec18_20",
      year_first <= 2022 & year_first >= 2021 ~ "elec21_22",
      year_first == 2300 | is.na(year_first) ~ "never_elec")
  ) %>% 
  mutate(
    num_establishment = ifelse(is.na(num_establishment), 0, num_establishment),
    total_employee = ifelse(is.na(total_employee), 0, total_employee)
  ) %>% 
  filter(!district %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu") ) %>% 
  filter(year %in% c("2011", "2014", "2017", "2020"))


##diff in diff


rwa_did_3 <- rwa_regress_3 %>% 
  filter(year_first > 2011) %>% 
  filter(!is.na(employed_capital_group)) %>% 
  mutate(
    electrified = case_when(
      year_first > year ~ 0, 
      year_first <= year ~ 1
    )
  ) %>% 
  mutate(electrified = ifelse(is.na(electrified), 0, electrified)) 


###NTL Value

ntl_ec_model <- felm(value ~  electrified  | village_id + year, cluster = "village_id", data = rwa_did_3)

summary(ntl_ec_model)

###Num Estabilsment 


num_ec_model <- felm( num_establishment ~  electrified  | village_id + year, cluster = "village_id", data = rwa_did_3) 

summary(num_ec_model)


###Num Employee

employee_ec_model <-  felm(total_employee ~  electrified  | village_id + year, cluster = "village_id", data = rwa_did_3) 

summary(employee_ec_model)


###Poison establishment

rwa_ec_poison <- rwa_did_3 %>% 
  filter(num_establishment != 0)

num_ec_log <-  felm(log(num_establishment) ~  electrified  | village_id + year, cluster = "village_id", data = rwa_ec_poison) 

summary(num_ec_log)


###Poison  employee

rwa_ec_poison <- rwa_did_3 %>% 
  filter(total_employee != 0)

employee_ec_log <- felm( log(total_employee) ~  electrified  | village_id + year, cluster = "village_id", data = rwa_ec_poison) 

summary(employee_ec_log)



village_list <- list(
  `nighttime_light` = ntl_ec_model,
  `num_establishment` = num_ec_model,
  `total_employee` = employee_ec_model,
  `log_num_establishment` = num_ec_log,
  `log_total_employee` = employee_ec_log
)


stargazer(
  village_list,
  title = "Capital 3 Difference in differences (2011,2014,2017,2020)",
  stars = TRUE,
  notes = "Exclude villages that are electrified on or before 2011, \n exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu")











#Capital_4 analysis----


rwa_regress_4 <- rwa_regress %>% 
  filter(employed_capital_group == 4)


rwa_regress_4 <- rwa_regress_4 %>% 
  mutate(
    always_elec = ifelse(year_first <= 2011, 1, 0),
    elec12_14 = ifelse( year_first <= 2014 & year_first >= 2012, 1, 0),
    elec15_17 = ifelse(year_first <= 2017 & year_first >= 2015 , 1, 0),
    elec18_20 = ifelse( year_first <= 2020 & year_first >= 2018, 1,0),
    elec21_22 = ifelse(year_first <= 2022 & year_first >= 2021, 1,0),
    never_elec = ifelse(year_first == 2300 | is.na(year_first), 1, 0)
  ) %>% 
  mutate(
    status = case_when(
      year_first <= 2011 ~ "always_elec",
      year_first <= 2014 & year_first >= 2012 ~ "elec12_14",
      year_first <= 2017 & year_first >= 2015 ~ "elec15_17",
      year_first <= 2020 & year_first >= 2018 ~ "elec18_20",
      year_first <= 2022 & year_first >= 2021 ~ "elec21_22",
      year_first == 2300 | is.na(year_first) ~ "never_elec")
  ) %>% 
  mutate(
    num_establishment = ifelse(is.na(num_establishment), 0, num_establishment),
    total_employee = ifelse(is.na(total_employee), 0, total_employee)
  ) %>% 
  filter(!district %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu") ) %>% 
  filter(year %in% c("2011", "2014", "2017", "2020"))


##diff in diff


rwa_did_4 <- rwa_regress_4 %>% 
  filter(year_first > 2011) %>% 
  filter(!is.na(employed_capital_group)) %>% 
  mutate(
    electrified = case_when(
      year_first > year ~ 0, 
      year_first <= year ~ 1
    )
  ) %>% 
  mutate(electrified = ifelse(is.na(electrified), 0, electrified)) 


###NTL Value

ntl_ec_model <- felm(value ~  electrified  | village_id + year, cluster = "village_id", data = rwa_did_4)

summary(ntl_ec_model)

###Num Estabilsment 


num_ec_model <- felm( num_establishment ~  electrified  | village_id + year, cluster = "village_id", data = rwa_did_4) 

summary(num_ec_model)


###Num Employee

employee_ec_model <-  felm(total_employee ~  electrified  | village_id + year, cluster = "village_id", data = rwa_did_4) 

summary(employee_ec_model)


###Poison establishment

rwa_ec_poison <- rwa_did_4 %>% 
  filter(num_establishment != 0)

num_ec_log <-  felm(log(num_establishment) ~  electrified  | village_id + year, cluster = "village_id", data = rwa_ec_poison) 

summary(num_ec_log)


###Poison  employee

rwa_ec_poison <- rwa_did_4 %>% 
  filter(total_employee != 0)

employee_ec_log <- felm( log(total_employee) ~  electrified  | village_id + year, cluster = "village_id", data = rwa_ec_poison) 

summary(employee_ec_log)



village_list <- list(
  `nighttime_light` = ntl_ec_model,
  `num_establishment` = num_ec_model,
  `total_employee` = employee_ec_model,
  `log_num_establishment` = num_ec_log,
  `log_total_employee` = employee_ec_log
)


stargazer(
  village_list,
  title = "Capital 4 Difference in differences (2011,2014,2017,2020)",
  stars = TRUE,
  notes = "Exclude villages that are electrified on or before 2011, \n exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu")

















