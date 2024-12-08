
##########################
#Author: Xiaoming Zhang
#Date of last modification: 05302024
#purpose:NISR establishment census analysis
############################


#library----

pacman::p_load(fixest, tidyverse, dplyr, here, sf, ggplot2, lfe,readxl, writexl, janitor, haven, stringr, stargazer, kableExtra)


#read file----

if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX, "Rwanda Energy/EAQIP/datawork/Historical data"
)


#Dataset preparation----


rwa_long <- read_xlsx( path = file.path(data_path, "nisr stata", "Attachment data", "rwa_long.xlsx"))
village_2011 <- read_xlsx(path = file.path(data_path, "Establishment census", "village_level_2011.xlsx"))
village_2014 <- read_xlsx(path = file.path(data_path, "Establishment census", "village_level_2014.xlsx"))
village_2017 <- read_xlsx(path = file.path(data_path, "Establishment census", "village_level_2017.xlsx"))
village_2020 <- read_xlsx(path = file.path(data_path, "Establishment census", "village_level_2020.xlsx"))

rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))
village_join <- rwa_villages %>% 
  st_drop_geometry()


key_2011 <-village_2011%>% 
  select(village_id, num_establishment,total_employee, employed_capital_1, employed_capital_2, employed_capital_3, employed_capital_4) %>% 
  mutate(
    year = 2011
  )

key_2014 <-village_2014%>% 
  select(village_id,num_establishment, total_employee, employed_capital_1, employed_capital_2, employed_capital_3, employed_capital_4) %>% 
  mutate(
    year = 2014
  )

key_2017 <-village_2017%>% 
  select(village_id,num_establishment, total_employee, employed_capital_1, employed_capital_2, employed_capital_3, employed_capital_4) %>% 
  mutate(
    year = 2017
  )

key_2020 <-village_2020%>% 
  select(village_id, num_establishment,total_employee, employed_capital_1, employed_capital_2, employed_capital_3, employed_capital_4) %>% 
  mutate(
    year = 2020
  )

key_all <- bind_rows(key_2011, key_2014, key_2017, key_2020) 

key_all <- key_all %>% 
  mutate(village_id = as.character(village_id))

rwa_regress <- left_join(rwa_long, key_all, by = c("village_id", "year"))

rwa_regress <- rwa_regress %>% 
  group_by(district, Distr_ID, Sector_ID, Cell_ID, year) %>% 
  summarise(
    value = mean(value),
    usage = sum(usage, na.rm = TRUE),
    num_establishment = sum(num_establishment, na.rm = TRUE),
    total_employee = sum(total_employee, na.rm = TRUE),
    employed_capital_1 = sum(employed_capital_1, na.rm = TRUE),
    employed_capital_2 = sum(employed_capital_2, na.rm = TRUE),
    employed_capital_3 = sum(employed_capital_3, na.rm = TRUE),
    employed_capital_4 = sum(employed_capital_4, na.rm = TRUE),
    year_first = min(year_first)
  ) 

rwa_regress <- rwa_regress %>% 
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
    total_employee = ifelse(is.na(total_employee), 0, total_employee),
    employed_capital_1 = ifelse(is.na(employed_capital_1), 0, employed_capital_1),
    employed_capital_2 = ifelse(is.na(employed_capital_2), 0, employed_capital_2),
    employed_capital_3 = ifelse(is.na(employed_capital_3), 0, employed_capital_3),
    employed_capital_4 = ifelse(is.na(employed_capital_4), 0, employed_capital_4)
  ) %>% 
  clean_names()



#Balance table-----



rwa_bt <- rwa_regress %>% 
  mutate(
    status = case_when(
      year_first <= 2011 ~ "always_elec",
      year_first <= 2014 & year_first >= 2012 ~ "elec12_14",
      year_first <= 2017 & year_first >= 2015 ~ "elec15_17",
      year_first <= 2020 & year_first >= 2018 ~ "elec18_20",
      year_first <= 2022 & year_first >= 2021 ~ "elec21_22",
      year_first == 2300 | is.na(year_first) ~ "never_elec"
    )
  )  %>% 
  filter(year == 2011) %>% 
  filter(!district %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu") )

rwa_bt_selec <- rwa_bt %>% 
  select(
    cell_id, status, value, usage, num_establishment, total_employee, employed_capital_1, employed_capital_2, employed_capital_3, employed_capital_4
  )


rwa_bt_selec <- rwa_bt_selec %>% 
  mutate(
    cell_id = as.numeric(cell_id)
  )


write_dta(rwa_bt_selec, path = file.path(data_path, "Establishment census", "balance_table_cell.dta"))







#Diff in diff----


rwa_did <- rwa_regress %>% 
  mutate(
    electrified = case_when(
      year_first > year ~ 0, 
      year_first <= year ~ 1
    )
  ) %>% 
  mutate(electrified = ifelse(is.na(electrified), 0, electrified))

##All ntl----

ntl_all <- rwa_did %>% 
  filter(year_first > 2010) %>% 
  filter(year > 2010) 

ntl_all_model <- 
  felm( value ~  electrified  | cell_id + year, cluster = "cell_id", data = ntl_all) 
summary(ntl_all_model)

cluster <- as.numeric(n_distinct(ntl_all$cell_id))
n_mean <- round(mean(ntl_all$value, na.rm = TRUE), 3)
st_dev <- round(sd(ntl_all$value, na.rm = TRUE), 3)

stargazer(
  ntl_all_model,
  title = "Nighttime Light Value(2011-2021)",
  stars = TRUE,
  notes = "Exclude villages that are electrified before 2011, cluster = 1369, mean = 2.932, standard deviation = 5.314 "
  )







##Establishment census years----

rwa_ec <- rwa_did %>% 
  filter(year_first > 2010) %>% 
  filter(year == 2011 | year == 2014 | year == 2017 | year == 2020) %>% 
  mutate(
    num_establishment = ifelse(is.na(num_establishment), 0, num_establishment),
    total_employee = ifelse(is.na(total_employee), 0, total_employee),
    employed_capital_1 = ifelse(is.na(employed_capital_1), 0, employed_capital_1),
    employed_capital_2 = ifelse(is.na(employed_capital_2), 0, employed_capital_2),
    employed_capital_3 = ifelse(is.na(employed_capital_3), 0, employed_capital_3),
    employed_capital_4 = ifelse(is.na(employed_capital_4), 0, employed_capital_4)
  ) 

###NTL Value

ntl_ec_model <- felm( value ~  electrified  | cell_id + year, cluster = "cell_id", data = rwa_ec )

summary(ntl_ec_model)

###Estabilsment census year


num_ec_model <- felm( num_establishment ~  electrified  | cell_id + year, cluster = "cell_id", data = rwa_ec )

summary(num_ec_model)


###Employee

employee_ec_model <-  felm( total_employee ~  electrified  | cell_id + year, cluster = "cell_id", data = rwa_ec )

summary(employee_ec_model)


###Poison establishment

rwa_ec_poison <- rwa_ec %>% 
  filter(num_establishment != 0)

num_ec_log <-  felm( log(num_establishment) ~  electrified  | cell_id + year, cluster = "cell_id", data = rwa_ec_poison)

summary(num_ec_log)


###Poison  employee

rwa_ec_poison <- rwa_ec %>% 
  filter(total_employee != 0)

employee_ec_log <- felm( log(total_employee) ~  electrified  | cell_id + year, cluster = "cell_id", data = rwa_ec_poison)

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
  title = "Establishment years (2011,2014,2017,2020)",
  stars = TRUE,
  note = "Exclude villages that are electrified before 2011"
)




#Event Study----

rwa_event <- rwa_regress %>% 
  filter(year == 2011 | year == 2014 | year == 2017 | year == 2020)


##Ntl----

#12_14
elec12_14 <- rwa_event %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )  

table(elec12_14$year)

ntl12_14 <- felm(value ~ p0_2014:elec12_14 + p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + year, cluster = "cell_id", data = elec12_14)


summary(ntl12_14)

##15_17
elec15_17 <- rwa_event %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  )


ntl15_17 <- felm(value ~ p_2_2011:elec15_17 + p0_2017:elec15_17 + p1_2020:elec15_17|cell_id +year, cluster = "cell_id",data = elec15_17)

summary(ntl15_17)

##18_20
elec18_20 <- rwa_event %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  )

ntl18_20 <- felm(value ~ p_3_2011:elec18_20 + p_2_2014:elec18_20 + p0_2020:elec18_20|cell_id +year, cluster = "cell_id",data = elec18_20)

summary(ntl18_20)

ntl <- list(
  `For 2014` = ntl12_14,
  `For 2017` = ntl15_17,
  `For 2020` = ntl18_20
)

stargazer(
  ntl,
  title = "Event Study Nighttime light as outcome (range 0~60)",
  stars = TRUE
)


##Ntl log----

#12_14
elec12_14 <- rwa_event %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )  %>% 
  filter(value != 0) %>% 
  mutate(
    log_value = log(value)
  ) 
  


table(elec12_14$year)

ntl12_14 <- felm(log_value ~ p0_2014:elec12_14 + p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + year, cluster = "cell_id", data = elec12_14)


summary(ntl12_14)

##15_17
elec15_17 <- rwa_event %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  )  %>% 
  filter(value != 0) %>% 
  mutate(
    log_value = log(value)
  ) 



ntl15_17 <- felm(log_value ~ p_2_2011:elec15_17 + p0_2017:elec15_17 + p1_2020:elec15_17|cell_id +year, cluster = "cell_id",data = elec15_17)

summary(ntl15_17)

##18_20
elec18_20 <- rwa_event %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  ) %>% 
  filter(value != 0) %>% 
  mutate(
    log_value = log(value)
  ) 


ntl18_20 <- felm(log_value ~ p_3_2011:elec18_20 + p_2_2014:elec18_20 + p0_2020:elec18_20|cell_id +year, cluster = "cell_id",data = elec18_20)

summary(ntl18_20)

ntl <- list(
  `For 2014` = ntl12_14,
  `For 2017` = ntl15_17,
  `For 2020` = ntl18_20
)

stargazer(
  ntl,
  title = "Event Study log Nighttime light as outcome (range 0~60)",
  stars = TRUE
)


##Num establishment----

#12_14
elec12_14 <- rwa_event %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )  

table(elec12_14$year)

establishment12_14 <- felm(num_establishment ~ p0_2014:elec12_14 + p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + year, cluster = "cell_id", data = elec12_14)


summary(establishment12_14)

##15_17
elec15_17 <- rwa_event %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  )


establishment15_17 <- felm(num_establishment ~ p_2_2011:elec15_17 + p0_2017:elec15_17 + p1_2020:elec15_17|cell_id +year, cluster = "cell_id", data = elec15_17)



##18_20
elec18_20 <- rwa_event %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  )

establishment18_20 <- felm(num_establishment ~ p_3_2011:elec18_20 + p_2_2014:elec18_20 + p0_2020:elec18_20|cell_id +year, cluster = "cell_id", data = elec18_20)



establishment <- list(
  `For 2014` = establishment12_14,
  `For 2017` = establishment15_17,
  `For 2020` = establishment18_20
)

stargazer(
  establishment,
  title = "Event Study Number of Establishments as outcome ",
  stars = TRUE
  # note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)



##Log Num establishment----

#12_14
elec12_14 <- rwa_event %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )  %>% 
  filter(num_establishment != 0) %>% 
  mutate(
    log_num_establishment = log(num_establishment)
  )

table(elec12_14$year)

establishment12_14 <- felm(log_num_establishment ~ p0_2014:elec12_14 + p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + year, cluster = "cell_id", data = elec12_14)


summary(establishment12_14)

##15_17
elec15_17 <- rwa_event %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  ) %>% 
  filter(num_establishment != 0) %>% 
  mutate(
    log_num_establishment = log(num_establishment)
  )


establishment15_17 <- felm(log_num_establishment ~ p_2_2011:elec15_17 + p0_2017:elec15_17 + p1_2020:elec15_17|cell_id +year, cluster = "cell_id", data = elec15_17)


summary(establishment15_17)
##18_20
elec18_20 <- rwa_event %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  ) %>% 
  filter(num_establishment != 0) %>% 
  mutate(
    log_num_establishment = log(num_establishment)
  )

establishment18_20 <- felm(log_num_establishment ~ p_3_2011:elec18_20 + p_2_2014:elec18_20 + p0_2020:elec18_20|cell_id +year, cluster = "cell_id", data = elec18_20)


summary(establishment18_20)
establishment <- list(
  `For 2014` = establishment12_14,
  `For 2017` = establishment15_17,
  `For 2020` = establishment18_20
)

stargazer(
  establishment,
  title = "Log Event Study Number of Establishments as outcome ",
  stars = TRUE
  # note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)

##Total_employee ----

##12-14
elec12_14 <- rwa_event %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )  

table(elec12_14$year)

employee12_14 <- felm(total_employee ~ p0_2014:elec12_14 + p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + year,cluster = "cell_id", data = elec12_14)


summary(employee12_14)

##15_17
elec15_17 <- rwa_event %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  )


employee15_17 <- felm(total_employee ~ p_2_2011:elec15_17 + p0_2017:elec15_17 + p1_2020:elec15_17|cell_id +year,cluster = "cell_id", data = elec15_17)



##18_20
elec18_20 <- rwa_event %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  )

employee18_20 <- felm(total_employee ~ p_3_2011:elec18_20 + p_2_2014:elec18_20 + p0_2020:elec18_20|cell_id +year,cluster = "cell_id", data = elec18_20)



employee <- list(
  `For 2014` = employee12_14,
  `For 2017` = employee15_17,
  `For 2020` = employee18_20
)

stargazer(
  employee,
  title = "Event Study Total Employee as outcome ",
  stars = TRUE
)




##Log Total_employee ----

##12-14
elec12_14 <- rwa_event %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )  %>% 
  filter(total_employee != 0) %>% 
  mutate(
    log_total_employee = log(total_employee)
  )

table(elec12_14$year)

employee12_14 <- felm(log_total_employee ~ p0_2014:elec12_14 + p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + year,cluster = "cell_id", data = elec12_14)


summary(employee12_14)

##15_17
elec15_17 <- rwa_event %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  ) %>%  
  filter(total_employee != 0) %>% 
  mutate(
    log_total_employee = log(total_employee)
  )


employee15_17 <- felm(log_total_employee ~ p_2_2011:elec15_17 + p0_2017:elec15_17 + p1_2020:elec15_17|cell_id +year,cluster = "cell_id", data = elec15_17)

summary(employee15_17)

##18_20
elec18_20 <- rwa_event %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  ) %>% 
  filter(total_employee != 0) %>% 
  mutate(
    log_total_employee = log(total_employee)
  )


employee18_20 <- felm(log_total_employee ~ p_3_2011:elec18_20 + p_2_2014:elec18_20 + p0_2020:elec18_20|cell_id +year,cluster = "cell_id", data = elec18_20)

summary(employee18_20)

employee <- list(
  `For 2014` = employee12_14,
  `For 2017` = employee15_17,
  `For 2020` = employee18_20
)

stargazer(
  employee,
  title = "Event Study Log Total Employee as outcome ",
  stars = TRUE
)



#District-year fixed effects-----


rwa_event <- rwa_event %>% 
  mutate(district_year = paste0(distr_id, year),
         sector_year = paste0(sector_id, year))

##Log Ntl----

#12_14
elec12_14 <- rwa_event %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )   %>% 
  filter(value != 0) %>% 
  mutate(
    log_value = log(value)
  ) 

table(elec12_14$year)


ntl12_14 <- felm(log_value ~ p0_2014:elec12_14 + p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + district_year, clustervar = "cell_id", data = elec12_14)

summary(ntl12_14)

##15_17
elec15_17 <- rwa_event %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  ) %>% 
  filter(value != 0) %>% 
  mutate(
    log_value = log(value)
  ) 


ntl15_17 <- felm(log_value ~ p_2_2011:elec15_17 + p0_2017:elec15_17 + p1_2020:elec15_17|cell_id + district_year, cluster = "cell_id", data = elec15_17)

summary(ntl15_17)

##18_20
elec18_20 <- rwa_event %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  ) %>% 
  filter(value != 0) %>% 
  mutate(
    log_value = log(value)
  ) 

ntl18_20 <- felm(log_value ~ p_3_2011:elec18_20 + p_2_2014:elec18_20 + p0_2020:elec18_20|cell_id + district_year, cluster = "cell_id", data = elec18_20)

summary(ntl18_20)

ntl <- list(
  `For 2014` = ntl12_14,
  `For 2017` = ntl15_17,
  `For 2020` = ntl18_20
)

stargazer(
  ntl,
  title = "Event Study Log Nighttime light as outcome (range 0~60)",
  stars = TRUE# out = file.path(output_path, "es_ntl.html")
)



##Num establishment----

#12_14
elec12_14 <- rwa_event %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )  

table(elec12_14$year)

establishment12_14 <- felm(num_establishment ~ p0_2014:elec12_14 + p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + district_year, cluster = "cell_id" ,data = elec12_14)


summary(establishment12_14)

##15_17
elec15_17 <- rwa_event %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  )


establishment15_17 <- felm(num_establishment ~ p_2_2011:elec15_17 + p0_2017:elec15_17 + p1_2020:elec15_17|cell_id + district_year, cluster = "cell_id",data = elec15_17)



##18_20
elec18_20 <- rwa_event %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  )

establishment18_20 <- felm(num_establishment ~ p_3_2011:elec18_20 + p_2_2014:elec18_20 + p0_2020:elec18_20|cell_id + district_year, cluster = "cell_id",data = elec18_20)



establishment <- list(
  `For 2014` = establishment12_14,
  `For 2017` = establishment15_17,
  `For 2020` = establishment18_20
)




stargazer(
  establishment,
  title = "Event Study Number of Establishments as outcome",
  stars = TRUE
)




##Log Num establishment----

#12_14
elec12_14 <- rwa_event %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )  %>% 
  filter(num_establishment != 0) %>% 
  mutate(
    log_num_establishment = log(num_establishment)
  )

table(elec12_14$year)

establishment12_14 <- felm(log_num_establishment ~ p0_2014:elec12_14 + p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + district_year, cluster = "cell_id" ,data = elec12_14)


summary(establishment12_14)

##15_17
elec15_17 <- rwa_event %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  ) %>% 
  filter(num_establishment != 0) %>% 
  mutate(
    log_num_establishment = log(num_establishment)
  )


establishment15_17 <- felm(log_num_establishment ~ p_2_2011:elec15_17 + p0_2017:elec15_17 + p1_2020:elec15_17|cell_id + district_year, cluster = "cell_id",data = elec15_17)



##18_20
elec18_20 <- rwa_event %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  ) %>% 
  filter(num_establishment != 0) %>% 
  mutate(
    log_num_establishment = log(num_establishment)
  )

establishment18_20 <- felm(log_num_establishment ~ p_3_2011:elec18_20 + p_2_2014:elec18_20 + p0_2020:elec18_20|cell_id + district_year, cluster = "cell_id",data = elec18_20)

summary(establishment18_20)

establishment <- list(
  `For 2014` = establishment12_14,
  `For 2017` = establishment15_17,
  `For 2020` = establishment18_20
)




stargazer(
  establishment,
  title = "Event Study Log Number of Establishments as outcome",
  stars = TRUE
)


##log Total_employee ----

##12-14
elec12_14 <- rwa_event %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )  %>% 
  filter(total_employee != 0) %>% 
  mutate(
    log_total_employee = log(total_employee)
  )

table(elec12_14$year)

employee12_14 <- felm(log_total_employee ~ p0_2014:elec12_14 + p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + district_year, cluster = "cell_id",  data = elec12_14)

summary(employee12_14)

##15_17
elec15_17 <- rwa_event %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  )%>% 
  filter(total_employee != 0) %>% 
  mutate(
    log_total_employee = log(total_employee)
  )


employee15_17 <- felm(log_total_employee ~ p_2_2011:elec15_17 + p0_2017:elec15_17 + p1_2020:elec15_17|cell_id + district_year, cluster = "cell_id",  data = elec15_17)

summary(employee15_17)


##18_20
elec18_20 <- rwa_event %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  )%>% 
  filter(total_employee != 0) %>% 
  mutate(
    log_total_employee = log(total_employee)
  )

employee18_20 <- felm(log_total_employee ~ p_3_2011:elec18_20 + p_2_2014:elec18_20 + p0_2020:elec18_20|cell_id + district_year, cluster = "cell_id",  data = elec18_20)

summary(employee18_20)

employee <- list(
  `For 2014` = employee12_14,
  `For 2017` = employee15_17,
  `For 2020` = employee18_20
)



stargazer(
  employee,
  title = "Event Study Log Number of Total employee as outcome",
  stars = TRUE
)







#Sector-year fixed effects-----


##Ntl----

#12_14

elec12_14 <- rwa_event %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )

ntl12_14 <- felm(value ~ p0_2014:elec12_14 + p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + sector_year, clustervar = "cell_id", data = elec12_14)

summary(ntl12_14)

##15_17
elec15_17 <- rwa_event %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  )

ntl15_17 <- felm(value ~ p_2_2011:elec15_17 + p0_2017:elec15_17 + p1_2020:elec15_17|cell_id + sector_year, cluster = "cell_id", data = elec15_17)

summary(ntl15_17)

##18_20

elec18_20 <- rwa_event %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  )

ntl18_20 <- felm(value ~ p_3_2011:elec18_20 + p_2_2014:elec18_20 + p0_2020:elec18_20|cell_id + sector_year, cluster = "cell_id", data = elec18_20)

summary(ntl18_20)

ntl <- list(
  `For 2014` = ntl12_14,
  `For 2017` = ntl15_17,
  `For 2020` = ntl18_20
)

stargazer(
  ntl,
  title = "Event Study Nighttime light as outcome (range 0~60)",
  stars = TRUE  # out = file.path(output_path, "es_ntl.html")
)


##Log-Ntl----

#12_14

elec12_14 <- rwa_event %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )  %>% 
  filter(value != 0) %>% 
  mutate(
    log_value = log(value)
  )  
  

ntl12_14 <- felm(log_value ~ p0_2014:elec12_14 + p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + sector_year, clustervar = "cell_id", data = elec12_14)

summary(ntl12_14)

##15_17
elec15_17 <- rwa_event %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  ) %>% 
  filter(value != 0) %>% 
  mutate(
    log_value = log(value)
  ) 

table(elec15_17$year)



ntl15_17 <- felm(log_value ~ p_2_2011:elec15_17 + p0_2017:elec15_17 + p1_2020:elec15_17|cell_id + district_year, cluster = "cell_id", data = elec15_17)

summary(ntl15_17)

##18_20

elec18_20 <- rwa_event %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  ) %>% 
  filter(value != 0) %>% 
  mutate(
    log_value = log(value)
  ) 

ntl18_20 <- felm(log_value ~ p_3_2011:elec18_20 + p_2_2014:elec18_20 + p0_2020:elec18_20|cell_id + sector_year, cluster = "cell_id", data = elec18_20)

summary(ntl18_20)

ntl <- list(
  `For 2014` = ntl12_14,
  `For 2017` = ntl15_17,
  `For 2020` = ntl18_20
)

stargazer(
  ntl,
  title = "Event Study Log Nighttime light as outcome (range 0~60)",
  stars = TRUE  # out = file.path(output_path, "es_ntl.html")
)


##Num establishment----

#12_14
elec12_14 <- rwa_event %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )  

table(elec12_14$year)

establishment12_14 <- felm(num_establishment ~ p0_2014:elec12_14 + p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + sector_year, cluster = "cell_id" ,data = elec12_14)


summary(establishment12_14)

##15_17
elec15_17 <- rwa_event %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  )


establishment15_17 <- felm(num_establishment ~ p_2_2011:elec15_17 + p0_2017:elec15_17 + p1_2020:elec15_17|cell_id + sector_year, cluster = "cell_id",data = elec15_17)



##18_20
elec18_20 <- rwa_event %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  )

establishment18_20 <- felm(num_establishment ~ p_3_2011:elec18_20 + p_2_2014:elec18_20 + p0_2020:elec18_20|cell_id + sector_year, cluster = "cell_id",data = elec18_20)



establishment <- list(
  `For 2014` = establishment12_14,
  `For 2017` = establishment15_17,
  `For 2020` = establishment18_20
)




stargazer(
  establishment,
  title = "Event Study Number of Establishments as outcome",
  stars = TRUE
)


##Log Num establishment----

#12_14
elec12_14 <- rwa_event %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )  %>% 
  filter(num_establishment != 0) %>% 
  mutate(
    log_num_establishment = log(num_establishment)
  )

table(elec12_14$year)

establishment12_14 <- felm(log_num_establishment ~ p0_2014:elec12_14 + p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + sector_year, cluster = "cell_id" ,data = elec12_14)


summary(establishment12_14)

##15_17
elec15_17 <- rwa_event %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  )%>% 
  filter(num_establishment != 0) %>% 
  mutate(
    log_num_establishment = log(num_establishment)
  )


establishment15_17 <- felm(log_num_establishment ~ p_2_2011:elec15_17 + p0_2017:elec15_17 + p1_2020:elec15_17|cell_id + sector_year, cluster = "cell_id",data = elec15_17)

summary(establishment15_17)


##18_20
elec18_20 <- rwa_event %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  )%>% 
  filter(num_establishment != 0) %>% 
  mutate(
    log_num_establishment = log(num_establishment)
  )

establishment18_20 <- felm(log_num_establishment ~ p_3_2011:elec18_20 + p_2_2014:elec18_20 + p0_2020:elec18_20|cell_id + sector_year, cluster = "cell_id",data = elec18_20)

summary(establishment18_20)


establishment <- list(
  `For 2014` = establishment12_14,
  `For 2017` = establishment15_17,
  `For 2020` = establishment18_20
)




stargazer(
  establishment,
  title = "Event Study Log Number of Establishments as outcome",
  stars = TRUE
)





##Total_employee ----

##12-14
elec12_14 <- rwa_event %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )  

table(elec12_14$year)

employee12_14 <- felm(total_employee ~ p0_2014:elec12_14 + p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + sector_year, cluster = "cell_id",  data = elec12_14)

summary(employee12_14)

##15_17
elec15_17 <- rwa_event %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  )


employee15_17 <- felm(total_employee ~ p_2_2011:elec15_17 + p0_2017:elec15_17 + p1_2020:elec15_17|cell_id + sector_year, cluster = "cell_id",  data = elec15_17)



##18_20
elec18_20 <- rwa_event %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  )

employee18_20 <- felm(total_employee ~ p_3_2011:elec18_20 + p_2_2014:elec18_20 + p0_2020:elec18_20|cell_id + sector_year, cluster = "cell_id",  data = elec18_20)



employee <- list(
  `For 2014` = employee12_14,
  `For 2017` = employee15_17,
  `For 2020` = employee18_20
)



stargazer(
  employee,
  title = "Event Study Number of Total employee as outcome",
  stars = TRUE
)



##Log Total_employee ----

##12-14
elec12_14 <- rwa_event %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )   %>% 
  filter(total_employee != 0) %>% 
  mutate(
    log_total_employee = log(total_employee)
  )

table(elec12_14$year)

employee12_14 <- felm(log_total_employee ~ p0_2014:elec12_14 + p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + sector_year, cluster = "cell_id",  data = elec12_14)

summary(employee12_14)

##15_17
elec15_17 <- rwa_event %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  ) %>% 
  filter(total_employee != 0) %>% 
  mutate(
    log_total_employee = log(total_employee)
  )


employee15_17 <- felm(log_total_employee ~ p_2_2011:elec15_17 + p0_2017:elec15_17 + p1_2020:elec15_17|cell_id + sector_year, cluster = "cell_id",  data = elec15_17)



##18_20
elec18_20 <- rwa_event %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  ) %>% 
  filter(total_employee != 0) %>% 
  mutate(
    log_total_employee = log(total_employee)
  )

employee18_20 <- felm(log_total_employee ~ p_3_2011:elec18_20 + p_2_2014:elec18_20 + p0_2020:elec18_20|cell_id + sector_year, cluster = "cell_id",  data = elec18_20)


summary(employee18_20)
employee <- list(
  `For 2014` = employee12_14,
  `For 2017` = employee15_17,
  `For 2020` = employee18_20
)



stargazer(
  employee,
  title = "Event Study Log Number of Total employee as outcome",
  stars = TRUE
)