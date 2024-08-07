##########################
#Author: Xiaoming Zhang
#Date of last modification: 07223024
#purpose:Tax data incorporation
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
tax_data <- readRDS(file.path(data_path, "cit-cell-panel-12-22.rds"))

tax_data <- tax_data %>% 
  mutate(cell_id = as.character(cell_id))

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
    num_establishment = ifelse( year!= 2011 & year != 2014 & year != 2017 & year != 2020, NA, num_establishment),
    total_employee = ifelse(year!= 2011 & year != 2014 & year != 2017 & year != 2020, NA, total_employee),
    employed_capital_1 = ifelse(year!= 2011 & year != 2014 & year != 2017 & year != 2020, NA, employed_capital_1),
    employed_capital_2 = ifelse(year!= 2011 & year != 2014 & year != 2017 & year != 2020, NA, employed_capital_2),
    employed_capital_3 = ifelse(year!= 2011 & year != 2014 & year != 2017 & year != 2020, NA, employed_capital_3),
    employed_capital_4 = ifelse(year!= 2011 & year != 2014 & year != 2017 & year != 2020, NA, employed_capital_4)
  ) %>%
  clean_names()

#Join data----

rwa_regress <- full_join(rwa_regress, tax_data, by = c("cell_id", "year"))

#What is this?
# rwa_long <- rwa_long %>% 
#   complete(village_id, year, fill = list(n = 0, total_employee = 0, total_manager = 0, total_owner = 0)) %>% 
#   mutate(year = 2011) %>% 
#   mutate(village_id = as.character(village_id))
# 

#Descriptive statistics----

summary_stats <- rwa_regress %>%
  group_by(year) %>%
  summarize(
    turnover_mean = mean(turnover, na.rm = TRUE),
    turnover95 = mean(turnover95, na.rm = TRUE),
    purchases_mean = mean(purchases, na.rm = TRUE),
    purchases95 = mean(purchases95, na.rm = TRUE),
    tax_due_mean = mean(tax_due, na.rm = TRUE),
    tax_due95 = mean(tax_due95, na.rm = TRUE),
    n_firms = sum(n_firms, na.rm = TRUE),
    n_real_regime_firms = sum(n_real_regime_firms, na.rm = TRUE),
    n_act_firms = sum(n_act_firms, na.rm = TRUE)
  ) %>% 
  filter(year != 2010 & year !=2011)


# Create and format LaTeX table with kableExtra
latex_table <- summary_stats %>%
  kbl(booktabs = TRUE, format = "latex", caption = "Summary Statistics for 2012-2022") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

# Print the LaTeX code
print(latex_table)


#Balance Table----

#2014----



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
  filter(year == 2014) %>% 
  filter(!district %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu") )

rwa_bt_selec <- rwa_bt %>% 
  select(
    cell_id, status, usage, num_establishment, total_employee, n_firms, turnover, turnover95, purchases, purchases95, tax_due, tax_due95
  )


rwa_bt_selec <- rwa_bt_selec %>% 
  mutate(
    cell_id = as.numeric(cell_id)
  )


write_dta(rwa_bt_selec, path = file.path(data_path, "Establishment census", "balance_table_wtax.dta"))




#Diff in diff----


rwa_did <- rwa_regress %>% 
  mutate(
    electrified = case_when(
      year_first > year ~ 0, 
      year_first <= year ~ 1
    )
  ) %>% 
  mutate(electrified = ifelse(is.na(electrified), 0, electrified))

##Difference in differences


rwa_tax <- rwa_did %>% 
  filter(year_first > 2010) %>% 
  filter(year > 2011) %>% 
  mutate(
    n_firms = ifelse(is.na(n_firms), 0, n_firms),
    turnover = ifelse(is.na(turnover), 0, turnover),
    turnover95 = ifelse(is.na(turnover95), 0, turnover95),
    purchases = ifelse(is.na(purchases), 0, purchases),
    purchases95 = ifelse(is.na(purchases95), 0, purchases95),
    tax_due = ifelse(is.na(tax_due), 0, tax_due),
    tax_due95 = ifelse(is.na(tax_due95), 0, tax_due95)
  ) 

###NTL Value

ntl_model <- felm( value ~  electrified  | cell_id + year, cluster = "cell_id", data = rwa_tax )

summary(ntl_model)

###n_firms

n_firms_model <- felm( n_firms ~  electrified  | cell_id + year, cluster = "cell_id", data = rwa_tax )

summary(n_firms_model)

###turnover


turnover_model <- felm( turnover ~  electrified  | cell_id + year, cluster = "cell_id", data = rwa_tax )

summary(turnover_model)


###turnover95

turnover95_model <-  felm( turnover95 ~  electrified  | cell_id + year, cluster = "cell_id", data = rwa_tax )

summary(turnover95_model)



###purchases

purchases_model <-  felm( purchases ~  electrified  | cell_id + year, cluster = "cell_id", data = rwa_tax )

summary(purchases_model)


###purchases95


purchases95_model <-  felm( purchases95 ~  electrified  | cell_id + year, cluster = "cell_id", data = rwa_tax )

summary(purchases95_model)


###tax_due

tax_due_model <-  felm( tax_due ~  electrified  | cell_id + year, cluster = "cell_id", data = rwa_tax )

summary(tax_due_model)

###tax_due95

tax_due95_model <-  felm( tax_due95 ~  electrified  | cell_id + year, cluster = "cell_id", data = rwa_tax )

summary(tax_due95_model)




village_list <- list(
  `nightlight` = ntl_model,
  `n_firms` = n_firms_model,
  `turnover` = turnover_model,
  `turnover95` = turnover95_model,
  `purchases` = purchases_model,
  `purchases95` = purchases95_model,
  `tax_due` = tax_due_model,
  `tax_due95` = tax_due95_model
)





stargazer(
  village_list,
  title = "Years 2012-2021",
  stars = TRUE,
  note = "Exclude villages that are electrified before 2011, Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)








#Event study----






