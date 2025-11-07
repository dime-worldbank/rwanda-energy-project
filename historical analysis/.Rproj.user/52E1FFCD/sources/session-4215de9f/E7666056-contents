##########################
#Author: Xiaoming Zhang
#Date of last modification: 07223024
#purpose:Tax data incorporation
############################


#library----

pacman::p_load(fixest, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, plm, haven, stringr, modelsummary, kableExtra, stargazer, lfe, ggfixest, install = TRUE)


#read file----

if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX, "Rwanda Energy/EAQIP/datawork/Historical data"
)



#Dataset preparation----
tax_data <- readRDS(file.path(data_path, "cit-cell-panel-12-22 (1).rds"))

tax_data <- tax_data %>% 
  mutate(cell_id = as.character(cell_id)) %>% 
  filter(!is.na(cell_id))
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
  clean_names() %>% 
  filter(!district %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu"))  %>% 
  mutate(
    distr_year = paste0(distr_id, "-", year)
  ) %>% 
  ungroup()




##Join data----

rwa_regress <- full_join(rwa_regress, tax_data, by = c("cell_id", "year"))

write_xlsx(rwa_regress, path = file.path(data_path, "rwa_regress_wtax.xlsx"))
# rwa_regress <- rwa_regress %>%
#   complete(village_id, year, fill = list(n = 0, total_employee = 0, total_manager = 0, total_owner = 0)) %>%
# 

#Descriptive statistics----

summary_stats <- rwa_regress %>%
  group_by(year) %>%
  mutate(
    turnover = turnover / 1000000,
    turnover95 = turnover95 / 1000000,
    purchases = purchases / 1000000,
    purchases95 = purchases95 / 1000000,
    tax_due = tax_due / 1000000,
    tax_due95 = tax_due95 / 1000000
  ) %>% 
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

table(rwa_regress$year_first)



#Output cell data----

cell_elec <- rwa_regress %>% 
  select(cell_id, year_first) %>% 
  mutate(
    year_first = case_when(
      year_first == 1900 ~ 2010,
      year_first == 2300 ~ NA,
      .default = year_first
    )
  )


cell_elec <- cell_elec %>% 
  distinct(cell_id, .keep_all = TRUE)

rwa_cell <- st_read(dsn = file.path(data_path, "rwa_cell", "Cell.shp"))

rwa_cell <- rwa_cell %>% 
  st_drop_geometry() %>% 
  select(Province,District, Sector, Name, Cell_ID) %>% 
  clean_names() %>% 
  mutate(
    cell_id = as.character(cell_id)
  )

cell_elec <- left_join(cell_elec, rwa_cell, by = c("cell_id"))

cell_elec <- cell_elec %>% 
  select(cell_id, province, district, sector, name, year_first) 

cell_elec <- cell_elec %>% 
  mutate(
    year_first = as.character(year_first),
    year_first = case_when(
      year_first == 2010 ~ "On or before 2010",
      is.na(year_first) ~ "Not yet electrified",
      .default = year_first
    )
  )



write_xlsx(cell_elec, path = file.path(data_path, "cell electrification year.xlsx"))
#Balance Table----

#2012

View(rwa_regress)

rwa_bt <- rwa_regress %>% 
  ungroup() %>% 
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
  filter(year == 2012) %>% 
  filter(!district %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu")) %>% 
  mutate(
    turnover = turnover / 1000000,
    turnover95 = turnover95 / 1000000,
    purchases = purchases / 1000000,
    purchases95 = purchases95 / 1000000,
    tax_due = tax_due / 1000000,
    tax_due95 = tax_due95 / 1000000
  ) %>% 
  mutate(
    purchaes = as.numeric(purchases),
    purchases95 = as.numeric(purchases95)
  )  %>% 
  complete(
    cell_id, year, fill = list (n_firms= 0, turnover = 0, turnover95 = 0, purchases = 0, purchaes95 = 0, tax_due = 0, tax_due95 = 0)
  ) %>% 
  mutate(purchases95 = ifelse(is.na(purchases95), 0, purchases95))
    
rwa_bt <- rwa_bt %>% 
  mutate(
    n_firms_1 = ifelse(n_firms !=0, 1, 0),
    turnover_1 = ifelse(turnover !=0, 1, 0),
    turnover95_1 = ifelse(turnover95 != 0, 1, 0),
    purchases_1 = ifelse(purchases != 0, 1,0),
    purchases95_1 = ifelse(purchases95 != 0, 1, 0),
    tax_due_1 = ifelse(tax_due !=0, 1, 0),
    tax_due95_1 = ifelse(tax_due95 != 0, 1,0)
  )

rwa_bt_selec <- rwa_bt %>% 
  select(
    cell_id, status, n_firms,n_firms_1,
    turnover, turnover_1, 
    turnover95, turnover95_1, 
    purchases, purchases_1, 
    purchases95, purchases95_1, 
    tax_due,tax_due_1,
    tax_due95, tax_due95_1
  ) 

rwa_bt_selec <- rwa_bt_selec %>% 
  mutate(
    cell_id = as.numeric(cell_id)
  ) 
# %>%
#   complete(village_id, year, fill = list(n = 0, total_employee = 0, total_manager = 0, total_owner = 0)) %>%
#   #   mutate(year = 2011) %>%
  #   mutate(village_id = as.character(village_id))
  #


write_dta(rwa_bt_selec, path = file.path(data_path, "Establishment census", "balance_table_wtax.dta"))




#2011



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
  filter(year == 2011) 

rwa_bt_selec <- rwa_bt %>% 
  select(
    cell_id, status, num_establishment, total_employee
  ) %>% 
  mutate(
    num_establishment_1 = ifelse(num_establishment != 0, 1, 0),
    total_employee_1 = ifelse(total_employee != 0, 1, 0)
  )


rwa_bt_selec <- rwa_bt_selec %>% 
  mutate(
    cell_id = as.numeric(cell_id)
  )


write_dta(rwa_bt_selec, path = file.path(data_path, "Establishment census", "balance_table_2011ec.dta"))


##plot-----

# Load necessary libraries
library(tibble)

# Create the data frame
balance_table <- tribble(
  ~Variable, ~always_elec_Mean, ~always_elec_SD, ~elec12_14_Mean, ~elec12_14_SD, 
  ~elec15_17_Mean, ~elec15_17_SD, ~elec18_20_Mean, ~elec18_20_SD, 
  ~elec21_22_Mean, ~elec21_22_SD, ~never_elec_Mean, ~never_elec_SD,
  
  # Row 1 (value)
  "value", 8.748, 49.795, 0.448, 4.194, 0.192, 1.416, 1.573, 16.287, 
  0.755, 6.532, 0.613, 7.774,
  
  # Row 2 (num_establishment)
  "num_establishment", 19.346, 83.784, 5.869, 12.358, 4.335, 7.016, 
  5.684, 19.833, 3.636, 7.119, 4.539, 14.439,
  
  # Row 3 (total_employee)
  "total_employee", 47.214, 246.258, 10.805, 51.010, 9.605, 71.903, 
  10.011, 47.631, 5.790, 17.283, 7.798, 41.006,
  
  # Row 4 (employed_capital_1)
  "employed_capital_1", 13.133, 55.770, 4.378, 9.859, 3.347, 5.780, 
  4.205, 14.833, 2.715, 5.650, 3.484, 10.174,
  
  # Row 5 (employed_capital_2)
  "employed_capital_2", 4.551, 25.168, 1.054, 2.712, 0.739, 1.691, 
  1.072, 4.238, 0.706, 1.981, 0.798, 4.019,
  
  # Row 6 (employed_capital_3)
  "employed_capital_3", 0.403, 4.330, 0.051, 0.263, 0.021, 0.157, 
  0.062, 0.425, 0.032, 0.181, 0.034, 0.279,
  
  # Row 7 (employed_capital_4)
  "employed_capital_4", 0.234, 2.029, 0.016, 0.156, 0.013, 0.133, 
  0.026, 0.260, 0.009, 0.097, 0.016, 0.216
)

# Print the data frame to view
print(balance_table)

library(tidyr)
library(dplyr)

# Reshape the data to long format
balance_table_long <- balance_table_plot %>%
  pivot_longer(cols = c(elec12_14, elec15_17, elec18_20, elec21_22, never_elec),
               names_to = "Electrification_Status",
               values_to = "Mean_Value")

balance_table_plot <- balance_table %>% 
  filter(Variable != "value") %>% 
  select(Variable, ends_with("_Mean")) %>% 
  rename(
    elec12_14 = elec12_14_Mean,
    elec15_17 = elec15_17_Mean,
    elec18_20 = elec18_20_Mean,
    elec21_22 = elec21_22_Mean,
    never_elec = never_elec_Mean
  ) %>% 
  select(Variable, elec12_14, elec15_17, elec18_20, elec21_22, never_elec)

balance_table_long <- balance_table_plot %>% 
  pivot_longer(cols = -Variable, names_to = "Electrification_Status", values_to = "Mean_Value")

# View the long-format data
print(balance_table_long)



ggplot(balance_table_long, aes(x = Electrification_Status, y = Mean_Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Mean Value of Variables Across Electrification Statuses",
    x = "Electrification Status",
    y = "Mean Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  )













#Diff in diff----

View(rwa_did)
rwa_did <- rwa_regress %>% 
  mutate(
    electrified = case_when(
      year_first > year ~ 0, 
      year_first <= year ~ 1
    )
  ) %>% 
  # mutate(electrified = ifelse(is.na(electrified), 0, electrified)) %>% 
  filter(!district %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu")) %>% 
  mutate(
    turnover = turnover / 1000000,
    turnover95 = turnover95 / 10000000,
    purchases = purchases / 1000000,
    purchases95 = purchases95 / 1000000,
    tax_due = tax_due / 1000000,
    tax_due95 = tax_due95 / 1000000
  )

##Difference in differences


rwa_tax <- rwa_did %>% 
  filter(year_first > 2010) %>% 
  filter(year >= 2012)
  # mutate(across(c(n_firms, turnover, turnover95, purchases, purchases95, tax_due, tax_due95), 
  #               ~ replace_na(., 0)))



###NTL Value

ntl_model <- felm( value ~  electrified  | cell_id + year, cluster = "cell_id", data = rwa_tax )

summary(ntl_model)

###n_firms

n_firms_model <- feglm( n_firms ~  electrified  | cell_id + year, cluster = "cell_id", data = rwa_tax , family = 'poisson')

summary(n_firms_model)

###turnover


turnover_model <- felm( turnover ~  electrified  | cell_id + year, cluster = "cell_id", data = rwa_tax )

summary(turnover_model)


###turnover95

turnover95_model <-  felm( log(turnover95 +1) ~  electrified  | cell_id + year, cluster = "cell_id", data = rwa_tax )

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


#Log----


rwa_tax_log <- rwa_tax_log %>% 
  filter(year_first > 2010) %>% 
  filter(year >= 2012)%>%
  mutate(
    n_firms_log = log(n_firms),
    turnover_log = log(turnover),
    turnover95_log = log(turnover95),
    purchases_log = log(purchases),
    purchases95_log = log(purchases95),
    tax_due_log = log(tax_due),
    tax_due95_log = log(tax_due95)
  ) %>% 
  mutate(
    n_firms_log = ifelse(n_firms_log == -Inf, NA, n_firms_log),
    turnover_log = ifelse(turnover_log == -Inf, NA, turnover_log),
    turnover95_log = ifelse(turnover95_log == -Inf, NA, turnover95_log),
    purchases_log = ifelse(purchases_log == -Inf, NA, purchases_log),
    purchases95_log = ifelse(purchases95_log == -Inf, NA, purchases95_log),
    tax_due_log = ifelse(tax_due_log == -Inf, NA, tax_due_log),
    tax_due95_log = ifelse(tax_due95_log == -Inf, NA, tax_due95_log)
  )

##Models

n_firms_model <- felm(n_firms_log ~ electrified | cell_id + year, cluster = "cell_id", data = rwa_tax_log)
summary(n_firms_model)

turnover_model <- felm(turnover_log ~ electrified | cell_id + year, cluster = "cell_id", data = rwa_tax_log)
summary(turnover_model)

turnover95_model <- felm(turnover95_log ~ electrified | cell_id + year, cluster = "cell_id", data = rwa_tax_log)
summary(turnover95_model)

purchases_model <- felm(purchases_log ~ electrified | cell_id + year, cluster = "cell_id", data = rwa_tax_log)
summary(purchases_model)

purchases95_model <- felm(purchases95_log ~ electrified | cell_id + year, cluster = "cell_id", data = rwa_tax_log)
summary(purchases95_model)

tax_due_model <- felm(tax_due_log ~ electrified | cell_id + year, cluster = "cell_id", data = rwa_tax_log)
summary(tax_due_model)

tax_due95_model <- felm(tax_due95_log ~ electrified | cell_id + year, cluster = "cell_id", data = rwa_tax_log)
summary(tax_due95_model)




village_list <- list(
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




#Poisson----


rwa_poisson <- rwa_tax %>% 
  mutate(
    electrified = case_when(
      year_first > year ~ 0, 
      year_first <= year ~ 1
    )
  ) %>% 
  filter(year_first > 2010) %>% 
  filter(year >= 2012)%>%
  filter(!is.na(n_firms)) %>% 
  mutate(electrified = ifelse(is.na(electrified), 0, electrified)) %>% 
  filter(!district %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu")) %>% 
  mutate(
    turnover = turnover / 1000000,
    turnover95 = turnover95 / 10000000,
    purchases = purchases / 1000000,
    purchases95 = purchases95 / 1000000,
    tax_due = tax_due / 1000000,
    tax_due95 = tax_due95 / 1000000
  ) %>% 
  mutate(
    tax_due = ifelse(tax_due <0, 0, tax_due),
    tax_due95 = ifelse(tax_due95 <0, 0, tax_due)
  )


#Poisson


n_firms_model <- feglm(n_firms ~ electrified | year,  data = rwa_poisson, family =  'poisson')
summary(n_firms_model)

turnover_model <- feglm(turnover ~ electrified | year,  data = rwa_poisson,  family =  'poisson')
summary(turnover_model)

turnover95_model <- feglm(turnover95 ~ electrified | year,  data = rwa_poisson,  family =  'poisson')
summary(turnover95_model)

purchases_model <- feglm(purchases ~ electrified | year,  data = rwa_poisson,  family =  'poisson')
summary(purchases_model)

purchases95_model <- feglm(purchases95 ~ electrified | year,  data = rwa_poisson, family =  'poisson')
summary(purchases95_model)

tax_due_model <- feglm(tax_due ~ electrified | year,  data = rwa_poisson, family =  'poisson')
summary(tax_due_model)

tax_due95_model <- feglm(tax_due95 ~ electrified | year, data = rwa_poisson,  family =  'poisson')
summary(tax_due95_model)


# Create a list of the models
models <- list(n_firms_model, 
               turnover_model,
               turnover95_model,
               purchases_model,
               purchases95_model,
               tax_due_model,
               tax_due95_model)

# Output the table to LaTeX format
etable(models, tex = TRUE, title = "Poisson Regression Results", digits = 3)











#Event study----

#minus 8~8-----

rwa_event <- rwa_regress %>% 
  filter(year >= 2012 ) %>% 
  mutate(
    electrified = case_when(
      year_first > year ~ 0, 
      year_first <= year ~ 1
  )) %>% 
  mutate(
    n_firms_1 = ifelse(n_firms !=0, 1, 0),
    turnover_1 = ifelse(turnover !=0, 1, 0),
    turnover95_1 = ifelse(turnover95 != 0, 1, 0),
    purchases_1 = ifelse(purchases != 0, 1,0),
    purchases95_1 = ifelse(purchases95 != 0, 1, 0),
    tax_due_1 = ifelse(tax_due !=0, 1, 0),
    tax_due95_1 = ifelse(tax_due95 != 0, 1,0)
  ) %>% 
  mutate(
    turnover = turnover / 1000000,
    turnover95 = turnover95 / 10000000,
    purchases = purchases / 1000000,
    purchases95 = purchases95 / 1000000,
    tax_due = tax_due / 1000000,
    tax_due95 = tax_due95 / 1000000
   ) %>% 
  mutate(
    distr_year = paste0(distr_id, "-", year)
  ) %>% 
  mutate(
    n_firms95 = ifelse(n_firms > quantile(n_firms, 0.95, na.rm = TRUE), quantile(n_firms, 0.95, na.rm = TRUE), n_firms)
  )

rwa_event <- rwa_event %>% 
  mutate(
    rel_time = year - year_first
  ) %>% 
  filter(rel_time >= -8 & rel_time <= 8)


##n_firms----

# n_firms <- feols( n_firms~ i(rel_time, -1)|cell_id + year, data = rwa_event)
# summary(n_firms)
# 
# # install.packages("DescTools")
# # library(DescTools)
# 
# n_firms_glm <- feglm(DescTools::Winsorize(n_firms, prob = c(0,.95)) ~ sunab(year_first, year)|cell_id + year, data = rwa_event, family = 'poisson')
# summary(n_firms_glm)



n_firms <- feglm(n_firms~ sunab(year_first, year)|cell_id + year, data = rwa_event, family = 'poisson')
summary(n_firms)

n_firms_dy <- feglm(n_firms~ sunab(year_first, year)|cell_id + distr_year, data = rwa_event, family = 'poisson')
summary(n_firms_dy)

n_firms95 <- feglm(n_firms95~ sunab(year_first, year)|cell_id + year, data = rwa_event, family = 'poisson')
summary(n_firms95)

n_firms95_dy <- feglm(n_firms95 ~ sunab(year_first, year)|cell_id + distr_year, data = rwa_event, family = 'poisson')
summary(n_firms95_dy)



# Create a list of the models
models <- list(n_firms, n_firms_dy, n_firms95, n_firms95_dy)

# Output the table to LaTeX format
etable(models, tex = TRUE, title = "Poisson Regression Results", digits = 3)

###graph----
ggiplot(n_firms) +
  ggtitle("Effect of Electrification on Number of Firms") +  # Add title
  geom_line(aes(group = 1))   # Add line connecting points
 




##turnover----


turnover <- feglm(turnover~ sunab(year_first, year)|cell_id + year, data = rwa_event, family = 'poisson')
summary(turnover)

turnover_dy <- feglm(turnover~ sunab(year_first, year)|cell_id + distr_year, data = rwa_event, family = 'poisson')
summary(turnover_dy)

turnover95 <- feglm(turnover95~ sunab(year_first, year)|cell_id + year, data = rwa_event, family = 'poisson')
summary(turnover95)

turnover95_dy <- feglm(turnover95 ~ sunab(year_first, year)|cell_id + distr_year, data = rwa_event, family = 'poisson')
summary(turnover95_dy)



# Create a list of the models
models <- list(turnover, turnover_dy, turnover95, turnover95_dy)

# Output the table to LaTeX format
etable(models, tex = TRUE, title = "Poisson Regression Results", digits = 3)

###graph----
ggiplot(turnover) +
  ggtitle("Effect of Electrification on Turnover") +  # Add title
  geom_line(aes(group = 1))   # Add line connecting points
  
  


##Purchases----


purchases <- feglm(purchases~ sunab(year_first, year)|cell_id + year, data = rwa_event, family = 'poisson')
summary(purchases)

purchases_dy <- feglm(purchases~ sunab(year_first, year)|cell_id + distr_year, data = rwa_event, family = 'poisson')
summary(purchases_dy)

purchases95 <- feglm(purchases95~ sunab(year_first, year)|cell_id + year, data = rwa_event, family = 'poisson')
summary(purchases95)

purchases95_dy <- feglm(purchases95 ~ sunab(year_first, year)|cell_id + distr_year, data = rwa_event, family = 'poisson')
summary(purchases95_dy)



# Create a list of the models
models <- list(purchases, purchases_dy, purchases95, purchases95_dy)

# Output the table to LaTeX format
etable(models, tex = TRUE, title = "Poisson Regression Results", digits = 3)

###graph----
ggiplot(purchases) +
  ggtitle("Effect of Electrification on Purchases") +  # Add title
  geom_line(aes(group = 1))   # Add line connecting points





##Tax_due----


tax_due <- feglm(pmax(tax_due,0)~ sunab(year_first, year)|cell_id + year, data = rwa_event, family = 'poisson')
summary(tax_due)

tax_due_dy <- feglm(pmax(tax_due,0)~ sunab(year_first, year)|cell_id + distr_year, data = rwa_event, family = 'poisson')
summary(tax_due_dy)

tax_due95 <- feglm(pmax(tax_due95,0)~ sunab(year_first, year)|cell_id + year, data = rwa_event, family = 'poisson')
summary(tax_due95)

tax_due95_dy <- feglm(pmax(tax_due95,0) ~ sunab(year_first, year)|cell_id + distr_year, data = rwa_event, family = 'poisson')
summary(tax_due95_dy)



# Create a list of the models
models <- list(tax_due, tax_due_dy, tax_due95, tax_due95_dy)

# Output the table to LaTeX format
etable(models, tex = TRUE, title = "Poisson Regression Results", digits = 3)

###graph----
ggiplot(tax_due) +
  ggtitle("Effect of Electrification on tax_due") +  # Add title
  geom_line(aes(group = 1))   # Add line connecting points








#09162024----


#event study--------

rwa_regress <- read_xlsx(path = file.path(data_path, "rwa_regress_wtax.xlsx"))

##construction----


rwa_event <- rwa_regress  %>% 
  select(district, distr_id,  sector_id, cell_id,year,year_first, value, n_firms, purchases, purchases95, turnover, turnover95, tax_due, tax_due95) %>% 
  filter(year %in% c(2012:2021)) %>% 
  filter(year_first >= 2011) %>% 
  complete(cell_id, year, fill = list(n_firms = 0, purchases = 0, purchases95 = 0,
                                      turnover = 0, turnover95 = 0, tax_due = 0, tax_due95 = 0)) %>% 
  distinct(cell_id, year, .keep_all = TRUE) %>% 
  mutate(
    year_first = ifelse(year_first == 2300, 0, year_first )
  ) %>% 
  mutate(distr_year = paste0(district, "_", year)) %>% 
  mutate(
    n_firms95 = ifelse(n_firms > quantile(n_firms, 0.95, na.rm = TRUE), quantile(n_firms, 0.95, na.rm = TRUE), n_firms)
  ) %>% 
  mutate(
    n_firms_1 = ifelse(n_firms >0, 1, 0),
    n_firms95_1 = ifelse(n_firms >0, 1, 0),
    purchases_1 = ifelse(purchases >0, 1, 0),
    purchases95_1 = ifelse(purchases95 > 0, 1, 0),
    turnover_1 = ifelse(turnover > 0, 1,0),
    turnover95_1 = ifelse(turnover95 > 0, 1, 0),
    tax_due_1 = ifelse(tax_due != 0, 1, 0),
    tax_due95_1 = ifelse(tax_due95 != 0, 1, 0)
  ) %>% 
  mutate(
    turnover = turnover / 1000000,
    turnover95 = turnover95 / 10000000,
    purchases = purchases / 1000000,
    purchases95 = purchases95 / 1000000,
    tax_due = tax_due / 1000000,
    tax_due95 = tax_due95 / 1000000
  ) 



rwa_event_3 <- rwa_event %>% subset(year_first %in% c(0, 2013:2017)) %>%
  mutate(elec_m3ormore = ifelse(year_first == 0, 0, year - year_first <= -3) %>% as.numeric,
         elec_m2 = ifelse(year_first == 0, 0, year - year_first == -2) %>% as.numeric,
         elec_0 = ifelse(year_first == 0, 0, year - year_first == 0) %>% as.numeric,
         elec_1 = ifelse(year_first == 0, 0, year - year_first == 1) %>% as.numeric,
         elec_2 = ifelse(year_first == 0, 0, year - year_first == 2) %>% as.numeric,
         elec_3ormore = ifelse(year_first == 0, 0, year - year_first >= 3) %>% as.numeric) 


sum(is.na(rwa_event_3$distr_year))


rwa_event_3 %>% 
 count(cell_id) %>% 
  count(n)




#Poisson-----
##n_firms----

n_firms <- feglm(fml = n_firms  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3, family = "poisson")
summary(n_firms)


n_firms95 <- feglm(fml = n_firms95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3, family = "poisson")
summary(n_firms95)


n_firms_distr <- feglm(fml = n_firms  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3, family = "poisson")
summary(n_firms_distr)


n_firms95_distr <- feglm(fml = n_firms95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3, family = "poisson")
summary(n_firms95_distr)

# Create a list of the models
models <- list(n_firms, n_firms95, n_firms_distr, n_firms95_distr)

# Output the table to LaTeX format
etable(models, tex = TRUE, title = "Poisson Regression Results", digits = 3)





##purchases-----


purchases <- feglm(fml = purchases  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3, family = "poisson")
summary(purchases)


purchases95 <- feglm(fml = purchases95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3, family = "poisson")
summary(purchases95)


purchases_distr <- feglm(fml = purchases  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3, family = "poisson")
summary(purchases_distr)


purchases95_distr <- feglm(fml = purchases95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3, family = "poisson")
summary(purchases95_distr)

# Create a list of the models
models <- list(purchases, purchases95, purchases_distr, purchases95_distr)

# Output the table to LaTeX format
etable(models, tex = TRUE, title = "Poisson Regression Results", digits = 3)






##turnover-----


turnover <- feglm(fml = turnover  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3, family = "poisson")
summary(turnover)


turnover95 <- feglm(fml = turnover95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3, family = "poisson")
summary(turnover95)


turnover_distr <- feglm(fml = turnover  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3, family = "poisson")
summary(turnover_distr)


turnover95_distr <- feglm(fml = turnover95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3, family = "poisson")
summary(turnover95_distr)

# Create a list of the models
models <- list(turnover, turnover95, turnover_distr, turnover95_distr)

# Output the table to LaTeX format
etable(models, tex = TRUE, title = "Poisson Regression Results", digits = 3)





##tax_due----

tax_due <- feglm(fml = pmax(tax_due, 0 ) ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3, family = "poisson")
summary(tax_due)

tax_due95 <- feglm(fml = pmax(tax_due95, 0) ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3, family = "poisson")
summary(tax_due95)

tax_due_distr <- feglm(fml = pmax(tax_due, 0 ) ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3, family = "poisson")
summary(tax_due_distr)

tax_due95_distr <- feglm(fml = pmax(tax_due95, 0) ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3, family = "poisson")
summary(tax_due95)




# Create a list of the models
models <- list(tax_due, tax_due95, tax_due_distr, tax_due95_distr)

# Output the table to LaTeX format
etable(models, tex = TRUE, title = "Poisson Regression Results", digits = 3)





























#OLS level-----

##ntl value----



value <- felm(value  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(value)



value_distr <- felm( value  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(value_distr)


# Create a list of the models
models <- list(value, value_distr)


stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)




##n_firms----

n_firms <- felm(n_firms  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(n_firms)


n_firms95 <- felm(n_firms95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(n_firms95)


n_firms_distr <- felm( n_firms  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(n_firms_distr)


n_firms95_distr <- felm(n_firms95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(n_firms95_distr)

# Create a list of the models
models <- list(n_firms, n_firms95, n_firms_distr, n_firms95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)






##turnover-----


turnover <- felm(turnover  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(turnover)


turnover95 <- felm(turnover95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(turnover95)


turnover_distr <- felm( turnover  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(turnover_distr)


turnover95_distr <- felm(turnover95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(turnover95_distr)

# Create a list of the models
models <- list(turnover, turnover95, turnover_distr, turnover95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)

















##purchases-----

purchases <- felm(purchases  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(purchases)


purchases95 <- felm(purchases95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(purchases95)


purchases_distr <- felm( purchases  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(purchases_distr)


purchases95_distr <- felm(purchases95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(purchases95_distr)

# Create a list of the models
models <- list(purchases, purchases95, purchases_distr, purchases95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)




##tax_due----
tax_due <- felm(tax_due  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(tax_due)


tax_due95 <- felm(tax_due95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(tax_due95)


tax_due_distr <- felm( tax_due  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(tax_due_distr)


tax_due95_distr <- felm(tax_due95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(tax_due95_distr)

# Create a list of the models
models <- list(tax_due, tax_due95, tax_due_distr, tax_due95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)












#Any > 0-----

##ntl value----



# value <- felm(value  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
# summary(value)
# 
# 
# 
# value_distr <- felm( value  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
# summary(value_distr)
# 
# 
# # Create a list of the models
# models <- list(value, value_distr)
# 
# 
# stargazer(
#   models,
#   title = "OLS Regression Results ",
#   stars = TRUE,
#   notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
# )
# 
# 


##n_firms----

n_firms <- felm(n_firms_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(n_firms)


n_firms95 <- felm(n_firms95_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(n_firms95)


n_firms_distr <- felm( n_firms_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(n_firms_distr)


n_firms95_distr <- felm(n_firms95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(n_firms95_distr)

# Create a list of the models
models <- list(n_firms, n_firms95, n_firms_distr, n_firms95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)






##turnover-----


turnover <- felm(turnover_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(turnover)


turnover95 <- felm(turnover95_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(turnover95)


turnover_distr <- felm( turnover_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(turnover_distr)


turnover95_distr <- felm(turnover95_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(turnover95_distr)

# Create a list of the models
models <- list(turnover, turnover95, turnover_distr, turnover95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)

















##purchases-----

purchases <- felm(purchases_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(purchases)


purchases95 <- felm(purchases95_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(purchases95)


purchases_distr <- felm(purchases_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(purchases_distr)


purchases95_distr <- felm(purchases95_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(purchases95_distr)

# Create a list of the models
models <- list(purchases, purchases95, purchases_distr, purchases95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)




##tax_due----
tax_due <- felm(tax_due_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(tax_due)


tax_due95 <- felm(tax_due95_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(tax_due95)


tax_due_distr <- felm( tax_due_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(tax_due_distr)


tax_due95_distr <- felm(tax_due95_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(tax_due95_distr)

# Create a list of the models
models <- list(tax_due, tax_due95, tax_due_distr, tax_due95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)





#Log outcome------

##n_firms----

rwa_event_lot <- rwa_event_3 %>% 
  filter(n_firms > 0)

n_firms <- felm(log(n_firms)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_lot)
summary(n_firms)


n_firms95 <- felm(log(n_firms95)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_lot)
summary(n_firms95)


n_firms_distr <- felm( log(n_firms)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_lot)
summary(n_firms_distr)


n_firms95_distr <- felm(log(n_firms95)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_lot)
summary(n_firms95_distr)

# Create a list of the models
models <- list(n_firms, n_firms95, n_firms_distr, n_firms95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)






##turnover-----
rwa_event_lot <- rwa_event_3 %>% 
  filter(turnover > 0)

turnover <- felm(log(turnover)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_lot)
summary(turnover)


turnover95 <- felm(log(turnover95)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_lot)
summary(turnover95)


turnover_distr <- felm( log(turnover)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_lot)
summary(turnover_distr)


turnover95_distr <- felm(log(turnover95)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_lot)
summary(turnover95_distr)

# Create a list of the models
models <- list(turnover, turnover95, turnover_distr, turnover95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)

















##purchases-----

rwa_event_lot <- rwa_event_3 %>% 
  filter(purchases > 0)



purchases <- felm(log(purchases)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_lot)
summary(purchases)


purchases95 <- felm(log(purchases95)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_lot)
summary(purchases95)


purchases_distr <- felm(log(purchases)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_lot)
summary(purchases_distr)


purchases95_distr <- felm(log(purchases95)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_lot)
summary(purchases95_distr)

# Create a list of the models
models <- list(purchases, purchases95, purchases_distr, purchases95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)







##tax_due----
rwa_event_lot <- rwa_event_3 %>% 
  filter(tax_due > 0)



tax_due <- felm(log(tax_due)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_lot)
summary(tax_due)


tax_due95 <- felm(log(tax_due95)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_lot)
summary(tax_due95)


tax_due_distr <- felm( log(tax_due)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_lot)
summary(tax_due_distr)


tax_due95_distr <- felm(log(tax_due95)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_lot)
summary(tax_due95_distr)

# Create a list of the models
models <- list(tax_due, tax_due95, tax_due_distr, tax_due95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)






#09242024----

rwa_regress <- read_xlsx(path = file.path(data_path, "rwa_regress_wtax.xlsx"))


village_2011 <- read_xlsx(path = file.path(data_path, "Establishment census", "village_level_2011.xlsx"))
village_2014 <- read_xlsx(path = file.path(data_path, "Establishment census", "village_level_2014.xlsx"))
village_2017 <- read_xlsx(path = file.path(data_path, "Establishment census", "village_level_2017.xlsx"))
village_2020 <- read_xlsx(path = file.path(data_path, "Establishment census", "village_level_2020.xlsx"))

rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))
village_join <- rwa_villages %>% 
  st_drop_geometry() %>% 
  clean_names()


key_2011 <-village_2011%>% 
  select(village_id, registered_rra) %>% 
  mutate(
    year = 2011
  )

key_2014 <-village_2014%>% 
  select(village_id,registered_rra) %>% 
  mutate(
    year = 2014
  )

key_2017 <-village_2017%>% 
  select(village_id, registered_rra) %>% 
  mutate(
    year = 2017
  )

key_2020 <-village_2020%>% 
  select(village_id, registered_rra) %>% 
  mutate(
    year = 2020
  )

key_all <- bind_rows(key_2011, key_2014, key_2017, key_2020) 

key_all <- key_all %>% 
  mutate(village_id = as.character(village_id))

key_all_join <- left_join(key_all, village_join, by = c("village_id")) 


rra_registered <- key_all_join %>% 
  select(village_id, year, registered_rra) %>% 
  group_by(village_id, year) %>% 
  summarise(
    registered_rra = sum(registered_rra, na.rm = TRUE)
  )  %>% 
  ungroup() %>% 
  complete(year, village_id, fill = list(registered_rra = 0)) 


write_xlsx(rra_registered, path = file.path(data_path, "rra_registered.xlsx"))






key_all_join <- key_all_join %>% 
  select(cell_id, year, registered_rra) %>% 
  group_by(cell_id, year) %>% 
  summarise(
    registered_rra = sum(registered_rra, na.rm = TRUE)
  )  %>% 
  ungroup() %>% 
  complete(year, cell_id, fill = list(registered_rra = 0)) 


rwa_regress.1 <- left_join(rwa_regress, key_all_join, by = c("cell_id", "year"))

##Construction----

rwa_event <- rwa_regress.1  %>% 
  select(district, distr_id,  sector_id, cell_id,year,year_first, value, registered_rra, num_establishment, n_firms, n_real_regime_firms, n_act_firms,purchases, purchases95, turnover, turnover95, tax_due, tax_due95) %>% 
  filter(year %in% c(2012:2021)) %>% 
  filter(year_first >= 2011) %>% 
  complete(cell_id, year, fill = list(n_firms = 0, n_real_regime_firms= 0, n_act_firms = 0 , purchases = 0, purchases95 = 0,
                                      turnover = 0, turnover95 = 0, tax_due = 0, tax_due95 = 0)) %>% 
  distinct(cell_id, year, .keep_all = TRUE) %>% 
  mutate(
    year_first = ifelse(year_first == 2300, 0, year_first )
  ) %>% 
  mutate(distr_year = paste0(district, "_", year)) %>% 
  mutate(
    n_firms95 = ifelse(n_firms > quantile(n_firms, 0.95, na.rm = TRUE), quantile(n_firms, 0.95, na.rm = TRUE), n_firms)
  ) %>% 
  mutate(
    n_firms_1 = ifelse(n_firms >0, 1, 0),
    n_firms95_1 = ifelse(n_firms >0, 1, 0),
    real_regime_firms_1 = ifelse(n_real_regime_firms >0, 1, 0),
    act_firms_1 = ifelse(n_act_firms >0, 1, 0),
    purchases_1 = ifelse(purchases >0, 1, 0),
    purchases95_1 = ifelse(purchases95 > 0, 1, 0),
    turnover_1 = ifelse(turnover > 0, 1,0),
    turnover95_1 = ifelse(turnover95 > 0, 1, 0),
    tax_due_1 = ifelse(tax_due != 0, 1, 0),
    tax_due95_1 = ifelse(tax_due95 != 0, 1, 0),
  ) %>% 
  mutate(
    turnover = turnover / 1000000,
    turnover95 = turnover95 / 10000000,
    purchases = purchases / 1000000,
    purchases95 = purchases95 / 1000000,
    tax_due = tax_due / 1000000,
    tax_due95 = tax_due95 / 1000000
  ) 

rwa_event <- rwa_event %>% 
  mutate(
    status = case_when(
      year_first < 2012 ~ "always_elec",
      year_first <= 2014 & year_first >= 2012 ~ "elec12_14",
      year_first <= 2017 & year_first >= 2015 ~ "elec15_17",
      year_first <= 2020 & year_first >= 2018 ~ "elec18_20",
      year_first <= 2022 & year_first >= 2021 ~ "elec21_22",
      year_first == 0 | is.na(year_first) ~ "never_elec"
    )) %>% 
  mutate(
    always_elec = ifelse(year_first <= 2011, 1, 0),
    elec12_14 = ifelse( year_first <= 2014 & year_first >= 2012, 1, 0),
    elec15_17 = ifelse(year_first <= 2017 & year_first >= 2015 , 1, 0),
    elec18_20 = ifelse( year_first <= 2020 & year_first >= 2018, 1,0),
    elec21_22 = ifelse(year_first <= 2022 & year_first >= 2021, 1,0),
    never_elec = ifelse(year_first == 0 | is.na(year_first), 1, 0)
  ) 
  

rwa_event_3 <- rwa_event %>% subset(year_first %in% c(0, 2013:2017)) %>%
  mutate(elec_m3ormore = ifelse(year_first == 0, 0, year - year_first <= -3) %>% as.numeric,
         elec_m2 = ifelse(year_first == 0, 0, year - year_first == -2) %>% as.numeric,
         elec_0 = ifelse(year_first == 0, 0, year - year_first == 0) %>% as.numeric,
         elec_1 = ifelse(year_first == 0, 0, year - year_first == 1) %>% as.numeric,
         elec_2 = ifelse(year_first == 0, 0, year - year_first == 2) %>% as.numeric,
         elec_3ormore = ifelse(year_first == 0, 0, year - year_first >= 3) %>% as.numeric) 


sum(is.na(rwa_event_3$distr_year))


rwa_event_3 %>%
  count(cell_id) %>%
  count(n)


#Comparing ec to rra----
rwa_event.1 <- rwa_event %>% 
  filter(year %in% c( "2014", "2017", "2020")) %>% 
  mutate(
    distr_year = paste0(district, "_", year)
  ) %>% 
  mutate(
      `rra_ec_firm` = round(n_firms/num_establishment, 2),
      rra_ec_firm = ifelse(is.na(rra_ec_firm), 0, rra_ec_firm)
  )





#12_14
elec12_14 <- rwa_event.1%>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )  

table(elec12_14$year)

num_establishment12_14 <- felm(num_establishment ~  p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + year, cluster = "cell_id", data = elec12_14)
summary(num_establishment12_14)

ec_rra12_14 <- felm(registered_rra ~  p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + year, cluster = "cell_id", data = elec12_14)
summary(ec_rra12_14)

firms12_14 <- felm(n_firms ~ p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + year, cluster = "cell_id", data = elec12_14)
summary(firms12_14)

real12_14 <- felm(n_real_regime_firms ~ p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + year, cluster = "cell_id", data = elec12_14)
summary(real12_14)

act12_14 <- felm(n_act_firms ~ p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + year, cluster = "cell_id", data = elec12_14)
summary(act12_14)





electrified12_14 <- list(
  `num_establishment` = num_establishment12_14,
  `registered_rra(ec)` = ec_rra12_14,
  `n_firms(rra)` = firms12_14,
  `real_regime_firms(rra)` = real12_14,
  `act_firms(rra)` = act12_14
)


stargazer(
  electrified12_14,
  title = "Event Study Number of Firms",
  stars = TRUE,
  add.lines = list(
    c("Mean of Dependent Variable", 
      round(mean(elec12_14$num_establishment), 2), 
      round(mean(elec12_14$registered_rra), 2), 
      round(mean(elec12_14$n_firms), 2), 
      round(mean(elec12_14$n_real_regime_firms), 2), 
      round(mean(elec12_14$n_act_firms), 2)
  ))
  )
 



#District_year

num_establishment12_14 <- felm(num_establishment ~  p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + distr_year, cluster = "cell_id", data = elec12_14)
summary(num_establishment12_14)

ec_rra12_14 <- felm(registered_rra ~  p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + distr_year, cluster = "cell_id", data = elec12_14)
summary(ec_rra12_14)

firms12_14 <- felm(n_firms ~ p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + distr_year, cluster = "cell_id", data = elec12_14)
summary(firms12_14)

real12_14 <- felm(n_real_regime_firms ~ p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + distr_year, cluster = "cell_id", data = elec12_14)
summary(real12_14)

act12_14 <- felm(n_act_firms ~ p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + distr_year, cluster = "cell_id", data = elec12_14)
summary(act12_14)



electrified12_14 <- list(
  `num_establishment` = num_establishment12_14,
  `registered_rra(ec)` = ec_rra12_14,
  `n_firms(rra)` = firms12_14,
  `real_regime_firms(rra)` = real12_14,
  `act_firms(rra)` = act12_14
)

stargazer(
  electrified12_14,
  title = "Event Study Number of Firms",
  stars = TRUE,
  add.lines = list(
    c("Mean of Dependent Variable", 
      round(mean(elec12_14$num_establishment), 2),
      round(mean(elec12_14$registered_rra), 2), 
      round(mean(elec12_14$n_firms), 2), 
      round(mean(elec12_14$n_real_regime_firms), 2), 
      round(mean(elec12_14$n_act_firms), 2)
    ))
)
  


#15_17
elec15_17 <- rwa_event.1%>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_1_2014 = ifelse(year == 2014 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  )

table(elec15_17$year)

num_establishment15_17<- felm(num_establishment ~  p_1_2014:elec15_17 + p1_2020:elec15_17|cell_id + year, cluster = "cell_id", data = elec15_17)
summary(num_establishment15_17)

ec_rra15_17 <- felm(registered_rra ~  p_1_2014:elec15_17 + p1_2020:elec15_17|cell_id + year, cluster = "cell_id", data = elec15_17)
summary(ec_rra15_17)

firms15_17 <- felm(n_firms ~  p_1_2014:elec15_17 + p1_2020:elec15_17|cell_id + year, cluster = "cell_id", data = elec15_17)
summary(firms15_17)

real15_17 <- felm(n_real_regime_firms ~  p_1_2014:elec15_17 + p1_2020:elec15_17|cell_id + year, cluster = "cell_id", data = elec15_17)
summary(real15_17)

act15_17 <- felm(n_act_firms ~  p_1_2014:elec15_17 + p1_2020:elec15_17|cell_id + year, cluster = "cell_id", data = elec15_17)
summary(act15_17)




electrified15_17 <- list(
  `num_establishment` = num_establishment15_17,
  `registered_rra(ec)` = ec_rra15_17,
  `n_firms(rra)` = firms15_17,
  `real_regime_firms(rra)` = real15_17,
  `act_firms(rra)` = act15_17
)

stargazer(
  electrified15_17,
  title = "Event Study Number of Firms",
  stars = TRUE,
  add.lines = list(
    c("Mean", 
      round(mean(elec15_17$num_establishment), 2), 
      round(mean(elec15_17$registered_rra), 2), 
      round(mean(elec15_17$n_firms), 2), 
      round(mean(elec15_17$n_real_regime_firms), 2), 
      round(mean(elec15_17$n_act_firms), 2)
    ))
)











#15_17
elec15_17 <- rwa_event.1%>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_1_2014 = ifelse(year == 2014 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  )


num_establishment15_17<- felm(num_establishment ~  p_1_2014:elec15_17 + p1_2020:elec15_17|cell_id + distr_year, cluster = "cell_id", data = elec15_17)
summary(num_establishment15_17)


ec_rra15_17 <- felm(registered_rra ~  p_1_2014:elec15_17 + p1_2020:elec15_17|cell_id + distr_year, cluster = "cell_id", data = elec15_17)
summary(ec_rra15_17)

firms15_17 <- felm(n_firms ~  p_1_2014:elec15_17 + p1_2020:elec15_17|cell_id + distr_year, cluster = "cell_id", data = elec15_17)
summary(firms15_17)

real15_17 <- felm(n_real_regime_firms ~  p_1_2014:elec15_17 + p1_2020:elec15_17|cell_id + distr_year, cluster = "cell_id", data = elec15_17)
summary(real15_17)

act15_17 <- felm(n_act_firms ~  p_1_2014:elec15_17 + p1_2020:elec15_17|cell_id + distr_year, cluster = "cell_id", data = elec15_17)
summary(act15_17)


electrified15_17 <- list(
  `num_establishment` = num_establishment15_17,
  `registered_rra(ec)` = ec_rra15_17,
  `n_firms(rra)` = firms15_17,
  `real_regime_firms(rra)` = real15_17,
  `act_firms(rra)` = act15_17
)

stargazer(
  electrified15_17,
  title = "Event Study Number of Firms",
  stars = TRUE,
  add.lines = list(
    c("Mean", 
      round(mean(elec15_17$num_establishment), 2), 
      round(mean(elec15_17$registered_rra), 2), 
      round(mean(elec15_17$n_firms), 2), 
      round(mean(elec15_17$n_real_regime_firms), 2), 
      round(mean(elec15_17$n_act_firms), 2)
    ))
)





#RRA to EC firms fraction----



fraction12_14 <- felm(rra_ec_firm ~  p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + year, cluster = "cell_id", data = elec12_14)
summary(fraction12_14)

fraction12_14_distr <- felm(rra_ec_firm ~  p1_2017:elec12_14 + p2_2020:elec12_14|cell_id + distr_year, cluster = "cell_id", data = elec12_14)
summary(fraction12_14_distr)

fraction15_17 <- felm(rra_ec_firm ~  p_1_2014:elec15_17 + p1_2020:elec15_17|cell_id + year, cluster = "cell_id", data = elec15_17)
summary(fraction15_17)

fraction15_17_distr <- felm(rra_ec_firm ~  p_1_2014:elec15_17 + p1_2020:elec15_17|cell_id + distr_year, cluster = "cell_id", data = elec15_17)
summary(fraction15_17_distr)


rra_to_ec <- list(
 fraction12_14, fraction12_14_distr
)

stargazer(
  rra_to_ec,
  title = "Event Study RRA Number of Firms to EC Number of Establishments",
  stars = TRUE,
  add.lines = list(
    c("Mean", 
      round(mean(elec12_14$rra_ec_firm), 2), 
      round(mean(elec12_14$rra_ec_firm), 2)
    ))
)





rra_to_ec <- list(
  fraction15_17, fraction15_17_distr
)

stargazer(
  rra_to_ec,
  title = "Event Study RRA Number of Firms to EC Number of Establishments",
  stars = TRUE,
  add.lines = list(
    c("Mean", 
      round(mean(elec15_17$rra_ec_firm), 2), 
      round(mean(elec15_17$rra_ec_firm), 2)
    ))
)
















#OLS level-----



##ntl value----

###Construction----

rwa_event_3 <- rwa_event_3 %>%
  mutate(
    ntl_qt = ntile(value, 4)  
  )


value_qt <- felm(ntl_qt  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(value_qt)


value <- felm(value  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(value)


value_distr <- felm( value  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(value_distr)


value_distr_qt <- felm( ntl_qt  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(value_distr_qt)


# Create a list of the models
models <- list(value, value_qt, value_distr, value_distr_qt)


stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)





##n_firms----

n_firms <- felm(n_firms  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(n_firms)


n_firms95 <- felm(n_firms95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(n_firms95)


n_firms_distr <- felm( n_firms  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(n_firms_distr)


n_firms95_distr <- felm(n_firms95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(n_firms95_distr)

# Create a list of the models
models <- list(n_firms, n_firms95, n_firms_distr, n_firms95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu",
  add.lines = list(
    c("Mean", 
      round(mean(rwa_event_3$n_firms), 2), 
      round(mean(rwa_event_3$n_firms95), 2), 
      round(mean(rwa_event_3$n_firms), 2), 
      round(mean(rwa_event_3$n_firms95), 2)
    ))
)



##n_real_firms----

n_real_regime_firms <- felm(n_real_regime_firms  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(n_real_regime_firms)


n_real_regime_firms_distr <- felm( n_real_regime_firms  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(n_real_regime_firms_distr)



# Create a list of the models
models <- list(n_real_regime_firms, n_real_regime_firms_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu",
  add.lines = list(
    c("Mean", 
      round(mean(rwa_event_3$n_real_regime_firms), 2), 
      round(mean(rwa_event_3$n_real_regime_firms), 2)
    ))
)


##n_acting_firms----

n_act_firms <- felm(n_act_firms  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(n_act_firms)


n_act_firms_distr <- felm( n_act_firms  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(n_act_firms_distr)



# Create a list of the models
models <- list(n_act_firms, n_act_firms_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu",
  add.lines = list(
    c("Mean", 
      round(mean(rwa_event_3$n_act_firms), 2), 
      round(mean(rwa_event_3$n_act_firms), 2)
    ))
)


##turnover-purchases----


rwa_event_3 <- rwa_event_3 %>% 
  mutate(
    `turnover_purchases` = turnover - purchases
  ) %>% 
  mutate(
    `turnover_purchases_1` = ifelse(turnover_purchases >0 , 1, 0)
  )

turnover_purchases <- felm(turnover_purchases  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(turnover_purchases)


turnover_purchases_distr <- felm(turnover_purchases  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(turnover_purchases)


# Create a list of the models
models <- list(turnover_purchases, turnover_purchases_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu",
  add.lines = list(
    c("Mean", 
      round(mean(rwa_event_3$turnover_purchases), 2), 
      round(mean(rwa_event_3$turnover_purchases), 2)
    ))
)












##turnover-----


turnover <- felm(turnover  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(turnover)


turnover95 <- felm(turnover95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(turnover95)


turnover_distr <- felm( turnover  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(turnover_distr)


turnover95_distr <- felm(turnover95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(turnover95_distr)

# Create a list of the models
models <- list(turnover, turnover95, turnover_distr, turnover95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu",
  add.lines = list(
    c("Mean", 
      round(mean(rwa_event_3$turnover), 2), 
      round(mean(rwa_event_3$turnover95), 2),
      round(mean(rwa_event_3$turnover), 2), 
      round(mean(rwa_event_3$turnover95), 2)
    ))
)

















##purchases-----

purchases <- felm(purchases  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(purchases)


purchases95 <- felm(purchases95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(purchases95)


purchases_distr <- felm( purchases  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(purchases_distr)


purchases95_distr <- felm(purchases95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(purchases95_distr)

# Create a list of the models
models <- list(purchases, purchases95, purchases_distr, purchases95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu",
  add.lines = list(
    c("Mean", 
      round(mean(rwa_event_3$purchases), 2), 
      round(mean(rwa_event_3$purchases95), 2),
      round(mean(rwa_event_3$purchases), 2), 
      round(mean(rwa_event_3$purchases95), 2)
    ))
)




##tax_due----
tax_due <- felm(tax_due  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(tax_due)


tax_due95 <- felm(tax_due95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(tax_due95)


tax_due_distr <- felm( tax_due  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(tax_due_distr)


tax_due95_distr <- felm(tax_due95  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(tax_due95_distr)

# Create a list of the models
models <- list(tax_due, tax_due95, tax_due_distr, tax_due95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu",
  add.lines = list(
    c("Mean", 
      round(mean(rwa_event_3$tax_due), 2), 
      round(mean(rwa_event_3$tax_due95), 2),
      round(mean(rwa_event_3$tax_due), 2), 
      round(mean(rwa_event_3$tax_due95), 2)
    ))
)












#Any > 0-----



##n_firms----

n_firms <- felm(n_firms_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(n_firms)


n_firms95 <- felm(n_firms95_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(n_firms95)


n_firms_distr <- felm( n_firms_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(n_firms_distr)


n_firms95_distr <- felm(n_firms95_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(n_firms95_distr)

# Create a list of the models
models <- list(n_firms, n_firms95, n_firms_distr, n_firms95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu",
  add.lines = list(
    c("Mean", 
      round(mean(rwa_event_3$n_firms_1), 2), 
      round(mean(rwa_event_3$n_firms95_1), 2), 
      round(mean(rwa_event_3$n_firms_1), 2), 
      round(mean(rwa_event_3$n_firms95_1), 2)
    ))
)





##n_real_firms----

n_real_regime_firms <- felm(real_regime_firms_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(n_real_regime_firms)


n_real_regime_firms_distr <- felm( real_regime_firms_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(n_real_regime_firms_distr)



# Create a list of the models
models <- list(n_real_regime_firms, n_real_regime_firms_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu",
  add.lines = list(
    c("Mean", 
      round(mean(rwa_event_3$real_regime_firms_1), 2), 
      round(mean(rwa_event_3$real_regime_firms_1), 2)
    ))
)


##n_acting_firms----

n_act_firms <- felm(act_firms_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(n_act_firms)


n_act_firms_distr <- felm(act_firms_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(n_act_firms_distr)



# Create a list of the models
models <- list(n_act_firms, n_act_firms_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu",
  add.lines = list(
    c("Mean", 
      round(mean(rwa_event_3$act_firms_1), 2), 
      round(mean(rwa_event_3$act_firms_1), 2)
    ))
)


##turnover-purchases----


turnover_purchases <- felm(turnover_purchases_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(turnover_purchases)


turnover_purchases_distr <- felm(turnover_purchases_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(turnover_purchases)


# Create a list of the models
models <- list(turnover_purchases, turnover_purchases_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu",
  add.lines = list(
    c("Mean", 
      round(mean(rwa_event_3$turnover_purchases_1), 2), 
      round(mean(rwa_event_3$turnover_purchases_1), 2)
    ))
)










##turnover-----


turnover <- felm(turnover_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(turnover)


turnover95 <- felm(turnover95_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(turnover95)


turnover_distr <- felm( turnover_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(turnover_distr)


turnover95_distr <- felm(turnover95_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(turnover95_distr)

# Create a list of the models
models <- list(turnover, turnover95, turnover_distr, turnover95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu",
  add.lines = list(
    c("Mean", 
      round(mean(rwa_event_3$turnover_1), 2), 
      round(mean(rwa_event_3$turnover95_1), 2),
      round(mean(rwa_event_3$turnover_1), 2), 
      round(mean(rwa_event_3$turnover95_1), 2)
    ))
)

















##purchases-----

purchases <- felm(purchases_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(purchases)


purchases95 <- felm(purchases95_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(purchases95)


purchases_distr <- felm(purchases_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(purchases_distr)


purchases95_distr <- felm(purchases95_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(purchases95_distr)

# Create a list of the models
models <- list(purchases, purchases95, purchases_distr, purchases95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu",
  add.lines = list(
    c("Mean", 
      round(mean(rwa_event_3$purchases_1), 2), 
      round(mean(rwa_event_3$purchases95_1), 2),
      round(mean(rwa_event_3$purchases_1), 2), 
      round(mean(rwa_event_3$purchases95_1), 2)
    ))
)




##tax_due----
tax_due <- felm(tax_due_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(tax_due)


tax_due95 <- felm(tax_due95_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(tax_due95)


tax_due_distr <- felm( tax_due_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(tax_due_distr)


tax_due95_distr <- felm(tax_due95_1  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(tax_due95_distr)

# Create a list of the models
models <- list(tax_due, tax_due95, tax_due_distr, tax_due95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu",
  add.lines = list(
    c("Mean", 
      round(mean(rwa_event_3$tax_due_1), 2), 
      round(mean(rwa_event_3$tax_due95_1), 2),
      round(mean(rwa_event_3$tax_due_1), 2), 
      round(mean(rwa_event_3$tax_due95_1), 2)
    ))
)





#Log outcome------

##n_firms----

rwa_event_lot <- rwa_event_3 %>% 
  filter(n_firms > 0)

n_firms <- felm(log(n_firms)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_lot)
summary(n_firms)


n_firms95 <- felm(log(n_firms95)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_lot)
summary(n_firms95)


n_firms_distr <- felm( log(n_firms)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_lot)
summary(n_firms_distr)


n_firms95_distr <- felm(log(n_firms95)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_lot)
summary(n_firms95_distr)

# Create a list of the models
models <- list(n_firms, n_firms95, n_firms_distr, n_firms95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)





##turnover-----
rwa_event_lot <- rwa_event_3 %>% 
  filter(turnover > 0)

turnover <- felm(log(turnover)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_lot)
summary(turnover)


turnover95 <- felm(log(turnover95)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_lot)
summary(turnover95)


turnover_distr <- felm( log(turnover)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_lot)
summary(turnover_distr)


turnover95_distr <- felm(log(turnover95)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_lot)
summary(turnover95_distr)

# Create a list of the models
models <- list(turnover, turnover95, turnover_distr, turnover95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)

















##purchases-----

rwa_event_lot <- rwa_event_3 %>% 
  filter(purchases > 0)



purchases <- felm(log(purchases)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_lot)
summary(purchases)


purchases95 <- felm(log(purchases95)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_lot)
summary(purchases95)


purchases_distr <- felm(log(purchases)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_lot)
summary(purchases_distr)


purchases95_distr <- felm(log(purchases95)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_lot)
summary(purchases95_distr)

# Create a list of the models
models <- list(purchases, purchases95, purchases_distr, purchases95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)







##tax_due----
rwa_event_lot <- rwa_event_3 %>% 
  filter(tax_due > 0)



tax_due <- felm(log(tax_due)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_lot)
summary(tax_due)


tax_due95 <- felm(log(tax_due95)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_lot)
summary(tax_due95)


tax_due_distr <- felm( log(tax_due)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_lot)
summary(tax_due_distr)


tax_due95_distr <- felm(log(tax_due95)  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_lot)
summary(tax_due95_distr)

# Create a list of the models
models <- list(tax_due, tax_due95, tax_due_distr, tax_due95_distr)


# Output the models using stargazer, including fixed effects
stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)






































