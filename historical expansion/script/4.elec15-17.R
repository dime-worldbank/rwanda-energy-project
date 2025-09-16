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

#Read file-----

expansion_join <- read_xlsx(path = file.path(output_path, "expansion_join.xlsx"))

#earp_existing_mv----

earp_existing_mv <- st_read(dsn = file.path(data_path, "shapefiles", "earp_existingMV.shp"))

earp_existing_mv <- st_transform(earp_existing_mv, crs = st_crs(rwa_villages))

earp_existing_mv <- st_intersection(earp_existing_mv, rwa_villages)

earp_existing_mv <- earp_existing_mv %>% 
  distinct(Village_ID) %>% 
  mutate(
    earp_existing_mv = 1
  )


#Sample restriction----

expansion_join_drop15_17 <- expansion_join%>% 
  filter(electrified_year %in% c("2015", "2016", "2017") |electrified_year > 2020) %>% 
  mutate(
    `elec15_17` = ifelse(electrified_year %in% c("2015", "2016", "2017"), 1, 0)
  )%>% 
  anti_join(earp_existing_mv, by = c("village_id" = "Village_ID"))



#Event study without ISIC restriction----



##Establishment census-----

ec_2011 <- read_xlsx(path = file.path(data_path, "2011", "village_level_2011.xlsx"))

ec_2011 <- ec_2011 %>%
  mutate(village_id = as.character(village_id)) %>%
  select(village_id, num_establishment, total_employee) %>%
  mutate(year = 2011)

ec_2014 <- read_xlsx(path = file.path(data_path, "2014", "village_level_2014.xlsx"))

ec_2014 <- ec_2014 %>%
  mutate(village_id = as.character(village_id)) %>%
  select(village_id, num_establishment, total_employee) %>%
  mutate(year = 2014)


ec_2017 <- read_xlsx(path = file.path(data_path, "2017", "village_level_2017.xlsx"))

ec_2017 <- ec_2017 %>%
  mutate(village_id = as.character(village_id)) %>%
  select(village_id, num_establishment, total_employee) %>%
  mutate(year = 2017)

ec_2020 <- read_xlsx(path = file.path(data_path, "2020", "village_level_2020.xlsx"))

ec_2020 <- ec_2020 %>%
  mutate(village_id = as.character(village_id)) %>%
  select(village_id, num_establishment, total_employee) %>%
  mutate(year = 2020)


ec_all <- rbind(ec_2011, ec_2014, ec_2017, ec_2020)



earp_join <- expansion_join_drop15_17 %>%
  select(village_id, elec15_17, cell_id, sector_id, district_id)


earp_did <- left_join( earp_join, ec_all, by = c("village_id"))



earp_did <- earp_did %>%
  complete(year, village_id,
           fill = list(num_establishment = 0, total_employee = 0)) %>%
  filter(!is.na(year)) %>% 
  select(year, village_id, num_establishment, total_employee)


earp <- expansion_join_drop15_17%>%
  select(village_id, elec15_17,  cell_office, health_center, primary_school)

earp_did <- left_join(earp_did, earp, by = c("village_id"))

earp_did <- earp_did %>%
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
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  ) 




mean(earp_did$num_establishment)
mean(earp_did$total_employee)

##Regressions--------

num_establishment <- felm(num_establishment ~ p_1_2011*elec15_17 + p1_2017*elec15_17 + p2_2020*elec15_17|village_id + cell_year + cell_office_year + health_center_year + primary_school_year |0|sector_id, data = earp_did)
summary(num_establishment)

total_employee <- felm (total_employee ~ p_1_2011*elec15_17 + p1_2017*elec15_17 + p2_2020*elec15_17|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did)
summary(total_employee)



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



earp_join <- expansion_join_drop15_17%>% 
  select(village_id,elec15_17, cell_id, sector_id, district_id) 

earp_did_isic<- left_join( earp_join, ec_all, by = c("village_id"))


earp_did_isic <- earp_did_isic %>%
  complete(year, village_id, isic_level1,
           fill = list(num_establishment = 0, total_employee = 0)) %>% 
  filter(!is.na(year))   %>% 
  filter(!is.na(isic_level1)) %>% 
  select(year, village_id, num_establishment, total_employee, isic_level1)


earp <- expansion_join_drop15_17%>%
  select(village_id, elec15_17,  cell_office, health_center, primary_school)


earp_did_isic <- left_join(earp_did_isic, earp, by = c("village_id"))



earp_did_isic <- earp_did_isic %>% 
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





##Utility_join-----
utility_long_join <- utility_long %>% 
  filter(year == 2011 | year == 2014 | year == 2017 | year == 2020) 


earp_did_isic <- left_join(earp_did_isic, utility_long_join, by = c("village_id", "year"))


earp_did_isic <- earp_did_isic %>% 
  mutate(
    usage = ifelse(is.na(usage), 0, usage)
  )


##0 check------
check_zero <- earp_did_isic %>% 
  group_by(isic, year ) %>% 
  summarise(
    n_establishment = sum(num_establishment),
    total_employee = sum(total_employee)
  )

earp_did_nozero<- earp_did_isic %>% 
  filter(
    isic != 12 
  ) %>% 
  filter(
    isic != 15
  )

earp_did_nozero %>% 
  group_by(isic) %>% 
  summarise(
    `mean(total_employee >0)` = mean(total_employee > 0), 
    `mean(total_employee)` = mean(total_employee)
  ) %>% 
  ungroup() %>% 
  arrange(-`mean(total_employee)`)

earp_did_nozero<- earp_did_isic %>% 
  filter(
    isic %in% c(7,9,16,19,3,1,17,2)
  ) %>% 
  mutate(usage = usage / (100 * 365))


#First batch of regression--------
##ISIC 19-----

earp_did_19 <- earp_did_nozero %>% 
  filter(isic == 19)


num_establishment_19 <- felm(num_establishment ~ p_1_2011*elec15_17 + p1_2017*elec15_17 + p2_2020*elec15_17|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_19)
summary(num_establishment_19)

total_employee_19 <- felm (total_employee ~ p_1_2011*elec15_17 + p1_2017*elec15_17 + p2_2020*elec15_17|village_id + cell_year + cell_office_year + health_center_year + primary_school_year |0|sector_id, data = earp_did_19)
summary(total_employee_19)



##ISIC 3-----

earp_did_3 <- earp_did_nozero %>% 
  filter(isic == 3)

mean(earp_did_3$num_establishment)
mean(earp_did_3$total_employee)

num_establishment_3 <- felm(num_establishment ~ p_1_2011*elec15_17 + p1_2017*elec15_17 + p2_2020*elec15_17|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_3)
summary(num_establishment_3)

total_employee_3 <- felm (total_employee ~ p_1_2011*elec15_17 + p1_2017*elec15_17 + p2_2020*elec15_17|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_3)
summary(total_employee_3)


##ISIC 7-----
earp_did_7 <- earp_did_nozero %>% 
  filter(isic == 7)

mean(earp_did_7$num_establishment)
mean(earp_did_7$total_employee)

num_establishment_7 <- felm(num_establishment ~ p_1_2011*elec15_17 + p1_2017*elec15_17 + p2_2020*elec15_17+ p_1_2011*elec15_17 + p1_2017*elec15_17 + p2_2020*elec15_17|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_7)
summary(num_establishment_7)

total_employee_7 <- felm (total_employee ~ p_1_2011*elec15_17 + p1_2017*elec15_17 + p2_2020*elec15_17+ p_1_2011*elec15_17 + p1_2017*elec15_17 + p2_2020*elec15_17|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_7)
summary(total_employee_7)








regs <- list(
  "num_establishment" = num_establishment,
  "total_employee" = total_employee,
  "establishment_isic3" = num_establishment_3,
  "employee_isic_3" = total_employee_3,
  "establishment_isic7" = num_establishment_7,
  "employee_isic_7" = total_employee_7,
  "establishment_isic19" = num_establishment_19,
  "employee_isic_19" = total_employee_19
)

stargazer(
  regs,
  output = file.path(output_path, "event study.tex"),
  title = "Regression Results"
)


# Function to compute mean for a given variable when earp == 0
compute_mean <- function(df, var, earp_var = "elec15_17") {
  earp_col <- gsub("^`|`$", "", earp_var) # remove backticks
  mean(df[[var]][df[[earp_col]] == 0], na.rm = TRUE)
}

# Compute means for each regression sample
mean_vals <- c(
  compute_mean(earp_did,   "num_establishment"),
  compute_mean(earp_did,   "total_employee"),
  compute_mean(earp_did_3, "num_establishment"),
  compute_mean(earp_did_3, "total_employee"),
  compute_mean(earp_did_7, "num_establishment"),
  compute_mean(earp_did_7, "total_employee"),
  compute_mean(earp_did_19,"num_establishment"),
  compute_mean(earp_did_19,"total_employee")
)

# Format as LaTeX line
latex_mean_line <- paste(
  "Mean &",
  paste(sprintf("%.3f", mean_vals), collapse = " & "),
  "\\"
)

print(latex_mean_line)







#Second batch of regression----

##ISIC 16----
earp_did_16 <- earp_did_nozero %>% 
  filter(isic == 16)

mean(earp_did_16$num_establishment)
mean(earp_did_16$total_employee)

num_establishment_16 <- felm(num_establishment ~ p_1_2011*elec15_17 + p1_2017*elec15_17 + p2_2020*elec15_17|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_16)
summary(num_establishment_16)

total_employee_16 <- felm (total_employee ~ p_1_2011*elec15_17 + p1_2017*elec15_17 + p2_2020*elec15_17|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_16)
summary(total_employee_16)


##ISIC 17----
earp_did_17 <- earp_did_nozero %>% 
  filter(isic == 17)

mean(earp_did_17$num_establishment)
mean(earp_did_17$total_employee)

num_establishment_17 <- felm(num_establishment ~ p_1_2011*elec15_17 + p1_2017*elec15_17 + p2_2020*elec15_17|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_17)
summary(num_establishment_17)

total_employee_17 <- felm (total_employee ~ p_1_2011*elec15_17 + p1_2017*elec15_17 + p2_2020*elec15_17|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_17)
summary(total_employee_17)



##ISIC 9----
earp_did_9 <- earp_did_nozero %>% 
  filter(isic == 9)

mean(earp_did_9$num_establishment)
mean(earp_did_9$total_employee)

num_establishment_9 <- felm(num_establishment ~ p_1_2011*elec15_17 + p1_2017*elec15_17 + p2_2020*elec15_17|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_9)
summary(num_establishment_9)

total_employee_9 <- felm (total_employee ~ p_1_2011*elec15_17 + p1_2017*elec15_17 + p2_2020*elec15_17|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_9)
summary(total_employee_9)






regs <- list(
  "num_establishment" = num_establishment,
  "total_employee" = total_employee,
  "establishment_isic16" = num_establishment_16,
  "employee_isic_16" = total_employee_16,
  "establishment_isic117" = num_establishment_17,
  "employee_isic_17" = total_employee_17,
  "establishment_isic9" = num_establishment_9,
  "employee_isic_9" = total_employee_9
)

stargazer(
  regs,
  output = file.path(output_path, "event study.tex"),
  title = "Regression Results"
)


mean_vals2 <- c(
  compute_mean(earp_did,    "num_establishment"),
  compute_mean(earp_did,    "total_employee"),
  compute_mean(earp_did_16, "num_establishment"),
  compute_mean(earp_did_16, "total_employee"),
  compute_mean(earp_did_17, "num_establishment"),
  compute_mean(earp_did_17, "total_employee"),
  compute_mean(earp_did_9,  "num_establishment"),
  compute_mean(earp_did_9,  "total_employee")
)

# Format LaTeX row
latex_mean_line2 <- paste(
  "Mean &",
  paste(sprintf("%.3f", mean_vals2), collapse = " & "),
  "\\\\"
)

cat(latex_mean_line2)




#EARP Public private sector-----

join_drop15_17 <- expansion_join_drop15_17 %>% 
  select(elec15_17, village_id, cell_id, sector_id, cell_office, health_center, primary_school)


earp_private <- earp_did_nozero %>% 
  mutate(
    private_sector = ifelse(isic %in% c(9,7,13,19), 1, 0)
  ) 

earp_p <- earp_private %>% 
  filter(private_sector == 1) %>% 
  group_by(year, village_id, private_sector) %>%
  summarise(
    num_establishment = sum(num_establishment, na.rm = TRUE),
    total_employee = sum(total_employee, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  complete(year, village_id,
           fill = list(num_establishment = 0, total_employee = 0)) %>% 
  mutate(
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  ) %>% 
  left_join(join_drop15_17, by = c("village_id") )  %>% 
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year)
  )

num_establishment_p <- felm(num_establishment ~ p_1_2011*elec15_17 + p1_2017*elec15_17 + p2_2020*elec15_17|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_p)
summary(num_establishment_p)

total_employee_p <- felm (total_employee ~ p_1_2011*elec15_17 + p1_2017*elec15_17 + p2_2020*elec15_17|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_p)
summary(total_employee_p)

earp_pu <- earp_private %>% 
  filter(private_sector == 0) %>% 
  group_by(year, village_id, private_sector) %>%
  summarise(
    num_establishment = sum(num_establishment, na.rm = TRUE),
    total_employee = sum(total_employee, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  complete(year, village_id,
           fill = list(num_establishment = 0, total_employee = 0)) %>% 
  mutate(
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  ) %>% 
  left_join(join_drop15_17, by = c("village_id") )  %>% 
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year)
  )

num_establishment_pu <- felm(num_establishment ~ p_1_2011*elec15_17 + p1_2017*elec15_17 + p2_2020*elec15_17|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_pu)
summary(num_establishment_pu)

total_employee_pu <- felm (total_employee ~ p_1_2011*elec15_17 + p1_2017*elec15_17 + p2_2020*elec15_17|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_pu)
summary(total_employee_pu)



regs <- list(
  "num_establishment" = num_establishment,
  "total_employee" = total_employee,
  "num_establishment_private" = num_establishment_p,
  "total_employee_private" = total_employee_p,
  "num_establishment_public" = num_establishment_pu,
  "total_employee_public" = total_employee_pu
)



stargazer(
  regs,
  output = file.path(output_path, "event study.tex"),
  title = "Regression Results"
)

mean_vals3 <- c(
  compute_mean(earp_did,    "num_establishment"),
  compute_mean(earp_did,    "total_employee"),
  compute_mean(earp_p,    "num_establishment"),
  compute_mean(earp_p,    "total_employee"),
  compute_mean(earp_pu, "num_establishment"),
  compute_mean(earp_pu, "total_employee")
)

# Format LaTeX row
latex_mean_line3 <- paste(
  "Mean &",
  paste(sprintf("%.3f", mean_vals3), collapse = " & "),
  "\\"
)

print(latex_mean_line3)
