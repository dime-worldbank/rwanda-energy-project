##
#Author: Xiaoming Zhang
#Date: 05082025
#Topic: Infrastrucutre, Electrified year by year regression, and event study regressions that are adding the infrastructure/productive users 


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

rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))
rwa_villages <- st_make_valid(rwa_villages)

rwa_district <- st_read(dsn = file.path(data_path, "rwa_district", "District.shp"))
rwa_cell <- st_read(dsn = file.path(data_path, "rwa_cell", "Cell.shp"))


rwa_cell_output <- rwa_cell %>% 
  st_drop_geometry() %>% 
  select(District, Sector, Name) %>% 
  rename(Cell = Name)

write_xlsx(rwa_cell_output, path = file.path(output_path, "Rwanda Cells.xlsx"))
rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))
rwa_villages <- st_make_valid(rwa_villages)


#Earp MV----
earp_planned_mv <- st_read(dsn = file.path(data_path, "shapefiles", "earp_plannedMV.shp"))

earp_planned_mv <- st_transform(earp_planned_mv, crs = st_crs(rwa_villages))


village_mv_earp <- st_intersection(rwa_villages, earp_planned_mv)

village_mv_earp <- village_mv_earp %>%
  mutate(length_m = st_length(.)) %>%
  group_by(Village_ID) %>%
  summarise(
    mv_length = sum(length_m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    earp_mv       = if_else(mv_length > units::set_units(0, "m"), 1, 0),
    earp_mv_300   = if_else(mv_length >= units::set_units(300, "m"), 1, 0)
  ) %>% 
  rename(
    village_id = Village_ID
  ) %>% 
  st_drop_geometry()



#Earp LV----

earp_planned_lv <- st_read(dsn = file.path(data_path, "shapefiles", "earp_plannedLV.shp"))

earp_planned_lv <- st_transform(earp_planned_lv, crs = st_crs(rwa_villages))


village_lv_earp <- st_intersection(rwa_villages, earp_planned_lv)

village_lv_earp <- village_lv_earp %>% 
  mutate(length_m = st_length(.)) %>%
  group_by(Village_ID) %>%
  summarise(
    lv_length = sum(length_m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    earp_lv       = if_else(lv_length > units::set_units(0, "m"), 1, 0),
    earp_lv_300   = if_else(lv_length>= units::set_units(300, "m"), 1, 0)
  ) %>% 
  rename(
    village_id = Village_ID
  ) %>% 
  st_drop_geometry()


#mv_2011----

mv_2011 <- st_read(dsn = file.path( data_path, "Existing Electrical Network_2011", "Existing_MVLine.shp"))

mv_2011 <- st_transform(mv_2011, crs = st_crs(rwa_district))


mv_2011 <- st_intersection(mv_2011, rwa_villages)

mv_2011 <- mv_2011 %>% 
  distinct(Village_ID) %>% 
  mutate(
    mv_2011 = 1
  )

#earp_existing_mv----

earp_existing_mv <- st_read(dsn = file.path(data_path, "shapefiles", "earp_existingMV.shp"))

earp_existing_mv <- st_transform(earp_existing_mv, crs = st_crs(rwa_villages))

earp_existing_mv <- st_intersection(earp_existing_mv, rwa_villages)

earp_existing_mv <- earp_existing_mv %>% 
  distinct(Village_ID) %>% 
  mutate(
    earp_existing_mv = 1
  )

#mv_2022----


mv_2022 <- st_read(dsn = file.path( data_path, "Existing Electrical Network_2022", "Existing_MVLine.shp"))

mv_2022 <- st_transform(mv_2022, crs = st_crs(rwa_villages))

mv_2022 <- st_intersection(mv_2022, rwa_villages)

mv_2022 <- mv_2022 %>% 
  distinct(Village_ID) %>% 
  mutate(
    mv_2022 = 1
  )



#lv_2022----



lv_2022 <- st_read(dsn = file.path( data_path, "Existing Electrical Network_2022", "Existing_LVLine.shp"))

lv_2022 <- st_transform(lv_2022, crs = st_crs(rwa_villages))

lv_2022 <- st_intersection(lv_2022, rwa_villages)

lv_2022 <- lv_2022 %>% 
  distinct(Village_ID) %>% 
  mutate(
    lv_2022 = 1
  )

#Infrastructure:Cell Office----

infra <- st_read(dsn = file.path(data_path, "shapefiles", "infrastructure.shp"))

infra <- st_transform(infra , crs = st_crs(rwa_villages))


##Cell_office----

cell_office_sf <- infra %>% 
  filter(Infra_Type == "Cell_Office")

cell_office <- st_intersection(cell_office_sf, rwa_villages)

cell_office <- cell_office %>% 
  distinct(Village_ID) %>% 
  mutate(
    cell_office = 1
  ) %>% 
  st_drop_geometry() %>% 
  select(Village_ID, cell_office)

cell_office_plot <- ggplot() +
  geom_sf(data = rwa_cell, fill = NA, color = "grey60", size = 0.2) +
  geom_sf(data = cell_office_sf, colour = "green",size   = 1) +
  coord_sf(expand = FALSE) +            
  labs(title = "Cell Office by Cell") +
  theme_void()

cell_office_plot

ggsave(
  filename = file.path(output_path, "cell_office.png"),
  plot     = cell_office_plot,
  width    = 8,   
  height   = 6,   
  scale    = 0.5, 
  dpi      = 300
)

##Primary_school-----

primary_school_sf <- infra %>% 
  filter(Infra_Type == "Primary_School")

primary_school <- st_intersection(primary_school_sf, rwa_villages)

primary_school <- primary_school %>% 
  distinct(Village_ID) %>% 
  mutate(
    primary_school = 1
  ) %>% 
  st_drop_geometry() %>% 
  select(Village_ID, primary_school)


primary_school_plot <- ggplot() +
  geom_sf(data = rwa_cell, fill = NA, color = "grey60", size = 0.2) +
  geom_sf(data = primary_school_sf, colour = "blue",size   = 1) +
  coord_sf(expand = FALSE) +            
  labs(title = "Primary School by Cell") +
  theme_void()

ggsave(
  filename = file.path(output_path, "primary_school.png"),
  plot     = primary_school_plot,
  width    = 8,   
  height   = 6,   
  scale    = 0.5, 
  dpi      = 300
)

##Health center------

health_center_sf <- infra %>% 
  filter(Infra_Type == "Health_Centre")

health_center <- st_intersection(health_center_sf, rwa_villages)

health_center <- health_center %>% 
  distinct(Village_ID) %>% 
  mutate(
    health_center = 1
  ) %>% 
  st_drop_geometry() %>% 
  select(Village_ID, health_center)


health_center_plot <- ggplot() +
  geom_sf(data = rwa_cell, fill = NA, color = "grey60", size = 0.2) +
  geom_sf(data = health_center_sf, colour = "red",size   = 1) +
  coord_sf(expand = FALSE) +            
  labs(title = "Health Centres by Cell") +
  theme_void()

ggsave(
  filename = file.path(output_path, "health_center.png"),
  plot     = health_center_plot,
  width    = 8,   
  height   = 6,   
  scale    = 0.5, 
  dpi      = 300
)



#Utility electrification ----

utility <- read_xlsx(path = file.path(data_path, "usage_id_0416.xlsx"))

electrification_status <- utility %>%
  # Gather all “YYYY_usage” columns into long form
  pivot_longer(
    cols        = matches("^\\d{4}_usage$"),
    names_to    = "year",
    names_pattern = "(\\d{4})_usage",
    values_to   = "usage"
  ) %>%
  # For each village & year, check if any usage > 0
  group_by(village_id, year) %>%
  summarise(electrified = as.integer(any(usage > 0)), .groups = "drop") %>%
  # Spread back into wide form with one column per year
  pivot_wider(
    names_from  = year,
    values_from = electrified,
    names_prefix = "electrified_",
    values_fill  = 0
  ) 


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

  

#nep status----

nep <- read_xlsx(path = file.path(data_path, "FINAL_LIST_OF_REVISED_NEP_VILLAGES_JULY_2023-PUBLISHED (3).xlsx"))

nep <- nep %>% 
  mutate(village_id = as.character(Code_vil_1)) %>% 
  rename(nep = `NEP Revision July 2023`) %>% 
  select(
    Province, District, Sector, Cellule, Village, village_id, nep
  )

#JOIN together----

expansion_join <- left_join(nep, electrification_status, by = c("village_id"))

expansion_join <- left_join(expansion_join, village_lv_earp, by = c("village_id"))

expansion_join <- left_join(expansion_join, village_mv_earp, by = c("village_id" ))

expansion_join <- left_join(expansion_join, mv_2022, by =  c("village_id" = "Village_ID"))

expansion_join <- left_join(expansion_join, lv_2022, by =  c("village_id" = "Village_ID"))

expansion_join <- left_join(expansion_join, cell_office, by = c("village_id" = "Village_ID"))

expansion_join <- left_join(expansion_join, health_center, by = c("village_id" = "Village_ID"))

expansion_join <- left_join(expansion_join, primary_school, by = c("village_id" = "Village_ID"))

expansion_join <- left_join(expansion_join, earp_existing_mv, by = c("village_id" = "Village_ID"))

expansion_join <- expansion_join %>% 
  mutate(
    mv_length = as.numeric(mv_length),
    lv_length = as.numeric(lv_length)
  ) %>% 
  mutate(
    mv_length = mv_length/1000,
    lv_length = lv_length/1000
  ) %>% 
  mutate(across(
    -all_of(c("Province", "District", "Sector", "Cellule", "Village", "village_id", "nep")), 
    ~ replace_na(.x, 0)
  )) 


expansion_join <- expansion_join %>% 
  mutate(
    nep_grid = ifelse(nep %in% c("GE", "Fill In"), 1, 0),
    cell_id = substr(village_id, 1, 6),
    sector_id = substr(village_id, 1, 4),
    district_id = substr(village_id, 1, 2),
  )

expansion_join <- expansion_join %>% 
  mutate(
    `earp_mv or earp_lv`  = ifelse(earp_lv == 1 | earp_mv == 1, 1, 0)
  )

write_xlsx(expansion_join, path = file.path(output_path, "expansion_join.xlsx"))
write_xlsx(utility_long, path = file.path(output_path, "utility_long.xlsx"))
write_xlsx(electrification_status, path = file.path(output_path, "electrification_status.xlsx"))

#Electrified year by year----


expansion_join_no4 <- expansion_join %>% 
  filter(! District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu")) 



# container to hold the felm objects
regs <- list()   
# Define the years for which you want to run the regression
years <- 2014:2022

for(yr in years) {
  # Construct the dependent variable name, e.g., "electrified_2012"
  dep_var <- paste0("electrified_", yr)
  
  # Build the regression formula as a string, then convert to formula
  # Note: We're using earp_2011 as the regressor, and fixed effects for cell_id, clustering on sector_id
  reg_formula <- as.formula(paste0(dep_var, " ~  electrified_2013 +     `earp_mv or earp_lv`   | cell_id | 0 | sector_id"))
  
  # Run the regression using felm
  model <- felm(reg_formula, data = expansion_join_no4)
  
  # Store the model in the list with the dependent variable name as key
  regs[[dep_var]] <- model
  
  # Create a LaTeX file for the current regression using stargazer
  stargazer(
    model,
    type = "latex",
    title = paste("Regression Results for", dep_var),
    out = file.path(output_path, paste0("regression_cellfe_(control13)", yr, ".tex"))
  )
}



for(yr in years) {
  # Construct the dependent variable name, e.g., "electrified_2012"
  dep_var <- paste0("electrified_", yr)
  
  # Build the regression formula as a string, then convert to formula
  # Note: We're using earp_2011 as the regressor, and fixed effects for cell_id, clustering on sector_id
  reg_formula <- as.formula(paste0(dep_var, " ~  earp_existing_mv +     `earp_mv or earp_lv`  | cell_id | 0 | sector_id"))
  
  # Run the regression using felm
  model <- felm(reg_formula, data = expansion_join_no4)
  
  # Store the model in the list with the dependent variable name as key
  regs[[dep_var]] <- model
  
  # Create a LaTeX file for the current regression using stargazer
  stargazer(
    model,
    type = "latex",
    title = paste("Regression Results for", dep_var),
    out = file.path(output_path, paste0("regression_cellfe_(existing_mv)", yr, ".tex"))
  )
}




#MAIN-----
#Dropping electrified 2013----


electrified_13 <- electrification_status %>% 
  filter(electrified_2013 == 1)

expansion_join_drop13 <- expansion_join %>% 
  filter(!village_id %in% electrified_13$village_id) 

expansion_join_drop13 <- expansion_join_drop13 %>% 
  mutate(
    `earp_mv or earp_lv` = ifelse(earp_lv == 1 | earp_mv == 1, 1, 0)
  )

#Anti join MV 2011
expansion_join_drop13 <- expansion_join_drop13 %>% 
  anti_join(earp_existing_mv, by = c("village_id" = "Village_ID"))

write_xlsx(expansion_join_drop13, path = file.path(output_path, "expansion_join_drop13.xlsx"))




##Infrastructure reg----


cell_office <- felm(cell_office ~ `earp_mv or earp_lv`|cell_id|0|sector_id, data = expansion_join_drop13)
summary(cell_office)

primary_school <- felm(primary_school ~ `earp_mv or earp_lv`|cell_id|0|sector_id, data = expansion_join_drop13)
summary(primary_school)

health_center <- felm(health_center ~ `earp_mv or earp_lv`|cell_id|0|sector_id, data = expansion_join_drop13 )
summary(health_center)


regs <- list(
  "cell_office" = cell_office,
  "primary_school" = primary_school,
  "health_center" = health_center
)


stargazer(
  regs,
  output = file.path(output_path, "infra.tex"),
  title = "Regression Results"
)

mean(expansion_join_drop13$cell_office)
mean(expansion_join_drop13$primary_school)
mean(expansion_join_drop13$health_center)



#Drop the four districts----


expansion_join_drop4 <- expansion_join_drop13 %>% 
  filter(! District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu"))




electrified_2018 <- felm(electrified_2018 ~  `earp_mv or earp_lv` |cell_id|0|sector_id, data = expansion_join_drop4)
summary(electrified_2018)

electrified_2020 <- felm(electrified_2020 ~  `earp_mv or earp_lv`|cell_id|0|sector_id, data = expansion_join_drop4)
summary(electrified_2020)

electrified_2022 <- felm(electrified_2022 ~ `earp_mv or earp_lv`| cell_id|0|sector_id, data = expansion_join_drop4)
summary(electrified_2022)

regs <- list(
  "electrified_2018" = electrified_2018,
  "electrified_2020" = electrified_2020,
  "electrified_2022" = electrified_2022
)

stargazer(
  regs,
  output = file.path(output_path, "electrified.tex"),
  title = "Regression Results"
)
mean(expansion_join_drop4$electrified_2018)
mean(expansion_join_drop4$electrified_2020)
mean(expansion_join_drop4$electrified_2022)



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



earp_join <- expansion_join_drop13 %>%
  select(village_id, earp_mv, earp_lv, cell_id, sector_id, district_id)


earp_did <- left_join( earp_join, ec_all, by = c("village_id"))



earp_did <- earp_did %>%
  complete(year, village_id,
           fill = list(num_establishment = 0, total_employee = 0)) %>%
  filter(!is.na(year))


earp <- expansion_join_drop13%>%
  select(village_id, earp_mv, earp_lv) %>%
  rename(
    mv = earp_mv,
    lv = earp_lv
  )

earp_did <- left_join(earp_did, earp, by = c("village_id"))

earp_did <- earp_did %>%
  mutate(
    earp_mv = ifelse(is.na(earp_mv ), mv, earp_mv ),
    earp_lv = ifelse(is.na(earp_lv), lv, earp_lv)
  ) %>%
  select(-mv, -lv) %>%
  mutate(
    cell_id    = substr(village_id, 1, 6),
    sector_id  = substr(village_id, 1, 4),
    district_id= substr(village_id, 1, 2)
  ) %>%
  mutate(
    cell_year = paste0(cell_id, "_", year),
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  ) %>%
  filter(!village_id %in% electrified_2013$village_id)

earp_did <- earp_did %>% 
  mutate(
    `earp_mv or earp_lv`  = ifelse(earp_lv == 1 | earp_mv  == 1, 1, 0)
  )


mean(earp_did$num_establishment)
mean(earp_did$total_employee)

##MV or LV Event study----

num_establishment <- felm(num_establishment ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|village_id + cell_year |0|sector_id, data = earp_did)
summary(num_establishment)

total_employee <- felm (total_employee ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|village_id + cell_year |0|sector_id, data = earp_did)
summary(total_employee)


regs <- list(
  "num_establishment" = num_establishment,
  "total_employee" = total_employee
)

stargazer(
  regs,
  output = file.path(output_path, "event study.tex"),
  title = "Regression Results"
)


#ISIC selection------

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



earp_join <- expansion_join_drop13%>% 
  select(village_id, earp_mv, earp_lv, cell_id, sector_id, district_id) 




##clean------

earp_did_isic<- left_join( earp_join, ec_all, by = c("village_id"))


earp_did_isic <- earp_did_isic %>%
  complete(year, village_id, isic_level1,
           fill = list(num_establishment = 0, total_employee = 0)) %>% 
  filter(!is.na(year))  


earp <- expansion_join_drop13%>% 
  select(village_id, earp_mv, earp_lv, earp_existing_mv, health_center, primary_school, cell_office) %>% 
  rename(
    mv = earp_mv,
    lv = earp_lv
  )

earp_did_isic <- left_join(earp_did_isic, earp, by = c("village_id"))



earp_did_isic <- earp_did_isic %>% 
  mutate(
    earp_mv = ifelse(is.na(earp_mv), mv, earp_mv),
    earp_lv = ifelse(is.na(earp_lv), lv, earp_lv)
  ) %>% 
  select(-mv, -lv) %>% 
  mutate(
    cell_id    = substr(village_id, 1, 6),
    sector_id  = substr(village_id, 1, 4),
    district_id= substr(village_id, 1, 2)
  ) %>% 
  mutate(
    cell_year = paste0(cell_id, "_", year),
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  ) %>% 
  filter(!is.na(isic_level1)) %>%   
  mutate(
    isic_level1 = as.character(isic_level1),
    `earp_mv or earp_lv`  = ifelse(earp_lv == 1 | earp_mv == 1, 1, 0)
  ) %>% 
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

##ISIC Regression-------

###Cell-id-----
poisson_employee <- feglm(
  total_employee ~ usage:isic | village_id^year + cell_id^isic^year,
  family = "poisson",
  data =earp_did_nozero
)


poisson_establishment <- feglm(
  num_establishment ~ usage:isic | village_id^year + cell_id^isic^year,
  family = "poisson",
  data =earp_did_nozero
)

poisson_reg <- list(
    "num_establishment" = poisson_establishment,
    "total_employee" = poisson_employee
  )


modelsummary(
  poisson_reg, 
  output = "huxtable",
  stars = TRUE,
  fmt = 10 # show 10 digits after the decimal point
)


#absorbed isic----


poisson_employee <- fepois(
  total_employee ~ usage | village_id^year + cell_id^isic^year,
  data = earp_did_nozero
)


summary(poisson_employee)

poisson_establishment <- fepois(
  num_establishment ~ usage | village_id^year + cell_id^isic^year,
  data = earp_did_nozero
)

summary(poisson_establishment)







# ###feols----
# 
# 
# 
# poisson_employee_feols <- feols(
#   total_employee ~ usage:isic | village_id^year + isic^year,
#   data = earp_did_nozero
# )
# 
# summary(poisson_employee_feols)
# 
# 
# poisson_employee_boss <- fepois(
#   total_employee ~ usage:isic | cell_id^isic^year,
#   data = earp_did_nozero
# )
# 
# summary(poisson_employee_boss)

##ISIC 19-----

earp_did_19 <- earp_did_nozero %>% 
  filter(isic == 19)


num_establishment_19 <- felm(num_establishment ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|village_id + cell_year|0|sector_id, data = earp_did_19)
summary(num_establishment_19)

total_employee_19 <- felm (total_employee ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|village_id + cell_year|0|sector_id, data = earp_did_19)
summary(total_employee_19)


# regs <- list(
#   "num_establishment" = num_establishment,
#   "total_employee" = total_employee
# )
# 
# stargazer(
#   regs,
#   output = file.path(output_path, "event study.tex"),
#   title = "Regression Results"
# )



##ISIC 3-----

earp_did_3 <- earp_did_nozero %>% 
  filter(isic == 3)

mean(earp_did_3$num_establishment)
mean(earp_did_3$total_employee)

num_establishment_3 <- felm(num_establishment ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|village_id + cell_year|0|sector_id, data = earp_did_3)
summary(num_establishment_3)

total_employee_3 <- felm (total_employee ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|village_id + cell_year|0|sector_id, data = earp_did_3)
summary(total_employee_3)


# regs <- list(
#   "num_establishment" = num_establishment,
#   "total_employee" = total_employee
# )
# 
# stargazer(
#   regs,
#   output = file.path(output_path, "event study.tex"),
#   title = "Regression Results"
# )


##ISIC 7-----
earp_did_7 <- earp_did_nozero %>% 
  filter(isic == 7)

mean(earp_did_7$num_establishment)
mean(earp_did_7$total_employee)

num_establishment_7 <- felm(num_establishment ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|village_id + cell_year|0|sector_id, data = earp_did_7)
summary(num_establishment_7)

total_employee_7 <- felm (total_employee ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|village_id + cell_year|0|sector_id, data = earp_did_7)
summary(total_employee_7)

summary(num_establishment)
summary(total_employee)

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

mean(earp_did$num_establishment)
mean(earp_did$total_employee)
mean(earp_did_3$num_establishment)
mean(earp_did_3$total_employee)
mean(earp_did_7$num_establishment)
mean(earp_did_7$total_employee)
mean(earp_did_19$num_establishment)
mean(earp_did_19$total_employee)


##ISIC 16----
earp_did_16 <- earp_did_nozero %>% 
  filter(isic == 16)

mean(earp_did_16$num_establishment)
mean(earp_did_16$total_employee)

num_establishment_16 <- felm(num_establishment ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|village_id + cell_year|0|sector_id, data = earp_did_16)
summary(num_establishment_16)

total_employee_16 <- felm (total_employee ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|village_id + cell_year|0|sector_id, data = earp_did_16)
summary(total_employee_16)

##ISIC 17----
earp_did_17 <- earp_did_nozero %>% 
  filter(isic == 17)

mean(earp_did_17$num_establishment)
mean(earp_did_17$total_employee)

num_establishment_17 <- felm(num_establishment ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|village_id + cell_year|0|sector_id, data = earp_did_17)
summary(num_establishment_17)

total_employee_17 <- felm (total_employee ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|village_id + cell_year|0|sector_id, data = earp_did_17)
summary(total_employee_17)


##ISIC 9----
earp_did_9 <- earp_did_nozero %>% 
  filter(isic == 9)

mean(earp_did_9$num_establishment)
mean(earp_did_9$total_employee)

num_establishment_9 <- felm(num_establishment ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|village_id + cell_year|0|sector_id, data = earp_did_9)
summary(num_establishment_9)

total_employee_9 <- felm (total_employee ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|village_id + cell_year|0|sector_id, data = earp_did_9)
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

mean(earp_did$num_establishment)
mean(earp_did$total_employee)
mean(earp_did_16$num_establishment)
mean(earp_did_16$total_employee)
mean(earp_did_17$num_establishment)
mean(earp_did_17$total_employee)
mean(earp_did_9$num_establishment)
mean(earp_did_9$total_employee)





#Poisson-----

poisson_establishment <- feglm(
  num_establishment ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|
  village_id + cell_year,
  family = "poisson",
  data =earp_did
)

summary(poisson_establishment)


poisson_employee <- feglm(
  total_employee ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|
    village_id + cell_year,
  family = "poisson",
  data =earp_did
)

summary(poisson_employee)


##Poisson 3------

poisson_establishment_3 <- feglm(
  num_establishment ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|
    village_id + cell_year,
  family = "poisson",
  data =earp_did_3
)

summary(poisson_establishment_3)


poisson_employee_3 <- feglm(
  total_employee ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|
    village_id + cell_year,
  family = "poisson",
  data =earp_did_3
)

summary(poisson_employee_3)


##Poisson 7----

poisson_establishment_7 <- feglm(
  num_establishment ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|
    village_id + cell_year,
  family = "poisson",
  data =earp_did_7
)

summary(poisson_establishment_7)


poisson_employee_7 <- feglm(
  total_employee ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|
    village_id + cell_year,
  family = "poisson",
  data =earp_did_7
)

summary(poisson_employee_7)


##Poisson 19-----


poisson_establishment_19 <- feglm(
  num_establishment ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|
    village_id + cell_year,
  family = "poisson",
  data =earp_did_19
)

summary(poisson_establishment_19)


poisson_employee_19 <- feglm(
  total_employee ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|
    village_id + cell_year,
  family = "poisson",
  data =earp_did_19
)

summary(poisson_employee_19)


poisson_reg <- list(
  "num_establishment" = poisson_establishment,
  "total_employee" = poisson_employee,
  "establishment_isic3" = poisson_establishment_3,
  "employee_isic_3" = poisson_employee_3,
  "establishment_isic7" = poisson_establishment_7,
  "employee_isic_7" = poisson_employee_7,
  "establishment_isic19" = poisson_establishment_19,
  "employee_isic_19" = poisson_employee_19
)

modelsummary(
  poisson_reg, 
  output = "huxtable",
  stars = TRUE
)


#Electrified & Usage-----

##Clean====
electrified_long <- electrification_status %>% 
  pivot_longer(
    cols = starts_with("electrified_"),          # the four status columns
    names_to   = "year",                         # new column holding the year
    names_pattern = "electrified_(\\d{4})",      # extract the 4-digit year
    values_to  = "electrified"                   # 0/1 (or NA) indicator
  ) %>% 
  mutate(year = as.integer(year))  %>% 
  filter(
    year %in% c(2011, 2014, 2017, 2020)
  ) %>% 
  select(village_id, year, electrified)

earp_did_electrified <- left_join(earp_did, electrified_long, by = c("village_id", "year"))


earp_did_electrified <- left_join(earp_did_electrified, utility_long_join, by = c("village_id", "year"))


earp_did_electrified <- earp_did_electrified %>% 
  mutate(
    electrified = ifelse(is.na(electrified), 0, electrified),
    usage = ifelse(is.na(usage), 0, usage),
    usage = usage/ (100*365)
  )


district <- rwa_district %>% 
  select(Dist_ID, District) %>% 
  mutate(Dist_ID = as.character(Dist_ID)) 

earp_did_electrified <- left_join(earp_did_electrified, district, by = c("district_id" = "Dist_ID"))


earp_did_electrified <- earp_did_electrified %>% 
  filter(! District %in% c("Nyabihu", "Ngororero", "Nyamasheke", "Rubavu"))
##Regression=--------

electrified <- felm( electrified ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|village_id+ cell_year|0|sector_id, data = earp_did_electrified)
summary(electrified)

stargazer(
  electrified,
  output = file.path(output_path, "event study.tex"),
  title = "Regression Results"
)


mean(earp_did_electrified$electrified)



#Usage experiment------


earp_did_1 <- earp_did_electrified %>% 
  mutate(
    usage_bw = pmax(usage, 0.01) ,
    usage_tw = pmin(usage, 0.95)
  )

usage_reg <- felm(
  usage ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|
    village_id + cell_year,
  data =earp_did_1
)

summary(usage_reg)

usage_reg_bw <- felm(
  usage_bw ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|
    village_id + cell_year,
  data =earp_did_1
)

summary(usage_reg_bw)


usage_reg_tw <- felm(
  usage_tw ~ p0_2014*`earp_mv or earp_lv` + p1_2017*`earp_mv or earp_lv` + p2_2020*`earp_mv or earp_lv`|
    village_id + cell_year,
  data =earp_did_1
)

summary(usage_reg_tw)

mean(earp_did_1$usage)
mean(earp_did_1$usage_bw)
mean(earp_did_1$usage_tw)


reg <- list(
  "usage" = usage_reg,
  "usage_bottom_win(0.01)"= usage_reg_bw,
  "usage_top_win(0.95)" = usage_reg_tw
)


stargazer(
  reg,
  output = file.path(output_path, "event study.tex"),
  title = "Regression Results"
)
