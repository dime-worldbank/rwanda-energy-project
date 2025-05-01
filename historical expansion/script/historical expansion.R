##
#Author: Xiaoming Zhang
#Date: 02182025
#Topic: reading the historical expansion files shared by Clementine


pacman::p_load(knitr, lfe,fixest, modelsummary, stargazer, tidyverse, dplyr, here, sf, haven, ggplot2, readxl,  writexl, janitor, randomizr, RCT, purrr, RODBC, DBI)

getwd()

# install.packages("RODBC")
# install.packages("DBI")

library(RODBC)
library(DBI)

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

earp_planned_mv <- st_read(dsn = file.path(data_path, "earp_plannedMV.shp"))

earp_planned_mv <- st_transform(earp_planned_mv, crs = st_crs(rwa_villages))


mv_2011 <- st_read(dsn = file.path( data_path, "Existing Electrical Network_2011", "Existing_MVLine.shp"))

mv_2011 <-st_transform(mv_2011, crs = st_crs(rwa_district))

mv_2022 <- st_read(dsn = file.path( data_path, "Existing Electrical Network_2022", "Existing_MVLine.shp"))

mv_2022 <-st_transform(mv_2022, crs = st_crs(rwa_district))

ggplot() +
  geom_sf(data = rwa_district, aes(color = "RWA District"), fill = NA) +
  geom_sf(data = earp_planned_mv, aes(color = "Planned MV"), fill = NA) +
  geom_sf(data = mv_2011, aes(color = "MV 11"), fill = NA) +
  scale_color_manual(name = "Data Source",
                     values = c("RWA District" = "black",
                                "Planned MV" = "red",
                                "MV 11" = "blue")) +

  theme_void()


ggplot() +
  geom_sf(data = rwa_district, aes(color = "RWA District"), fill = NA) +
  geom_sf(data = earp_planned_mv, aes(color = "Planned MV"), fill = NA) +
  geom_sf(data = mv_2022, aes(color = "MV 22"), fill = NA) +
  scale_color_manual(name = "Data Source",
                     values = c("RWA District" = "black",
                                "Planned MV" = "red",
                                "MV 22" = "blue")) +
  
  theme_void()
#Overlay earp mv with village----

village_mv_earp <- st_intersection(rwa_villages, earp_planned_mv)

village_mv_earp <- village_mv_earp %>% 
  distinct(Village_ID) %>% 
  mutate(
    earp_2011 = 1
  )

#Utility electrification ----

utility <- read_xlsx(path = file.path(data_path, "usage_id_0416.xlsx"))

utility_2011 <- utility %>% 
  select(village_id, `2011_usage`) %>% 
  filter(`2011_usage` > 0) %>% 
  distinct(village_id) %>% 
  mutate(
    electrified_2011 = 1
  )

utility_2022 <- utility %>% 
  select(village_id, `2022_usage`) %>% 
  filter(`2022_usage` > 0) %>% 
  distinct(village_id) %>% 
  mutate(
    electrified_2022 = 1
  )


#nep status----

nep <- read_xlsx(path = file.path(data_path, "FINAL_LIST_OF_REVISED_NEP_VILLAGES_JULY_2023-PUBLISHED (3).xlsx"))

nep <- nep %>% 
  mutate(village_id = as.character(Code_vil_1)) %>% 
  rename(nep = `NEP Revision July 2023`) %>% 
  select(
    Province, District, Sector, Cellule, Village, village_id, nep
  )

#mv lines currently ----

mv_2022 <- st_read(dsn = file.path( data_path, "Existing Electrical Network_2022", "Existing_MVLine.shp"))

mv_2022 <- st_transform(mv_2022, crs = st_crs(rwa_villages))

mv_2022 <- st_intersection(mv_2022, rwa_villages)

mv_2022 <- mv_2022 %>% 
  distinct(Village_ID) %>% 
  mutate(
    exist_mv = 1
  )



#Plot the map graph------



ggplot() +
  geom_sf(data = rwa_district, aes(color = "RWA District"), fill = NA) +
  geom_sf(data = earp_planned_mv, aes(color = "Planned MV"), fill = NA) +
  geom_sf(data = mv_22, aes(color = "MV 22"), fill = NA) +
  scale_color_manual(name = "Data Source",
                     values = c("RWA District" = "black",
                                "Planned MV" = "red",
                                "MV 22" = "blue")) +
  labs(title = "Comparison of Planned and Existing MV Line in Rwanda",
       subtitle = "Overlay of District map, Planned MV 2010, and MV Network 2022",
  ) +
  theme_minimal()




#join together----

expansion_join <- left_join(nep, utility_2011, by = c("village_id"))

expansion_join <- left_join(expansion_join, utility_2022, by = c("village_id"))

expansion_join <- left_join(expansion_join, village_mv_earp, by = c("village_id" = "Village_ID"))

expansion_join <- left_join(expansion_join, mv_2022, by =  c("village_id" = "Village_ID"))

expansion_join <- expansion_join %>% 
  mutate(across(c("electrified_2011", "electrified_2022", "earp_2011", "exist_mv", ), 
                ~ replace_na(., 0)))

expansion_join <- expansion_join %>% 
  mutate(
    nep_grid = ifelse(nep %in% c("GE", "Fill In"), 1, 0),
    cell_id = substr(village_id, 1, 6),
    sector_id = substr(village_id, 1, 4),
    district_id = substr(village_id, 1, 2),
  )



#Regression----

electrified_2022 <- lm(electrified_2022 ~ electrified_2011 + earp_2011, data = expansion_join)

summary(electrified_2022)

mv_exist_2022 <- lm(exist_mv ~ electrified_2011 + earp_2011, data = expansion_join)

summary(mv_exist_2022)

nep_reg<- lm(nep_grid ~ electrified_2011 + earp_2011, data = expansion_join)

summary(nep_reg)


regs <- list(
  "electrified_2022" = electrified_2022,
  "mv_exist_2022" = mv_exist_2022,
  "nep" = nep_reg
)

stargazer( 
  regs,
  type = "latex", 
  title = "Regression Results", 
  out = "regression_output.latex"
)

##Cell_ID------


electrified_2022 <- felm(electrified_2022 ~ electrified_2011 + earp_2011|cell_id|0|sector_id, data = expansion_join)

summary(electrified_2022)

mv_exist_2022 <- felm(exist_mv ~ electrified_2011 + earp_2011|cell_id|0|sector_id, data = expansion_join)

summary(mv_exist_2022)

nep_reg<- felm(nep_grid ~ electrified_2011 + earp_2011|cell_id|0|sector_id, data = expansion_join)

summary(nep_reg)


regs <- list(
  "electrified_2022" = electrified_2022,
  "mv_exist_2022" = mv_exist_2022,
  "nep" = nep_reg
)

stargazer( 
  regs,
  type = "latex", 
  title = "Regression Results", 
  out = "regression_output.latex"
)

##sector_ID----


electrified_2022 <- felm(electrified_2022 ~ electrified_2011 + earp_2011|sector_id|0|sector_id, data = expansion_join)

summary(electrified_2022)


mv_exist_2022 <- felm(exist_mv ~ electrified_2011 + earp_2011|sector_id|0|sector_id, data = expansion_join)

summary(mv_exist_2022)

nep_reg<- felm(nep_grid ~ electrified_2011 + earp_2011|sector_id|0|sector_id, data = expansion_join)

summary(nep_reg)


regs <- list(
  "electrified_2022" = electrified_2022,
  "mv_exist_2022" = mv_exist_2022,
  "nep" = nep_reg
)

stargazer( 
  regs,
  type = "latex", 
  title = "Regression Results", 
  out = "regression_output.latex"
)



##district_ID----




electrified_2022 <- felm(electrified_2022 ~ electrified_2011 + earp_2011|district_id|0|sector_id, data = expansion_join)

summary(electrified_2022)


mv_exist_2022 <- felm(exist_mv ~ electrified_2011 + earp_2011|district_id|0|sector_id, data = expansion_join)

summary(mv_exist_2022)

nep_reg<- felm(nep_grid ~ electrified_2011 + earp_2011|district_id|0|sector_id, data = expansion_join)

summary(nep_reg)


regs <- list(
  "electrified_2022" = electrified_2022,
  "mv_exist_2022" = mv_exist_2022,
  "nep" = nep_reg
)

stargazer( 
  regs,
  type = "latex", 
  title = "Regression Results", 
  out = "regression_output.latex"
)




#Distance------

length_mv <- st_intersection(rwa_villages, earp_planned_mv)

length_mv$length <- st_length(length_mv) 

length_mv_earp <- length_mv %>%
  mutate(length = as.numeric(length)) %>% 
  group_by(Village_ID) %>% 
  summarise(
    length = sum(length)
  ) %>% 
  filter(
    length > 200
  ) %>% 
  mutate(earp_200 = 1) %>% 
  select(Village_ID, earp_200)

length_mv_earp <- length_mv_earp %>% 
  st_drop_geometry()

expansion_join_1 <- left_join(expansion_join, length_mv_earp, by = c("village_id" = "Village_ID") )

expansion_join_1 <- expansion_join_1 %>% 
  mutate(
    earp_200 = ifelse(is.na(earp_200), 0, earp_200)
  )
##Regression----
# 
# electrified_2022 <- lm(electrified_2022 ~ electrified_2011 + earp_200, data = expansion_join_1)
# 
# 
# summary(electrified_2022)
# 
# mv_exist_2022 <- lm(exist_mv ~ electrified_2011 + earp_200, data = expansion_join_1)
# 
# summary(mv_exist_2022)
# 
# nep_reg<- lm(nep_grid ~ electrified_2011 + earp_200, data = expansion_join_1)
# 
# summary(nep_reg)
# 
# 
# regs <- list(
#   "electrified_2022" = electrified_2022,
#   "mv_exist_2022" = mv_exist_2022,
#   "nep" = nep_reg
# )
# 
# stargazer( 
#   regs,
#   type = "latex", 
#   title = "Regression Results", 
#   out = "regression_output.latex"
# )
# 

##Cell_ID------


electrified_2022 <- felm(electrified_2022 ~ electrified_2011 + earp_200|cell_id|0|sector_id, data = expansion_join_1)

summary(electrified_2022)

mv_exist_2022 <- felm(exist_mv ~ electrified_2011 + earp_200|cell_id|0|sector_id, data = expansion_join_1)

summary(mv_exist_2022)

nep_reg<- felm(nep_grid ~ electrified_2011 + earp_200|cell_id|0|sector_id, data = expansion_join_1)

summary(nep_reg)


regs <- list(
  "electrified_2022" = electrified_2022,
  "mv_exist_2022" = mv_exist_2022,
  "nep" = nep_reg
)

stargazer( 
  regs,
  type = "latex", 
  title = "Regression Results", 
  out = "regression_output.latex"
)

##sector_ID----


electrified_2022 <- felm(electrified_2022 ~ electrified_2011 + earp_200|sector_id|0|sector_id, data = expansion_join_1)

summary(electrified_2022)


mv_exist_2022 <- felm(exist_mv ~ electrified_2011 + earp_200|sector_id|0|sector_id, data = expansion_join_1)

summary(mv_exist_2022)

nep_reg<- felm(nep_grid ~ electrified_2011 + earp_200|sector_id|0|sector_id, data = expansion_join_1)

summary(nep_reg)


regs <- list(
  "electrified_2022" = electrified_2022,
  "mv_exist_2022" = mv_exist_2022,
  "nep" = nep_reg
)

stargazer( 
  regs,
  type = "latex", 
  title = "Regression Results", 
  out = "regression_output.latex"
)



##district_ID----




electrified_2022 <- felm(electrified_2022 ~ electrified_2011 + earp_200|district_id|0|sector_id, data = expansion_join_1)

summary(electrified_2022)


mv_exist_2022 <- felm(exist_mv ~ electrified_2011 + earp_200|district_id|0|sector_id, data = expansion_join_1)

summary(mv_exist_2022)

nep_reg<- felm(nep_grid ~ electrified_2011 + earp_200|district_id|0|sector_id, data = expansion_join_1)

summary(nep_reg)


regs <- list(
  "electrified_2022" = electrified_2022,
  "mv_exist_2022" = mv_exist_2022,
  "nep" = nep_reg
)

stargazer( 
  regs,
  type = "latex", 
  title = "Regression Results", 
  out = "regression_output.latex"
)


#Dropping villages electrified in 2011-------

expansion_join_no11 <- expansion_join_1 %>% 
  filter(electrified_2011 == 0)

##Regression----
# 
# electrified_2022 <- lm(electrified_2022 ~  earp_200, data = expansion_join_no11)
# 
# summary(electrified_2022)
# 
# mv_exist_2022 <- lm(exist_mv ~ earp_200, data = expansion_join_no11)
# 
# summary(mv_exist_2022)
# 
# nep_reg<- lm(nep_grid ~ earp_200, data = expansion_join_no11)
# 
# summary(nep_reg)
# 
# 
# regs <- list(
#   "electrified_2022" = electrified_2022,
#   "mv_exist_2022" = mv_exist_2022,
#   "nep" = nep_reg
# )
# 
# stargazer( 
#   regs,
#   type = "latex", 
#   title = "Regression Results", 
#   out = "regression_output.latex"
# )


##Cell_ID------


electrified_2022 <- felm(electrified_2022 ~ earp_200|cell_id|0|sector_id, data = expansion_join_no11)

summary(electrified_2022)

mv_exist_2022 <- felm(exist_mv ~  earp_200|cell_id|0|sector_id, data = expansion_join_no11)

summary(mv_exist_2022)

nep_reg<- felm(nep_grid ~  earp_200|cell_id|0|sector_id, data = expansion_join_no11)

summary(nep_reg)


regs <- list(
  "electrified_2022" = electrified_2022,
  "mv_exist_2022" = mv_exist_2022,
  "nep" = nep_reg
)

stargazer( 
  regs,
  type = "latex", 
  title = "Regression Results", 
  out = "regression_output.latex"
)

##sector_ID----


electrified_2022 <- felm(electrified_2022 ~ earp_200|sector_id|0|sector_id, data = expansion_join_no11)

summary(electrified_2022)


mv_exist_2022 <- felm(exist_mv ~ earp_200|sector_id|0|sector_id, data = expansion_join_no11)

summary(mv_exist_2022)

nep_reg<- felm(nep_grid ~ earp_200|sector_id|0|sector_id, data = expansion_join_no11)

summary(nep_reg)


regs <- list(
  "electrified_2022" = electrified_2022,
  "mv_exist_2022" = mv_exist_2022,
  "nep" = nep_reg
)

stargazer( 
  regs,
  type = "latex", 
  title = "Regression Results", 
  out = "regression_output.latex"
)



##district_ID----




electrified_2022 <- felm(electrified_2022 ~ earp_200|district_id|0|sector_id, data = expansion_join_no11)

summary(electrified_2022)


mv_exist_2022 <- felm(exist_mv ~  earp_200|district_id|0|sector_id, data = expansion_join_no11)

summary(mv_exist_2022)

nep_reg<- felm(nep_grid ~  earp_200|district_id|0|sector_id, data = expansion_join_no11)

summary(nep_reg)


regs <- list(
  "electrified_2022" = electrified_2022,
  "mv_exist_2022" = mv_exist_2022,
  "nep" = nep_reg
)

stargazer( 
  regs,
  type = "latex", 
  title = "Regression Results", 
  out = "regression_output.latex"
)




#Drop the four districts----


expansion_join_drop <- expansion_join_no11 %>% 
  filter(! District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu"))





# ##Regression----
# 
# electrified_2022 <- lm(electrified_2022 ~ electrified_2011 + earp_2011, data = expansion_join_drop)
# 
# summary(electrified_2022)
# 
# mv_exist_2022 <- lm(exist_mv ~ electrified_2011 + earp_2011, data = expansion_join_drop)
# 
# summary(mv_exist_2022)
# 
# nep_reg<- lm(nep_grid ~ electrified_2011 + earp_2011, data = expansion_join_drop)
# 
# summary(nep_reg)
# 
# 
# regs <- list(
#   "electrified_2022" = electrified_2022,
#   "mv_exist_2022" = mv_exist_2022,
#   "nep" = nep_reg
# )
# 
# stargazer(
#   regs,
#   type = "latex",
#   title = "Regression Results",
#   out = "regression_output.latex"
# )

##Cell_ID------


electrified_2022 <- felm(electrified_2022 ~ electrified_2011 + earp_2011|cell_id|0|sector_id, data = expansion_join_drop)

summary(electrified_2022)

mv_exist_2022 <- felm(exist_mv ~ electrified_2011 + earp_2011|cell_id|0|sector_id, data = expansion_join_drop)

summary(mv_exist_2022)

nep_reg<- felm(nep_grid ~ electrified_2011 + earp_2011|cell_id|0|sector_id, data = expansion_join_drop)

summary(nep_reg)


regs <- list(
  "electrified_2022" = electrified_2022,
  "mv_exist_2022" = mv_exist_2022,
  "nep" = nep_reg
)

stargazer( 
  regs,
  type = "latex", 
  title = "Regression Results", 
  out = "regression_output.latex"
)





##sector_ID----

electrified_2022 <- felm(electrified_2022 ~ electrified_2011 + earp_2011|sector_id|0|sector_id, data = expansion_join_drop)

summary(electrified_2022)


mv_exist_2022 <- felm(exist_mv ~ electrified_2011 + earp_2011|sector_id|0|sector_id, data = expansion_join_drop)

summary(mv_exist_2022)

nep_reg<- felm(nep_grid ~ electrified_2011 + earp_2011|sector_id|0|sector_id, data = expansion_join_drop)

summary(nep_reg)


regs <- list(
  "electrified_2022" = electrified_2022,
  "mv_exist_2022" = mv_exist_2022,
  "nep" = nep_reg
)

stargazer( 
  regs,
  type = "latex", 
  title = "Regression Results", 
  out = "regression_output.latex"
)



##district_ID----


electrified_2022 <- felm(electrified_2022 ~ electrified_2011 + earp_2011|district_id|0|sector_id, data = expansion_join_drop)

summary(electrified_2022)


mv_exist_2022 <- felm(exist_mv ~ electrified_2011 + earp_2011|district_id|0|sector_id, data = expansion_join_drop)

summary(mv_exist_2022)

nep_reg<- felm(nep_grid ~ electrified_2011 + earp_2011|district_id|0|sector_id, data = expansion_join_drop)

summary(nep_reg)


regs <- list(
  "electrified_2022" = electrified_2022,
  "mv_exist_2022" = mv_exist_2022,
  "nep" = nep_reg
)

stargazer( 
  regs,
  type = "latex", 
  title = "Regression Results", 
  out = "regression_output.latex"
)



#Making map----

rwa_villages <- rwa_villages %>% 
  select(Village_ID )

rwa_villages <- st_make_valid(rwa_villages)


expansion_plot <- left_join(expansion_join_drop, rwa_villages, by= c("village_id" = "Village_ID"))

expansion_plot <- st_as_sf(expansion_plot)

ggplot(expansion_plot) +
  geom_sf(aes(fill = factor(earp_2011)), color = NA) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),
                    labels = c("No EARP", "Any EARP")) +
  labs(title = "Villages by EARP 2011 Status",
       fill = "EARP 2011") +
  theme_void()




#With those electrified in 2011

expansion_join_plot <- expansion_join %>% 
  filter(! District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu"))
  
expansion_plot <- left_join(expansion_join_plot, rwa_villages, by= c("village_id" = "Village_ID"))

expansion_plot <- st_as_sf(expansion_plot)

ggplot(expansion_plot) +
  geom_sf(aes(fill = factor(earp_2011)), color = NA) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),
                    labels = c("No EARP", "Any EARP")) +
  labs(title = "Villages by EARP 2011 Status",
       fill = "EARP 2011") +
  theme_void()


#Year by year regression----

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
  ) %>% 
  select(
    -electrified_2011, -electrified_2022
  )

electrified_year <- left_join(expansion_join, electrification_status, by = c("village_id"))

electrified_year <- electrified_year %>% 
  mutate(across(.cols = paste0("electrified_", 2010:2021), 
                ~ replace_na(.x, 0))) %>% 
  filter(! District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu")) %>% 
  rename(earp = earp_2011)


##Cell_id ----

regs <- list()

# Define the years for which you want to run the regression
years <- 2010:2022

for(yr in years) {
  # Construct the dependent variable name, e.g., "electrified_2012"
  dep_var <- paste0("electrified_", yr)
  
  # Build the regression formula as a string, then convert to formula
  # Note: We're using earp_2011 as the regressor, and fixed effects for cell_id, clustering on sector_id
  reg_formula <- as.formula(paste0(dep_var, " ~ earp + electrified_2013 | cell_id | 0 | sector_id"))
  
  # Run the regression using felm
  model <- felm(reg_formula, data = electrified_year)
  
  # Store the model in the list with the dependent variable name as key
  regs[[dep_var]] <- model
  
  # Create a LaTeX file for the current regression using stargazer
  stargazer(
    model,
    type = "latex",
    title = paste("Regression Results for", dep_var),
    out = file.path(output_path, paste0("regression_cellfe_", yr, ".tex"))
  )
}

for(yr in years) {
  # Construct the dependent variable name, e.g., "electrified_2012"
  dep_var <- paste0("electrified_", yr)
  
  # Build the regression formula as a string, then convert to formula
  # Note: We're using earp_2011 as the regressor, and fixed effects for cell_id, clustering on sector_id
  reg_formula <- as.formula(paste0(dep_var, " ~ earp + electrified_2013 | sector_id | 0 | sector_id"))
  
  # Run the regression using felm
  model <- felm(reg_formula, data = electrified_year)
  
  # Store the model in the list with the dependent variable name as key
  regs[[dep_var]] <- model
  
  # Create a LaTeX file for the current regression using stargazer
  stargazer(
    model,
    type = "latex",
    title = paste("Regression Results for", dep_var),
    out = file.path(output_path, paste0("regression_sectorfe_", yr, ".tex"))
  )
}



#Event study----



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

earp_join <- expansion_join %>% 
  select(village_id, earp_2011, cell_id, sector_id, district_id) %>% 
  rename(
    earp = earp_2011
  )

#electrified 2023----
electrified_2013 <- electrified_year %>% 
  filter(electrified_2013 == 1)

earp_did <- left_join( earp_join, ec_all, by = c("village_id"))



earp_did <- earp_did %>%
  complete(year, village_id,
           fill = list(num_establishment = 0, total_employee = 0)) %>% 
  filter(!is.na(year))  



earp_2011 <- expansion_join %>% 
  select(village_id, earp_2011, District)

earp_did <- left_join(earp_did, earp_2011, by = c("village_id"))

earp_did <- earp_did %>% 
  mutate(
    earp = ifelse(is.na(earp), earp_2011, earp)
  ) %>% 
  select(-earp_2011) %>% 
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

num_establishment <- felm(num_establishment ~ p0_2014*earp + p1_2017*earp + p2_2020*earp|village_id + cell_year|0|sector_id, data = earp_did)
summary(num_establishment)

total_employee <- felm (total_employee ~ p0_2014*earp + p1_2017*earp + p2_2020*earp|village_id + cell_year|0|sector_id, data = earp_did)
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

#Employee number----



##12_14----
elec12_14 <- rwa_regress %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )
table(elec12_14$year)

village_fe <- feols(total_employee ~ p0_2014*elec12_14 + p1_2017*elec12_14 + p2_2020*elec12_14|village_id + year, data = elec12_14)
district_fe <- feols(total_employee ~ p0_2014*elec12_14 + p1_2017*elec12_14 +p2_2020*elec12_14|village_id + distr_year, data = elec12_14)
sector_fe <- feols(total_employee ~ p0_2014*elec12_14 + p1_2017*elec12_14 +p2_2020*elec12_14|village_id + sector_year, data = elec12_14)

model12_14 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)
modelsummary(
  regs,
  output = "regression_output.latex",
  title = "Regression Results"
)


##15_17----






elec15_17 <- rwa_regress %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  )


village_fe <- feols(total_employee ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +year, data = elec15_17)
district_fe <- feols(total_employee ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +distr_year, data = elec15_17)
sector_fe <- feols(total_employee ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +sector_year, data = elec15_17)

model15_17 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model15_17,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2015-2017 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)


##18_20----
elec18_20 <- rwa_regress %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  )

village_fe <- feols(total_employee ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +year, data = elec18_20)
district_fe <- feols(total_employee ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +distr_year, data = elec18_20)
sector_fe <- feols(total_employee ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +sector_year, data = elec18_20)

model18_20 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model18_20,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2018-2020 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)



#Balance table regressions----



#2011 establishment census------


ec_2011 <- read_xlsx(path = file.path(data_path, "2011", "village_level_2011.xlsx"))

ec_2011 <- ec_2011 %>% 
  mutate(village_id = as.character(village_id)) %>% 
  select(village_id, num_establishment, total_employee)

expansion_ec <- left_join(expansion_join, ec_2011, by = c("village_id"))

expansion_ec <- expansion_ec %>% 
  filter(! District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu")) %>% 
  mutate(across(c("num_establishment", "total_employee"), 
                ~ replace_na(., 0))) %>% 
  mutate(
    num_establishment = pmin(num_establishment, quantile(num_establishment, 0.99, na.rm = TRUE)),
    total_employee = pmin(total_employee, quantile(total_employee, 0.99, na.rm = TRUE))
  ) %>% 
  mutate(cell_id = as.numeric(cell_id),
         sector_id = as.numeric(sector_id)) %>% 
  rename(earp = earp_2011)

elec_2013 <- electrified_year %>% 
  select(village_id, electrified_2013)

expansion_ec <- left_join(expansion_ec, elec_2013)

expansion_ec <- expansion_ec %>% 
  mutate(
    earp = ifelse(electrified_2013 == 1, 0, earp)
  )
  
  
  
  
  
write_dta(expansion_ec, path = file.path(data_path, "expansion_ec.dta"))

##Regression-----

##sector_id----
n_establishment <- felm(num_establishment ~ earp + electrified_2013|sector_id|0|sector_id, data = expansion_ec )

summary(n_establishment)
employee <- felm(total_employee ~ earp + electrified_2013|sector_id|0|sector_id, data = expansion_ec )

summary(employee)


regs <- list(
  "Number of Establishments" = n_establishment,
  "Number of Employees" = employee
)

stargazer( 
  regs,
  type = "latex", 
  title = "Regression Results", 
  out = file.path(output_path, "regression_output.latex")
)



##cell_id----
n_establishment <- felm(num_establishment ~ earp + electrified_2013|cell_id|0|sector_id, data = expansion_ec )
summary(n_establishment)

employee <- felm(total_employee ~ earp +electrified_2013|cell_id|0|sector_id, data = expansion_ec )
summary(employee)


regs <- list(
  "Number of Establishments" = n_establishment,
  "Number of Employees" = employee
)

stargazer( 
  regs,
  type = "latex", 
  title = "Regression Results", 
  out = file.path(output_path, "regression_output.latex")
)

expansion_ec %>%
  filter(earp == 0) %>%
  summarise(
    n= n(),
    mean_num_establishment = mean(num_establishment, na.rm = TRUE),
    mean_total_employee  = mean(total_employee, na.rm = TRUE)
  )


