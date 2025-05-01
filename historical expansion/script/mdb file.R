##
#Author: Xiaoming Zhang
#Date: 02182025
#Topic: reading the historical expansion files shared by Clementine


pacman::p_load(knitr, lfe, stargazer, tidyverse, dplyr, here, sf, ggplot2, readxl,  writexl, janitor, randomizr, RCT, purrr, RODBC, DBI)

getwd()

# install.packages("RODBC")
# install.packages("DBI")

library(RODBC)
library(DBI)

dropbox <- 'C:/Users/wb614406/Dropbox'

data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Historical Expansion"
)

rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))
rwa_villages <- st_make_valid(rwa_villages)

rwa_district <- st_read(dsn = file.path(data_path, "rwa_district", "District.shp"))
rwa_cell <- st_read(dsn = file.path(data_path, "rwa_cell", "Cell.shp"))

rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))
rwa_villages <- st_make_valid(rwa_villages)





datab<-file.path( data_path, "EDCL EARP Program/OneDrive_2024-10-07/SOFRECO 1/Deliverables Combined Report/GDB_All/EWSA_Rwanda_Combined_Zones.mdb")
channel<-odbcConnectAccess2007(datab)


#New method-----

planned_mv<-sqlFetch(channel,"Planned_MVLines_COMBINED_Shape_Index")

create_line <- function(minx, miny, maxx, maxy) {
  coords <- matrix(c(minx, miny, 
                     maxx, maxy), 
                   ncol = 2, byrow = TRUE)
  st_linestring(coords)
}

lines <- mapply(
  create_line,
  planned_mv$MinGX, planned_mv$MinGY,
  planned_mv$MaxGX, planned_mv$MaxGY,
  SIMPLIFY = FALSE
)

lines_sfc <- st_sfc(lines)


planned_mv_sf <- st_sf(planned_mv, geometry = lines_sfc)

st_crs(planned_mv_sf) <- EPSG:32735

planned_mv_sf <- st_transform(planned_mv_sf, st_crs(rwa_district))

View(planned_mv_sf)

ggplot() +
  geom_sf(data = rwa_district, aes(color = "RWA District"), fill = NA, color = "black") +
  geom_sf(data = planned_mv_sf, aes(color = "Planned MV"), color = "red") +
  theme_minimal()


ggplot() +
  geom_sf(data = rwa_district, aes(color = "RWA District"), fill = NA) +
  geom_sf(data = planned_mv_sf, aes(color = "Planned MV"), fill = NA) +
  geom_sf(data = mv_22, aes(color = "MV 22"), fill = NA) +
  scale_color_manual(name = "Data Source",
                     values = c("RWA District" = "black",
                                "Planned MV" = "red",
                                "MV 22" = "blue")) +
  labs(title = "Comparison of Planned and Existing MV Line in Rwanda",
       subtitle = "Overlay of District map, Planned MV 2010, and MV Network 2022",
  ) +
  theme_minimal()



#Planned MV----


#Scale code-----
bbox_planned <- st_bbox(planned_mv_sf)
bbox_planned


bbox_planned <- st_bbox(rwa_district)
bbox_planned

# Calculate scale factors
scale_x <- (xmax_rwa - xmin_rwa) / (xmax_mv - xmin_mv)
scale_y <- (ymax_rwa - ymin_rwa) / (ymax_mv - ymin_mv)



planned_mv<-sqlFetch(channel,"Planned_MVLines_COMBINED_Shape_Index")

create_line <- function(minx, miny, maxx, maxy) {
  coords <- matrix(c(minx*0.01 * 0.9563277, miny*0.01 *0.9444023, 
                     maxx*0.01 *0.9563277, maxy*0.01 *0.9444023), 
                   ncol = 2, byrow = TRUE)
  st_linestring(coords)
}

lines <- mapply(
  create_line,
  planned_mv$MinGX, planned_mv$MinGY,
  planned_mv$MaxGX, planned_mv$MaxGY,
  SIMPLIFY = FALSE
)

lines_sfc <- st_sfc(lines)


planned_mv_sf <- st_sf(planned_mv, geometry = lines_sfc, crs= st_crs(rwa_district))
st_crs(rwa_district)

planned_mv_sf <- st_transform(planned_mv_sf, st_crs(rwa_district))




shift_vector <- c(-10682.8, -67329.9) 
st_geometry(planned_mv_sf) <- st_geometry(planned_mv_sf) + shift_vector
st_crs(planned_mv_sf) <- st_crs(rwa_district) 


  
  ggplot() +
    geom_sf(data = rwa_district, aes(color = "RWA District"), fill = NA) +
    geom_sf(data = planned_mv_sf, aes(color = "Planned MV"), fill = NA) +
    geom_sf(data = mv_22, aes(color = "MV 22"), fill = NA) +
    scale_color_manual(name = "Data Source",
                       values = c("RWA District" = "black",
                                  "Planned MV" = "red",
                                  "MV 22" = "blue")) +
    labs(title = "Comparison of Planned and Existing MV Line in Rwanda",
         subtitle = "Overlay of District map, Planned MV 2010, and MV Network 2022",
         ) +
    theme_minimal()

  
#Overlay earp mv with village----

village_mv_earp <- st_intersection(rwa_villages, planned_mv_sf)
  
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

electrified_2022 <- lm(electrified_2022 ~ electrified_2011 + earp_2011, data = expansion_join, family = binomial)

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

#Cell_ID------


electrified_2022 <- felm(electrified_2022 ~ electrified_2011 + earp_2011|cell_id, data = expansion_join_1)

summary(electrified_2022)


electrified_2022 <- felm(electrified_2011 ~earp_2011|cell_id, data = expansion_join)

summary(electrified_2022)

mv_exist_2022 <- felm(exist_mv ~ electrified_2011 + earp_2011|cell_id, data = expansion_join)

summary(mv_exist_2022)

nep_reg<- felm(nep_grid ~ electrified_2011 + earp_2011|cell_id, data = expansion_join)

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

#sector_ID----


electrified_2022 <- felm(electrified_2022 ~ electrified_2011 + earp_2011|sector_id, data = expansion_join)

summary(electrified_2022)


mv_exist_2022 <- felm(exist_mv ~ electrified_2011 + earp_2011|sector_id, data = expansion_join)

summary(mv_exist_2022)

nep_reg<- felm(nep_grid ~ electrified_2011 + earp_2011|sector_id, data = expansion_join)

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



#district_ID----




electrified_2022 <- felm(electrified_2022 ~ electrified_2011 + earp_2011|district_id, data = expansion_join)

summary(electrified_2022)


mv_exist_2022 <- felm(exist_mv ~ electrified_2011 + earp_2011|district_id, data = expansion_join)

summary(mv_exist_2022)

nep_reg<- felm(nep_grid ~ electrified_2011 + earp_2011|district_id, data = expansion_join)

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
#Cross_tab-------


tab <- expansion_join %>% 
  group_by(electrified_2011) %>%
  rename(electrified_2011_utility = electrified_2011) %>% 
  mutate(
    electrified_2011_utility = ifelse(electrified_2011_utility == 0, "Not electrified 2011", "Electrified_2011")
  ) %>% 
  summarise(
    earp_mv_2011 = sum(earp_2011 == 1),
    no_earp_mv_2011 = sum(earp_2011 == 0)
  ) %>% 
  mutate(
    percentage_earp = paste0(round(earp_mv_2011/(earp_mv_2011 + no_earp_mv_2011), 4)*100, "%" )
  )
