##
#Author: Xiaoming Zhang
#Date: 06112025
#Topic: Reading and writing files in mega


pacman::p_load(knitr, lfe,fixest, modelsummary, stargazer, tidyverse, dplyr, here, sf, haven, ggplot2, readxl,  writexl, janitor, randomizr, RCT, purrr, RODBC, DBI)

getwd()


data_path <- file.path(
  "C:/Users/wb614406/OneDrive - WBG/rwandaenergy - Documents/historical expansion/data"
)

output_path <- file.path(
  "C:/Users/wb614406/OneDrive - WBG/rwandaenergy - Documents/historical expansion/outputs"
)


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
  type = "latex",
  out = file.path(output_path, "event study.tex"),
  title = "Regression Results"
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

