##################################
#2025-9-23 distance to lv 
#Xiaoming
########################################

pacman::p_load(knitr, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr, googlesheets4)
library(googlesheets4)
getwd()


#Prepare data------

##complete------


complete  <- read_xlsx(path = file.path(data_path_2, "survey status of vulnerable households in sample villages_final.xlsx"))

complete_survey <- complete %>% 
  filter(`Completed by Lattanzio` == "Yes")

village_181 <- complete %>% 
  filter(`Dropped from scope due to 15kv` == "No") %>% 
  distinct(villageid_key)

##hfc as sf-------

hfc_output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/HFC/output"
)



hfc_constr_raw <- read_xlsx(file.path(hfc_output_path, "hfc_constr_0728.xlsx"))


hfc_constr <- hfc_constr_raw %>% 
  filter(consent == 1) %>%
  filter(!is.na(A1_1)) %>%
  distinct(hh_head_name, hh_id, A1_2, A1_3, .keep_all = TRUE) %>%
  filter(village %in% village_181$villageid_key) %>%
  group_by(hh_id) %>%
  arrange(desc(hh_head_name %in% complete$hh_head_name)) %>% 
  slice(1) %>%
  ungroup()

hfc_sf <- st_as_sf(hfc_constr, coords = c("coordinate.Longitude", "coordinate.Latitude"), crs = 4326)
hfc_sf <- st_make_valid(hfc_sf)


##household survey------

baseline_data <- file.path(dropbox,  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/data")
rwa_villages <- st_read(dsn = file.path(baseline_data, "rwa_villages", "Village.shp"))
rwa_villages <- st_make_valid(rwa_villages)
rwa_villages <- st_transform(rwa_villages, crs = 4326)

rwa_district <- st_read(dsn = file.path(baseline_data, "rwa_district", "District.shp"))
rwa_district <- st_make_valid(rwa_district)
rwa_district <- st_transform(rwa_district, crs = 4326)

sample_villages <- rwa_villages %>%
  mutate(District = str_to_title(District)) %>% 
  filter(District %in% c("Karongi", "Rutsiro", "Rulindo", "Rusizi"))



##Lv line read------

karongi_lv <- st_read(file.path(baseline_data,"Karongi Surveyed 0116","Surveyed_LV_Lines.shp"), quiet=TRUE)
rulindo_lv <- st_read(file.path(baseline_data,"Rulindo Surveyed 0116","Surveyed_LV_Lines.shp"), quiet=TRUE)
rutsiro_lv <- st_read(file.path(baseline_data,"Rutsiro Surveyed 0116","Surveyed_LV_Lines.shp"), quiet=TRUE)

# Transform to match villages; compute length in target CRS units
crs_target <- st_crs(rwa_villages)
karongi_lv <- karongi_lv %>% st_transform(crs_target) %>% transmute(length = as.numeric(st_length(geometry)), geometry)
rulindo_lv <- rulindo_lv %>% st_transform(crs_target) %>% transmute(length = as.numeric(st_length(geometry)), geometry)
rutsiro_lv <- rutsiro_lv %>% st_transform(crs_target) %>% transmute(length = as.numeric(st_length(geometry)), geometry)

# Combine + (optional) join to villages
lv_line <- bind_rows(karongi_lv, rulindo_lv, rutsiro_lv)


#hfc_constr distance to nearest lv line

lv_line <-st_zm(lv_line)

hfc_sf <- st_transform(hfc_sf, crs = st_crs(rwa_villages))

dist_matrix <- st_distance(hfc_sf, lv_line)

dist_matrix <- as.data.frame(dist_matrix)


dist_matrix$min_meter <- apply(dist_matrix, 1, min, na.rm = TRUE)

dist <- dist_matrix %>% 
  select(min_meter) %>% 
  mutate(min_meter = round(min_meter,2))



hfc_sf_regress <- hfc_sf %>%
  mutate(distance_to_lv = dist) %>% 
  st_drop_geometry() %>% 
  mutate(
    distance_to_lv = as.numeric(unlist(distance_to_lv)),
    # distance_to_lv = distance_to_lv / 1000  # Convert meters to kilometers
  ) %>% 
  select(distance_to_lv, hh_id) %>% 
  rename(household_id = hh_id)


#Join back to master-----
master <- left_join(master, hfc_sf_regress)

distance_regress <- master %>% 
  filter(!is.na(distance_to_lv)) %>% 
  rename(village_id = villageid_key) %>% 
  mutate(
    cell_id    = substr(village_id, 1, 6),
    sector_id  = substr(village_id, 1, 4),
    district_id= substr(village_id, 1, 2)
  ) 


lm_regress <- lm(readyboard ~ distance_to_lv, data = distance_regress)
summary(lm_regress)

felm_regress <- felm(readyboard ~ distance_to_lv | village_id | 0 |cell_id, data = distance_regress)
summary(felm_regress)

library(huxtable)
huxreg(
  "Readyboard" = felm_regress,
  error_pos = "below",
  number_format = 3
) %>%
  add_rows(c("Village FE", "Yes")) %>%
  add_rows(c("Cluster: Cell ID", "Yes")) %>%
  set_caption("Regression of Readyboard on Distance to LV (with Village FE)") %>%
  print_screen()
