#####################
#Author: Xiaoming Zhang
#Date: March 27th 2025
#Purpose: distance to building
#######################################


## libraries

library(tidyverse)
library(lubridate)
library(googlesheets4)
library(readxl)
library(dplyr)
library(tidyr)
library(sf)

#Read buildings----

karongi_building <-  st_read(dsn = file.path(data_path, "karongi_openbuildings_2.5.shp"))
karongi_building <- karongi_building %>% 
  filter(DN == 1)

rutsiro_building <- st_read(dsn = file.path(data_path, "rutsiro_openbuildings_2.5.shp"))
rutsiro_building <- rutsiro_building %>% 
  filter(DN == 1)

rulindo_building <- st_read(dsn = file.path(data_path, "rulindo_openbuildings_2.5.shp"))
rulindo_building <- rulindo_building %>% 
  filter(DN == 1)

rusizi_building <- st_read(dsn = file.path(data_path, "rusizi_openbuildings_2.5.shp"))
rusizi_building <- rusizi_building %>% 
  filter(DN == 1)

buildings <- rbind(karongi_building, rutsiro_building, rulindo_building, rusizi_building)

buildings <- buildings %>% 
  filter(DN == 1)


#HFC sf-----
hfc_sf <- hfc_constr %>% 
  st_as_sf(coords = c("coordinate.Longitude", "coordinate.Latitude"), crs = 4326)  # WGS84

#Rwa villages----

rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))


rwa_villages <- st_make_valid(rwa_villages)

rwa_villages <- st_transform(rwa_villages, crs = st_crs(hfc_sf))


sample_villages <- rwa_villages %>% 
  filter(Village_ID %in% village_check$villageid_key)

hfc_sf <- st_intersection(hfc_sf, sample_villages)

hfc_sf_clean <- hfc_sf %>% 
  select(hh_id, District, Distr_ID, Sector, Sector_ID, Cell, Cell_ID, Name, Village_ID)
#Filter only sample villages for the four districts


#Function----
match_hh_to_nearest_building <- function(district_name, hfc_sf, sample_villages, building_sf) {
  
  # Filter for the district
  sample <- sample_villages %>% 
    filter(District == district_name)
  
  sample_building <- st_intersection(building_sf, sample) %>%
    group_by(Village_ID) %>%
    mutate(building_id = paste0(Village_ID, "_", row_number())) %>%
    ungroup()
  
  hfc <- hfc_sf %>% 
    filter(District == district_name)
  
  # Return NULL if there's no data
  if (nrow(sample_building) == 0 | nrow(hfc) == 0) return(NULL)
  
  # Nearest building matching
  nearest_index <- st_nearest_feature(hfc, sample_building)
  nearest_geom <- st_geometry(sample_building)[nearest_index]
  dist_to_nearest <- st_distance(st_geometry(hfc), nearest_geom, by_element = TRUE)
  
  # Add results
  hfc <- hfc %>%
    mutate(
      nearest_building_id = sample_building$building_id[nearest_index],
      nearest_building_geom = nearest_geom,
      dist_to_nearest_building = dist_to_nearest
    )
  
  return(hfc)
}

karongi_hfc <- match_hh_to_nearest_building("Karongi",  hfc_sf_clean, sample_villages, karongi_building)
rutsiro_hfc <- match_hh_to_nearest_building("Rutsiro", hfc_sf_clean, sample_villages, rutsiro_building)
rulindo_hfc <- match_hh_to_nearest_building("Rulindo", hfc_sf_clean, sample_villages, rulindo_building)
rusizi_hfc  <- match_hh_to_nearest_building("Rusizi",  hfc_sf_clean, sample_villages, rusizi_building)

hfc_buildings <- rbind(karongi_hfc, rutsiro_hfc, rulindo_hfc, rusizi_hfc)

hfc_buildings <- hfc_buildings %>% 
  mutate(dist_to_nearest_building = round(dist_to_nearest_building, 2))

n_distinct(hfc_buildings$hh_id)
n_distinct(hfc_buildings$nearest_building_id)



hfc_buildings <- hfc_buildings %>%
  mutate(dist_m = as.numeric(dist_to_nearest_building) ) %>% 
  mutate(dist_m = ifelse(dist_m >= 50, 50, dist_m))

# Plot histogram----
ggplot(hfc_buildings, aes(x = dist_m)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(
    title = "Distance to Nearest Sample Building",
    x = "Distance (meters)",
    y = "Number of Households"
  ) +
  theme_minimal()




