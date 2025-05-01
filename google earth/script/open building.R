##########################
#Author: Xiaoming Zhang
#Date: March 25th 2025
#Purpose: Google earth engine
################################




pacman::p_load(knitr, raster, lfe,fixest, modelsummary, stargazer, tidyverse, dplyr, here, sf, haven, ggplot2, readxl,  writexl, janitor, randomizr, RCT, purrr, RODBC, DBI)

getwd()


dropbox <- 'C:/Users/wb614406/Dropbox'

data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/admin_data"
)

library(raster)

# Replace the path with the actual downloaded file path
rwanda_buildings <- raster(file.path(data_path, "rwanda_openbuildings_mask.tif"))

# Plot the raster
plot(rwanda_buildings, main = "Open Buildings Mask - Rwanda")


rwa_district <- st_read(dsn = file.path(data_path, "rwa_district", "District.shp"))

rwa_district <- st_transform(rwa_district, crs = crs(rwanda_buildings))


# Filter to target districts
target_districts <- rwa_district %>%
  filter(District %in% c("Karongi", "Rutsiro", "Rulindo", "Rusizi"))

# Convert sf to Spatial for raster masking
target_districts_sp <- as_Spatial(target_districts)

# Crop raster to extent of the four districts
rwanda_buildings_crop <- crop(rwanda_buildings, extent(target_districts_sp))

# Mask raster to include only the area within those districts
rwanda_buildings_masked <- mask(rwanda_buildings_crop, target_districts_sp)

# Plot the result
plot(rwanda_buildings_masked, main = "Buildings in Karongi, Rutsiro, Rulindo, Rusizi")

# Save the cropped and masked raster (optional)
writeRaster(rwanda_buildings_masked, 
            filename = file.path(data_path, "rwanda_buildings_0.7.tif"), 
            format = "GTiff", overwrite = TRUE)



#Over 0.65 significant------


# Replace the path with the actual downloaded file path
rwanda_buildings <- raster(file.path(data_path, "rwanda_openbuildings_mask65.tif"))

# Plot the raster
plot(rwanda_buildings, main = "Open Buildings Mask - Rwanda")


rwa_district <- st_read(dsn = file.path(data_path, "rwa_district", "District.shp"))

rwa_district <- st_transform(rwa_district, crs = crs(rwanda_buildings))

district <- c("Karongi", "Rutsiro", "Rulindo", "Rusizi")


# Loop over each district
for (d in district) {
  message("Processing: ", d)
  
  # Subset district
  this_district <- rwa_district %>% filter(District == d)
  this_district_sp <- as_Spatial(this_district)
  
  # Crop and mask
  buildings_crop <- crop(rwanda_buildings, extent(this_district_sp))
  buildings_masked <- mask(buildings_crop, this_district_sp)
  
  # Plot result
  plot(buildings_masked, main = paste("Buildings in", d))
  
  writeRaster(buildings_masked, 
              filename = file.path(data_path,paste0(d, "_buildings_0.65.tif")), 
              format = "GTiff", overwrite = TRUE)
  
}







#Running by separate districts------



rwa_villages <- st_read(dsn = file.path(data_path,"rwa_villages", "Village.shp"))


rwa_villages <- st_make_valid(rwa_villages)


karongi_villages <- rwa_villages %>% 
  filter(District == "Karongi" )


karongi_building <-  raster(file.path(data_path, "karongi_openbuildings_2.5.tif"))
plot(karongi_building)

karongi_villages <- st_transform(karongi_villages, crs = st_crs(karongi_building))

karongi_rubengera <- karongi_villages %>% 
  filter(Sector == "Rubengera" & Name == "Bigugu")

karongi_rubengera_sp <- as_Spatial(karongi_rubengera)

karongi_buildings_crop <- crop(karongi_building, extent(karongi_rubengera_sp))

karongi_buildings_masked <- mask(karongi_buildings_crop, karongi_rubengera_sp)

# Plot the result
plot(karongi_buildings_masked)

