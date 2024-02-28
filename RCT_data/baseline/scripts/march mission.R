##############
#Author: Xiaoming Zhang
#Date: 2.26.2024
#Purpose: join Rutsiro new scope
#############


pacman::p_load(tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr)

getwd()

#Dropbox path----


if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/data/data"
)


#read files
rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village_area.shp"))

karongi_LV_surveyed <- st_read(dsn = here("data", "Karongi Surveyed 0116", "Surveyed_LV_Lines.shp"))
rulindo_LV_surveyed <- st_read(dsn = here("data", "Rulindo Surveyed 0116", "Surveyed_LV_Lines.shp"))
rutsiro_LV_surveyed <- st_read(dsn = here("data", "Rulindo Surveyed 0116", "Surveyed_LV_Lines.shp"))

existing_LV <- st_read(dsn = here("data","Existing Electrical Network_2022", "Existing_LVLine.shp"))
existing_MV <- st_read(dsn = here("data","Existing Electrical Network_2022", "Existing_MVLine.shp"))
existing_HV <- st_read(dsn = here("data","Existing Electrical Network_2022", "Existing_HVLine.shp"))
karongi_meter <- st_read(dsn = here("data", "eucl meter", "karongi_meter.shp"))
rulindo_meter <- st_read(dsn = here("data", "eucl meter", "rulindo_meter.shp"))
rutsiro_meter <- st_read(dsn = here("data", "eucl meter", "rutsiro_meter.shp"))

existing_LV <- st_transform(existing_LV, crs = st_crs(rwa_villages))
existing_MV <- st_transform(existing_MV, crs = st_crs(rwa_villages))
existing_HV <- st_transform(existing_HV, crs = st_crs(rwa_villages))

rulindo_LV_surveyed <- st_transform(rulindo_LV_surveyed, crs =  st_crs(rwa_villages))
karongi_LV_surveyed <- st_transform(karongi_LV_surveyed, crs =  st_crs(rwa_villages))

karongi_meter <- st_transform(karongi_meter, crs = st_crs(rwa_villages))
rulindo_meter <- st_transform(rulindo_meter, crs = st_crs(rwa_villages))
rutsiro_meter <- st_transform(rutsiro_meter, crs = st_crs(rwa_villages))

