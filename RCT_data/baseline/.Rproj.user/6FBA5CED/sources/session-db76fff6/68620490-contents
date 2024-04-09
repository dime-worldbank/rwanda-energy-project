##############
#Author: Xiaoming Zhang
#Date: 2.28.2024
#Purpose: Clean cooking stove 
#############


pacman::p_load(tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, stargazer, olsrr)

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

output_path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/outputs"
)


#read files
rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village_area.shp"))

rwa_villages <- st_make_valid(rwa_villages)
