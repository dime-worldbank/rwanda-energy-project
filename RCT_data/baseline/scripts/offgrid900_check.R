##############
#Author: Xiaoming Zhang
#Date: 8.28. 2024
#Purpose: Randomization primary construction
#############


pacman::p_load(knitr, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr)

getwd()

#Dropbox----


if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}


path <- file.path(
  DROPBOX,
  "Rwanda Energy/EAQIP/datawork/RCT_data",
  "baseline/data/data/four_district_2402.xlsx"
)


data_path <- file.path(
  DROPBOX,
  "Rwanda Energy/EAQIP/datawork/RCT_data",
  "baseline/data/data"
)

scope_path <- file.path(
  DROPBOX,
  "Rwanda Energy/EAQIP/datawork/RCT_data",
  "baseline/data/Updated scope villages& households"
)

output_path <- file.path(
  DROPBOX,
  "Rwanda Energy/EAQIP/datawork/RCT_data",
  "baseline/outputs"
)



four_scope_newly <- read_xlsx(path = file.path(scope_path, "scope_193_0807.xlsx"))
offgrid_900 <- read_xlsx(path = file.path(data_path, "Additional 900 Off grid villages_Prioriy 2- NEP 2023.xlsx"))



rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))

village_join <- rwa_villages %>%
  clean_names() %>%
  select(village_id, district, sector, cell, name) %>%
  st_drop_geometry()

offgrid_900 <- left_join(offgrid_900, village_join, by = c("District" = "district",
                                                                 "Sector" = "sector",
                                                                 "Cellule" = "cell",
                                                                 "Village" = "name"))
sum(is.na(offgrid_900$village_id))


filter <- four_scope_newly %>% 
  filter(village_id %in% offgrid_900$village_id)





