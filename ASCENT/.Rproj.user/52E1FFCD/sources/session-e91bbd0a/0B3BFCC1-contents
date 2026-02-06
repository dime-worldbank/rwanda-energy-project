#############
#Author: Xiaoming Zhang
#Date: 03252025
#Topic: Join Sector
###################

pacman::p_load(knitr, lfe,fixest, modelsummary, stargazer, tidyverse, dplyr, here, sf, haven, ggplot2, readxl,  writexl, janitor, randomizr, RCT, purrr, RODBC, DBI)

getwd()

dropbox <- 'C:/Users/wb614406/Dropbox'

data_path <- file.path(
  dropbox,
  "Rwanda Energy/ASCENT/datawork/data"
)

output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/outputs"
)


#Join sector 

target <- read_xlsx(path = file.path(data_path, 
                                     "Target_Sectors_ePC.xlsx"))


target <- target %>% 
  filter(
    !is.na(Sectors)
  )


rwa_sector <- st_read(dsn = file.path(data_path, "rwa_sector", "Sector.shp"))


rwa_sector <- rwa_sector %>% 
  st_drop_geometry() %>% 
  select(Province, District, Name)


target <- left_join(target, rwa_sector, by = c("District" = "District", "Sectors" = "Name"))

target <- target %>% 
  select(Province, everything())


write_xlsx(target, path = file.path(data_path, "Target_Sectors_ePC_format.xlsx"))







