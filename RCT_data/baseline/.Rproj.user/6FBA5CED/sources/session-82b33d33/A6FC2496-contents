##############
#Author: Xiaoming Zhang
#Date: 2.27.2024
#Purpose: Read Rusizi meter and integrate into the four district 
#############


pacman::p_load(tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr)

getwd()

#Dropbox----


if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

#Method One----
data_path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/data/data"
)

#Read the rusizi meter data 
four_district_2402 <- read_xlsx(path = file.path(data_path, "four_district_2402.xlsx"))
rusizi_meter <- st_read(dsn = file.path(data_path, "eucl meter", "rusizi_meter.shp"))

rusizi_meter <- rusizi_meter %>% 
  st_drop_geometry() %>% 
  group_by(Village_ID) %>% 
  summarise(rusizi_meter = n())

four_district_2402.1 <- left_join(four_district_2402, rusizi_meter, by = c("village_id" = "Village_ID"))

four_district_2402.1 <- four_district_2402.1 %>% 
  mutate(meter_eucl = ifelse(district %in% c("Rusizi"), rusizi_meter, meter_eucl))

four_district_2402.1 <- four_district_2402.1 %>% 
  mutate(meter_eucl = ifelse(is.na(meter_eucl), 0, meter_eucl)) %>% 
  select(-rusizi_meter)

four_district_2402.1 <- four_district_2402.1 %>% 
  mutate(scope_2402 = ifelse(is.na(scope_2402), 0, scope_2402))

#integrate master file

write_xlsx(four_district_2402.1, path = file.path(data_path, "four_district_2402.xlsx"))
