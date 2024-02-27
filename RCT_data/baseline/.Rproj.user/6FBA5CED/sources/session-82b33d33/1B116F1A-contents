##############
#Author: Xiaoming Zhang
#Date: 2.26.2024
#Purpose: join Rutsiro new scope
#############


pacman::p_load(tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr)

getwd()

#Dropbox----


if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

#Method One----
path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/data/data/four_district_2401.xlsx"
)

#read file----
four_district_2401 <- read_xlsx(path)

rutsiro_lv <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/data/data/Rutsiro Surveyed 0116/Surveyed_LV_Lines.shp"
)

rutsiro_lv <- st_read(dsn = rutsiro_lv)

rwa_villages <- st_read(dsn =  file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/data/data/rwa_villages/Village.shp"
))

#intersect with rwa_villages----
rutsiro_lv <- st_transform(rutsiro_lv, crs = st_crs(rwa_villages))

rutsiro_scope <- st_intersection(rutsiro_lv, rwa_villages)

rutsiro_scope <- rutsiro_scope %>% 
  mutate(Village_ID = as.character(Village_ID )) 

rutsiro_scope <- rutsiro_scope %>% 
  mutate(scope_2402 = 1) %>% 
  select(Village_ID, scope_2402)

rutsiro_scope <- rutsiro_scope %>% 
  st_drop_geometry() 

rutsiro_scope <- rutsiro_scope %>% 
  distinct(Village_ID, .keep_all = TRUE)


four_district_2402 <- left_join(four_district_2401, rutsiro_scope, by = c("village_id" = "Village_ID"))

four_district_2402 <- four_district_2402 %>% 
  mutate(scope_2402 = ifelse(district %in% c("Rutsiro"), scope_2402, scope_2401))

write_xlsx(four_district_2402, path = file.path(
  DROPBOX,"Rwanda Energy/datawork/RCT_data","baseline/data/data/four_district_2402.xlsx" ))

#See the scope_clean results----


four_district_2402.1 <- four_district_2402 %>% 
  mutate(any_offgrid = case_when(
    et_sum != 0 | priority_0 == 0 ~ "partial",
    .default = "newly"
  ))

four_district_2402.1 <- four_district_2402.1 %>% 
  mutate(
    any_grid = case_when(
      grid_status %in% c("newly") & nep_revision%in% c("GE", "Fill In")  ~ "newly",
      .default = "partial"
    )
  )

four_district_2402.1 <- four_district_2402.1  %>% 
  mutate(status = case_when(
    any_grid == "newly" & any_offgrid == "newly" ~ "newly",
    .default = "partial"
  ))


four_district_2402.1  <- four_district_2402.1 %>% 
  mutate(
    status = ifelse(ubudehe_1 <20, "partial", status)
  )

four_district_2402.1 <- four_district_2402.1 %>% 
  mutate(meter_percent = round(meter_eucl/total_hh, 2))


#Karongi+Rutsiro+Rulindo----

three_scope_2402.1 <- four_district_2402.1 %>% 
  filter(district %in% c("Karongi", "Rulindo", "Rutsiro") & scope_2402 == 1)


table(three_scope_2402.1$status)

#30% percent of meter and 20 ubudehe

three_scope_2402.2 <- three_scope_2402.1 %>% 
  filter(meter_percent < 0.3 & ubudehe_1 >= 20)

table(three_scope_2402.2$status)  

three_scope_2402.2 %>% 
  filter(status == "newly") %>% 
  group_by(district) %>% 
  summarise(
    village_n = n(),
    ubudehe_1 = village_n * 20
  )

#check rutsiro-----
rutsiro_scope_2402 <- three_scope_2402.1 %>% 
  filter(district %in% c("Rutsiro"))

