##########################
#Author: Xiaoming Zhang
#Date: 02132024
#purpose:primary cleaning for the usage data
############################


#library----

pacman::p_load(tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, haven, stringr)


#read file----

if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX, "Rwanda Energy/datawork/Historical data"
)

usage_1 <- read_xlsx(path = file.path(data_path, "EUCL cons usage_1.xlsx"))
usage_2 <- read_xlsx(path = file.path(data_path, "EUCL cons usage_2.xlsx"))

rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))

village_list <- rwa_villages %>% 
  st_drop_geometry()
#usage_1 clean----


usage_1_clean <- usage_1 %>% 
  mutate(
    District = str_to_title(District),
    Sector = str_to_title(Sector),
    Cell = str_to_title(Cell),
    Village = str_to_title(Village)
    )

usage_1_clean <- usage_1_clean %>% 
  mutate(
    Village = ifelse(Village %in% c("--"), NA, Village),
    Cell = ifelse(Cell %in% c("--"), NA, Cell),
    Sector = ifelse(Sector %in% c("--"), NA, Sector)
  )

#usage_2 clean----


usage_2_clean <- usage_2 %>% 
  mutate(
    District = str_to_title(District),
    Sector = str_to_title(Sector),
    Cell = str_to_title(Cell),
    Village = str_to_title(Village)
  )

usage_2_clean <- usage_2_clean %>% 
  mutate(
    Village = ifelse(Village %in% c("--"), NA, Village),
    Cell = ifelse(Cell %in% c("--"), NA, Cell),
    Sector = ifelse(Sector %in% c("--"), NA, Sector)
  )


usage_2_test <- usage_2_clean %>% 
  filter(is.na(Cell))

if (all(is.na(usage_2_test$Village) )) {
  print("No Violations found:")
} else {
  print("Violations found.")
}



village_missing <- (sum(is.na(usage_1_clean$Village)) + sum(is.na(usage_2_clean$Village)))/ (nrow(usage_1_clean)+ nrow(usage_2_clean))
cell_missing <- (sum(is.na(usage_1_clean$Cell)) + sum(is.na(usage_2_clean$Cell)))/ (nrow(usage_1_clean)+ nrow(usage_2_clean))
sector_missing <- (sum(is.na(usage_1_clean$Sector)) + sum(is.na(usage_2_clean$Sector)))/ (nrow(usage_1_clean)+ nrow(usage_2_clean))
district_missing <- (sum(is.na(usage_1_clean$District)) + sum(is.na(usage_2_clean$District)))/ (nrow(usage_1_clean)+ nrow(usage_2_clean))




#Check on the time----


year <- usage_1_clean %>% 
  select(ends_with("Usage(Kwh)"))



year <- year %>%
  mutate(
    across(everything(), ~ ifelse(. == 0.0, NA, .))
  )

year$first <- apply(year,1,function(x) names(which(which(!is.na(x))>0))[1])

year$year_first <- substr(year$first, 1, 4)


usage_1_clean$year_first <- year$year_first

strange_1 <- usage_1_clean %>% 
  filter(as.numeric(`Meter installed year`) >  as.numeric(year_first))



#Check on the time----


year_2 <- usage_2_clean %>% 
  select(ends_with("Usage(Kwh)"))



year_2 <- year_2 %>%
  mutate(
    across(everything(), ~ ifelse(. <= 0.0, NA, .))
  )

year_2$first <- apply(year_2,1,function(x) names(which(which(!is.na(x))>0))[1])

year_2$year_first <- substr(year_2$first, 1, 4)


usage_2_clean$year_first <- year_2$year_first

strange_2 <- usage_2_clean %>% 
  filter(as.numeric(`Meter installed year`) >  as.numeric(year_first))

write_xlsx(list("Usage_1" = strange_1, "Usage_2" = strange_2), path = file.path(data_path,"usage year mismatch.xlsx"))

#join two usage----

usage <- bind_rows(usage_1_clean, usage_2_clean)


write.csv(usage, file = file.path(data_path, "usage_bind_raw.csv"))


n_distinct(usage$`Meter ID`)

duplicate <- usage %>% 
  group_by(`Meter ID`) %>% 
  summarise( n = n()) %>% 
  filter( n > 1)


filtered_data <- usage %>%
  filter(`Meter ID` %in% duplicate$`Meter ID`) %>% 
  arrange(`Meter ID`)

write_xlsx(filtered_data, path = file.path(data_path, "usage duplicates.xlsx"))


#usage clean with village_id----
usage <- read.csv(file.path(data_path,"usage_bind_raw.csv"))
usage_clean <- usage %>% 
  mutate(
    District = str_to_title(str_replace_all(District, "[^A-Za-z]", "")),
    Sector = str_to_title(str_replace_all(Sector, "[^A-Za-z]", "")),
    Cell = str_to_title(str_replace_all(Cell, "[^A-Za-z]", "")),
    Village = str_to_title(str_replace_all(Village, "[^A-Za-z]", ""))
  )

village_list_join <- village_list %>% 
  select(District, Sector, Cell, Name, Village_ID)

usage_clean.1 <- left_join(usage_clean, village_list_join, by = c("District" = "District",
                                                             "Sector" = "Sector",
                                                             "Cell" = "Cell",
                                                             "Village" = "Name"
                                                             ))


usage_id <- usage_clean.1 %>% 
  filter(!is.na(Village_ID))

usage_noid <- usage_clean.1 %>% 
  filter(is.na(Village_ID))


#Match with Meter location for the no id ones----

karongi_meter <- st_read(dsn = file.path(data_path, "eucl meter", "karongi_meter.shp"))
rulindo_meter <- st_read(dsn = file.path(data_path, "eucl meter", "rulindo_meter.shp"))
rutsiro_meter <- st_read(dsn = file.path(data_path, "eucl meter", "rutsiro_meter.shp"))
rusizi_meter <- st_read(dsn = file.path(data_path, "eucl meter", "rusizi_meter.shp"))


##Karongi join----
karongi_meter$Plot_Numbe <- gsub("/", "", karongi_meter$Plot_Numbe)
karongi_meter$parcel_id <- substr(karongi_meter$Plot_Numbe, 1, 7)


karongi_meter <- karongi_meter %>% 
  clean_names()
  
karongi_location <- karongi_meter %>% 
  select(village_id, plot_numbe, meter_numb) 

karongi_location <- karongi_location %>% 
  mutate(meter_numb = as.character(meter_numb))

####join  to id----

karongi_noid <- usage_noid%>% 
  filter(District %in% c("Karongi"))

karongi_cons_id <- left_join(karongi_noid, karongi_location, by = c("Meter ID"= "meter_numb"))

karongi_cons_id <- karongi_cons_id %>% 
  select(-Village_ID) %>% 
  rename(Village_ID = village_id)
  
  
karongi_cons_id <- karongi_cons_id%>% 
  filter(!is.na(Village_ID))



##rulindo join----
rulindo_meter$Plot_Numbe <- gsub("/", "", rulindo_meter$Plot_Numbe)
rulindo_meter$parcel_id <- substr(rulindo_meter$Plot_Numbe, 1, 7)


rulindo_meter <- rulindo_meter %>% 
  clean_names()

rulindo_location <- rulindo_meter %>% 
  select(village_id, plot_numbe, meter_numb) 

rulindo_location <- rulindo_location %>% 
  mutate(meter_numb = as.character(meter_numb))

####join  to id----

rulindo_noid <- usage_noid%>% 
  filter(District %in% c("Rulindo"))

rulindo_cons_id <- left_join(rulindo_noid, rulindo_location, by = c("Meter ID"= "meter_numb"))

rulindo_cons_id <- rulindo_cons_id %>% 
  select(-Village_ID) %>% 
  rename(Village_ID = village_id)


rulindo_cons_id <- rulindo_cons_id%>% 
  filter(!is.na(Village_ID))


##rutsiro join----
rutsiro_meter$Plot_Numbe <- gsub("/", "", rutsiro_meter$Plot_Numbe)
rutsiro_meter$parcel_id <- substr(rutsiro_meter$Plot_Numbe, 1, 7)


rutsiro_meter <- rutsiro_meter %>% 
  clean_names()

rutsiro_location <- rutsiro_meter %>% 
  select(village_id, plot_numbe, meter_numb) 

rutsiro_location <- rutsiro_location %>% 
  mutate(meter_numb = as.character(meter_numb))

####join  to id----

rutsiro_noid <- usage_noid%>% 
  filter(District %in% c("Rutsiro"))

rutsiro_cons_id <- left_join(rutsiro_noid, rutsiro_location, by = c("Meter ID"= "meter_numb"))

rutsiro_cons_id <- rutsiro_cons_id %>% 
  select(-Village_ID) %>% 
  rename(Village_ID = village_id)


rutsiro_cons_id <- rutsiro_cons_id%>% 
  filter(!is.na(Village_ID))



##rusizi join----
rusizi_meter$Plot_Numbe <- gsub("/", "", rusizi_meter$Plot_Numbe)
rusizi_meter$parcel_id <- substr(rusizi_meter$Plot_Numbe, 1, 7)


rusizi_meter <- rusizi_meter %>% 
  clean_names()

rusizi_location <- rusizi_meter %>% 
  select(village_id, plot_numbe, meter_numb) 

rusizi_location <- rusizi_location %>% 
  mutate(meter_numb = as.character(meter_numb))

####join  to id----

rusizi_noid <- usage_noid%>% 
  filter(District %in% c("Rusizi"))

rusizi_cons_id <- left_join(rusizi_noid, rusizi_location, by = c("Meter ID"= "meter_numb"))

rusizi_cons_id <- rusizi_cons_id %>% 
  select(-Village_ID) %>% 
  rename(Village_ID = village_id)


rusizi_cons_id <- rusizi_cons_id%>% 
  filter(!is.na(Village_ID)) 

#four district join back----

combined_df <- rbind(karongi_cons_id, rutsiro_cons_id, rulindo_cons_id, rusizi_cons_id)

usage_id.1 <- usage_id %>% 
  mutate(
    plot_numbe = NA,
    geometry = NA
  )

usage_id.1 <- rbind(usage_id.1, combined_df)

n_distinct(usage_id.1$`Meter ID`)

#Join names of villages-----

village_list_join <- village_list_join %>% 
  clean_names()

usage_id.2 <- left_join(usage_id.1, village_list_join, by = c("Village_ID" = "village_id"))

View(usage_id.2)
usage_id.2 <- usage_id.2 %>% 
  mutate(
    sector = str_to_title(sector)
  )

usage_id.2 <- usage_id.2 %>% 
  mutate(
    District = district,
    Sector = sector,
    Cell = cell,
    Village = name
  ) %>% 
  select(-district, -sector, -cell, -name)

usage_noid.2 <- usage %>%
  anti_join(usage_id.2, by = "Meter ID")

nrow(usage_id.2) + nrow(usage_noid.2)
nrow(usage)

#duplicates----

usage_id_duplicates <- usage_id.2 %>% 
  group_by(`Meter ID`) %>% 
  summarise( n = n()) %>% 
  filter(n >1)

usage_id_duplicates_new <- usage_id_duplicates %>% 
  anti_join(duplicate, by = "Meter ID")

duplicate.new <- usage_id.2 %>% 
  filter(`Meter ID` %in% usage_id_duplicates_new$`Meter ID`) 
  

usage_id_0402 <- usage_id.2 %>% 
  anti_join(duplicate.new, by = "Meter ID")

#Clean_duplicate.new 

duplicate_fix <- duplicate.new %>% 
 filter(Village_ID %in%c("24140107", "24140108")) %>% 
  distinct(`Meter ID`, .keep_all = TRUE)

duplicate_other <- duplicate.new %>% 
  anti_join(duplicate_fix, by = "Meter ID") 

duplicate_fix.1 <- duplicate_other %>% 
  filter(Village_ID %in%c("32080103", "32080104")) %>% 
  distinct(`Meter ID`, .keep_all = TRUE)

duplicate_other <- duplicate_other %>% 
  anti_join(duplicate_fix.1, by = "Meter ID") 

duplicate_fix.2 <- duplicate_other %>% 
  filter(Village_ID %in%c("41150201", "41080302","41150202", 	
"41160508", "34100606", "34100607", "31060202", "31050403", )) %>% 
  distinct(`Meter ID`, .keep_all = TRUE)
