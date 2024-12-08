##########################
#Author: Xiaoming Zhang
#Last Edit Date: 04122024
#purpose: Construct Village ID for the usage data
############################


#library----

pacman::p_load(tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, haven, stringr, gtsummary)


#read file----

if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX, "Rwanda Energy/datawork/Historical data"
)


rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))

village_list <- rwa_villages %>% 
  st_drop_geometry()

#usage clean with village_id----
usage <- read.csv(file.path(data_path,"usage_bind_raw.csv"))

usage <- usage %>% 
  clean_names()

usage <- usage %>%
  rename(
    `2010_usage` = x2010_usage_k_wh,
    `2011_usage` = x2011_usage_k_wh,
    `2012_usage` = x2012_usage_k_wh,
    `2013_usage` = x2013_usage_k_wh,
    `2014_usage` = x2014_usage_k_wh,
    `2015_usage` = x2015_usage_k_wh,
    `2016_usage` = x2016_usage_k_wh,
    `2017_usage` = x2017_usage_k_wh,
    `2018_usage` = x2018_usage_k_wh,
    `2019_usage` = x2019_usage_k_wh,
    `2020_usage` = x2020_usage_k_wh,
    `2021_usage` = x2021_usage_k_wh,
    `2022_usage` = x2022_usage_k_wh
  )

usage_clean <- usage %>% 
  mutate(
    district = str_to_title(str_replace_all(district, "[^A-Za-z]", "")),
    sector = str_to_title(str_replace_all(sector, "[^A-Za-z]", "")),
    cell = str_to_title(str_replace_all(cell, "[^A-Za-z]", "")),
    village = str_to_title(str_replace_all(village, "[^A-Za-z]", ""))
  )


duplicate <- usage %>% 
  group_by(meter_id) %>% 
  summarise( n = n()) %>% 
  filter( n > 1)

usage_unique <- usage_clean %>% 
  anti_join(duplicate, by = c('meter_id'))

village_list_join <- village_list %>% 
  select(District, Sector, Cell, Name, Village_ID)

usage_clean.1 <- left_join(usage_unique, village_list_join, by = c("district" = "District",
                                                                  "sector" = "Sector",
                                                                  "cell" = "Cell",
                                                                  "village" = "Name"
))

usage_clean.1 <- usage_clean.1 %>% 
  distinct(meter_id, .keep_all = TRUE)

nrow(usage_clean.1) + nrow(duplicate)*2 == nrow(usage)
nrow(usage)
nrow(usage_clean.1) == nrow(usage_unique)
nrow(usage_unique)


usage_id <- usage_clean.1 %>% 
  filter(!is.na(Village_ID))

usage_noid <- usage_clean.1 %>% 
  filter(is.na(Village_ID)) %>% 
  mutate(meter_id = as.character(meter_id))

n_distinct(usage_noid)

#Match with Meter location for the no id ones----

karongi_meter <- st_read(dsn = file.path(data_path, "eucl meter", "karongi_meter.shp"))
rulindo_meter <- st_read(dsn = file.path(data_path, "eucl meter", "rulindo_meter.shp"))
rutsiro_meter <- st_read(dsn = file.path(data_path, "eucl meter", "rutsiro_meter.shp"))
rusizi_meter <- st_read(dsn = file.path(data_path, "eucl meter", "rusizi_meter.shp"))


##Karongi join----


karongi_meter <- karongi_meter %>% 
  clean_names()

karongi_location <- karongi_meter %>% 
  select(village_id, plot_numbe, meter_numb) 

karongi_location <- karongi_location %>% 
  mutate(meter_numb = as.character(meter_numb)) %>% 
  distinct(meter_numb, .keep_all = TRUE)

n_distinct(karongi_location$meter_numb) == nrow(karongi_location)

####join  to id----

karongi_noid <- usage_noid%>% 
  filter(district %in% c("Karongi"))


karongi_cons_id <- left_join(karongi_noid, karongi_location, by = c("meter_id"= "meter_numb"))

karongi_cons_id <- karongi_cons_id %>% 
  select(-Village_ID) %>% 
  rename(Village_ID = village_id)


karongi_cons_id <- karongi_cons_id%>% 
  filter(!is.na(Village_ID))


n_distinct(karongi_cons_id$meter_id) == nrow(karongi_cons_id)



##rulindo join----


rulindo_meter <- rulindo_meter %>% 
  clean_names()

rulindo_location <- rulindo_meter %>% 
  select(village_id, plot_numbe, meter_numb) 

rulindo_location <- rulindo_location %>% 
  mutate(meter_numb = as.character(meter_numb)) %>% 
  distinct(meter_numb, .keep_all = TRUE)


####join  to id----

rulindo_noid <- usage_noid%>% 
  filter(district %in% c("Rulindo"))

rulindo_cons_id <- left_join(rulindo_noid, rulindo_location, by = c("meter_id"= "meter_numb"))

rulindo_cons_id <- rulindo_cons_id %>% 
  select(-Village_ID) %>% 
  rename(Village_ID = village_id)


rulindo_cons_id <- rulindo_cons_id%>% 
  filter(!is.na(Village_ID))

n_distinct(rulindo_cons_id$meter_id) == nrow(rulindo_cons_id)


##rutsiro join----


rutsiro_meter <- rutsiro_meter %>% 
  clean_names()

rutsiro_location <- rutsiro_meter %>% 
  select(village_id, plot_numbe, meter_numb) 

rutsiro_location <- rutsiro_location %>% 
  mutate(meter_numb = as.character(meter_numb)) %>% 
  distinct(meter_numb, .keep_all = TRUE)


####join  to id----

rutsiro_noid <- usage_noid%>% 
  filter(district %in% c("Rutsiro"))

rutsiro_cons_id <- left_join(rutsiro_noid, rutsiro_location, by = c("meter_id"= "meter_numb"))

rutsiro_cons_id <- rutsiro_cons_id %>% 
  select(-Village_ID) %>% 
  rename(Village_ID = village_id)


rutsiro_cons_id <- rutsiro_cons_id%>% 
  filter(!is.na(Village_ID))

n_distinct(rutsiro_cons_id$meter_id) == nrow(rutsiro_cons_id)



##rusizi join----

rusizi_meter <- rusizi_meter %>% 
  clean_names()

rusizi_location <- rusizi_meter %>% 
  select(village_id, plot_numbe, meter_numb) 

rusizi_location <- rusizi_location %>% 
  mutate(meter_numb = as.character(meter_numb)) %>% 
  distinct(meter_numb, .keep_all = TRUE)


####join  to id----

rusizi_noid <- usage_noid%>% 
  filter(district %in% c("Rusizi"))

rusizi_cons_id <- left_join(rusizi_noid, rusizi_location, by = c("meter_id"= "meter_numb"))

rusizi_cons_id <- rusizi_cons_id %>% 
  select(-Village_ID) %>% 
  rename(Village_ID = village_id)


rusizi_cons_id <- rusizi_cons_id%>% 
  filter(!is.na(Village_ID)) 

n_distinct(rusizi_cons_id$meter_id) == nrow(rusizi_cons_id)




#four district join back----

combined_df <- rbind(karongi_cons_id, rutsiro_cons_id, rulindo_cons_id, rusizi_cons_id)

n_distinct(combined_df$meter_id) == nrow(combined_df)
nrow(combined_df) + 661673
usage_id <- usage_id %>% 
  mutate(
    plot_numbe = NA,
    geometry = NA
  )

usage_id_0402 <- rbind(usage_id, combined_df)

n_distinct(usage_id_0402$meter_id) == nrow(usage_id_0402)

#Join names of villages-----

usage_id_0402 <- usage_id_0402 %>% 
  clean_names()

usage_id_0402<- left_join(usage_id_0402, village_list_join, by = c("village_id" = "Village_ID"))


usage_id_0402 <- usage_id_0402 %>% 
  mutate(
    sector = str_to_title(sector)
  )

usage_id_0402 <- usage_id_0402 %>% 
  mutate(
    district = District,
    sector = Sector,
    cell = Cell,
    village = Name
  ) %>% 
  select(-District, -Sector, -Cell, -Name)

usage_unique <- usage_unique %>% 
  mutate(meter_id = as.character(meter_id))

usage_noid_0402 <- usage_unique %>% 
  anti_join(usage_id_0402, by = c("meter_id"))

nrow(usage_id_0402) + nrow(usage_noid_0402) == nrow(usage_unique)

#0402save----

str(usage_id_0402)

write_xlsx(usage_id_0402, path = file.path(data_path, "usage_villageid.xlsx"))


#parcel_id----

###Karongi----
sum(is.na(karongi_meter$Plot_Numbe))  

karongi_parcel <- karongi_meter %>% 
  filter(!is.na(plot_numbe)) %>% 
  clean_names()

nrow(karongi_parcel)

karongi_parcel$plot_numbe <- gsub("/", "", karongi_parcel$plot_numbe)
karongi_parcel$parcel_id<- paste0(substr(karongi_parcel$plot_numbe, 1, 1), substr(karongi_parcel$plot_numbe, 3, 7))


karongi_success <- karongi_parcel %>% 
  filter(parcel_id == cell_id)

nrow(karongi_success)

karongi_unsucces <- karongi_parcel %>% 
  filter(parcel_id != cell_id)

nrow(karongi_unsucces)



###rutsiro----
sum(is.na(rutsiro_meter$Plot_Numbe))  

rutsiro_parcel <- rutsiro_meter %>% 
  filter(!is.na(plot_numbe)) %>% 
  clean_names()

nrow(rutsiro_parcel)

rutsiro_parcel$plot_numbe <- gsub("/", "", rutsiro_parcel$plot_numbe)
rutsiro_parcel$parcel_id<- paste0(substr(rutsiro_parcel$plot_numbe, 1, 1), substr(rutsiro_parcel$plot_numbe, 3, 7))


rutsiro_success <- rutsiro_parcel %>% 
  filter(parcel_id == cell_id)

nrow(rutsiro_success)

rutsiro_unsucces <- rutsiro_parcel %>% 
  filter(parcel_id != cell_id)

nrow(rutsiro_unsucces)


###rulindo----
sum(is.na(rulindo_meter$Plot_Numbe))  

rulindo_parcel <- rulindo_meter %>% 
  filter(!is.na(plot_numbe)) %>% 
  clean_names()

nrow(rulindo_parcel)

rulindo_parcel$plot_numbe <- gsub("/", "", rulindo_parcel$plot_numbe)
rulindo_parcel$parcel_id<- paste0(substr(rulindo_parcel$plot_numbe, 1, 1), substr(rulindo_parcel$plot_numbe, 3, 7))


rulindo_success <- rulindo_parcel %>% 
  filter(parcel_id == cell_id)

nrow(rulindo_success)

rulindo_unsucces <- rulindo_parcel %>% 
  filter(parcel_id != cell_id)

nrow(rulindo_unsucces)



###rusizi----
sum(is.na(rusizi_meter$Plot_Numbe))  

rusizi_parcel <- rusizi_meter %>% 
  filter(!is.na(plot_numbe)) %>% 
  clean_names()

nrow(rusizi_parcel)

rusizi_parcel$plot_numbe <- gsub("/", "", rusizi_parcel$plot_numbe)
rusizi_parcel$parcel_id<- paste0(substr(rusizi_parcel$plot_numbe, 1, 1), substr(rusizi_parcel$plot_numbe, 3, 7))


rusizi_success <- rusizi_parcel %>% 
  filter(parcel_id == cell_id)


nrow(rusizi_success)

rusizi_unsucces <- rusizi_parcel %>% 
  filter(parcel_id != cell_id)

nrow(rusizi_unsucces)


#all----
karongi_success.1 <- karongi_success %>% 
  st_drop_geometry() %>% 
  select(meter_numb, plot_numbe, village_id, parcel_id, district)

rutsiro_success.1 <- rutsiro_success %>% 
  st_drop_geometry() %>% 
  select(meter_numb, plot_numbe, village_id, parcel_id, district)


rulindo_success.1 <- rulindo_success %>% 
  st_drop_geometry() %>% 
  select(meter_numb, plot_numbe, village_id, parcel_id, district)


rusizi_success.1 <- rusizi_success %>% 
  st_drop_geometry() %>% 
  select(meter_numb, plot_numbe, village_id, parcel_id, district)


parcel_success <- rbind(karongi_success.1, rutsiro_success.1, rusizi_success.1, rulindo_success.1)

parcel_success <- parcel_success %>% 
  distinct(meter_numb, .keep_all = TRUE)

n_distinct(parcel_success$meter_numb) == nrow(parcel_success)

write_xlsx(parcel_success, path = file.path(data_path, "parcel_success.xlsx"))



cell_id <- village_list %>% 
  select(Village_ID, Cell_ID)

usage_id_0402 <- left_join(usage_id_0402, cell_id, by = c("village_id" = "Village_ID"))


sum(is.na(usage_id_0402$plot_numbe))  

parcel <- usage_id_0402 %>% 
  filter(!is.na(plot_numbe)) %>% 
  clean_names()

nrow(parcel)


parcel$plot_numbe <- gsub("/", "", parcel$plot_numbe)
parcel$parcel_id<- paste0(substr(parcel$plot_numbe, 1, 1), substr(parcel$plot_numbe, 3, 7))


success <-parcel %>% 
  filter(parcel_id == cell_id)

nrow(success)

unsucces <- parcel %>% 
  filter(parcel_id != cell_id)

nrow(unsucces)

#save example

write_xlsx(list("karongi" = karongi_unsucces, "rulindo" = rulindo_unsucces, "rutsiro" = rutsiro_unsucces, "rusizi" = rusizi_unsucces), path = file.path(data_path,"parcel_id mismatch.xlsx"))

#Meter_ID not joined

karongi_not <- karongi_meter %>% 
  anti_join(karongi_cons_id, by = c("Meter_Numbe" = "meter_id"))



#Join the village_id and non village_ids

usage_noid_0402 <- usage_noid_0402 %>% 
  mutate(village_id = NA,
         plot_numbe = NA,
         geometry = NA, 
         Cell_ID = NA
         )


usage_id_0402 <- usage_id_0402 %>%
  rename(
    `2010_usage` = x2010_usage,
    `2011_usage` = x2011_usage,
    `2012_usage` = x2012_usage,
    `2013_usage` = x2013_usage,
    `2014_usage` = x2014_usage,
    `2015_usage` = x2015_usage,
    `2016_usage` = x2016_usage,
    `2017_usage` = x2017_usage,
    `2018_usage` = x2018_usage,
    `2019_usage` = x2019_usage,
    `2020_usage` = x2020_usage,
    `2021_usage` = x2021_usage,
    `2022_usage` = x2022_usage
  )


usage_unique_0403 <- rbind(usage_id_0402, usage_noid_0402)

write_xlsx(usage_noid_0402, path = file.path(data_path, "usage_noid_0402.xlsx") )



#0408 sector names----


usage_id_0403 <- read_xlsx( path = file.path(data_path,"usage_id_0403.xlsx"))
usage_noid_0403 <- read_xlsx( path = file.path(data_path,"usage_noid_0403.xlsx"))

usage_unique <- rbind(usage_id_0403, usage_noid_0403)

##More ids----

village_list <- village_list %>% 
  clean_names() %>% 
  mutate(
    sector = str_to_title(sector)
  )

Rugengabari <- usage_noid_0403%>% 
  filter(sector == "Rugengabali")

##sector construction----
usage_sector <- usage_noid_0403 %>%
  group_by(district, sector) %>%
  summarise(n = n()) %>%
  filter(!(sector %in% village_list$sector | is.na(sector))) 

usage_noid_list <- usage_noid_0403 %>% 
  group_by(district, sector, cell, village) %>% 
  summarise( n = n())

write_xlsx(usage_noid_list, path = file.path(data_path, "usage_noid_list.xlsx"))



usage_noid_0403 <- usage_noid_0403 %>% 
  mutate(
    sector = case_when(
      sector == "Rugengabali" & district == "Burera" ~ "Rugengabari",
      sector == "Rulenge" & district == "Ngoma" ~ "Rurenge",
      sector == "Mulinga" & district == "Nyabihu" ~ "Muringa",
      sector == "Mimuli" & district == "Nyagatare" ~"Mimuri",
      sector == "Kajongo" & district == "Nyamasheke" ~ "Kanjongo",
      sector == "Gishari" & district == "Rwamagana" ~ "Gishali",
      sector == "Nyakariro" & district == "Rwamagana" ~ "Nyakaliro",
      TRUE ~ sector 
    )
  )
usage_noid_0403 <- usage_noid_0403 %>% 
  mutate(
    sector = case_when(
      sector == " Nyakaliro" & district == "Rwamagana" ~ "Nyakaliro",
      TRUE ~ sector 
    )
  )

usage_sector <- usage_noid_0403 %>%
  group_by(district, sector) %>%
  summarise(n = n()) %>%
  filter(!(sector %in% village_list$sector | is.na(sector))) 

?case_when()

#join with village_id

village_list_join <- village_list %>% 
  select(name, cell, sector, district, village_id ) %>% 
  rename(village_id.x = village_id)


usage_id_0409_sector <- left_join(usage_noid_0403, village_list_join, by = c("village" = "name",
                                                                      "cell" = "cell",
                                                                      "sector" = "sector",
                                                                      "district" = "district"))
usage_id_0409_sector <- usage_id_0409_sector %>% 
  filter(!is.na(village_id.x)) %>% 
  mutate(
    village_id = village_id.x
  ) %>% 
  select(-village_id.x)


usage_noid_0409 <- usage_noid_0403 %>% 
  anti_join(usage_id_0409_sector, by = c("meter_id"))

nrow(usage_noid_0409) + nrow(usage_id_0409_sector) + nrow(usage_id_0403) == nrow(usage_unique)


#cell construction----

usage_villagenoid_0409 <- usage_noid_0409 %>% 
  filter(!is.na(village) & !is.na(cell) & !is.na(sector) & !is.na(district))

usage_novillage_0409 <- usage_noid_0409 %>% 
  filter(is.na(village) | is.na(cell) | is.na(sector) | is.na(district))

nrow(usage_villagenoid_0409) + nrow(usage_novillage_0409) == nrow(usage_noid_0409)

usage_cell <- usage_villagenoid_0409 %>%
  group_by(district, sector, cell) %>%
  summarise(n = n()) %>%
  filter(!(cell %in% village_list$cell)) 

write_xlsx(usage_cell , path = file.path(data_path, "usage_cell_namemismatch.xlsx"))


usage_villagenoid_construct <- usage_villagenoid_0409 %>% 
  mutate(
    cell  = case_when(
      district == "Bugesera" & sector == "Nyamata" & cell == "Nyamatayumujyi" ~ "Nyamata y' Umujyi",
      district == "Burera" & sector == "Bungwe" & cell == "Mudugali" ~ "Mudugari",
      district == "Burera" & sector == "Nemba" & cell == "Nyamugali" ~ "Nyamugari",
      district == "Burera" & sector == "Rugengabari" & cell == "Kiribata" ~ "Kilibata",
      district == "Burera" & sector == "Rwerere" & cell == "Rugali" ~ "Rugari",
      district == "Gakenke" & sector == "Coko" & cell == "Mbilima" ~ "Mbirima",
      district == "Gakenke" & sector == "Janja" & cell == "Karikungu" ~ "Karukungu",
      district == "Gakenke" & sector == "Rushashi" & cell == "Bulimba" ~ "Burima",
      district == "Gakenke" & sector == "Rushashi" & cell == "Bulimba" ~ "Burima",
      district == "Gasabo" & sector == "Gatsata" & cell == "Nyamugali" ~ "Nyamugari",
      district == "Gasabo" & sector == "Kimihurura" & cell == "Kimihiurura" ~ "Kimihurura",
      district == "Gasabo" & sector == "Remera" & cell == "Rukirii" ~ "Rukiri I",
      district == "Gasabo" & sector == "Remera" & cell == "Rukiriii" ~ "Rukiri Ii",
      district == "Gasabo" & sector == "Rusororo" & cell == "Kabugai" ~ "Kabuga I",
      district == "Gasabo" & sector == "Rusororo" & cell == "Kabugaii" ~ "Kabuga Ii",
      district == "Gatsibo" & sector == "Gatsibo" & cell == "Nyabicwamb" ~ "Nyabicwamba",
      district == "Gatsibo" & sector == "Muhura" & cell == "Rumuri" ~ "Rumuli",
      district == "Gicumbi" & sector == "Byumba" & cell == "Kibari" ~ "Kibali",
      district == "Gicumbi" & sector == "Cyumba" & cell == "Nyamabare" ~ "Nyambare",
      district == "Gicumbi" & sector == "Manyagiro" & cell == "Nyirakugiza" ~ "Nyiravugiza",
      district == "Gicumbi" & sector == "Nyamiyaga" & cell == "Gahumuriza" ~ "Gahumuliza",
      district == "Gicumbi" & sector == "Nyankenke" & cell == "Butere" ~ "Butare",
      district == "Gisagara" & sector == "Gikonko" & cell == "Cyili" ~ "Cyiri",
      district == "Huye" & sector == "Mukura" & cell == "Rangoa" ~ "Rango A",
      district == "Huye" & sector == "Tumba" & cell == "Rangoa" ~ "Rango B",
      district == "Kamonyi" & sector == "Nyamiyaga" & cell == "Kidadwe" ~ "Kidahwe",
      district == "Kamonyi" & sector == "Rugarika" & cell == "Sheri" ~ "Sheli",
      district == "Kayonza" & sector == "Kabare" & cell == "Ubimba" ~ "Rubimba",
      district == "Kayonza" & sector == "Murama" & cell == "Bunyegongo" ~ "Bunyentongo",
      district == "Kayonza" & sector == "Mwiri" & cell == "Nyamugali" ~ "Nyamugari",
      district == "Kicukiro" & sector == "Kigarama" & cell == "Bwerankoli" ~ "Bwerankori",
      district == "Kicukiro" & sector == "Kigarama" & cell == "Rwampala" ~ "Rwampara",
      district == "Kirehe" & sector == "Gatore" & cell == "Nyamiyango" ~ "Nyamiryango",
      district == "Kirehe" & sector == "Musaza" & cell == "Mabuga" ~ "Mubuga",
      district == "Muhanga" & sector == "Cyeza" & cell == "Bilingaga" ~ "Biringaga",
      district == "Musanze" & sector == "Shingiro" & cell == "Mudenge" ~ "Mudende",
      district == "Ngororero" & sector == "Hindiro" & cell == "Marankima" ~ "Marantima",
      district == "Nyabihu" & sector == "Kintobo" & cell == "Nyamugali" ~ "Nyamugari",
      district == "Nyagatare" & sector == "Karama" & cell == "Gikagata" ~ "Gikagati",
      district == "Nyagatare" & sector == "Katabagemu" & cell == "Nyakigango" ~ "Nyakigando",
      district == "Nyagatare" & sector == "Mimuri" & cell == "Mimuli" ~ "Mimuri",
      district == "Nyagatare" & sector == "Musheri" & cell == "Rugaramai" ~ "Rugarama I",
      district == "Nyagatare" & sector == "Musheri" & cell == "Rugaramaii" ~ "Rugarama Ii",
      district == "Nyagatare" & sector == "Nyagatare" & cell == "Sheke" ~ "Nsheke",
      district == "Nyagatare" & sector == "Rukomo" & cell == "Rukomoii" ~ "Rukomo Ii",
      district == "Nyagatare" & sector == "Rwempasha" & cell == "Cyenjojo" ~ "Cyenjonjo",
      district == "Nyagatare" & sector == "Rwimiyaga" & cell == "Nyarupubire" ~ "Nyarupfubire",
      district == "Nyagatare" & sector == "Rwimiyaga" & cell == "Rutungo" ~ "Rutungu",
      district == "Nyamasheke" & sector == "Cyato" & cell == "Rugali" ~ "Rugari",
      district == "Nyamasheke" & sector == "Macuba" & cell == "Rugali" ~ "Rugari",
      district == "Nyamasheke" & sector == "Ruharambuga" & cell == "Uwimana" ~ "Wimana",
      district == "Nyanza" & sector == "Mukingo" & cell == "Kiruri" ~ "Kiruli",
      district == "Nyanza" & sector == "Ntyazo" & cell == "Bugari" ~ "Bugali",
      district == "Nyarugenge" & sector == "Nyakabanda" & cell == "Munanirai" ~ "Munarira I",
      district == "Nyarugenge" & sector == "Nyakabanda" & cell == "Munaniraii" ~ "Munarira Ii",
      district == "Nyarugenge" & sector == "Nyakabanda" & cell == "Nyakabandai" ~ "Nyakabanda I",
      district == "Nyarugenge" & sector == "Rwezamenyo" & cell == "Kabugurui" ~ "Kabuguru I",
      district == "Nyarugenge" & sector == "Rwezamenyo" & cell == "Kabuguruii" ~ "Kabuguru Ii",
      district == "Nyarugenge" & sector == "Rwezamenyo" & cell == "Rwezamenyoi" ~ "Rwezamenyo I",
      district == "Nyarugenge" & sector == "Rwezamenyo" & cell == "Rwezamenyoii" ~ "Rwezamenyo Ii",
      district == "Nyaruguru" & sector == "Negera" & cell == "Bitari" ~ "Bitare",
      district == "Nyaruguru" & sector == "Nyagisozi" & cell == "Nkankwa" ~ "Nkakwa",
      district == "Nyaruguru" & sector == "Ruheru" & cell == "Buyenzi" ~ "Ruyenzi",
      district == "Rabavu" & sector == "Nyakiriba" & cell == "Kanyefurwa" ~ "Kanyefurwe",
      district == "Ruhango" & sector == "Kinazi" & cell == "Gisari" ~ "Gisali",
      district == "Rulindo" & sector == "Masoro" & cell == "Shengampuri" ~ "Shengampuli",
      district == "Rusizi" & sector == "Giheke" & cell == "Cyendajura" ~ "Cyendajuru",
      district == "Rutsiro" & sector == "Gihango" & cell == "Congonil" ~ "Congo-nil",
      district == "Rutsiro" & sector == "Rusebeya" & cell == "Mbeli" ~ "Mberi",
      district == "Rwamagana" & sector == "Karenge" & cell == "Nyabutare" ~ "Nyabubare",
      district == "Rwamagana" & sector == "Musha" & cell == "Budahamba" ~ "Budahanda",
      TRUE ~ cell 
    )
  )


usage_villagenoid_construct <- left_join(usage_villagenoid_construct, village_list_join,by = c("village" = "name",
                                                                                               "cell" = "cell",
                                                                                               "sector" = "sector",
                                                                                               "district" = "district") )

sum(is.na(usage_villagenoid_construct$village_id.x))

usage_id_0409_cell <- usage_villagenoid_construct %>% 
  mutate(
    village_id = village_id.x
  ) %>% 
  filter(!is.na(village_id)) %>% 
  select(-village_id.x)

#bind constructed cell and sector----

usage_id_0409_construct <- rbind(usage_id_0409_cell, usage_id_0409_sector)
usage_id_0409 <- rbind(usage_id_0409_construct, usage_id_0403)
usage_noid_0409 <- usage_unique %>% 
  anti_join(usage_id_0409, by = c("meter_id"))



usage_villagenoid_0409 <- usage_noid_0409 %>% 
  anti_join(usage_novillage_0409, by = c("meter_id"))


#village construct----

##general ----
usage_village <- usage_villagenoid_0409 %>%
  group_by(district, sector, cell, village) %>%
  summarise(n = n()) %>%
  filter(!(village %in% village_list$name) & n> 10) 

##I and Ii situations----
usage_villagenoid.1 <- usage_villagenoid_0409 %>% 
  mutate(
    village = case_when(
      stringr::str_sub(village, -1) == "i" ~ stringr::str_replace(village, "i$", " I"),
      stringr::str_sub(village, -2) == "ii" ~ stringr::str_replace(village, "ii$", " Ii"),
      TRUE ~ village
    )
  )


usage_villagenoid.1 <- left_join(usage_villagenoid.1, village_list_join,by = c("village" = "name",
                                                                                               "cell" = "cell",
                                                                                               "sector" = "sector",
                                                                                               "district" = "district") )

sum(!is.na(usage_villagenoid.1$village_id.x))

usage_id_village.1 <- usage_villagenoid.1 %>% 
  filter(!is.na(village_id.x)) %>%
  mutate(
    village_id = village_id.x,
  ) %>% 
  select(-village_id.x)
 
##Ri to Li----

usage_villagenoid.2 <- usage_villagenoid_0409 %>% 
  mutate(
    village = case_when(
      stringr::str_sub(village, -2) == "ri" ~ stringr::str_replace(village, "ri$", "li"),
      stringr::str_sub(village, -2) == "li" ~ stringr::str_replace(village, "li$", "ri"),
      TRUE ~ village
    )
  )


usage_villagenoid.2 <- left_join(usage_villagenoid.2, village_list_join,by = c("village" = "name",
                                                                               "cell" = "cell",
                                                                               "sector" = "sector",
                                                                               "district" = "district") )

sum(!is.na(usage_villagenoid.2$village_id.x))

usage_id_village.2 <- usage_villagenoid.2 %>% 
  filter(!is.na(village_id.x)) %>%
  mutate(
    village_id = village_id.x,
  ) %>% 
  select(-village_id.x)

##a to e or similar situations----
usage_villagenoid.3 <- usage_villagenoid_0409 %>% 
  mutate(
    village = case_when(
      stringr::str_sub(village, -1) == "a" ~ stringr::str_replace(village, "a$", "e"),
      stringr::str_sub(village, -1) == "e" ~ stringr::str_replace(village, "e$", "a"),
      TRUE ~ village
    )
  )


usage_villagenoid.3 <- left_join(usage_villagenoid.3, village_list_join,by = c("village" = "name",
                                                                               "cell" = "cell",
                                                                               "sector" = "sector",
                                                                               "district" = "district") )

sum(!is.na(usage_villagenoid.3$village_id.x))

usage_id_village.3 <- usage_villagenoid.3 %>% 
  filter(!is.na(village_id.x)) %>%
  mutate(
    village_id = village_id.x,
  ) %>% 
  select(-village_id.x)

##bind common mistakes-----

usage_id_0409_village.1 <- rbind(usage_id_village.1, usage_id_village.2, usage_id_village.3)

n_distinct(usage_id_0409_village.1$meter_id) == nrow(usage_id_0409_village.1)

usage_id_0409 <- rbind(usage_id_0409, usage_id_0409_village.1)

n_distinct(usage_id_0409$meter_id) == nrow(usage_id_0409)

usage_noid_0409 <- usage_unique %>% 
  anti_join(usage_id_0409, by = c("meter_id"))

nrow(usage_id_0409) + nrow(usage_noid_0409) == nrow(usage_unique)

##Other none common mistakes----
usage_villagenoid_0409 <- usage_noid_0409 %>%
  filter(!is.na(village) & !is.na(cell) & !is.na(sector) & !is.na(district))

usage_village <- usage_villagenoid_0409 %>%
  group_by(district, sector, cell, village) %>%
  summarise(n = n()) %>%
  filter(!(village %in% village_list$name) & n> 10) 


##iii to ii to i----
##ii
usage_villagenoid.4 <- usage_villagenoid_0409 %>% 
  mutate(
    village = case_when(
      stringr::str_sub(village, -2) == "ii" ~ stringr::str_replace(village, "ii$", " Ii"),
      TRUE ~ village
    )
  )


usage_villagenoid.4 <- left_join(usage_villagenoid.4, village_list_join,by = c("village" = "name",
                                                                               "cell" = "cell",
                                                                               "sector" = "sector",
                                                                               "district" = "district") )

usage_id_village.4 <- usage_villagenoid.4 %>% 
  filter(!is.na(village_id.x)) %>%
  mutate(
    village_id = village_id.x,
  ) %>% 
  select(-village_id.x)

sum(!is.na(usage_villagenoid.4$village_id.x))

##iii----


usage_villagenoid.5 <- usage_villagenoid_0409 %>% 
  mutate(
    village = case_when(
      stringr::str_sub(village, -3) == "iii" ~ stringr::str_replace(village, "iii$", " Iii"),
      TRUE ~ village
    )
  )


usage_villagenoid.5 <- left_join(usage_villagenoid.5, village_list_join,by = c("village" = "name",
                                                                               "cell" = "cell",
                                                                               "sector" = "sector",
                                                                               "district" = "district") )

sum(!is.na(usage_villagenoid.5$village_id.x))

usage_id_village.5 <- usage_villagenoid.5 %>% 
  filter(!is.na(village_id.x)) %>%
  mutate(
    village_id = village_id.x,
  ) %>% 
  select(-village_id.x)


usage_id_0409_village.2 <- rbind(usage_id_village.4, usage_id_village.5)

nrow(usage_villagenoid_0409)

usage_villagenoid_0409 <- usage_villagenoid_0409 %>% 
  anti_join(usage_id_0409_village.2, by = c("meter_id"))

#joinback with id----

usage_id_0409 <- rbind(usage_id_0409, usage_id_0409_village.2)

usage_noid_0409 <- usage_unique %>% 
  anti_join(usage_id_0409, by = c('meter_id'))

##Add A----

usage_villagenoid.6 <- usage_villagenoid_0409 %>% 
  mutate(
    village = paste0("A", str_to_lower(village))
  )


usage_villagenoid.6 <- left_join(usage_villagenoid.6, village_list_join,by = c("village" = "name",
                                                                               "cell" = "cell",
                                                                               "sector" = "sector",
                                                                               "district" = "district") )

sum(!is.na(usage_villagenoid.6$village_id.x))

usage_id_village.6 <- usage_villagenoid.6 %>% 
  filter(!is.na(village_id.x)) %>%
  mutate(
    village_id = village_id.x,
  ) %>% 
  select(-village_id.x)




## remove i, ii, iii----

usage_villagenoid.7 <- usage_villagenoid_0409 %>% 
  mutate(
    village = case_when(
      stringr::str_sub(village, -1) == "i" ~ stringr::str_replace(village, "i$", ""),
      stringr::str_sub(village, -2) == "ii" ~ stringr::str_replace(village, "ii$", ""),
      stringr::str_sub(village, -3) == "iii" ~ stringr::str_replace(village, "iii$", ""),
      TRUE ~ village
    )
  )


usage_villagenoid.7 <- left_join(usage_villagenoid.7, village_list_join,by = c("village" = "name",
                                                                               "cell" = "cell",
                                                                               "sector" = "sector",
                                                                               "district" = "district") )

sum(!is.na(usage_villagenoid.7$village_id.x))

usage_id_village.7 <- usage_villagenoid.7 %>% 
  filter(!is.na(village_id.x)) %>%
  mutate(
    village_id = village_id.x,
  ) %>% 
  select(-village_id.x)

usage_id_0409_village.3 <- rbind(usage_id_village.6, usage_id_village.7)


#joinback with id----

usage_id_0409.1 <- rbind(usage_id_0409, usage_id_0409_village.3)

usage_noid_0409.1 <- usage_unique %>% 
  anti_join(usage_id_0409.1, by = c('meter_id'))

n_distinct(usage_id_0409.1$meter_id) == nrow(usage_id_0409.1)
nrow(usage_id_0409.1) + nrow(usage_noid_0409.1)  == nrow(usage_unique)

#Temporary save----

write_xlsx(usage_id_0409.1 , path = file.path(data_path, "usage_id_0409.xlsx"))

write_xlsx(usage_noid_0409.1 , path = file.path(data_path, "usage_noid_0409.xlsx"))

write_xlsx(usage_novillage_0409, path = file.path(data_path, "usage_novillage_0409.xlsx"))

#usage_villagenoid_0409

usage_villagenoid_0409 <- usage_noid_0409.1 %>% 
  filter(!is.na(district) & !is.na(sector) & !is.na(cell) & !is.na(village))



#Ok now let's manually give a village id to them-----

nrow(usage_villagenoid_0409) + nrow(usage_novillage_0409 ) == nrow(usage_noid_0409.1)

noid_village <- usage_villagenoid_0409 %>% 
  group_by(district, sector, cell, village) %>% 
  summarise( n = n()) %>% 
  filter(n >1 )

villagenoid.1 <- usage_villagenoid_0409 %>% 
  group_by(district, sector, cell, village) %>% 
  mutate(village_id_fake = group_indices())


villagenoid.2 <- villagenoid.1 %>% 
  group_by(village_id_fake) %>% 
  summarise(
    district = district,
    sector = sector,
    cell = cell,
    village = village,
    n = n()
            ) %>% 
  distinct(village_id_fake, .keep_all = TRUE) %>% 
  filter(n > 1)

write_xlsx(villagenoid.2, path = file.path(data_path, "village noid construct.xlsx"))

#0412-----

usage_id_0409 <- read_xlsx(path = file.path(data_path, "usage_id_0409.xlsx"))
usage_novillage_0409 <- read_xlsx(path = file.path(data_path, "usage_novillage_0409.xlsx"))
usage_noid_0409 <- read_xlsx(path = file.path(data_path, "usage_noid_0409.xlsx"))

nrow(usage_id_0409) + nrow(usage_noid_0409) == 1342346

usage_villagenoid_0409 <- usage_noid_0409 %>% 
  filter(!is.na(village) & !is.na(cell) & !is.na(sector) & !is.na(district))

nrow(usage_villagenoid_0409) + nrow(usage_novillage_0409 ) == nrow(usage_noid_0409)

village_list_join <- village_list_join %>% 
  clean_names() %>% 
  rename(village = name)

village_list_join.1 <- village_list_join %>% 
  mutate(
    village = str_to_lower(village)
  )


##Fuzzy match---- 

install.packages("fuzzyjoin")
library(fuzzyjoin)


n_distinct(usage_villagenoid_0409$meter_id) == nrow(usage_villagenoid_0409)

usage_villagenoid_join <- usage_villagenoid_0409 %>%
  select(
    meter_id, district, sector, cell, village
  ) %>% 
  mutate(village = str_to_lower(village))

total_rows <- nrow(usage_villagenoid_0409)

half_rows <- total_rows/2

usage_villagenoid_join.1 <- usage_villagenoid_join %>% 
  slice(1:half_rows) 

usage_villagenoid_join.2 <- usage_villagenoid_join %>% 
  slice(half_rows+1 : total_rows)

fuzzy_join_result.1 <- stringdist_left_join(usage_villagenoid_join.1, village_list_join.1, by = c("district", "sector", "cell", "village"), max_dist = 2)
fuzzy_join_result.2 <- stringdist_left_join(usage_villagenoid_join.2, village_list_join.1, by = c("district", "sector", "cell", "village"), max_dist = 2)


fuzzy_join <- rbind(fuzzy_join_result.1, fuzzy_join_result.2)

fuzzy_join <- fuzzy_join %>% 
  filter(!is.na(village_id)) %>% 
  distinct(meter_id, .keep_all = TRUE)

fuzzy_join <- fuzzy_join %>% 
  group_by(district.x, sector.x, cell.x, village.x) %>% 
  mutate(village_id_fake = cur_group_id()) 


fuzzy_join_save <- fuzzy_join %>% 
  group_by(village_id_fake) %>% 
  summarise(
    sector.x = sector.x,
    sector.y = sector.y,
    cell.x = cell.x,
    cell.y = cell.y,
    village.x = village.x,
    village.y = village.y,
    n = n()
  ) %>% 
  distinct(village_id_fake, .keep_all = TRUE)


write_xlsx(fuzzy_join_save, path = file.path(data_path, "fuzzy_join_save.xlsx"))
#work on excel to make sure

fuzzy_join_edit<- read_xlsx(path = file.path(data_path, "fuzzy_join_edit.xlsx"))

fuzzy_join_edit.1 <- fuzzy_join_edit %>% 
  select(village.y, village_id_fake) %>% 
  rename(village = village.y)


fuzzy_join.1 <- left_join(fuzzy_join, fuzzy_join_edit.1, by = c("village_id_fake"))

fuzzy_join.2<- fuzzy_join.1 %>% 
  select(district.y, sector.y, cell.y, village, meter_id)
 
fuzzy_join.2 <- left_join(fuzzy_join.2, village_list_join.1, by = c("district.y" = "district",
                                                                    "sector.y" = "sector",
                                                                    "cell.y" = "cell",
                                                                    "village" = "village"))
sum(!is.na(fuzzy_join.2$village_id))
fuzzy_join.2 <- fuzzy_join.2 %>% 
  as.data.frame() %>% 
  ungroup()
  
fuzzy_join.3 <- fuzzy_join.2 %>% 
  select(meter_id, village_id) %>% 
  rename(village_id.x = village_id) %>% 
  filter(!is.na(village_id.x))


usage_id_fuzzy_0412 <- usage_villagenoid_0409 %>%
  filter(meter_id %in% fuzzy_join.3$meter_id)

n_distinct(usage_id_fuzzy_0412$meter_id) == n_distinct(fuzzy_join.3$meter_id)

usage_id_fuzzy_0412 <- left_join(usage_id_fuzzy_0412, fuzzy_join.3, by = c("meter_id"))

sum(is.na(usage_id_fuzzy_0412$village_id.x))

usage_id_fuzzy_0412 <- usage_id_fuzzy_0412 %>% 
  mutate(village_id = village_id.x) %>% 
  select(-village_id.x)

usage_id_0412 <- rbind(usage_id_0409, usage_id_fuzzy_0412)

usage_noid_0412 <- usage_noid_0409 %>% 
  anti_join(usage_id_0412, by = c("meter_id"))

nrow(usage_id_0412) + nrow(usage_noid_0412) == 1342346
nrow(usage_noid_0412)
write_xlsx(usage_id_0412, path = file.path(data_path, "usage_id_0412.xlsx"))
write_xlsx(usage_noid_0412, path = file.path(data_path, "usage_noid_0412.xlsx"))
