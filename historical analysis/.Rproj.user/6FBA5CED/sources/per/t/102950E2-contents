##########################
#Author: Xiaoming Zhang
#Date: 02132024
#purpose:establishment census analysis
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


#Match with Meter location

karongi_meter <- st_read(dsn = file.path(data_path, "eucl meter", "karongi_meter.shp"))
rulindo_meter <- st_read(dsn = file.path(data_path, "eucl meter", "rulindo_meter.shp"))

rulindo_meter <- rulindo_meter %>% 
  mutate(Meter_Numb = as.character(Meter_Numb))
test_join <- left_join(rulindo_meter, usage_1_clean, by = c( "Meter_Numb" = "Meter ID"))
