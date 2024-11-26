##############
#Author: Xiaoming Zhang
#Date: 6.27.2024
#Purpose: household head
#############


pacman::p_load(tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, stargazer, olsrr, fuzzyjoin)

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


#Read files-----

four_district_2405 <- read_xlsx( path = file.path(data_path, "four_district_2405.xlsx"))
# 
# scope_0408 <- read_xlsx( path = file.path(data_path, "Scope villages_040824.xlsx"))
# scope_info <- read_xlsx(path = file.path(data_path, "Scope Villages.xlsx"))

# karongi_head <- read_xlsx(path = file.path (data_path, "hh_head", "Karongi_hh_2024.xlsx"))
karongi_head_old <- read_xlsx(path = file.path (data_path, "hh_head", "KARONGI HH Heads.xlsx"))
karongi_head_new <- read_xlsx(path = file.path (data_path, "hh_head", "KARONGI HH Heads_062024.xlsx"))


rulindo_head_old <- read_xlsx(path = file.path (data_path, "hh_head", "Copie de Rulindo_Heads_of_HHs.xlsx"))
rulindo_head_new <- read_xlsx(path = file.path (data_path, "hh_head", "RULINDO_final.xlsx"))


rutsiro_head <- read_xlsx(path = file.path (data_path, "hh_head", "RUTSIRO HH_2024.xlsx"))

rusizi_head <- read_xlsx(path = file.path (data_path, "hh_head", "Rusizi data June 27,2024.xlsx"))


rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))

village_list <- rwa_villages %>% 
  st_drop_geometry()

village_join <- village_list %>% 
  select(District, Sector, Cell, Name, Village_ID) %>% 
  clean_names() %>% 
  rename(village = name) %>% 
  mutate(across(everything(), ~ str_to_title(.)))



#construct rulindo----


rulindo_head_new <- rulindo_head_new %>% 
  mutate(across(everything(), ~ str_to_title(.))) %>% 
  mutate(district = "Rulindo")

rulindo_villages <- village_join %>% 
  filter(district %in% c("Rulindo"))

rulindo_head_new <- left_join(rulindo_head_new, village_join, by = c("district", "sector", "cell", "village"))


rulindo_na <- rulindo_head_new %>% 
  filter(is.na(village_id))


rulindo_na <- stringdist_left_join(rulindo_na, rulindo_villages, by = c("district", "sector", "cell", "village"), max_dist = 2)

rulindo_na_join <- rulindo_na %>% 
  filter(!is.na(village_id.y)) %>% 
  rename(
    village_id = village_id.y,
    district = district.x,
    sector = sector.y,
    cell = cell.y,
    village = village.y
  ) %>% 
  clean_names() %>% 
  select(village_id, district, sector, cell, village, name, gender, nid)


rulindo_non_na_join <- rulindo_head_new%>% 
  filter(!is.na(village_id)) %>% 
  clean_names() %>% 
  select(
    village_id, district, sector, cell, village, name, gender, nid
  )


rulindo_hh <- bind_rows(rulindo_non_na_join, rulindo_na_join)

View(rulindo_hh)










#clean karongi_old----
#using the full list of households,
#using the old because it have more villages
karongi_head_join.1 <- karongi_head_old %>%
  clean_names() %>% 
  filter(category == 1 & status == "Regular") %>% 
  select(district, sector, cell, village, code, first_name, last_name, gender,  nid) %>% 
  mutate(
    first_name = str_to_title(first_name),
    last_name = str_to_title(last_name),
    first_name = ifelse(first_name %in% c("X", "Xxx", "."), NA, first_name),
    last_name = ifelse(last_name %in% c("X", "Xxx", "."), NA, last_name),
    village_id = substr(code, 1, 8)
  ) %>% 
  distinct(code, .keep_all = TRUE)


filter_karongi <- karongi_head_join.1 %>% 
  group_by(code) %>% 
  summarise(
    n = n()
  ) %>% 
  filter(n >2)

#clean rutsiro----

sum(is.na(rutsiro_head$Code))

rutsiro_head_join <- rutsiro_head %>%
  clean_names() %>% 
  filter(category == 1) %>% 
  select(district, sector, cell, village, code, first_name, last_name, gender, nid) %>% 
  mutate(
    first_name = str_to_title(first_name),
    last_name = str_to_title(last_name),
    first_name = ifelse(first_name %in% c("X", "Xxx", "."), NA, first_name),
    last_name = ifelse(last_name %in% c("X", "Xxx", "."), NA, last_name),
    village_id = substr(code, 1, 8)
  )

sum(is.na(rutsiro_head_join$code))


filter_rutsiro <- rutsiro_head_join%>% 
  group_by(code) %>% 
  summarise(
    n = n()
  ) %>% 
  filter(n >= 2)





#clean rusizi----
#using the new rusizi household list june 27th
rusizi_head_join<- rusizi_head %>% 
  clean_names() %>% 
  filter(category == 1) %>% 
  mutate(
    last_name = str_split_fixed(household_name, " ", 2)[, 1],  # Extract the first part as last name
    first_name = str_split_fixed(household_name, " ", 2)[, 2]  # Extract the rest as first name
  ) %>%
  select(district, sector, cell, village, code, first_name, last_name, gender, nid) %>% 
  mutate(
    district = str_to_title(district),
    first_name = str_to_title(first_name),
    last_name = str_to_title(last_name),
    first_name = ifelse(first_name %in% c("X", "Xxx", "."), NA, first_name),
    last_name = ifelse(last_name %in% c("X", "Xxx", "."), NA, last_name),
    village_id = substr(code, 1, 8)
  )

filter_rusizi <- rusizi_head_join %>% 
  group_by(code) %>% 
  summarise(
    n = n()
  ) %>% 
  filter( n >2)

#clean rulindo----
# The new rulindo_hh_head list

rulindo_head_join <- rulindo_hh %>%
  clean_names() %>%
  select(district, sector, cell, village, name, gender, nid, village_id) %>%
  mutate(
    code = NA,
    last_name = str_extract(name, "^[^\\s]+"),  # Extracts the first word (last name)
    first_name = str_trim(str_replace(name, "^[^\\s]+\\s+", "")) ,
    gender = ifelse(gender %in% c("M"), "male", "female")) %>% 
  select(district, sector, cell, village, code, first_name, last_name, gender, nid, village_id) 
  
# Identify NIDs with more than one occurrence
filter_rulindo <- rulindo_head_join %>% 
  group_by(nid) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

# Filter the main data frame to keep only those NIDs with duplicates
rulindo_check <- rulindo_head_join %>%
  filter(nid %in% filter_rulindo$nid)

# For those duplicates, keep only rows with distinct last_name
rulindo_distinct <- rulindo_check %>%
  group_by(nid) %>%
  filter(!duplicated(last_name) | !duplicated(first_name)) %>%
  ungroup()

# Step 2: Create a combined name column
rulindo_distinct <- rulindo_distinct %>%
  mutate(name = paste0(last_name, first_name))

# Function to check if two strings are anagrams
are_anagrams <- function(string1, string2) {
  identical(sort(unlist(strsplit(string1, NULL))), sort(unlist(strsplit(string2, NULL))))
}

# Custom function to filter names that are anagrams within each nid group
filter_anagrams <- function(df) {
  keep <- rep(TRUE, nrow(df))
  
  for (i in 1:(nrow(df) - 1)) {
    for (j in (i + 1):nrow(df)) {
      if (are_anagrams(df$name[i], df$name[j])) {
        keep[j] <- FALSE
      }
    }
  }
  
  return(df[keep, , drop = FALSE])  # Ensure it returns a data frame
}

rulindo_distinct <- rulindo_distinct %>%
  group_by(nid) %>%
  group_modify(~ filter_anagrams(.x)) %>%
  ungroup()



# Append rows that are not duplicates (nids with no duplicates)
rulindo_non_duplicates <- rulindo_head_join %>%
  filter(!nid %in% filter_rulindo$nid)

# Combine the results
rulindo_head_join.1 <- bind_rows(rulindo_non_duplicates, rulindo_distinct) 




#Rulindo Check the result----

sort_string <- function(string) {
  sorted_string <- string %>% 
    str_remove_all(" ") %>% 
    str_split("") %>% 
    unlist() %>% 
    sort() %>% 
    paste0(collapse = "")
  
  return(sorted_string)
}

rulindo_head_join.1 <- rulindo_head_join.1 %>%
  mutate(
    name = paste0(first_name, last_name),
    name.1 = sapply(name, sort_string)) 



rulindo_name <- rulindo_head_join.1 %>% 
group_by(village_id, name.1) %>% 
  summarise(
    n = n()
  ) %>%
  filter(n >2)

rulindo_name<- rulindo_head_join.1%>% 
  filter(village_id %in% rulindo_name$village_id) %>% 
  filter(name.1 %in% rulindo_name$name.1)


rulindo_head_join.2 <- anti_join(rulindo_head_join.1, rulindo_name,  by = c("village_id", "name.1"))

rulindo_head_join.2 <- rulindo_head_join.2 %>% 
  select(-name, -name.1)



#join all four----

head_join <- rbind(karongi_head_join.1, rulindo_head_join.2, rutsiro_head_join, rusizi_head_join)



village_list_join <- village_list %>% 
  rename(
    provinceid_key = Prov_ID,
    province_key = Province, 
    districtid_key = Distr_ID,
    district_key = District,
    sectorid_key = Sector_ID,
    sector_key = Sector,
    cellid_key = Cell_ID,
    cell_key = Cell,
    villageid_key = Village_ID,
    village_key = Name
  ) %>% 
  select(provinceid_key, province_key, districtid_key, district_key, sectorid_key, sector_key, cellid_key, cell_key, villageid_key, village_key) %>% 
  filter(district_key %in% c("Karongi", "Rulindo", "Rutsiro", "Rusizi"))

#household_head list--- 
household_head <- left_join(village_list_join, head_join, by = c("villageid_key" = "village_id"))

write_xlsx(household_head, path = file.path(data_path, "household_head_clean.xlsx"))
household_head_number<- household_head %>% 
  group_by(villageid_key) %>% 
  summarise(hh_head_06 = n())


four_district_2406 <- left_join(four_district_2405, household_head_number, by = c("village_id" = "villageid_key"))

View(four_district_2406)


filter <- four_district_2406 %>% 
  filter(hh_head_05 != hh_head_06) %>% 
  group_by(district) %>% 
  summarise( n = n())

write_xlsx(four_district_2406, path = file.path(data_path, "four_district_2406.xlsx"))


#Merge lot----





