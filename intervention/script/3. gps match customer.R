

pacman::p_load(knitr, stargazer, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr, lfe)
library(googlesheets4)
getwd()


# Import Data ----
dropbox <- 'C:/Users/wb614406/Dropbox'

output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/baseline analysis/output"
)

data_path_1 <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data"
)

hfc_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/HFC/data"
)


data_path_2 <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/Updated scope villages& households"
)



complete  <- read_xlsx(path = file.path(data_path_2, "vulnerable households in sample villages.xlsx"))


complete_second_round <- read_xlsx(path = file.path(data_path_2, "completed_second_round.xlsx"))  

complete_survey_hfc <- complete %>% 
  mutate(
    `Complete second round` = ifelse(household_id %in% complete_second_round$hh_id, "Yes", "No")
  ) %>% 
  mutate(
    `Completed_households` = ifelse(`Completed by Lattanzio` == "Yes" | `Complete second round` == "Yes", "Yes", "No")
  )  %>% 
  filter(`Completed_households` == "Yes") 




complete_survey <- read.csv(file.path(hfc_path,"Baseline second round",  "REP_baseline_test_WIDE (2).csv")) %>% 
  mutate(
    hh_id = as.character(hh_id)
  ) %>%
  filter(hh_id %in% complete_survey_hfc$household_id)


master <- master %>% 
  mutate(
    surveyed = ifelse(household_id %in% complete_survey$hh_id, 1, 0)
  )


#Rutsiro villages with no readyboard------

#Filter in villages with no readyboard
#Find the surveyed households that match gps of customer in no readyboard villages

rutsiro_complete <-  complete_survey %>%
  filter(district == 32) %>%
  select(hh_id, first_name, last_name, district, sector, cell, village, 
         coordinate.Longitude, coordinate.Latitude) %>%
  st_as_sf(coords = c("coordinate.Longitude", "coordinate.Latitude"), crs = st_crs(4236)) %>% 
  filter(village %in% rutsiro_dime$villageid_key) #Only in readyboard villages

rutsiro_customer <- st_transform(rutsiro_customer, crs = st_crs(4236) )


#Here starts the try#####################################################################################

# Step 1: Compute distance matrix as a numeric matrix
dist_matrix <- st_distance(rutsiro_complete, rutsiro_customer)
dist_matrix_numeric <- as.matrix(dist_matrix)  # convert units to numeric matrix (in meters)

# Step 2: Get index of the closest customer for each household
closest_idx <- apply(dist_matrix_numeric, 1, which.min)

# Step 3: Extract closest customer name and distance
closest_customers <- rutsiro_customer[closest_idx, ] %>%
  st_drop_geometry() %>%
  select(customer_first_name = first_name, customer_last_name = last_name)

# Step 4: Extract the closest distance for each household
closest_distances <- dist_matrix_numeric[cbind(seq_len(nrow(dist_matrix_numeric)), closest_idx)]

# Step 5: Combine with household info



rutsiro_matches <- rutsiro_complete %>%
  st_drop_geometry() %>%
  bind_cols(closest_customers, distance_m = as.numeric(closest_distances)) %>%
  filter(distance_m < 10) %>%
  distinct(hh_id, .keep_all = TRUE) %>%
  rowwise() %>%
  filter(
    str_to_lower(customer_first_name) == str_to_lower(first_name) |
      str_to_lower(customer_first_name) == str_to_lower(last_name) |
      str_to_lower(customer_last_name) == str_to_lower(first_name) |
      str_to_lower(customer_last_name) == str_to_lower(last_name)
  ) %>%
  ungroup() %>%
  rowid_to_column("row") %>% distinct(hh_id, .keep_all = TRUE)


master_join <- master %>% select(household_id, surveyed, readyboard)

rutsiro_matches <- left_join(rutsiro_matches, master_join, by = c("hh_id" = "household_id")) 

rutsiro_matches_nordb <- rutsiro_matches %>% 
  filter(readyboard == 0)

# write_xlsx(rutsiro_matches_nordb, path = file.path(data_path_2, "rutsiro customer_surveyed gps matches_nordb.xlsx"))

rutsiro_gps <- rutsiro_matches %>% 
  mutate(
    gps_match = 1
  ) %>% 
  select(hh_id, gps_match, customer_first_name, customer_last_name, distance_m)

rutsiro_join.3 <-left_join(rutsiro_join.2, rutsiro_gps , by = c("household_id" = "hh_id"))





rutsiro_join.3_nordb <- rutsiro_join.3 %>%
  filter(
    ! villageid_key %in% rutsiro_readyboard$villageid_key
  )

rutsiro_join_analysis <- rutsiro_join.3_nordb %>% 
  st_drop_geometry() %>% 
  mutate(any_match = pmax(gps_match, customer, na.rm = TRUE)) %>%
  group_by(villageid_key) %>% 
  summarise(
    vulnerable = sum(vulnerable, na.rm = TRUE),
    customer_name_match = sum(customer, na.rm = TRUE),
    
    surveyed = sum(surveyed, na.rm = TRUE),
    gps_match = sum(gps_match, na.rm = TRUE),
    
    any_match = sum(any_match, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(rwa_village, by = c("villageid_key" = "Village_ID")) %>% 
  clean_names() %>% 
  select(villageid_key, district, sector, cell, name, everything()) %>%
  st_drop_geometry() %>%
  select(-c(distr_id, cell_id, sector_id, province, prov_id, geometry)) %>%
  
  bind_rows(
    summarise(., 
              villageid_key = "Total",
              vulnerable = sum(vulnerable),
              customer_name_match = sum(customer_name_match),
              
              surveyed = sum(surveyed),
              gps_match = sum(gps_match),
              any_match = sum(any_match)
    )
  ) %>% 
  rename(
    `gps and name match` = gps_match
  )

# End of try############################################################################################

#write file----
rutsiro_epc_write <- read_xlsx(path = file.path(data_path_2, "Beneficiary List of Rutsiro.xlsx"), sheet = "household list") %>% 
  select(
    villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, Comments
  ) %>% 
  filter(villageid_key %in% rutsiro_join_analysis$villageid_key) 



rutsiro_epc_village_write <- read_xlsx(path = file.path(data_path_2, "Beneficiary List of Rutsiro.xlsx"), sheet = "village list") %>% 
  filter(village_id %in% rutsiro_join_analysis$villageid_key) %>% 
  select(-c(...7, ...8))

rutsiro_join.3_nordb <- left_join(rutsiro_join.3_nordb,rwa_village,  by = c("villageid_key" = "Village_ID")) %>% 
  clean_names() %>% 
  select(villageid_key, district, sector, cell, name, everything()) %>%
  st_drop_geometry() %>%
  select(-c(distr_id, cell_id, sector_id, province, prov_id, geometry)) 

rutsiro_matched_by_name <- rutsiro_join.3_nordb %>% 
  filter(!is.na(customer)) %>% 
  select(villageid_key, district, sector, cell, name, household_id, first_name_x, last_name_x, first_name_y, last_name_y, readyboard, surveyed, customer) 

rutsiro_matched_by_gps <- rutsiro_join.3_nordb %>% 
  filter(!is.na(gps_match)) %>% 
  select(villageid_key, district, sector, cell, name, household_id, first_name_x, last_name_x, customer_first_name,customer_last_name, readyboard, surveyed, gps_match)


list <- list(
  "customer_match_summary" = rutsiro_join_analysis,
  "matched by name" = rutsiro_matched_by_name,
  'matched by gps and name' = rutsiro_matched_by_gps, 
  "village_comment" = rutsiro_epc_village_write,
  "household_comment" = rutsiro_epc_write
  
)

write_xlsx(list, path = file.path(data_path_2, "Rutsiro customer matches_nordb.xlsx"))

##Readyboard villages


rutsiro_14 <- rutsiro_join.3 %>% 
  filter(villageid_key %in% rutsiro_readyboard$villageid_key) %>% 
  filter(
    readyboard == 0) %>% 
  filter(customer == 1 | gps_match == 1) 



write_xlsx(
  rutsiro_14,
  path = file.path(data_path_2, "Readyboard EPC negotiation", "Rutsiro", "rutsiro customer no readyboard(readyboard village).xlsx")
)







#Rulindo----



#rulindo villages with no readyboard------

#Filter in villages with no readyboard
#Find the surveyed households that match gps of customer in no readyboard villages

rulindo_complete <-  complete_survey %>%
  filter(district == 41) %>%
  select(hh_id, first_name, last_name, district, sector, cell, village, 
         coordinate.Longitude, coordinate.Latitude) %>%
  st_as_sf(coords = c("coordinate.Longitude", "coordinate.Latitude"), crs = st_crs(4236)) %>% 
  filter(village %in% rulindo_dime$villageid_key) #Only in readyboard villages

rulindo_customer <- st_transform(rulindo_customer, crs = st_crs(4236) )


#Here starts the try#####################################################################################

# Step 1: Compute distance matrix as a numeric matrix
dist_matrix <- st_distance(rulindo_complete, rulindo_customer)
dist_matrix_numeric <- as.matrix(dist_matrix)  # convert units to numeric matrix (in meters)

# Step 2: Get index of the closest customer for each household
closest_idx <- apply(dist_matrix_numeric, 1, which.min)

# Step 3: Extract closest customer name and distance
closest_customers <- rulindo_customer[closest_idx, ] %>%
  st_drop_geometry() %>%
  select(customer_first_name = first_name, customer_last_name = last_name)

# Step 4: Extract the closest distance for each household
closest_distances <- dist_matrix_numeric[cbind(seq_len(nrow(dist_matrix_numeric)), closest_idx)]

# Step 5: Combine with household info



rulindo_matches <- rulindo_complete %>%
  st_drop_geometry() %>%
  bind_cols(closest_customers, distance_m = as.numeric(closest_distances)) %>%
  filter(distance_m < 10) %>%
  distinct(hh_id, .keep_all = TRUE) %>%
  rowwise() %>%
  filter(
    str_to_lower(customer_first_name) == str_to_lower(first_name) |
      str_to_lower(customer_first_name) == str_to_lower(last_name) |
      str_to_lower(customer_last_name) == str_to_lower(first_name) |
      str_to_lower(customer_last_name) == str_to_lower(last_name)
  ) %>%
  ungroup() %>%
  rowid_to_column("row") %>% distinct(hh_id, .keep_all = TRUE)


master_join <- master %>% select(household_id, surveyed, readyboard)

rulindo_matches <- left_join(rulindo_matches, master_join, by = c("hh_id" = "household_id")) 
# 
# rulindo_matches_nordb <- rulindo_matches %>% 
#   filter(readyboard == 0)
# 
# write_xlsx(rulindo_matches_nordb, path = file.path(data_path_2, "rulindo customer_surveyed gps matches_nordb.xlsx"))

rulindo_gps <- rulindo_matches %>% 
  mutate(
    gps_match = 1
  ) %>% 
  select(hh_id, gps_match, customer_first_name, customer_last_name, distance_m)

rulindo_join.3 <-left_join(rulindo_join.2, rulindo_gps , by = c("household_id" = "hh_id"))





rulindo_join.3_nordb <- rulindo_join.3 %>%
  filter(
    ! villageid_key %in% rulindo_readyboard$villageid_key
  )

rulindo_join_analysis <- rulindo_join.3_nordb %>% 
  st_drop_geometry() %>% 
  mutate(any_match = pmax(gps_match, customer, na.rm = TRUE)) %>%
  group_by(villageid_key) %>% 
  summarise(
    vulnerable = sum(vulnerable, na.rm = TRUE),
    customer_name_match = sum(customer, na.rm = TRUE),
    
    surveyed = sum(surveyed, na.rm = TRUE),
    gps_match = sum(gps_match, na.rm = TRUE),
    
    any_match = sum(any_match, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(rwa_village, by = c("villageid_key" = "Village_ID")) %>% 
  clean_names() %>% 
  select(villageid_key, district, sector, cell, name, everything()) %>%
  st_drop_geometry() %>%
  select(-c(distr_id, cell_id, sector_id, province, prov_id, geometry)) %>%
  
  bind_rows(
    summarise(., 
              villageid_key = "Total",
              vulnerable = sum(vulnerable),
              customer_name_match = sum(customer_name_match),
              
              surveyed = sum(surveyed),
              gps_match = sum(gps_match),
              any_match = sum(any_match)
    )
  ) %>% 
  rename(
    `gps and name match` = gps_match
  )

# End of try############################################################################################

#write file----
rulindo_epc <- read_xlsx(path = file.path(data_path_2, "Lot-Rulindo-20250601.xlsx"))

rulindo_join.3_nordb <- left_join(rulindo_join.3_nordb,rwa_village,  by = c("villageid_key" = "Village_ID")) %>% 
  clean_names() %>% 
  select(villageid_key, district, sector, cell, name, everything()) %>%
  st_drop_geometry() %>%
  select(-c(distr_id, cell_id, sector_id, province, prov_id, geometry)) 

rulindo_matched_by_name <- rulindo_join.3_nordb %>% 
  filter(!is.na(customer)) %>% 
  select(villageid_key, district, sector, cell, name, household_id, first_name_x, last_name_x, first_name_y, last_name_y, readyboard, surveyed, customer) 

rulindo_matched_by_gps <- rulindo_join.3_nordb %>% 
  filter(!is.na(gps_match)) %>% 
  select(villageid_key, district, sector, cell, name, household_id, first_name_x, last_name_x, customer_first_name,customer_last_name, readyboard, surveyed, gps_match)


list <- list(
  "customer_match_summary" = rulindo_join_analysis,
  "matched by name" = rulindo_matched_by_name,
  'matched by gps and name' = rulindo_matched_by_gps, 
  "household_comment" = rulindo_epc
  
)

write_xlsx(list, path = file.path(data_path_2,"Readyboard EPC negotiation", "Rulindo", "rulindo customer matches(no readyboard village).xlsx"))





##Readyboard villages


rulindo_6 <- rulindo_join.3 %>% 
  filter(villageid_key %in% rulindo_readyboard$villageid_key) %>% 
  filter(
    readyboard == 0) %>% 
  filter(customer == 1 | gps_match == 1) %>% select(-village_id)



write_xlsx(
  rulindo_6,
  path = file.path(data_path_2, "Readyboard EPC negotiation", "Rulindo", "rulindo customer no readyboard(readyboard village).xlsx")
)





#Karongi------



#karongi villages with no readyboard------

#Filter in villages with no readyboard
#Find the surveyed households that match gps of customer in no readyboard villages

karongi_complete <-  complete_survey %>%
  filter(district == 31) %>%
  select(hh_id, first_name, last_name, district, sector, cell, village, 
         coordinate.Longitude, coordinate.Latitude) %>%
  st_as_sf(coords = c("coordinate.Longitude", "coordinate.Latitude"), crs = st_crs(4236)) %>% 
  filter(village %in% karongi_dime$villageid_key) #Only in readyboard villages

karongi_customer <- st_transform(karongi_customer, crs = st_crs(4236) )


#Here starts the try#####################################################################################

# Step 1: Compute distance matrix as a numeric matrix
dist_matrix <- st_distance(karongi_complete, karongi_customer)
dist_matrix_numeric <- as.matrix(dist_matrix)  # convert units to numeric matrix (in meters)

# Step 2: Get index of the closest customer for each household
closest_idx <- apply(dist_matrix_numeric, 1, which.min)

# Step 3: Extract closest customer name and distance
closest_customers <- karongi_customer[closest_idx, ] %>%
  st_drop_geometry() %>%
  select(customer_first_name = first_name, customer_last_name = last_name)

# Step 4: Extract the closest distance for each household
closest_distances <- dist_matrix_numeric[cbind(seq_len(nrow(dist_matrix_numeric)), closest_idx)]

# Step 5: Combine with household info



karongi_matches <- karongi_complete %>%
  st_drop_geometry() %>%
  bind_cols(closest_customers, distance_m = as.numeric(closest_distances)) %>%
  filter(distance_m < 10) %>%
  distinct(hh_id, .keep_all = TRUE) %>%
  rowwise() %>%
  filter(
    str_to_lower(customer_first_name) == str_to_lower(first_name) |
      str_to_lower(customer_first_name) == str_to_lower(last_name) |
      str_to_lower(customer_last_name) == str_to_lower(first_name) |
      str_to_lower(customer_last_name) == str_to_lower(last_name)
  ) %>%
  ungroup() %>%
  rowid_to_column("row") %>% distinct(hh_id, .keep_all = TRUE)


master_join <- master %>% select(household_id, surveyed, readyboard)

karongi_matches <- left_join(karongi_matches, master_join, by = c("hh_id" = "household_id")) 

karongi_matches_nordb <- karongi_matches %>% 
  filter(readyboard == 0)

write_xlsx(karongi_matches_nordb, path = file.path(data_path_2, "karongi customer_surveyed gps matches_nordb.xlsx"))

karongi_gps <- karongi_matches %>% 
  mutate(
    gps_match = 1
  ) %>% 
  select(hh_id, gps_match, customer_first_name, customer_last_name, distance_m)

karongi_join.3 <-left_join(karongi_join.2, karongi_gps , by = c("household_id" = "hh_id"))





karongi_join.3_nordb <- karongi_join.3 %>%
  filter(
    ! villageid_key %in% karongi_readyboard$villageid_key
  )

karongi_join_analysis <- karongi_join.3_nordb %>% 
  st_drop_geometry() %>% 
  mutate(any_match = pmax(gps_match, customer, na.rm = TRUE)) %>%
  group_by(villageid_key) %>% 
  summarise(
    vulnerable = sum(vulnerable, na.rm = TRUE),
    customer_name_match = sum(customer, na.rm = TRUE),
    
    surveyed = sum(surveyed, na.rm = TRUE),
    gps_match = sum(gps_match, na.rm = TRUE),
    
    any_match = sum(any_match, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(rwa_village, by = c("villageid_key" = "Village_ID")) %>% 
  clean_names() %>% 
  select(villageid_key, district, sector, cell, name, everything()) %>%
  st_drop_geometry() %>%
  select(-c(distr_id, cell_id, sector_id, province, prov_id, geometry)) %>%
  
  bind_rows(
    summarise(., 
              villageid_key = "Total",
              vulnerable = sum(vulnerable),
              customer_name_match = sum(customer_name_match),
              
              surveyed = sum(surveyed),
              gps_match = sum(gps_match),
              any_match = sum(any_match)
    )
  ) %>% 
  rename(
    `gps and name match` = gps_match
  )

# End of try############################################################################################

#write file----
karongi_epc_write <- read_xlsx(path = file.path(data_path_2, "Beneficiary List of karongi.xlsx"), sheet = "household list") %>% 
  select(
    villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, Comments
  ) %>% 
  filter(villageid_key %in% karongi_join_analysis$villageid_key) 



karongi_epc_village_write <- read_xlsx(path = file.path(data_path_2, "Beneficiary List of karongi.xlsx"), sheet = "village list") %>% 
  filter(village_id %in% karongi_join_analysis$villageid_key) 

karongi_join.3_nordb <- left_join(karongi_join.3_nordb,rwa_village,  by = c("villageid_key" = "Village_ID")) %>% 
  clean_names() %>% 
  select(villageid_key, district, sector, cell, name, everything()) %>%
  st_drop_geometry() %>%
  select(-c(distr_id, cell_id, sector_id, province, prov_id, geometry)) 

karongi_matched_by_name <- karongi_join.3_nordb %>% 
  filter(!is.na(customer)) %>% 
  select(villageid_key, district, sector, cell, name, household_id, first_name_x, last_name_x, first_name_y, last_name_y, readyboard, surveyed, customer) 

karongi_matched_by_gps <- karongi_join.3_nordb %>% 
  filter(!is.na(gps_match)) %>% 
  select(villageid_key, district, sector, cell, name, household_id, first_name_x, last_name_x, customer_first_name,customer_last_name, readyboard, surveyed, gps_match)


list <- list(
  "customer_match_summary" = karongi_join_analysis,
  "matched by name" = karongi_matched_by_name,
  'matched by gps and name' = karongi_matched_by_gps, 
  "village_comment" = karongi_epc_village_write,
  "household_comment" = karongi_epc_write
)

write_xlsx(list, path = file.path(data_path_2, "Readyboard EPC negotiation", "Karongi", "karongi customer matches(no readyboard).xlsx"))




##Readyboard villages


karongi_12 <- karongi_join.3 %>% 
  filter(villageid_key %in% karongi_readyboard$villageid_key) %>% 
  filter(
    readyboard == 0) %>% 
  filter(customer == 1 | gps_match == 1) %>% select(-village_id)



write_xlsx(
  karongi_12,
  path = file.path(data_path_2, "Readyboard EPC negotiation", "karongi", "karongi customer no readyboard(readyboard village).xlsx")
)

