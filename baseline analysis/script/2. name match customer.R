##############
#Author: Xiaoming Zhang
#Date: 3.13.2025
#Purpose: Compare EDCL list
#############


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




#Customer list match=====

rwa_village <- st_read(dsn =  file.path(data_path_1, "rwa_villages", "Village.shp"))

rwa_village <- st_make_valid(rwa_village)


#Customer_lv_match-----






#Rutsiro====


folder_path <- file.path(data_path_2, "Rutsiro LV documents", "LV documents")


# List all 'customers.shp' files
shp_files <- list.files(
  path = folder_path,
  pattern = "^customers\\.shp$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

# Initialize an empty list to store data
customers_list <- list()

# Loop to read each shapefile
for (i in seq_along(shp_files)) {
  shp_path <- shp_files[i]
  
  try({
    shp_data <- st_read(shp_path, quiet = TRUE)
    customers_list[[i]] <- shp_data
  }, silent = TRUE)
}

# Bind all shapefiles into one sf object
rutsiro_customer <- do.call(rbind, customers_list)


##Join with rwa_villages----
library(dplyr)
rutsiro_customer <- rutsiro_customer %>%
  st_as_sf(crs = st_crs(rwa_village))

rutsiro_customer <- st_transform(rutsiro_customer, crs = st_crs(rwa_village))

rutsiro_customer <- st_intersection(rutsiro_customer, rwa_village)




rutsiro_customer <- rutsiro_customer %>% 
  clean_names() %>% 
  mutate(
    names = str_replace_all(names, "[^A-Za-z\\s]", ""),  # Remove non-alphabetic characters
  ) %>%
  separate(names, into = c("last_name", "first_name"), sep = " ", extra = "merge", fill = "right") %>% 
  mutate(
    first_name = str_to_title(first_name),
    last_name = str_to_title(last_name)
  )  %>% 
  mutate(
    customer = 1
  ) %>% 
  select(first_name, last_name, village_id, customer) %>% 
  mutate(
    customer_id = row_number()
  )



rutsiro_join <- master %>% 
  filter(district == "Rutsiro") 

##first round fuzzy join-----
library(fuzzyjoin)

rutsiro_join.2 <- stringdist_left_join(
  rutsiro_join,
  rutsiro_customer,
  by = c("first_name" = "first_name", "last_name" = "last_name", "villageid_key" = "village_id"),
  method = "jw",
  max_dist = 0.2,
  distance_col = "dist"
) %>%
  mutate(total_dist = first_name.dist + last_name.dist) %>%
  group_by(household_id) %>%  # replace with the relevant household ID column
  slice_min(order_by = total_dist, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  mutate(
    customer = ifelse(villageid_key != village_id, NA, customer),
    customer_id = ifelse(villageid_key != village_id, NA, customer_id)
  ) %>% 
  select(
    villageid_key,household_id, first_name.x, last_name.x, first_name.y, last_name.y, readyboard, surveyed, vulnerable, customer, customer_id
  ) 


# Step 2: Identify unmatched cases
unmatched.1 <- rutsiro_join %>%
  filter(household_id %in% (rutsiro_join.2 %>%
                              filter(is.na(customer)) %>%
                              pull(household_id)))

last_name_village_fuzzy_first <- stringdist_inner_join(
  unmatched.1,
  rutsiro_customer,
  by = c("first_name" = "first_name",  "villageid_key" = "village_id"),
  method = "jw",
  max_dist = 0.5,
  distance_col = "first_name_dist"
) %>%
  filter(last_name.x == last_name.y,
         villageid_key == village_id) %>%
  group_by(household_id) %>%
  slice_min(order_by = first_name_dist, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  select(
    villageid_key, household_id, first_name.x, last_name.x,readyboard, surveyed, vulnerable, customer, last_name.y, first_name.y, customer_id
  )



rutsiro_join.2 <- bind_rows(rutsiro_join.2, last_name_village_fuzzy_first) %>%
  arrange(desc(customer)) %>%  # Ensure customer == 1 comes first
  group_by(household_id) %>%
  slice(1) %>%
  ungroup()


# Step 3: Identify still unmatched cases

unmatched.2 <- rutsiro_join %>%
  filter(household_id %in% (rutsiro_join.2 %>%
                              filter(is.na(customer)) %>%
                              pull(household_id))) %>% 
  mutate(
    first_name =  ifelse(first_name == "Xxx", "", first_name)
  )


rutsiro_customer <- rutsiro_customer %>% 
  mutate(
    first_name = ifelse(is.na(first_name), "", first_name)
  )

first_name_village_fuzzy_last <- stringdist_inner_join(
  unmatched.2,
  rutsiro_customer,
  by = c("last_name" = "last_name"),
  method = "jw",
  max_dist = 0.1,
  distance_col = "last_name_dist"
) %>% 
  filter(first_name.x == first_name.y,
         villageid_key == village_id) %>%
  group_by(household_id) %>%
  slice_min(order_by = last_name_dist, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  select(
    villageid_key, household_id, first_name.x, last_name.x,readyboard, surveyed, vulnerable, customer, last_name.y, first_name.y, customer_id
  )


rutsiro_join.2 <- bind_rows(rutsiro_join.2, first_name_village_fuzzy_last) %>%
  arrange(desc(customer)) %>%  # Ensure customer == 1 comes first
  group_by(household_id) %>%
  slice(1) %>%
  ungroup()






rutsiro_customer_analysis <- rutsiro_join.2 %>% 
  group_by(villageid_key) %>%
  summarise(
    n_vulnerable = n(),
    n_readyboard = sum(readyboard == 1, na.rm = TRUE),
    n_readyboard_customer = sum(readyboard == 1 & customer == 1, na.rm = TRUE),
    matched_customer = sum(customer == 1, na.rm = TRUE),
    n_surveyed = sum(surveyed, na.rm = TRUE), 
    n_surveyed_customer = sum(surveyed == 1 & customer == 1, na.rm = TRUE)
  ) %>%
  bind_rows(
    summarise(.,
              villageid_key = "Total",
              n_vulnerable = sum(n_vulnerable, na.rm = TRUE),
              n_readyboard = sum(n_readyboard, na.rm = TRUE),
              n_readyboard_customer = sum(n_readyboard_customer, na.rm = TRUE),
              n_surveyed = sum(n_surveyed, na.rm = TRUE), 
              n_surveyed_customer = sum(n_surveyed_customer, na.rm = TRUE),
              matched_customer = sum(matched_customer, na.rm = TRUE)
              
    )
  ) %>% 
  select(
    villageid_key, n_vulnerable, matched_customer, n_readyboard, n_readyboard_customer, n_surveyed, n_surveyed_customer
  ) %>% 
  mutate(
    `percentage of readyboard hh found on customer list` = paste0( round(n_readyboard_customer / n_readyboard * 100, 2), "%")
  ) %>% 
  rename(
    `both on vulnerable and customer list` = matched_customer
    
  ) %>% filter(n_readyboard >0)

View(rutsiro_customer_analysis)

###Rutsiro two villages-----

rutsiro_2 <- rutsiro_customer_analysis %>% 
  filter(villageid_key %in% c("32110106", "32110105" )) 

rutsiro_join.2_2 <- rutsiro_join.2 %>% 
  filter(villageid_key %in% c("32110106", "32110105")) %>% 
  filter(!is.na(customer))

rutsiro_epc_write_2 <- read_xlsx(path = file.path(data_path_2, "Beneficiary List of Rutsiro.xlsx"), sheet = "household list") %>% 
  select(
    villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, Comments
  ) %>% 
  filter(villageid_key %in% rutsiro_2$villageid_key) 



rutsiro_epc_village_write_2 <- read_xlsx(path = file.path(data_path_2, "Beneficiary List of Rutsiro.xlsx"), sheet = "village list") %>% 
  filter(village_id %in% rutsiro_2$villageid_key) %>% 
  select(-c(...7, ...8))

list <- list(
  "customer_match_summary" = rutsiro_2,
  "matched by name" = rutsiro_join.2_2,
  "village_comment" = rutsiro_epc_village_write_2,
  "household_comment" = rutsiro_epc_write_2
)


write_xlsx(
  list,
  path = file.path(data_path_2, "Rutsiro readyboard customer match two villages.xlsx")
)


##Rutsiro save------
#FIlter in villages with readyboard, those surveyed but not receiving readyboard

rutsiro_readyboard_nocus <- rutsiro_join.2 %>% 
  filter(readyboard == 1 & is.na(customer))

write_xlsx(rutsiro_readyboard_nocus , path = file.path(data_path_2, "rutsiro readyboard hh not on customer list.xlsx") )


rutsiro_readyboard <- rutsiro_epc %>% 
  filter(readyboard == 1)




###Rutsiro readyboard VILLAGES -----
#Not receiving readyboard but on customer list

rutsiro_14 <- rutsiro_join.2 %>% 
  filter(villageid_key %in% rutsiro_readyboard$villageid_key) %>% 
  filter(readyboard == 0 & customer == 1 ) 

rutsiro_epc_write_14 <- read_xlsx(path = file.path(data_path_2, "Beneficiary List of Rutsiro.xlsx"), sheet = "household list") %>% 
  select(
    villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, Comments
  ) %>% 
  filter(villageid_key %in% rutsiro_14$villageid_key) 



rutsiro_epc_village_write_14 <- read_xlsx(path = file.path(data_path_2, "Beneficiary List of Rutsiro.xlsx"), sheet = "village list") %>% 
  filter(village_id %in% rutsiro_14$villageid_key) %>% 
  select(-c(...7, ...8))

list <- list(
  "customer_no_readyboard" = rutsiro_14,
  "village_comment" = rutsiro_epc_village_write_14,
  "household_comment" = rutsiro_epc_write_14
)


write_xlsx(
  list,
  path = file.path(data_path_2, "Rutsiro customer no readyboard.xlsx")
)


#Readyboard villages, customer found but no readyboard----







rutsiro_surveyed_nordb.1 <- rutsiro_join.2 %>% 
  filter(
    villageid_key %in% rutsiro_readyboard$villageid_key
  ) %>% 
  filter(
    readyboard == 0 & surveyed == 1
  )


rutsiro_surveyed_nordb.2 <- rutsiro_join.2 %>% 
  filter(
    !villageid_key %in% rutsiro_readyboard$villageid_key
  ) %>% 
  filter(
   surveyed == 1
  )


write_xlsx(
  list(
    "surveyed_nordb_in_rdb_village" = rutsiro_surveyed_nordb.1,
    "surveyed_nordb_in_nordb_village" = rutsiro_surveyed_nordb.2
  ),
  path = file.path(data_path_2, "rutsiro_surveyed_noreadyboard.xlsx")
)

#Vulnerable in readyboard villages with no readyboard
rutsiro_vulnerable_nordb.1 <- rutsiro_join.2 %>% 
  filter(
    villageid_key %in% rutsiro_readyboard$villageid_key
  ) %>% 
  filter(
    readyboard == 0 & vulnerable == 1
  )

#Vulnerable in no readyboard villages with no readyboard

rutsiro_vulnerable_nordb.2 <- rutsiro_join.2 %>% 
  filter(
    !villageid_key %in% rutsiro_readyboard$villageid_key
  ) %>% 
  filter(
    readyboard == 0 & vulnerable == 1
  )



write_xlsx(
  list(
    "nordb_in_nordb_village" = rutsiro_vulnerable_nordb.2,
    "nordb_in_rdb_village" = rutsiro_vulnerable_nordb.1
  ),
  path = file.path(data_path_2, "rutsiro_vulnerable_noreadyboard.xlsx")
)

#Rulindo====


folder_path <- file.path(data_path_2, "Rulindo LV documents")

# Step 1: List all Excel files ending with "info_tables.xlsx"
excel_files <- list.files(
  path = folder_path,
  pattern = "info_tables\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

# Step 2: Read "customers" sheet from each file and select relevant columns
customer_tables <- lapply(excel_files, function(file) {
  tryCatch({
    read_xlsx(path = file, sheet = "customers") %>%
      select(Names, X, Y) %>%
      mutate(source_file = basename(file))  # Optional: track file source
  }, error = function(e) {
    message("Skipping file: ", file, " — ", e$message)
    NULL
  })
})


rulindo_customer <- bind_rows(customer_tables) %>% 
  filter(!is.na(Names))

rulindo_customer <- st_as_sf(rulindo_customer, coords = c("X", "Y"), crs = st_crs(rwa_village))

rulindo_customer <- rulindo_customer %>% 
  clean_names() %>% 
  select(names, geometry)



###Join the shp -----

shp_folder_path <- file.path(data_path_2, "Rulindo LV documents", "LV documents", "No excel")

# List all shapefiles named exactly 'customers.shp'
shp_files <- list.files(
  path = shp_folder_path,
  pattern = "^customers\\.shp$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

# Initialize an empty list to store data
shp_customers_list <- list()

# Loop to read each shapefile
for (i in seq_along(shp_files)) {
  shp_path <- shp_files[i]
  
  try({
    shp_data <- st_read(shp_path, quiet = TRUE)
    shp_customers_list[[i]] <- shp_data
  }, silent = TRUE)
}

# Bind all shapefiles into one sf object
rulindo_customer_shp <- bind_rows(shp_customers_list)

rulindo_customer_shp <- rulindo_customer_shp %>% 
  clean_names() %>% 
  select(names, geometry)

rulindo_customer_shp <- st_transform(rulindo_customer_shp, crs = st_crs(rwa_village))


rulindo_customer <- rbind(rulindo_customer, rulindo_customer_shp)



rulindo_customer <- st_intersection(rulindo_customer, rwa_village)

rulindo_customer<- rulindo_customer %>% 
  clean_names() %>% 
  mutate(
    names = str_replace_all(names, "[^A-Za-z\\s]", ""),  # Remove non-alphabetic characters
  ) %>%
  separate(names, into = c("last_name", "first_name"), sep = " ", extra = "merge", fill = "right") %>% 
  mutate(
    first_name = str_to_title(first_name),
    last_name = str_to_title(last_name)
  )  %>% 
  mutate(
    customer = 1
  ) %>% 
  select(first_name, last_name, village_id, customer) %>% 
  mutate(
    customer_id = row_number()
  )


##fUZZY JOIN-----

rulindo_join <- master %>% 
  filter(district == "Rulindo") 

##first round fuzzy join-----
library(fuzzyjoin)

rulindo_join.2 <- stringdist_left_join(
  rulindo_join,
  rulindo_customer,
  by = c("first_name" = "first_name", "last_name" = "last_name", "villageid_key" = "village_id"),
  method = "jw",
  max_dist = 0.2,
  distance_col = "dist"
) %>%
  mutate(total_dist = first_name.dist + last_name.dist) %>%
  group_by(household_id) %>%  # replace with the relevant household ID column
  slice_min(order_by = total_dist, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  mutate(
    customer = ifelse(villageid_key != village_id, NA, customer),
    customer_id = ifelse(villageid_key != village_id, NA, customer_id)
  ) %>%
  select(
    villageid_key,household_id, first_name.x, last_name.x, village_id, first_name.y, last_name.y, readyboard, surveyed, vulnerable, customer, customer_id
  ) 


# Step 2: Identify unmatched cases
unmatched.1 <- rulindo_join %>%
  filter(household_id %in% (rulindo_join.2 %>%
                              filter(is.na(customer)) %>%
                              pull(household_id)))

last_name_village_fuzzy_first <- stringdist_inner_join(
  unmatched.1,
  rulindo_customer,
  by = c("first_name" = "first_name",  "villageid_key" = "village_id"),
  method = "jw",
  max_dist = 0.5,
  distance_col = "first_name_dist"
) %>%
  filter(last_name.x == last_name.y,
         villageid_key == village_id) %>%
  group_by(household_id) %>%
  slice_min(order_by = first_name_dist, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  select(
    villageid_key,household_id, first_name.x, last_name.x, village_id, first_name.y, last_name.y, readyboard, surveyed, vulnerable, customer, customer_id
  ) 



rulindo_join.2 <- bind_rows(rulindo_join.2, last_name_village_fuzzy_first) %>%
  arrange(desc(customer)) %>%  # Ensure customer == 1 comes first
  group_by(household_id) %>%
  slice(1) %>%
  ungroup()


# Step 3: Identify still unmatched cases

unmatched.2 <- rulindo_join %>%
  filter(household_id %in% (rulindo_join.2 %>%
                              filter(is.na(customer)) %>%
                              pull(household_id))) %>% 
  mutate(
    first_name =  ifelse(first_name == "Xxx", "", first_name)
  )


rulindo_customer <- rulindo_customer %>% 
  mutate(
    first_name = ifelse(is.na(first_name), "", first_name)
  )

first_name_village_fuzzy_last <- stringdist_inner_join(
  unmatched.2,
  rulindo_customer,
  by = c("last_name" = "last_name"),
  method = "jw",
  max_dist = 0.1,
  distance_col = "last_name_dist"
) %>% 
  filter(first_name.x == first_name.y,
         villageid_key == village_id) %>%
  group_by(household_id) %>%
  slice_min(order_by = last_name_dist, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  select(
    villageid_key,household_id, first_name.x, last_name.x, village_id, first_name.y, last_name.y, readyboard, surveyed, vulnerable, customer, customer_id
  ) 


rulindo_join.2 <- bind_rows(rulindo_join.2, first_name_village_fuzzy_last) %>%
  arrange(desc(customer)) %>%  # Ensure customer == 1 comes first
  group_by(household_id) %>%
  slice(1) %>%
  ungroup()


# 
# 
# still_unmatched_list <- master %>% 
#   filter(
#     household_id %in% still_unmatched$household_id,
#     readyboard == 1
#   ) %>% 
#   select(
#     villageid_key, village, cell, sector, district, household_id, first_name, last_name, gender, nid
#   )
# 
# write_xlsx(still_unmatched_list, path = file.path(data_path_2, "rulindo readyboard customer mismatch.xlsx") 


rulindo_customer_analysis <- rulindo_join.2 %>% 
  group_by(villageid_key) %>%
  summarise(
    n_vulnerable = n(),
    n_readyboard = sum(readyboard == 1, na.rm = TRUE),
    n_readyboard_customer = sum(readyboard == 1 & customer == 1, na.rm = TRUE),
    matched_customer = sum(customer == 1, na.rm = TRUE),
    n_surveyed = sum(surveyed, na.rm = TRUE), 
    n_surveyed_customer = sum(surveyed == 1 & customer == 1, na.rm = TRUE)
  ) %>%
  bind_rows(
    summarise(.,
              villageid_key = "Total",
              n_vulnerable = sum(n_vulnerable, na.rm = TRUE),
              n_readyboard = sum(n_readyboard, na.rm = TRUE),
              n_readyboard_customer = sum(n_readyboard_customer, na.rm = TRUE),
              n_surveyed = sum(n_surveyed, na.rm = TRUE), 
              n_surveyed_customer = sum(n_surveyed_customer, na.rm = TRUE),
              matched_customer = sum(matched_customer, na.rm = TRUE)
              
    )
  ) %>% 
  select(
    villageid_key, n_vulnerable, matched_customer, n_readyboard, n_readyboard_customer, n_surveyed, n_surveyed_customer
  ) %>% 
  mutate(
    `percentage of readyboard hh found on customer list` = paste0( round(n_readyboard_customer / n_readyboard * 100, 2), "%")
  ) %>% 
  rename(
    `both on vulnerable and customer list` = matched_customer
    
  ) %>% 
  filter(
    !villageid_key %in% rulindo_15$villageid_key
  )

rulindo_readyboard <- rulindo_customer_analysis %>% 
  filter(n_readyboard > 0)

##Save Rulindo-------



rulindo_6 <- rulindo_join.2 %>% 
  filter(villageid_key %in% rulindo_readyboard$villageid_key) %>% 
  filter(readyboard == 0 & customer == 1 ) 

# rulindo_epc_write_6 <- read_xlsx(path = file.path(data_path_2, "Beneficiary List of rulindo.xlsx"), sheet = "household list") %>%
#   select(
#     villageid_key, village, cell, sector, district, first_name, last_name, gender, nid, Comments
#   ) %>%
#   filter(villageid_key %in% rulindo_6$villageid_key)
# 
# 
# 
# rulindo_epc_village_write_6 <- read_xlsx(path = file.path(data_path_2, "Beneficiary List of rulindo.xlsx"), sheet = "village list") %>%
#   filter(village_id %in% rulindo_6$villageid_key) %>%
#   select(-c(...7, ...8))
# 
# list <- list(
#   "customer_no_readyboard" = rulindo_6,
#   "village_comment" = rulindo_epc_village_write_6,
#   "household_comment" = rulindo_epc_write_6
# )


write_xlsx(
  rulindo_6,
  path = file.path(data_path_2, "Readyboard EPC negotiation", "Rulindo", "rulindo customer no readyboard.xlsx")
)


#Karongi------


karongi_customer <- st_read(dsn = file.path(data_path_2, "Karongi LV documents", "LV doucuments", "customers.shp"))

karongi_customer <-st_transform(karongi_customer, crs = st_crs(rwa_village))

karongi_customer <- st_intersection(karongi_customer, rwa_village)

karongi_customer<- karongi_customer %>% 
  clean_names() %>% 
  mutate(
    names = str_replace_all(names, "[^A-Za-z\\s]", ""),  # Remove non-alphabetic characters
  ) %>%
  separate(names, into = c("last_name", "first_name"), sep = " ", extra = "merge", fill = "right") %>% 
  mutate(
    first_name = str_to_title(first_name),
    last_name = str_to_title(last_name)
  )  %>% 
  mutate(
    customer = 1
  ) %>% 
  select(first_name, last_name, village_id, customer) %>% 
  mutate(
    customer_id = row_number()
  )


##fUZZY JOIN-----

karongi_join <- master %>% 
  filter(district == "Karongi") 

##first round fuzzy join-----
library(fuzzyjoin)

karongi_join.2 <- stringdist_left_join(
  karongi_join,
  karongi_customer,
  by = c("first_name" = "first_name", "last_name" = "last_name", "villageid_key" = "village_id"),
  method = "jw",
  max_dist = 0.2,
  distance_col = "dist"
) %>%
  mutate(total_dist = first_name.dist + last_name.dist) %>%
  group_by(household_id) %>%  # replace with the relevant household ID column
  slice_min(order_by = total_dist, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  mutate(
    customer = ifelse(villageid_key != village_id, NA, customer),
    customer_id = ifelse(villageid_key != village_id, NA, customer_id)
  ) %>%
  select(
    villageid_key,household_id, first_name.x, last_name.x, village_id, first_name.y, last_name.y, readyboard, surveyed, vulnerable, customer, customer_id
  ) 


# Step 2: Identify unmatched cases
unmatched.1 <- karongi_join %>%
  filter(household_id %in% (karongi_join.2 %>%
                              filter(is.na(customer)) %>%
                              pull(household_id)))

last_name_village_fuzzy_first <- stringdist_inner_join(
  unmatched.1,
  karongi_customer,
  by = c("first_name" = "first_name",  "villageid_key" = "village_id"),
  method = "jw",
  max_dist = 0.5,
  distance_col = "first_name_dist"
) %>%
  filter(last_name.x == last_name.y,
         villageid_key == village_id) %>%
  group_by(household_id) %>%
  slice_min(order_by = first_name_dist, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  select(
    villageid_key, household_id, first_name.x, last_name.x,readyboard, surveyed, vulnerable, customer, last_name.y, first_name.y, customer_id
  )



karongi_join.2 <- bind_rows(karongi_join.2, last_name_village_fuzzy_first) %>%
  arrange(desc(customer)) %>%  # Ensure customer == 1 comes first
  group_by(household_id) %>%
  slice(1) %>%
  ungroup()


# Step 3: Identify still unmatched cases

unmatched.2 <- karongi_join %>%
  filter(household_id %in% (karongi_join.2 %>%
                              filter(is.na(customer)) %>%
                              pull(household_id))) %>% 
  mutate(
    first_name =  ifelse(first_name == "Xxx", "", first_name)
  )


karongi_customer <- karongi_customer %>% 
  mutate(
    first_name = ifelse(is.na(first_name), "", first_name)
  )

first_name_village_fuzzy_last <- stringdist_inner_join(
  unmatched.2,
  karongi_customer,
  by = c("last_name" = "last_name"),
  method = "jw",
  max_dist = 0.1,
  distance_col = "last_name_dist"
) %>% 
  filter(first_name.x == first_name.y,
         villageid_key == village_id) %>%
  group_by(household_id) %>%
  slice_min(order_by = last_name_dist, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  select(
    villageid_key, household_id, first_name.x, last_name.x,readyboard, surveyed, vulnerable, customer, last_name.y, first_name.y, customer_id
  )


karongi_join.2 <- bind_rows(karongi_join.2, first_name_village_fuzzy_last) %>%
  arrange(desc(customer)) %>%  # Ensure customer == 1 comes first
  group_by(household_id) %>%
  slice(1) %>%
  ungroup()


# 
# 
# still_unmatched_list <- master %>% 
#   filter(
#     household_id %in% still_unmatched$household_id,
#     readyboard == 1
#   ) %>% 
#   select(
#     villageid_key, village, cell, sector, district, household_id, first_name, last_name, gender, nid
#   )
# 
# write_xlsx(still_unmatched_list, path = file.path(data_path_2, "karongi readyboard customer mismatch.xlsx") 

karongi_readyboard <- karongi_epc %>% 
  filter(readyboard == 1)

karongi_customer_analysis <- karongi_join.2 %>% 
  group_by(villageid_key) %>%
  summarise(
    n_vulnerable = n(),
    n_readyboard = sum(readyboard == 1, na.rm = TRUE),
    n_readyboard_customer = sum(readyboard == 1 & customer == 1, na.rm = TRUE),
    matched_customer = sum(customer == 1, na.rm = TRUE),
    n_surveyed = sum(surveyed, na.rm = TRUE), 
    n_surveyed_customer = sum(surveyed == 1 & customer == 1, na.rm = TRUE)
  ) %>%
  filter(
    !villageid_key %in% karongi_readyboard$villageid_key
  ) %>% 
  bind_rows(
    summarise(.,
              villageid_key = "Total",
              n_vulnerable = sum(n_vulnerable, na.rm = TRUE),
              n_readyboard = sum(n_readyboard, na.rm = TRUE),
              n_readyboard_customer = sum(n_readyboard_customer, na.rm = TRUE),
              n_surveyed = sum(n_surveyed, na.rm = TRUE), 
              n_surveyed_customer = sum(n_surveyed_customer, na.rm = TRUE),
              matched_customer = sum(matched_customer, na.rm = TRUE)
              
    )
  ) %>% 
  select(
    villageid_key, n_vulnerable, matched_customer, n_readyboard, n_readyboard_customer, n_surveyed, n_surveyed_customer
  ) %>% 
  mutate(
    `percentage of readyboard hh found on customer list` = paste0( round(n_readyboard_customer / n_readyboard * 100, 2), "%")
  ) %>% 
  rename(
    `both on vulnerable and customer list` = matched_customer
    
  ) 


#Karongi save-----
# 
# karongi_readyboard_nocus <- karongi_join.2 %>% 
#   filter(readyboard == 1 & is.na(customer))
# 
# write_xlsx(karongi_readyboard_nocus , path = file.path(data_path_2, "karongi readyboard hh not on customer list.xlsx") )
# 
# karongi_customer_analysis <- karongi_join.2 %>% 
#   group_by(villageid_key) %>%
#   summarise(
#     n_vulnerable = n(),
#     n_readyboard = sum(readyboard == 1, na.rm = TRUE),
#     n_readyboard_customer = sum(readyboard == 1 & customer == 1, na.rm = TRUE),
#     matched_customer = sum(customer == 1, na.rm = TRUE),
#     n_surveyed = sum(surveyed, na.rm = TRUE), 
#     n_surveyed_customer = sum(surveyed == 1 & customer == 1, na.rm = TRUE)
#   ) %>%
#   filter(
#     villageid_key %in% karongi_readyboard$villageid_key
#   ) %>% 
#   bind_rows(
#     summarise(.,
#               villageid_key = "Total",
#               n_vulnerable = sum(n_vulnerable, na.rm = TRUE),
#               n_readyboard = sum(n_readyboard, na.rm = TRUE),
#               n_readyboard_customer = sum(n_readyboard_customer, na.rm = TRUE),
#               n_surveyed = sum(n_surveyed, na.rm = TRUE), 
#               n_surveyed_customer = sum(n_surveyed_customer, na.rm = TRUE),
#               matched_customer = sum(matched_customer, na.rm = TRUE)
#               
#     )
#   ) %>% 
#   select(
#     villageid_key, n_vulnerable, matched_customer, n_readyboard, n_readyboard_customer, n_surveyed, n_surveyed_customer
#   ) %>% 
#   mutate(
#     `percentage of readyboard hh found on customer list` = paste0( round(n_readyboard_customer / n_readyboard * 100, 2), "%")
#   ) %>% 
#   rename(
#     `both on vulnerable and customer list` = matched_customer
#     
#   ) 
# 
# 
# 
