##############
#Author: Xiaoming Zhang
#Date: 1.27.2025
#Purpose: EDCL List
#############


pacman::p_load(knitr, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr)
library(googlesheets4)
getwd()


# Import Data ----
dropbox <- 'C:/Users/wb614406/Dropbox'

data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data"
)


# Compare
scope_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/Updated scope villages& households"
)
scope_village <- read_xlsx(path = file.path(scope_path, "scope_193_0807.xlsx"))
four_district <- read_xlsx(path = file.path(data_path,"data", "four_district_2408.xlsx" ))

rwa_village <- st_read(dsn =  file.path(data_path_1, "rwa_villages", "Village.shp"))

rwa_village <- st_make_valid(rwa_village)

village_join <- rwa_village %>% 
  select(Village_ID, District, Sector, Cell, Name) %>% 
  clean_names()

readyboard_village <- master %>% 
  group_by(villageid_key, district, sector, cell, village) %>% 
  summarise( n = sum(readyboard)) %>% 
  filter( n >0 )


scope_village <- scope_village %>% 
  filter(!village_id %in% rulindo_15$villageid_key)



village_number <- complete %>% 
  filter(!villageid_key %in% rulindo_15$villageid_key) %>% 
  group_by(villageid_key, district, sector, cell, village) %>% 
  summarise(
    n = n(),
    n_baseline = sum(`Completed by Lattanzio` == "Yes")
  ) 


treatment_join <- scope_village %>% 
  select(village_id, treat, lot)

village_number <- left_join(village_number, treatment_join, by = c("villageid_key" = "village_id"))

write_xlsx(village_number, path = file.path(data_path_2, "Readyboard EPC negotiation", "village_181_hh_numbers.xlsx"))

village_number_write <- village_number %>% 
  select()


rulindo <- village_number %>% 
  filter(lot == "Rulindo")







#Rusizi------

rusizi_readyboard_village <- readyboard_village %>% 
  filter(district%in% c("Rusizi"), .keep_all = TRUE)

rusizi_village <- scope_village %>% 
  filter(district %in% c("Rusizi")) 

##Lot 1=======

rusizi1_customer <- st_read(dsn = file.path(data_path_2, "Readyboard EPC negotiation", "Rusizi", "shp", "REV1_RUSIZI_LOT_1_CUSTOMMERS.shp") )

rusizi1_customer <- st_transform(rusizi1_customer, crs = st_crs(rwa_village))
rusizi1_customer <- st_intersection(rusizi1_customer, rwa_village)

rusizi_dime <- scope_village %>% 
  filter(
    district == "Rusizi"
  )


rusizi1_customer_village <- rusizi1_customer %>% 
  group_by(Village_ID) %>% 
  summarise( n = n()) 



##Lot 2-------

rusizi2_customer <- st_read(dsn = file.path(data_path_2, "Readyboard EPC negotiation", "Rusizi", "shp", "Rusizi_LOT_2_Custommers_REV2.shp") )

rusizi2_customer <- st_transform(rusizi2_customer, crs = st_crs(rwa_village))
rusizi2_customer <- st_intersection(rusizi2_customer, rwa_village)


rusizi2_customer_village <- rusizi2_customer %>% 
  group_by(Village_ID) %>% 
  summarise( n = n()) 




rusizi_customer_village <- rbind(rusizi1_customer_village, rusizi2_customer_village)



rusizi_in_scope <- rusizi_village %>% 
  filter(village_id %in% rusizi_customer_village$Village_ID) %>% 
  select(village_id, province, district, lot, sector, cell, name, scope_2407, ubudehe_1, treat)

rusizi_discrepency <- rusizi_village %>% 
  filter(!village_id %in% rusizi_customer_village$Village_ID) %>% 
  select(village_id, province, district, lot, sector, cell, name, scope_2407, ubudehe_1, treat)


rusizi_epc_discrepency <- rusizi_readyboard_village %>% 
  filter(!villageid_key %in% rusizi_customer_village$Village_ID)
  


write_xlsx(
  list(
    "Scope village keeped" = rusizi_in_scope,

    "Scope village dropped" = rusizi_discrepency,
    
    "EPC not in GIS list" = rusizi_epc_discrepency
  ),
  path = file.path(
    data_path_2,
    "Readyboard EPC negotiation",
    "Rusizi",
    "rusizi_scope_discrepency.xlsx"
  )
)
  
  
write_xlsx(rusizi_discrepency, path = file.path(data_path_2, "Readyboard EPC negotiation", "Rusizi", "rusizi_epc_discrepency.xlsx"))



#Rutsiro====

rutsiro_villages <- scope_village %>% 
  filter(district == "Rutsiro")


rutsiro_other_village <- rutsiro_villages %>% 
  filter(!village_id %in% rutsiro_dime_village$village_id)

rutsiro_readyboard <- readyboard_village %>% 
  filter(district == "Rutsiro") 


rutsiro_scope_updated  <- read_xlsx(path = file.path(data_path_2, "Readyboard EPC negotiation", "Rutsiro", "Villages_in_implementedScope_Rutsiro_All_Lots.xlsx"))


rutsiro_scope_updated <- rutsiro_scope_updated %>% 
  clean_names() %>% 
  rename(
    cell = cellule,
    name = village
  ) 

rutsiro_scope_updated <- left_join(
  rutsiro_scope_updated, village_join , 
  by = c("name", "sector","cell", "district")
                                   )

rutsiro_discrepency <- rutsiro_villages %>% 
  filter(!village_id %in% rutsiro_scope_updated$village_id )



rutsiro_complete <- complete %>% 
  filter(district == "RUTSIRO") %>% 
  group_by(villageid_key, village) %>% 
  summarise(
    n = n(),
    n_baseline = sum(`Completed by Lattanzio` == "Yes")
  )



rutsiro_0 <- rutsiro_complete %>% 
  filter(n_baseline == 0)

#Karongi------
karongi_villages <- scope_village %>% 
  filter(district == "Karongi")

karongi_other_village <- karongi_villages %>% 
  filter(!village_id %in% karongi_dime_village$village_id)

karongi_readyboard <- readyboard_village %>% 
  filter(district == "Karongi") 



karongi_scope_updated  <- read_xlsx(path = file.path(data_path_2, "Readyboard EPC negotiation", "karongi", "Villages_in_implementedScope_Karongi_All_Lots.xlsx"))


karongi_scope_updated <- karongi_scope_updated %>% 
  clean_names() %>% 
  rename(
    cell = cellule,
    name = village
  ) 

karongi_scope_updated <- left_join(
  karongi_scope_updated, village_join , 
  by = c("name", "sector","cell", "district")
)

karongi_discrepency <- karongi_villages %>% 
  filter(!village_id %in% karongi_scope_updated$village_id )

karongi_complete <- complete %>% 
  filter(district == "KARONGI") %>% 
  group_by(villageid_key, village) %>% 
  summarise(
    n = n(),
    n_baseline = sum(`Completed by Lattanzio` == "Yes")
  )


























#Archive--------

#Rusizi double check----


rusizi_lot1 <- read_xlsx(path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/New EDCL List/Rusizi/PM/Customers to be connected_RUSIZI LOT-1.xlsx")
rusizi_lot2 <- read_xlsx(path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/New EDCL List/Rusizi/PM/Customers to be connected_RUSIZI LOT 2.xlsx")

rusizi_lot1 <- rusizi_lot1 %>% 
  select(X,Y)

rusizi_lot2 <- rusizi_lot2 %>% 
  select(X,Y)

rusizi_pm <- rbind(rusizi_lot1, rusizi_lot2)

rusizi_pm <- st_as_sf(rusizi_pm, coords = c("X", "Y"), crs = st_crs(rwa_villages))  # Assuming WGS84 projection


rusizi_villages <- rwa_villages %>% 
  filter(District == "Rusizi") 

# Spatial join: Assign village names from `rwa_villages` to `rusizi_sf`
rusizi_pm<- st_intersection(rusizi_pm, rwa_villages)  # Adjust column name if different


rusizi_pm <- rusizi_pm %>% 
  distinct(Village_ID, .keep_all = TRUE) 

rusizi_discrepency_1 <- rusizi_dime %>% 
  filter(!village_id %in% rusizi_pm$Village_ID)

rusizi_weird <- rusizi_epc %>% 
  filter(!Code_vil_2 %in% rusizi_pm$Village_ID)

write_xlsx(rusizi_weird, path = file.path(data_path, "Rusizi_EPC_PM_discrepency.xlsx"))

rusizi_discrepeny_treat <- scope_treat %>% 
  filter(village_id %in% rusizi_discrepency$village_id)


write_xlsx(rusizi_dime, path = file.path(data_path, "Rusizi_randomization_list.xlsx"))



#Rusizi EPC customer check-------

rusizi_epc1 <- read_xls(path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/New EDCL List/Rusizi/EPC/Detailed customer list for EPC Rusizi..xls", sheet = "EPC RUSIZI-LOT_1")

rusizi_epc2 <- read_xls(path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/New EDCL List/Rusizi/EPC/Detailed customer list for EPC Rusizi..xls", sheet = "EPC RUSIZI-LOT_2")

rusizi_epc_customer <- rbind(rusizi_epc1, rusizi_epc2)

rusizi_epc_customer <- rusizi_epc_customer %>% 
  mutate(
    across(
      c(Village, Cell, Sector),
      ~ str_to_title(.x)
      
    )) %>% 
  rename(
    village = Village,
    cell = Cell,
    sector = Sector
  ) %>% 
  mutate(
    district = "Rusizi"
  ) 

village_join <- four_district %>% 
  select(village_id, name, cell, sector, district) %>% 
  rename(
    village = name
  
  )

rusizi_epc_customer <- rusizi_epc_customer %>% 
  left_join(village_join, by = c("district", "sector", "cell", "village"))

rusizi_epc_customer_village <- rusizi_epc_customer %>% 
  distinct(district, sector, cell, village, .keep_all = TRUE) %>% 
  select(district, sector, cell, village, village_id)


rusizi_discrepency_epccustomer <- rusizi_dime %>% 
  filter(!village_id %in% rusizi_epc_customer_village$village_id)
write_xlsx(rusizi_discrepency_epccustomer, path = file.path(data_path, "Rusizi_EPC_customer_discrepency.xlsx"))

#Karongi-----

rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))
rwa_villages <- st_make_valid(rwa_villages)

karongi_customer <- read_xls(path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/New EDCL List/Karongi/PM/Customers under ongoing EPC-Karongi.xls")

# Convert to sf object with X (longitude) and Y (latitude)
karongi_sf <- st_as_sf(karongi_customer, coords = c("x", "y"), crs = st_crs(rwa_villages))  # Assuming WGS84 projection


karongi_villages <- rwa_villages %>% 
  filter(District == "Karongi") 

# Spatial join: Assign village names from `rwa_villages` to `karongi_sf`
karongi_scope <- st_intersection(karongi_sf, rwa_villages)  # Adjust column name if different


karongi_scope <- karongi_scope %>% 
  distinct(Village_ID) 


karongi_dime <- four_district_scope %>% 
  filter(
    district == "Karongi"
  )

karongi_discrepency <- karongi_dime %>% 
  filter(!village_id %in% karongi_scope$Village_ID)



#Rutsiro------



rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))
rwa_villages <- st_make_valid(rwa_villages)

rutsiro_customer <- read_xls(path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/New EDCL List/Rutsiro/PM/Customers under ongoing EPC-Rutsiro.xls")

# Convert to sf object with X (longitude) and Y (latitude)
rutsiro_sf <- st_as_sf(rutsiro_customer, coords = c("x", "y"), crs = st_crs(rwa_villages))  # Assuming WGS84 projection


rutsiro_villages <- rwa_villages %>% 
  filter(District == "Rutsiro") 

# Spatial join: Assign village names from `rwa_villages` to `rutsiro_sf`
rutsiro_scope <- st_intersection(rutsiro_sf, rwa_villages)  # Adjust column name if different


rutsiro_scope <- rutsiro_scope %>% 
  distinct(Village_ID) 


rutsiro_dime <- four_district_scope %>% 
  filter(
    district == "Rutsiro"
  )

rutsiro_discrepency <- rutsiro_dime %>% 
  filter(!village_id %in% rutsiro_scope$Village_ID)






#Rulindo------



rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))
rwa_villages <- st_make_valid(rwa_villages)

rulindo_customer1 <- read_xlsx(path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/New EDCL List/Rulindo/PM/Customer_EPC Rulindo_Initial-Complementary Scope.xlsx")
rulindo_customer2 <- read_xlsx(path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/New EDCL List/Rulindo/PM/Customer_EPC Rulindo_Initial-Complementary Scope.xlsx", sheet = "Customer_Compl")

rulindo_customer <- bind_rows(rulindo_customer1, rulindo_customer2)

rulindo_customer_village <- rulindo_customer %>% 
  distinct(district, sector, cell, village, .keep_all = TRUE) %>% 
  mutate(sector = str_to_title(sector)) %>% 
  left_join(rulindo_village, by = c("district" = "District",
                                    "sector" = "Sector",
                                    "cell" = "Cell",
                                    "village" = "Name"))

write_xlsx(rulindo_customer_village, path =  file.path(data_path, "rulindo customer village.xlsx"))
# Convert to sf object with X (longitude) and Y (latitude)
rulindo_sf <- st_as_sf(rulindo_customer, coords = c("x", "y"), crs = st_crs(rwa_villages))  # Assuming WGS84 projection


rulindo_villages <- rwa_villages %>% 
  filter(District == "Rulindo") 

# Spatial join: Assign village names from `rwa_villages` to `rulindo_sf`
rulindo_scope <- st_intersection(rulindo_sf, rwa_villages)  # Adjust column name if different


rulindo_scope <- rulindo_scope %>% 
  distinct(Village_ID, .keep_all = TRUE) 


rulindo_dime <- four_district_scope %>% 
  filter(
    district == "Rulindo"
  )

rulindo_discrepency <- rulindo_dime %>% 
  filter(!village_id %in% rulindo_scope$Village_ID)




#15KV----


rulindo_15 <- read_xlsx(path = file.path(data_path, "Villages falling in 15kV_EPC Rulindo.xlsx"))



rulindo_discrepency_15 <- rulindo_15 %>% 
  filter(Code_vill%in% rulindo_dime$village_id)

write_xlsx(rulindo_discrepency_15, path = file.path(data_path, "Rulindo_15kv.xlsx"))


rulindo_discrepency_15_check <- rulindo_15 %>% 
  filter(Code_vill%in% rulindo_discrepency$village_id)


one_last <- rulindo_discrepency %>% 
  filter(!village_id %in% rulindo_discrepency_15$Code_vill)













scope_treat <- read_xlsx( path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/Updated scope villages& households/scope_193_0807.xlsx")


rulindo_check <- scope_treat %>% 
  filter(village_id %in% rulindo_discrepency$village_id) %>% 
  select(
    village_id, name, cell, sector, district, province, lot, treat
  )

rulindo_survey <- hfc_constr %>% 
  filter(village %in% rulindo_check$village_id) %>% 
  group_by(village) %>% 
  summarise(
    completed_survey = n()
  ) %>% 
  mutate(
    village = as.character(village)
  )
  
rulindo_check <- left_join(rulindo_check, rulindo_survey, by = c("village_id" = "village"))
  
  
  
  
  
write_xlsx(rulindo_check, path = file.path(output_path, "rulindo_discrepency.xlsx"))


##Rulindo graph----

rulindo_previous <- scope_villages %>% 
  filter(
    scope_2407 == 1 & district == "Rulindo"
  ) %>% 
  select(
    village_id
  ) %>% 
  mutate(
    previous_scope = 1
  )

rulindo_new <- rulindo_scope %>% 
  mutate(
    new_scope =1 
  )

rulindo_plot <- left_join(rulindo_villages, rulindo_previous, by = c("Village_ID" = "village_id"))

rulindo_plot <- left_join(rulindo_plot, rulindo_new, by = c("Village_ID"))



rulindo_rand <- rulindo_dime %>% 
  select(village_id) %>% 
  mutate(
    randomized  = 1
  )


rulindo_plot <- left_join(rulindo_plot, rulindo_rand, by = c("Village_ID" = "village_id"))




library(ggplot2)
library(sf)
library(dplyr)

# Ensure NA values in randomized column are handled
rulindo_plot <- rulindo_plot %>%
  mutate(
    scope_category = case_when(
      previous_scope == 1 & is.na(new_scope) ~ "Initial Scope",
      is.na(previous_scope) & new_scope == 1 ~ "New Scope",
      previous_scope == 1 & new_scope == 1 ~ "Both Scopes",
      TRUE ~ "Other"
    ),
    randomized = ifelse(is.na(randomized), "Not Randomized", "Randomized")  # Convert to categorical variable
  )

# Convert randomized column to a factor
rulindo_plot$randomized <- factor(rulindo_plot$randomized, levels = c("Not Randomized", "Randomized"))

# Plot the map
ggplot(rulindo_plot) +
  geom_sf(aes(fill = scope_category, alpha = randomized), color = "black") +
  scale_fill_manual(values = c(
    "Initial Scope" = "blue",
    "New Scope" = "red",
    "Both Scopes" = "purple"
  )) +
  scale_alpha_manual(values = c("Not Randomized" = 0.3, "Randomized" = 1.0)) +  # Darker for randomized, lighter otherwise
  labs(title = "Villages in Rulindo by Scope and Randomization",
       fill = "Scope Category",
       alpha = "Randomized Status") +
  theme_minimal()

ggplot(rulindo_plot) +
  geom_sf(aes(fill = scope_category), color = "black") +
  scale_fill_manual(values = c(
    "Initial Scope" = "blue",
    "New Scope" = "red",
    "Both Scopes" = "purple",
    "Other" = "lightgrey"
  )) +
  labs(title = "Villages in Rulindo by Scope",
       fill = "Scope Category") +
  theme_minimal()



#Filter for southeast----


# Define bounding box (adjust as needed based on visual analysis)
bbox <- st_as_sf(st_sfc(st_polygon(list(matrix(c(
  29.99, -1.90,  # Bottom-left (shifted right)
  30.15, -1.90,  # Bottom-right (extended right)
  30.15, -1.75,  # Top-right (extended right)
  29.99, -1.75 ,
  29.99, -1.90# Closing the polygon
), ncol = 2, byrow = TRUE)))), crs = 4326)



rulindo_plot <- st_transform(rulindo_plot, crs = 4326)


# Filter villages within the bounding box
filtered_villages <- st_filter(rulindo_plot, bbox)

# Plot to verify
ggplot(filtered_villages) +
  geom_sf(aes(fill = scope_category), color = "black") +
  scale_fill_manual(values = c(
    "Initial Scope" = "blue",
    "New Scope" = "red",
    "Both Scopes" = "purple",
    "Other" = "lightgrey"
  )) +
  labs(title = "Filtered Southeast Initial Scope Villages in Rulindo",
       fill = "Scope Category") +
  theme_minimal()



filtered_villages_join <- filtered_villages %>% 
  mutate(
    scope = "Initial Scope Southwest"
  ) %>% 
  select(Village_ID, scope) %>% 
  as.data.frame()

rulindo_plot <- rulindo_plot %>% 
  filter(scope_category == "Initial Scope")


rulindo_check <- left_join(rulindo_plot, filtered_villages_join, by = c("Village_ID"))

rulindo_check <- rulindo_check %>% 
  mutate(scope= ifelse(is.na(scope), "Initial Scope Other", scope)) %>% 
  st_drop_geometry() %>% 
  select(-previous_scope, -new_scope, -scope_category, - geometry.y) 

  
  
write_xlsx(rulindo_check, path = file.path(output_path, "rulindo follow up.xlsx"))




#Exclude discrepency----


discrepency <- rbind(karongi_discrepency, rutsiro_discrepency, rulindo_discrepency, rusizi_discrepency)

discrepency <- discrepency %>% 
  mutate(
    scope_2502 = 0
  ) %>% 
  select(village_id, scope_2502)

 four_scope_new <- left_join(scope_treat, discrepency, by = c("village_id"))



four_scope_new <- four_scope_new %>% 
  mutate(
    scope_2502 = ifelse(is.na(scope_2502), 1, 0)
  )


four_scope_2502 <- four_scope_new %>% 
  filter(
    scope_2502 == 1
  )



old <- four_scope_new %>% 
  group_by(district, treat) %>% 
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = treat, values_from = n)


total_row <- old %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(district = "Total")  # set the district name as "Total"

old <- bind_rows(old, total_row)

old <- old %>% 
  mutate(total = rowSums(across(where(is.numeric)), na.rm = TRUE))

new <- four_scope_2502 %>% 
  group_by(district, treat) %>% 
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = treat, values_from = n)

total_row <- new %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(district = "Total")  # set the district name as "Total"

new <- bind_rows(new, total_row)


new <- new %>% 
  mutate(total = rowSums(across(where(is.numeric)), na.rm = TRUE))


#Households dropped----

household <- four_scope_new %>% 
  filter(scope_2502 == 0) %>% 
  group_by(treat, district) %>% 
  summarise(
    n = sum(hh_head_06), .groups= "drop"
  ) %>% 
  pivot_wider(names_from = treat, values_from = n) 
  
total_row <- household %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(district = "Total")  # set the district name as "Total"

household <- bind_rows(household, total_row)

household <- household %>% 
  mutate(total = rowSums(across(where(is.numeric)), na.rm = TRUE))



#Village hfc----

village_hfc <- read_xlsx(path = file.path(data_path, "baseline analysis" , "data", "village_check_hfc.xlsx"))

village_discrepency <- village_hfc %>% 
  filter(villageid_key %in% discrepency$village_id)

village_discrepency <- village_discrepency %>% 
  mutate()

write_xlsx(village_discrepency, path = file.path(data_path, "baseline analysis" , "data", "village_discrepency_hfc.xlsx"))

village_uncomplete <- village_hfc %>% 
  mutate(
    completed_village = case_when(
      hh_head_06 == attempt ~ 1,
      complete >= 20 ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  filter(
    completed_village == 0
  )


village_complete <- village_hfc %>% 
  mutate(
    completed_village = case_when(
      hh_head_06 == attempt ~ 1,
      complete >= 20 ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  filter(
    completed_village == 1
  )

  
village_uncomplete_newscope <- village_hfc %>% 
  mutate(
    completed_village = case_when(
      hh_head_06 == attempt ~ 1,
      complete >= 20 ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  filter(
    completed_village == 0
  ) %>% 
  filter(!villageid_key %in% discrepency$village_id )


village_uncomplete_discrepency <- village_hfc %>% 
  filter(!villageid_key %in% discrepency$village_id ) %>% 
  mutate(
    expected_complete = round(pmin(20, hh_head_06 * (complete / attempt)),2),
    expected_attempt = round(expected_complete * (attempt / complete),2)
  ) %>% 
  relocate(expected_complete, expected_attempt, .after = complete)

attempt_10 <- village_uncomplete_discrepency %>% 
  filter(attempt >= 10) %>% 
  mutate(
    expected_complete = ifelse(is.na(expected_complete), 0, expected_complete)
  )

mean(attempt_10$expected_complete, na.rm = TRUE)
mean(attempt_10$expected_attempt, na.rm = TRUE)


#Discrepency ----



discrepency <- rbind(rulindo_discrepency, rutsiro_discrepency, karongi_discrepency, rusizi_discrepency)
write_xlsx(discrepency, path = file.path(data_path, "discrepency.xlsx"))


scope_new <- scope_treat %>% 
  filter(!village_id %in% discrepency$village_id)

unique_lots <- unique(scope_new$lot)  # Get unique lots


for (lot in unique_lots) {
  readyboard_village <- four_scope_newly %>% 
    filter(treat == "T1" |treat == "T3" ) %>% 
    filter(lot == !!lot) %>% # Use `!!` to evaluate `lot` variable
    select(village_id, name, cell, sector, district, province)
  
  readyboard_household <- household_head %>% 
    filter(villageid_key %in% readyboard_village$village_id) %>% 
    select(villageid_key, village, cell, sector, district, first_name, last_name, gender, nid )
  
  list <- list("village list" = readyboard_village, "household list" = readyboard_household)
  
  file_name <- paste0("Lot_", lot, ".xlsx")
  write_xlsx(list, path = file.path(scope_path, "EDCL", file_name))
}

readyboard <- scope_newly %>% 
  filter(treat == "T1" | treat == "T3") %>% 
  group_by(lot) %>% 
  summarise(vulnerable = sum(hh_head_06))

write_xlsx(readyboard, path = file.path(scope_path, "EDCL", "readyboard distribution per lot.xlsx"))



#Rulindo





















