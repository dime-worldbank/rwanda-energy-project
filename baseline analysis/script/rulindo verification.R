##############
#Author: Xiaoming Zhang
#Date: 2.27.2025
#Purpose: Rulindo Verification
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


#Rulindo villages----

rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))
rwa_villages <- st_make_valid(rwa_villages)

rulindo_village <- rwa_villages %>% 
  filter(District == "Rulindo") %>% 
  select(District, Sector, Cell, Name, Village_ID) %>% 
  st_drop_geometry() %>% 
  mutate(
    Sector = str_to_title(Sector)
  )


#Readyboard from EPC-----
file <- file.path(data_path, "Copy of Copy of READY BORDS LIST.xlsx")

sheets <- excel_sheets(file)

data_list <- lapply(sheets, function(sheet) {
  read_xlsx(file, sheet = sheet)
})

rulindo_224 <- do.call(rbind, data_list)

rulindo_224 <- rulindo_224 %>% 
  mutate(
    District = str_to_title(District),
    Sector = str_to_title(Sector),
    Cell = str_to_title(Cell),
    Village = str_to_title(Village)
  )  %>% 
  separate(`Names of head`, into = c("last_name", "first_name"), 
               sep = " ", extra = "merge", fill = "right") %>% 
  mutate(
    last_name = str_to_title(last_name)
  )

rulindo_224 <- rulindo_224 %>% 
  mutate(
    Cell = ifelse(Cell == "Rwiri", "Rwili", Cell)
  )

rulindo_224 <- left_join(rulindo_224, rulindo_village, by = c("District" = "District",
                                                              "Sector" = "Sector",
                                                              "Cell" = "Cell",
                                                              "Village" = "Name"))

rulindo_224_village <- rulindo_224 %>% 
  distinct(Village_ID, District, Sector, Cell, Village)




#Rulindo customer-----


rulindo_customer_village <- read_xlsx(path =  file.path(data_path, "rulindo customer village.xlsx"))

rulindo_check <- rulindo_224_village %>% 
  filter(!Village_ID %in% rulindo_customer_village$Village_ID) 

rulindo_discrepency_customer_readyboard <- rulindo_224_village%>% 
  filter(!Village_ID %in% rulindo_customer_village$Village_ID)


#DIME SCOPE-----

scope_treat <- read_xlsx( path = file.path(data_path, "Updated scope villages& households", "scope_193_0807.xlsx"))

rulindo_readyboard <- scope_treat %>% 
  filter(district == "Rulindo") %>% 
  filter(treat == "T1" | treat == "T3")

rulindo_12 <- read_xlsx(path = file.path(data_path, "Rulindo_15kv.xlsx"))

rulindo_readyboard <- rulindo_readyboard %>% 
  filter(!village_id %in% rulindo_12$Code_vill)

rulindo_readyboard_check <- rulindo_discrepency_readyboard %>% 
  filter(!village_id %in% rulindo_customer_village$Village_ID)

#Match with DIME-------
rulindo_discrepency_readyboard <- rulindo_readyboard %>% 
  filter(!village_id %in% rulindo_224_village$Village_ID)

rulindo_check <- rulindo_discrepency_readyboard %>% 
  filter(village_id %in% rulindo_customer_village$Village_ID)

rulindo_discrepency_customer <- rulindo_readyboard %>% 
  filter(!village_id %in% rulindo_customer_village$Village_ID)

rulindo_discrepency_EPC_PM <- rulindo_224_village %>% 
  filter(!Village_ID %in% rulindo_customer_village$Village_ID)

sum(rulindo_readyboard$customer)



rulindo_check <- rulindo_224_village %>% 
  filter(Village_ID %in% rulindo_12$Code_vill)

write_xlsx(rulindo_discrepency_readyboard, path = file.path(data_path, "rulindo_readyboard_discrepency.xlsx"))
write_xlsx(rulindo_discrepency_customer, path = file.path(data_path, "rulindo_readyboard_customer.xlsx"))
write_xlsx(rulindo_discrepency_EPC_PM, path = file.path(data_path, "rulindo_discrepency_EPC_PM.xlsx"))
write_xlsx(rulindo_readyboard, path = file.path(data_path, "rulindo_readyboard_13.xlsx"))



#Rulindo household-----

rulindo_main <-read_xlsx( path = file.path(data_path, "Updated scope villages& households", "Survey firm", "Lot_Rulindo.xlsx"), sheet = "household list")
rulindo_replacement <-read_xlsx( path = file.path(data_path, "Updated scope villages& households", "Survey firm", "Lot_Rulindo.xlsx"), sheet = "replacement_household")

rulindo_hh <- rbind(rulindo_main, rulindo_replacement)

rulindo_hh_join <- rulindo_hh %>% 
  select(villageid_key, household_id, nid) %>% 
  filter(villageid_key %in% rulindo_readyboard$village_id)

n_distinct(rulindo_hh_join$villageid_key)

rulindo_224_hh_join <- left_join(rulindo_224, rulindo_hh_join, by = c("Village_ID" = "villageid_key",
                                                                      "NID" = "nid"))
rulindo_224_hh_exist <- rulindo_224_hh_join %>% 
  filter(!is.na(household_id))

rulindo_224_hh_exist_village <- rulindo_224_hh_exist %>% 
  distinct(Village_ID, .keep_all = TRUE)

write_xlsx(rulindo_224_hh_exist, path = file.path(data_path, "rulindo_10hh_inEPC.xlsx"))

rulindo_14_hh <- rulindo_readyboard %>% 
 filter(village_id %in% rulindo_224_hh_exist$Village_ID)







