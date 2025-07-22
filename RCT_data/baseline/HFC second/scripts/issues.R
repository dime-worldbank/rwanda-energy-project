##########################
###HFC issues
#11.17.2024
#Xiaoming Zhang
#######################################


pacman::p_load(knitr, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr)

getwd()

kararo <- hfc_constr %>% filter(village_key == "Kararo")

kararo_id<- deployment_list %>% 
  filter(household_id %in% kararo$hh_id) 









village_backcheck <- village %>% 
  filter(
    villageid_key  == "41140403"
    
  ) %>% 
  select(
    districtid_key, district_key, sectorid_key, sector_key, cellid_key, cell_key, villageid_key, village_key
)


write_xlsx(village_backcheck, path = file.path(bc_output, "village_41140403.xlsx"))    



#Backcheck issue
complete_last <- read_xlsx(path = file.path(data_path, "vulnerable households in sample villages.xlsx"))

complete_last <- complete_last %>% 
  filter(
    `Approached by Lattanzio` == "No"  & `In Fully Completed village` == "No" & `Dropped from scope due to 15kv` == "No"
  )

check <- hfc_constr %>% 
  filter(
    hh_id %in% c("411404030434", "411404030153", "411401030071", "411401030497")
  )



surveyed <- hfc_constr %>%
  filter(
    hh_id %in% complete_last$household_id
  )


scope <- read_xlsx(path = file.path(data_path, "vulnerable households in sample villages.xlsx"))
rulindo_15 <- scope %>% 
  filter(
    `Dropped from scope due to 15kv` == "Yes"
  ) %>% 
  select(household_id, villageid_key)


rulindo_readyboard <- villageid_join %>% 
  filter(treatment == "T1" | treatment == "T3") %>% 
  left_join(
    village, by= c("villageid_key" = "villageid_key")
  ) %>% 
  filter(
    district_key == "Rulindo"
  ) %>% 
  filter(
    !villageid_key %in% rulindo_15$villageid_key
  ) 
