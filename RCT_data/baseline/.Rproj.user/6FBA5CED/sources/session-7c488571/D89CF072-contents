##############
#Author: Xiaoming Zhang
#Date: 1.31.2024
#Purpose: Place for clean scope codes, not including trials and errors
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
  "baseline/data/data/four_district_2402.xlsx"
)

#read file----
four_district_2402 <- read_xlsx(path)



#Status update----

#"newly" is in the randomization scope, "partial" is not

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

table(four_district_2402.1$scope_2401)

four_district_2402.1  <- four_district_2402.1 %>% 
  mutate(
    status = ifelse(ubudehe_1 <20, "partial", status)
  )


four_district_2402.1 <- four_district_2402.1 %>% 
  mutate(meter_percent = round(meter_eucl/total_hh, 2))

four_district_2402.1 <- four_district_2402.1 %>% 
  mutate(
    status = ifelse(meter_percent >= 0.3 , "partial", status)
  )


write_xlsx(four_district_2402.1, path = file.path(data_path, "four_district_2402.xlsx"))


#Karongi+Rutsiro+Rulindo+Rusizi----

four_scope_2402.1 <- four_district_2402.1 %>% 
  filter( scope_2402 == 1)


four_scope_2402.1 %>% 
  summarise(n = sum(customer)) %>% 
  


table(four_scope_2402.1$status)

#30% percent of meter and 20 ubudehe

four_scope_2402.2 <- four_scope_2402.1 %>% 
  filter(meter_percent < 0.3 & ubudehe_1 >= 20)

table(four_scope_2402.2$status)  

four_scope_2402.2 %>% 
  filter(status == "newly") %>% 
  group_by(district) %>% 
  summarise(
    village_n = n(),
    ubudehe_1 = village_n * 20
  )














  
  
  