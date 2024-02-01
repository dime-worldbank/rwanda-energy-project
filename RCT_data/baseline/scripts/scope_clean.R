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
  "baseline/data/data/four_district_2401.xlsx"
)

#read file----
four_district_2401 <- read_xlsx(path)


#Status update----

#"newly" is in the randomization scope, "partial" is not

 four_district_2401.1 <- four_district_2401 %>% 
  mutate(any_offgrid = case_when(
    et_sum !=0 | priority_0 == 0 ~ "partial",
    .default = "newly"
  ))

four_district_2401.1 <- four_district_2401.1 %>% 
  mutate(
    any_grid = case_when(
      grid_status %in% c("newly") & nep_revision%in% c("GE", "Fill In")  ~ "newly",
      .default = "partial"
    )
  )

four_district_2401.1 <- four_district_2401.1  %>% 
  mutate(status = case_when(
    any_grid == "newly" & any_offgrid == "newly" ~ "newly",
    .default = "partial"
  ))


four_district_2401.1  <- four_district_2401.1 %>% 
  mutate(
    status = ifelse(ubudehe_1 <20, "partial", status)
  )

four_district_2401.1 <- four_district_2401.1%>% 
  mutate(scope_2401 = ifelse(district %in% c("Rutsiro", "Rusizi"), scope_1024, scope_2401))

four_district_2401.1 <- four_district_2401.1 %>% 
  mutate(meter_percent = round(meter_eucl/total_hh, 2))


write_xlsx(four_district_2401.1, here("data", "four_district.xlsx"))


#Karongi+Rutsiro+Rulindo----

three_scope_2401.1 <- four_district_2401.1 %>% 
  filter(district %in% c("Karongi", "Rulindo", "Rutsiro") & scope_2401 == 1)


table(three_scope_2401.1$status)