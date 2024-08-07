##############
#Author: Xiaoming Zhang
#Date: 7.30.2024
#Purpose: Randomization primary construction
#############


pacman::p_load(knitr, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr)

getwd()

#Dropbox----


if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}


path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/data/data/four_district_2402.xlsx"
)


data_path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/data/data"
)

scope_path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/data/Updated scope villages& households"
)

output_path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/outputs"
)

four_district <- read_xlsx(path = file.path(data_path, "four_district_2408.xlsx"))

four_scope_newly<- four_district %>% 
  filter(scope_2407 == 1 & status == "newly")

table(four_scope_newly$lot)
                          
#randomization----


four_scope_newly <- four_scope_newly %>%
  group_by(lot) %>%
  arrange(lot, hh_head_06) %>% 
  mutate(
    median_position = ceiling(n() / 2),  # Calculate median position within each group
    hh_head_06_order = order(hh_head_06),  # Get the order of hh_head_06 values
    median = ifelse(rank(hh_head_06_order) <= median_position, "below", "above")
  ) %>%
  ungroup() 

four_scope_newly %>% 
  group_by(lot) %>% 
  summarise(
    n = n(),
    survey = n*20,
    vulnerable = sum(hh_head_06)
  )



four_scope_newly <- four_scope_newly %>% 
  mutate(
    strata = paste0(lot, "_", median)
  )

table(four_scope_newly$strata)

four_scope_newly %>% 
  group_by(lot, median) %>% 
  summarise( n = n())
      
      
      
      
#misfit global----


treatment <- treatment_assign(
  data = four_scope_newly,
  share_control = 0.25,
  share_ti = c(0.25, 0.25, 0.25),
  n_t = 3,
  strata_varlist = dplyr::vars(lot, median),
  missfits = "global",
  key = "village_id",
  seed = 080501
)




treatment <- as.data.frame(treatment$data) %>% 
  select(
    village_id, treat
  ) 

table(treatment$treat)

four_scope_newly <- left_join(four_scope_newly, treatment, by = c("village_id"))

table(four_scope_newly$treat)


rand_newly_sum <- four_scope_newly %>% 
  group_by(strata) %>% 
  summarise(
    C = sum(treat == 0),
    T1 = sum(treat == 1),
    T2 = sum(treat == 2),
    T3 = sum(treat == 3),
    sum = n()
  ) 


summarise_row <-rand_newly_sum %>% 
      summarise(
        strata = "Total",
        C = sum(rand_newly_sum$C),
        T1 = sum(rand_newly_sum$T1),
        T2 = sum(rand_newly_sum$T2),
        T3 = sum(rand_newly_sum$T3),
        sum = sum(rand_newly_sum$sum)
      )

rand_newly_sum <- bind_rows(rand_newly_sum, summarise_row)

View(rand_newly_sum)

kable(rand_newly_sum, format = "latex", booktabs = TRUE)


#Sample households----


household_head <- read_xlsx(path = file.path(data_path, "household_head_clean.xlsx"))


##HH_id construct----

get_unit_digit <- function(id) {
  digits_sum <- sum(as.numeric(unlist(strsplit(as.character(id), ""))))
  unit_digit <- digits_sum %% 10
  return(unit_digit)
}

household_head <- household_head %>%
  group_by(villageid_key) %>%
  mutate(
    villageid_key = as.numeric(villageid_key),
    household = row_number(),
    household_id = villageid_key*10000 + household*10,
    unit_digit = sapply(household_id, get_unit_digit),
    household_id = household_id + unit_digit
    ) %>%
  #   unit_digit = sapply(household_id, get_unit_digit),
  ungroup() %>% 
  mutate(
    villageid_key = as.character(villageid_key),
    household_id = as.character(household_id)
    )


write_xlsx(household_head, path = file.path(data_path, "household_head_clean.xlsx"))


##Sample hh----

household_head_scope <- household_head %>% 
  filter(villageid_key %in% four_scope_newly$village_id) 

household_select <- household_head_scope %>%
  group_by(villageid_key) %>%
  nest() %>%
  mutate(sampled = map(data, ~ if (nrow(.x) <= 20) .x else sample_n(.x, size = 20))) %>% 
  select(villageid_key, sampled) %>%
  unnest(sampled)


household_backup <- household_head_scope %>% 
  filter(!household_id %in% household_select$household_id)

household_backup <- household_backup %>%
  group_by(villageid_key) %>%
  nest() %>%
  mutate(sampled = map(data, ~ if (nrow(.x) <= 1) sample_n(.x, size = 1) else sample_n(.x, size = 2)))  %>% 
  select(villageid_key, sampled) %>%
  unnest(sampled)



check <- household_select %>% 
  group_by(villageid_key) %>% 
  summarise(
    n = n()
  )


# household_select <- anti_join(household_select, household_backup, by = c("household_id"))


household <- list( `selected` = household_select, `backup` = household_backup)


write_xlsx(
  household, path = file.path(scope_path, "scope_households_0807.xlsx")
)


#Join back to get summary----


join <- household_select %>% 
  group_by(villageid_key) %>%
  summarise(surveyed = n()) %>% 
  mutate(villageid_key = as.character(villageid_key))


four_scope_newly <- left_join(four_scope_newly, join, by = c("village_id" = "villageid_key"))

# four_scope_newly <- four_scope_newly %>% 
#   select(-lot_rusizi)

summarise <- four_scope_newly %>%
  group_by(lot) %>%
  summarise(
    n = n(),
    survey_hh = sum(surveyed),
    vulnerable = sum(hh_head_06)
  )


View(summarise)



write_xlsx(four_scope_newly, path = file.path(scope_path, "scope_193_0807.xlsx"))

###For EDCL----
unique_lots <- unique(four_scope_newly$lot)  # Get unique lots


for (lot in unique_lots) {
  readyboard_village <- four_scope_newly %>% 
    filter(treat == 1 |treat == 3 ) %>% 
    filter(lot == !!lot) %>% # Use `!!` to evaluate `lot` variable
    select(village_id, name, cell, sector, district, province)
  
  readyboard_household <- household_head %>% 
    filter(villageid_key %in% readyboard_village$village_id) %>% 
    select(villageid_key, village, cell, sector, district, first_name, last_name, gender, nid )
  
  list <- list("village list" = readyboard_village, "household list" = readyboard_household)
  
  file_name <- paste0("Lot_", lot, ".xlsx")
  write_xlsx(list, path = file.path(scope_path, "EDCL", file_name))
}


###For ready board subsidy----

for (lot in unique_lots) {
  offgrid_village <- four_scope_newly %>% 
    filter(treat == 2 |treat == 3) %>% 
    filter(lot == !!lot) %>% # Use `!!` to evaluate `lot` variable
    select(village_id, name, cell, sector, district, province)
  
 
  file_name <- paste0("Lot_", lot, ".xlsx")
  write_xlsx(offgrid_village, path = file.path(scope_path, "Eligibility Tool", file_name))
}


offgrid_village <- four_scope_newly %>% 
  filter(treat == 2 |treat == 3) %>% 
  select(village_id, name, cell, sector, district, province)


write_xlsx(offgrid_village, path = file.path(scope_path, "Eligibility Tool", "offgrid_subsidy.xlsx"))

###For survey firm----

unique_lots <- unique(four_scope_newly$lot)


  for (lot in unique_lots) {
    village <- four_scope_newly %>% 
      filter(lot == !!lot) %>% 
      select(village_id, name, cell, sector, district, province)
    
    village_join <- village %>% 
      select(village_id)
    
    households  <- household_select %>% 
      semi_join(village_join, by = c("villageid_key" = "village_id")) %>% 
      select(villageid_key, village, cell, sector, district, household_id, first_name, last_name, gender, nid)
    
    back_up <- household_backup %>% 
      semi_join(village_join, by = c("villageid_key" = "village_id")) %>% 
      select(villageid_key, village, cell, sector, district, household_id, first_name, last_name, gender, nid)
    
    list_to_write <- list("village list" = village, "household list" = households, "backup household" = back_up)
    
    file_name <- paste0("Lot_", lot, ".xlsx")
    
    write_xlsx(list_to_write, path = file.path(scope_path, "Lattanzio", file_name))
  }



#For surveyCTO----

villages <- read.csv(file = file.path(data_path, "vills.csv"))

villages <- villages %>% 
  mutate(villageid_key = as.character(villageid_key))

admin_raw <- left_join(four_scope_newly, villages, by = c("village_id" = "villageid_key"))

admin_raw <- admin_raw %>% 
  rename(
    num_to_survey = surveyed,
    villageid_key = village_id,
    treatment = treat
  ) %>% 
  mutate(
    sector_key = str_to_title(sector_key)
  ) %>% 
  select(ends_with("key"), num_to_survey, treatment)

hfc_data_path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/HFC"
)

write.csv(admin_raw, file = file.path(hfc_data_path, "data", "admin_raw.csv"))




