pre_load_data <- read.csv(
  # n=1973, 1973 unique households across 180 villages 
  # output of A-create-primary-phone-preload.R
  file.path(DATA_ML_PRELOAD, "contact_info.csv"))

# breakdown of treatment assignment of village and households by district 
## treatment is assigned by villageid_key 
## the level of group = villageid_key 

data = pre_load_data |> group_by(district, villageid_key, village, treat) |> 
  summarise(hh_count = n_distinct(householdid_key)) 

sum(data$hh_count) # should be 1973

# tidy up the table 
data = data |>
  mutate(district = str_to_title(district),
         treat = factor(treat, levels = c("C", "T1", "T2", "T3" ))) |>
  arrange(district, treat, village)

write.xlsx(data, file.path(DATA_CTO_ML, "treatment_village_household_count.xlsx"))

data2 = pre_load_data |> group_by(district, treat) |>
  summarise(vill_count = n_distinct(village),
            hh_count = n_distinct(householdid_key))