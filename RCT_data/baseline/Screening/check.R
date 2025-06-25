household_head <- read.csv(file.path(data_path, "household_head_baseline2024.csv"))

household_head <- household_head %>% 
  filter(
    household_id %in% deployment_household_join$household_id
  )


write.csv(household_head, 
          file = file.path(data_path, "household_head.csv"), 
          row.names = FALSE, 
          na = "")


eligible_single <- screening_survey %>% 
  filter(hh_id == "410101090141") %>% 
  select(
    district, district_key, sector, sector_key, cell, cell_key, village, village_key, hh_id, phonenumber, second_phonenumber, submissiondate, startdate
  ) 

eligible <- rbind(eligible, eligible_single)