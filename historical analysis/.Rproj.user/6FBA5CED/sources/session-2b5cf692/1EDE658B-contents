##########################
#Author: Xiaoming Zhang
#Date: 04032024
#purpose: Dig into the usage fraction of those with village_id
############################


#library----

pacman::p_load(tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, haven, stringr, gtsummary)


#read file----

if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX, "Rwanda Energy/datawork/Historical data"
)


#Read_file

usage_id <- read_xlsx(path = file.path(data_path, "usage_villageid.xlsx"))
usage_noid <- read_xlsx(path = file.path(data_path, "usage_noid_0402.xlsx"))

usage_noid <- usage_noid %>% 
  select(-Cell_ID)


usage_id <- usage_id %>%
  rename(
    `2010_usage` = x2010_usage,
    `2011_usage` = x2011_usage,
    `2012_usage` = x2012_usage,
    `2013_usage` = x2013_usage,
    `2014_usage` = x2014_usage,
    `2015_usage` = x2015_usage,
    `2016_usage` = x2016_usage,
    `2017_usage` = x2017_usage,
    `2018_usage` = x2018_usage,
    `2019_usage` = x2019_usage,
    `2020_usage` = x2020_usage,
    `2021_usage` = x2021_usage,
    `2022_usage` = x2022_usage
  )

usage_unique <- rbind(usage_id, usage_noid)

#Usage fraction----

usage_unique <- usage_unique %>%
  mutate(across(ends_with("usage"), as.numeric))

# usage_unique.1 <- usage_unique %>%
#   mutate(usage_combine = rowsum(select(ends_with("usage"))))
# 


usage_unique.1 <- usage_unique %>%
  mutate(usage_combine = rowSums(select(., ends_with("usage")), na.rm = TRUE))

usage_unique.1 <- usage_unique.1 %>% 
  mutate(id_usage = ifelse(is.na(village_id), 0, 1))



##balance table-----
balance_usage <- usage_unique.1 %>% 
  select(usage_combine, ends_with("usage"), id_usage) %>% 
  st_drop_geometry()

balance_usage  %>% 
  tbl_summary(by = id_usage) %>%  
  add_p()

#Fraction----
# Calculate the row-wise sum of "usage_combine" when "id_usage" equals 1
sum_id_1 <- sum(usage_unique.1$usage_combine[usage_unique.1$id_usage == 1], na.rm = TRUE)
sum_id_0 <- sum(usage_unique.1$usage_combine[usage_unique.1$id_usage == 0], na.rm = TRUE)
sum_id_0

# Calculate the row-wise sum of the entire "usage_combine" column
total_sum <- sum(usage_unique.1$usage_combine, na.rm = TRUE)
total_sum
# Calculate the proportion
proportion_1 <- sum_id_1 / total_sum
proportion_0 <- sum_id_0 / total_sum


#Fraction calculate----

fraction_calculate <- usage_unique.1 %>% 
  select(meter_installed_year, district, village_id, ends_with("usage"), usage_combine)

fraction_calculate <- fraction_calculate %>%
  rename(
    combine_usage = usage_combine,
    id_exist = id_usage
  )


usage_long <- fraction_calculate %>% 
  pivot_longer(
    cols = ends_with("usage"),
    values_to = "usage",
    names_to = "year"
  )


View(usage_long)

usage_combine <- usage_long %>% 
  filter(year == "combine_usage")

###Fraction each year----
usage_year <- usage_long %>% 
  filter(year != "combine_usage")


year_fraction <- usage_year %>% 
  group_by(year, id_exist) %>% 
  summarise(
    usage_sum = sum(usage)
  )

year_wide <- year_fraction %>% 
  pivot_wider(
    names_from= id_exist,
    values_from = usage_sum
  )

year_wide <- year_wide %>% 
  rename(
    has_village_id = `1`,
    no_village_id = `0`
  ) %>% 
  mutate(
    sum = has_village_id + no_village_id,
    fract_villageid = round(has_village_id/sum, 2)
  )
  
##Fraction each district

district_fraction <- usage_year %>% 
  group_by(district, id_exist) %>% 
  summarise(
    usage_sum = sum(usage)
  )

district_wide <- district_fraction %>% 
  pivot_wider(
    names_from= id_exist,
    values_from = usage_sum
  )

district_wide <- district_wide %>% 
  rename(
    has_village_id = `1`,
    no_village_id = `0`
  ) %>% 
  mutate(
    sum = has_village_id + no_village_id,
    fract_villageid = round(has_village_id/sum, 2)
  )

##Fraction each dy

dy_fraction <- usage_year %>% 
  group_by(district, year, id_exist) %>% 
  summarise(
    usage_sum = sum(usage)
  )

dy_wide <- dy_fraction %>% 
  pivot_wider(
    names_from= id_exist,
    values_from = usage_sum
  )

dy_wide <- dy_wide %>% 
  rename(
    has_village_id = `1`,
    no_village_id = `0`
  ) %>% 
  mutate(
    sum = has_village_id + no_village_id,
    fract_villageid = round(has_village_id/sum, 2)
  )


write_xlsx(list("district" = district_wide, "year" = year_wide, "district and year" = dy_wide) , path = file.path(data_path, "fraction of usage.xlsx"))

#Join the District of Nyagatare and Ngororero

rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))

village_list <- rwa_villages %>% 
  st_drop_geometry() %>% 
  clean_names() %>% 
  select(village_id, district, sector, cell, name)

n_distinct(usage_unique.1) == nrow(usage_unique.1)

usage_two <- usage_noid %>% 
  filter(district %in% c("Nyagatare", "Ngororero"))

village_list %>% 
  group_by(sector) %>% 
  summarise(
    n = n()
  ) 

#Sector
usage_sector <- usage_noid %>%
  group_by(sector) %>%
  summarise(n = n()) %>%
  filter(!(sector %in% village_list$sector))

village_list <- village_list %>% 
  mutate(
    sector = str_to_title(sector)
  )


#join with the noid one's 


usage_0403 <- left_join(usage_noid, village_list, by = c("district" = "district",
                                                                   "sector" = "sector",
                                                                   "cell" = "cell",
                                                                   "village" = "name"
))


usage_id_0403 <- usage_0403 %>% 
  distinct(meter_id, .keep_all = TRUE) %>% 
  filter(!is.na(village_id.y))
  
usage_id_0403 <- usage_id_0403 %>% 
  rename(
    village_id = village_id.x
  ) %>% 
  mutate(
    village_id = village_id.y
  ) %>% 
  select(-village_id.y)


usage_noid_0403 <- usage_noid %>% 
  anti_join(usage_id_0403, by = "meter_id")

usage_id_0403.1 <- rbind(usage_id_0403, usage_id)

nrow(usage_id_0403.1) + nrow(usage_noid_0403) == nrow(usage_unique)


#write_xlsx

write_xlsx(usage_id_0403.1 , path = file.path(data_path,"usage_id_0403.xlsx"))
write_xlsx(usage_noid_0403 , path = file.path(data_path,"usage_noid_0403.xlsx"))


#Redo the fraction----

usage_unique <- rbind(usage_id_0403.1, usage_noid_0403)

usage_unique <- usage_unique %>% 
  mutate(combine_usage = rowSums(select(., ends_with("usage")), na.rm = TRUE),
         id_exist = ifelse(is.na(village_id), 0, 1))

sum(usage_unique$id_exist == 1)


# Calculate the row-wise sum of "combine_usage" when "id_usage" equals 1
sum_id_1 <- sum(usage_unique$combine_usage[usage_unique$id_exist == 1], na.rm = TRUE)
sum_id_0 <- sum(usage_unique$combine_usage[usage_unique$id_exist == 0], na.rm = TRUE)
sum_id_0

# Calculate the row-wise sum of the entire "combine_usage" column
total_sum <- sum(usage_unique$combine_usage, na.rm = TRUE)
total_sum
# Calculate the proportion
proportion_1 <- sum_id_1 / total_sum
proportion_0 <- sum_id_0 / total_sum






#Fraction calculate----

fraction_calculate <- usage_unique %>% 
  select(meter_installed_year, district, village_id, ends_with("usage"), id_exist)



usage_long <- fraction_calculate %>% 
  pivot_longer(
    cols = ends_with("usage"),
    values_to = "usage",
    names_to = "year"
  )


View(usage_long)

usage_combine <- usage_long %>% 
  filter(year == "combine_usage")


###Fraction each year----
usage_year <- usage_long %>% 
  filter(year != "combine_usage")


year_fraction <- usage_year %>% 
  group_by(year, id_exist) %>% 
  summarise(
    usage_sum = sum(usage)
  )

year_wide <- year_fraction %>% 
  pivot_wider(
    names_from= id_exist,
    values_from = usage_sum
  )

year_wide <- year_wide %>% 
  rename(
    has_village_id = `1`,
    no_village_id = `0`
  ) %>% 
  mutate(
    sum = has_village_id + no_village_id,
    fract_villageid = round(has_village_id/sum, 2)
  )

View(year_wide)
##Fraction each district

district_fraction <- usage_year %>% 
  group_by(district, id_exist) %>% 
  summarise(
    usage_sum = sum(usage)
  )

district_wide <- district_fraction %>% 
  pivot_wider(
    names_from= id_exist,
    values_from = usage_sum
  )

district_wide <- district_wide %>% 
  rename(
    has_village_id = `1`,
    no_village_id = `0`
  ) %>% 
  mutate(
    sum = has_village_id + no_village_id,
    fract_villageid = round(has_village_id/sum, 2)
  )

View(district_wide)
##Fraction each dy

dy_fraction <- usage_year %>% 
  group_by(district, year, id_exist) %>% 
  summarise(
    usage_sum = sum(usage)
  )

dy_wide <- dy_fraction %>% 
  pivot_wider(
    names_from= id_exist,
    values_from = usage_sum
  )

dy_wide <- dy_wide %>% 
  rename(
    has_village_id = `1`,
    no_village_id = `0`
  ) %>% 
  mutate(
    sum = has_village_id + no_village_id,
    fract_villageid = round(has_village_id/sum, 2)
  )


write_xlsx(list("district" = district_wide, "year" = year_wide, "district and year" = dy_wide) , path = file.path(data_path, "fraction of usage.xlsx"))
