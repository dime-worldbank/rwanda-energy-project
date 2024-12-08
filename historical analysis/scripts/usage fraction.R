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

usage_id <- read_xlsx(path = file.path(data_path, "usage_id_0412.xlsx"))
usage_noid <- read_xlsx(path = file.path(data_path, "usage_noid_0412.xlsx"))

usage_unique <- rbind(usage_id, usage_noid)

#Usage fraction----

usage_unique <- usage_unique %>%
  mutate(across(ends_with("usage"), as.numeric))

usage_unique.1 <- usage_unique %>%
  mutate(usage_combine = rowSums(select(., ends_with("usage")), na.rm = TRUE))

usage_unique.1 <- usage_unique.1 %>% 
  mutate(id_usage = ifelse(is.na(village_id), 0, 1))

library(kableExtra)
##balance table-----
balance_usage <- usage_unique.1 %>% 
  select(usage_combine, ends_with("usage"), id_usage) %>% 
  st_drop_geometry()

balance_table<-function(data, treatment) {
  
  variables<-NULL 
  
  data <- data %>% dplyr::arrange(!!rlang::sym(treatment))
  
  valores_trat <- base::unique(dplyr::pull(data, !!rlang::sym(treatment)))
  
  trats<-valores_trat[2:base::length(valores_trat)]
  
  bal_tables<-purrr::map(trats, function (x)
    data %>%
      dplyr::filter(!!rlang::sym(treatment) == valores_trat[1] | !!rlang::sym(treatment) ==  !!x))
  
  
  
  bal_tables<-purrr::map(bal_tables, function (x) x %>%
                           tidyr::pivot_longer(names_to = "variables", values_to = "value", -treatment) )
  
  
  # Creo por separado la primera para poner la mean de control 
  
  bal_tables<-purrr::map(bal_tables, function(x) x %>% 
                           dplyr::group_by(variables) %>% 
                           dplyr::summarise(mean_control = stats::t.test(value~!!rlang::sym(treatment))$estimate[1],
                                            mean_trat = stats::t.test(value~!!rlang::sym(treatment))$estimate[2], 
                                            p_value = stats::t.test(value~!!rlang::sym(treatment))$p.value))
  
  valores_trat<-base::as.character(valores_trat[2:base::length(valores_trat)])
  
  bal_tables <- purrr::map2_dfc(.x = bal_tables, .y = valores_trat,
                                function(x,y) dplyr::rename_all(x, ~stringr::str_c(., y)))
  
  #Quedandome solo con una de variables y mean_control
  means_control <- base::names(bal_tables %>% dplyr::select(dplyr::contains("control")) )
  variables_nombres <- base::names(bal_tables %>% dplyr::select(dplyr::contains("variables")))
  means_trat <- base::names(bal_tables %>% dplyr::select(dplyr::contains("trat")))
  p_values <-base::names(bal_tables %>% dplyr::select(dplyr::contains("p_value")))
  
  bal_tables<-
    bal_tables %>% 
    dplyr::select(tidyselect::all_of(variables_nombres[1]), 
                  tidyselect::all_of(means_control[1]), 
                  tidyselect::all_of(means_trat), 
                  tidyselect::all_of(p_values))
  
  # Convert to data frame for kable
  bal_tables_df <- as.data.frame(bal_tables)
  
 return(bal_tables_df)
  # bal_tables_df %>% dplyr::mutate_if(is.numeric, funs(as.character(signif(., 3)))) %>%
  #   kable(bal_tables, format = "html", caption = caption) %>%
  #   kable_styling(bootstrap_options = c("striped"),
  #                 # full_width = T,
  #                 font_size = 15) %>%
  #   scroll_box(height = "300px")
  
}

balance_table(balance_usage, "id_usage")


#Fraction----
# Calculate the row-wise sum of "usage_combine" when "id_usage" equals 1
sum_id_1 <- sum(usage_unique.1$usage_combine[usage_unique.1$id_usage == 1], na.rm = TRUE)
sum_id_0 <- sum(usage_unique.1$usage_combine[usage_unique.1$id_usage == 0], na.rm = TRUE)
sum_id_0
sum_id_1

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


#parcel-id----

usage_unique <- rbind(usage_id, usage_noid)

parcel_success <- read_xlsx(path = file.path(data_path, "parcel_success.xlsx"))

parcel_join <- parcel_success %>% 
  select(meter_numb, parcel_id, plot_numbe, village_id, district) %>% 
  mutate(
    meter_numb = as.character(meter_numb)
  )

usage_unique <- left_join(usage_unique, parcel_join, by = c("meter_id" = "meter_numb"))

usage_unique <- usage_unique %>% 
  mutate(
    village_id = ifelse(is.na(village_id.x), village_id.y, village_id.x),
    plot_numbe = ifelse(is.na(plot_numbe.x), plot_numbe.y, plot_numbe.x)
  ) %>% 
  rename(
    district = district.x
  ) %>% 
  select(-district.y, -village_id.x, -village_id.y, -plot_numbe.x, -plot_numbe.y )

sum(!is.na(usage_unique$village_id))

usage_id_0416 <- usage_unique %>% 
  filter(!is.na(village_id))

usage_noid_0416 <- usage_unique %>% 
  filter(is.na(village_id))

write.csv(usage_unique, file = file.path(data_path, "usage_unique_0416.csv"))

write_xlsx(usage_id_0416, path = file.path(data_path, "usage_id_0416.xlsx"))
write_xlsx(usage_noid_0416, path = file.path(data_path, "usage_noid_0416.xlsx"))

nrow(usage_id_0416) + nrow(usage_noid_0416) == nrow(usage_unique)

usage_id_0416 <- usage_unique %>% 
  filter(!is.na(village_id)) 

usage_noid_0416 <- usage_unique %>% 
  filter(is.na(village_id))

filter <- usage_unique %>% 
  filter(!is.na(village_id)&is.na(sector))

no_sector <- usage_unique %>% 
  filter(is.na(sector))

no_sector_join <- usage_unique %>% 
  filter(is.na(sector) & !is.na(village_id))

write_xlsx(no_sector, path = file.path(data_path, "usage no sector.xlsx"))
nrow(no_sector_join)/nrow(no_sector)

nrow(no_sector)/nrow(usage_unique)

yes_sector <- usage_unique %>% 
  filter(!is.na(sector))

yes_sector_join <- usage_unique %>% 
  filter(!is.na(sector) & !is.na(village_id))

nrow(yes_sector_join)/nrow(yes_sector)

nrow(yes_sector)/nrow(usage_unique)
# 0416 Fraction calculate----
usage_unique.1 <- usage_unique %>% 
  mutate(
    id_exist = ifelse(is.na(village_id), 0, 1)
  )
  
  
fraction_calculate <- usage_unique.1 %>% 
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


#District with low usage----

district_low <- district_wide %>% 
  filter(fract_villageid < 0.1)

usage_lowid <- usage_unique %>% 
  filter(district %in% district_low$district)


usage_lowid_stat <- usage_lowid %>% 
  group_by(district) %>% 
  summarise(
    with_id = sum(!is.na(village_id)),
    no_id = sum(is.na(village_id)),
    no_id_no_sector = sum(is.na(sector)&is.na(village_id))
  )

usage_lowid_stat <- usage_lowid_stat %>% 
  mutate(
    withid_fraction = round(with_id/(with_id + no_id),2)
  )

district_low_join <- district_low %>% 
  select(district, fract_villageid) %>% 
  rename(
    with_id_usage_fraction = fract_villageid
  )

usage_lowid_stat <- left_join(usage_lowid_stat, district_low_join, by = c("district"))

write_xlsx(usage_lowid_stat, path = file.path(data_path, "clarification on low usage.xlsx"))
test <- usage_lowid %>% 
  filter(is.na(village_id) & is.na(sector)) %>% 
  group_by(district) %>% 
  summarise(n = n())
  