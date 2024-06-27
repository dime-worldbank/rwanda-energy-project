
##########################
#Author: Xiaoming Zhang
#Date: 02132024
#purpose:establishment census analysis
############################


#library----

pacman::p_load(tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, haven, stringr)


#read file----

if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX, "Rwanda Energy/datawork/Historical data"
)


#read data----

rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))
rwa_district <- st_read(dsn = file.path(data_path, "rwa_district", "District.shp"))
rwa_sector <- st_read(dsn = file.path(data_path, "rwa_sector", "Sector.shp"))

rwa_district <- st_transform(rwa_district, crs = st_crs(rwa_district))
rwa_sector <- st_transform(rwa_sector, crs = st_crs(rwa_villages))

existing_MV11 <- st_read(dsn = file.path(data_path, "Existing Electrical Network_2011", "Existing_MVLine.shp"))
existing_HV11 <- st_read(dsn = file.path(data_path, "Existing Electrical Network_2011", "Existing_HVLine.shp"))

existing_MV11 <- st_transform(existing_MV11, crs = st_crs(rwa_villages))
existing_HV11 <- st_transform(existing_HV11, crs = st_crs(rwa_villages))

existing_LV22 <- st_read(dsn = file.path(data_path, "Existing Electrical Network_2022", "Existing_LVLine.shp"))
existing_MV22 <- st_read(dsn = file.path(data_path, "Existing Electrical Network_2022", "Existing_MVLine.shp"))
existing_HV22 <- st_read(dsn = file.path(data_path, "Existing Electrical Network_2022", "Existing_HVLine.shp"))

existing_LV22 <- st_transform(existing_LV22, crs = st_crs(rwa_villages))
existing_MV22 <- st_transform(existing_MV22, crs = st_crs(rwa_villages))
existing_HV22 <- st_transform(existing_HV22, crs = st_crs(rwa_villages))

analysis_2011_village <- read_xlsx(path = file.path(data_path, "analysis_2011(village).xlsx"))
analysis_2011_sector <-  read_xlsx(path = file.path(data_path, "analysis_2011.xlsx"))
analysis_2014  <- read_xlsx(path = file.path(data_path, "analysis_2014.xlsx"))
analysis_2017  <- read_xlsx(path = file.path(data_path, "analysis_2017.xlsx"))
analysis_2020  <- read_xlsx(path = file.path(data_path, "analysis_2020.xlsx"))

ntl_wide <- read_xlsx(path = file.path(data_path, "ntl_wide_92_21(connect11&22+rd).xlsx"))


#add sector in----



ntl_long <- ntl_wide %>% 
  pivot_longer(
    cols = matches("^(19|20)"),  
    names_to = "Year",
    values_to = "Value"
  )


write_xlsx(ntl_long, path = file.path(data_path, "ntl_long_92_21(connect11&22+rd).xlsx"))

sector <- rwa_villages %>% 
  st_drop_geometry() %>% 
  select(Sector_ID, Village_ID)


ntl_long <- left_join(ntl_long, sector, by = c("Village_ID"))

ntl_long <- ntl_long %>% 
  select(Sector_ID, everything())

ntl_wide <- left_join(ntl_wide, sector, by = c("Village_ID"))

ntl_wide <- ntl_wide %>% 
  select(Sector_ID, everything())

ntl_wide_sector <- ntl_wide %>%
  group_by(Sector_ID) %>%
  summarise(across(matches("^(19|20)"), mean, .names = "{.col}"))

other <- ntl_wide %>% 
  select(Sector_ID, connect22_lv, connect22_mv, connect11_mv, road) %>% 
  distinct(Sector_ID, .keep_all = TRUE)



ntl_wide_sector <- left_join(ntl_wide_sector, other)

ntl_wide_sector <- ntl_wide_sector %>% 
  select(Sector_ID, connect11_mv, connect22_mv, connect22_lv, road, everything())

#Join with establishment census----

est_2011 <- analysis_2011 %>% 
  select(sector_id, num_establishments_11) %>% 
  mutate(sector_id = as.character(sector_id))

est_2014 <- analysis_2014 %>% 
  select(sector_id, num_establishments_14) %>% 
  mutate(sector_id = as.character(sector_id))

est_2017 <- analysis_2017 %>% 
  select(sector_id, num_establishments_17) %>% 
  mutate(sector_id = as.character(sector_id))

est_2020 <- analysis_2020 %>% 
  select(sector_id, num_establishments_20) %>% 
  mutate(sector_id = as.character(sector_id))





sector_wide <- left_join(ntl_wide_sector, est_2011, by = c("Sector_ID" = "sector_id") )

sector_wide <- left_join(sector_wide, est_2014, by = c("Sector_ID" = "sector_id") )

sector_wide <- left_join(sector_wide, est_2017, by = c("Sector_ID" = "sector_id") )

sector_wide <- left_join(sector_wide, est_2020, by = c("Sector_ID" = "sector_id") )



#Join village----

village_2011 <- analysis_2011_village %>% 
  select(Village_ID, num_establishments) %>% 
  mutate(Village_ID = as.character(Village_ID))

village_wide <- left_join(ntl_wide, village_2011 , by = c("Village_ID"))



#simple regression----


village <- lm(num_establishments ~ connect11_mv, data = village_wide)

summary(village)


sector <- lm(num_establishments_11 ~ connect11_mv, data = sector_wide)

summary(sector)


#event study----

sector_long_es <- sector_wide %>% 
  select(Sector_ID, starts_with("connect"), starts_with("num_establishments")) %>% 
  rename(
    `2011` = num_establishments_11,
    `2014` = num_establishments_14,
    `2017` = num_establishments_17,
    `2020` = num_establishments_20
  ) 

sector_long_es <- sector_long_es %>% 
  pivot_longer(
    cols = starts_with("20"),
    names_to = "year",
    values_to ="num_establishment"
  )

sector_es <- sector_long_es %>% 
  mutate(
   `2011` = ifelse(year == 2011, 1, 0),
   `2014` = ifelse(year == 2014, 1,0),
   `2017` = ifelse(year == 2017, 1, 0),
   `2020` = ifelse(year == 2020, 1, 0)
  )
  
 
  
sector_es <- sector_es %>%
  mutate(connect22_mv = ifelse(connect11_mv == 1 & connect22_mv == 0, 1, connect22_mv) )



sector_es <- sector_es %>%
  mutate(connect22_mv_ONLY = ifelse(connect11_mv == 0 & connect22_mv == 1, 1, 0))



# Print the cases if any

cases <- sector_es %>%
  filter(connect11_mv == 1 & connect22_mv == 0)

# Print the cases if any
print(cases)
print(cases)
  
years <- c("2011", "2014" , "2017", "2020")

# Create a formula for the main effects of each year
main_effects <- paste0("`", years, "`")

# Create a formula for interaction terms with connect11_mv for each year
interaction_terms_connect11 <- paste0("connect11_mv * `", years, "`")

# Create a formula for interaction terms with connect22_mv for each year
interaction_terms_connect22 <- paste0("connect22_mv * `", years, "`")
interaction_terms_connect22_lv <- paste0("connect22_lv* `", years, "`")

# Assuming Village_ID is a factor variable
sector_es$Sector_ID <- as.factor(sector_es$Sector_ID)

###Sector fixed effects----
# Create the formula with village fixed effects
formula_fe <- as.formula(paste("num_establishment ~", 
                               paste(c(main_effects, interaction_terms_connect22_lv), collapse = " + ")))

library(fixest)
# Fit the linear model with village fixed effects
es_fe <- lm(formula_fe, data = sector_es)

summary(es_fe)











#####extract coefs---- 
# Extract coefficient table with estimates and standard errors
coef_table <- coeftable(es_fe)

coef_table_df<- as.data.frame(coef_table)

year <- c("2020", "2011", "2014" , "2017","2020", "2011", "2014" , "2017")
coef_table_df$year <- year

group <- c("no lv 2022", "no lv 2022", "no lv 2022", "no lv 2022", "lv 2022", "lv 2022", "lv 2022","lv 2022")
coef_table_df$groupr <- group



coef_table_df <- coef_table_df %>% 
  mutate(
    estimate.1 = case_when(
      group %in% c("no lv 2022")& year %in% c("2011", "2014", "2017") ~ Estimate + 401.86,
      group %in% c("lv 2022") & year %in% c("2011", "2014", "2017")~ Estimate + 401.86 + 248.93 ,
      group %in% c("lv 2022") & year %in% c("2020")~ Estimate + 401.86 ,
      TRUE ~ Estimate
      )
  )






coefs_plot <- coef_table_df %>% 
  clean_names() 


#graph-----


ggplot(coefs_plot, aes(x = as.numeric(year), y = estimate_1, group = group, color = group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = estimate_1 - 1.96 * std_error, ymax = estimate_1 + 1.96 * std_error),
                colour = "black", width = 0.1, position = position_dodge(0.1)) +
  geom_hline(yintercept = 0.0, linetype = "dashed") +
  # geom_vline(xintercept = c(2008), linetype = "dashed") +
  labs(title = "Event Study Plot",
       x = "Year",
       y = "Estimated Establishment Number")




district_fe <- feols(num_establishment ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +distr_year, data = elec15_17)
sector_fe <- feols(num_establishment ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +sector_year, data = elec15_17)

model15_17 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model15_17,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2015-2017 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)


district_fe <- feols(num_establishment ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +distr_year, data = elec18_20)
sector_fe <- feols(num_establishment ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +sector_year, data = elec18_20)

model18_20 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model18_20,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2018-2020 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)






#Employee number----

##12_14----
elec12_14 <- rwa_regress %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )
table(elec12_14$year)

village_fe <- feols(total_employee ~ p0_2014*elec12_14 + p1_2017*elec12_14 + p2_2020*elec12_14|village_id + year, data = elec12_14)
district_fe <- feols(total_employee ~ p0_2014*elec12_14 + p1_2017*elec12_14 +p2_2020*elec12_14|village_id + distr_year, data = elec12_14)
sector_fe <- feols(total_employee ~ p0_2014*elec12_14 + p1_2017*elec12_14 +p2_2020*elec12_14|village_id + sector_year, data = elec12_14)

model12_14 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model12_14,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2012-2014 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)


##15_17----
elec15_17 <- rwa_regress %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  )


village_fe <- feols(total_employee ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +year, data = elec15_17)
district_fe <- feols(total_employee ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +distr_year, data = elec15_17)
sector_fe <- feols(total_employee ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +sector_year, data = elec15_17)

model15_17 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model15_17,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2015-2017 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)


##18_20----
elec18_20 <- rwa_regress %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  )

village_fe <- feols(total_employee ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +year, data = elec18_20)
district_fe <- feols(total_employee ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +distr_year, data = elec18_20)
sector_fe <- feols(total_employee ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +sector_year, data = elec18_20)

model18_20 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model18_20,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2018-2020 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)










#Capital group 1----

##12_14----
elec12_14 <- rwa_regress %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )
table(elec12_14$year)

village_fe <- feols(employed_capital_1 ~ p0_2014*elec12_14 + p1_2017*elec12_14 + p2_2020*elec12_14|village_id + year, data = elec12_14)
district_fe <- feols(employed_capital_1 ~ p0_2014*elec12_14 + p1_2017*elec12_14 +p2_2020*elec12_14|village_id + distr_year, data = elec12_14)
sector_fe <- feols(employed_capital_1 ~ p0_2014*elec12_14 + p1_2017*elec12_14 +p2_2020*elec12_14|village_id + sector_year, data = elec12_14)

model12_14 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model12_14,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2012-2014 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)


##15_17----
elec15_17 <- rwa_regress %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  )


village_fe <- feols(employed_capital_1 ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +year, data = elec15_17)
district_fe <- feols(employed_capital_1 ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +distr_year, data = elec15_17)
sector_fe <- feols(employed_capital_1 ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +sector_year, data = elec15_17)

model15_17 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model15_17,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2015-2017 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)


##18_20----
elec18_20 <- rwa_regress %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  )

village_fe <- feols(employed_capital_1 ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +year, data = elec18_20)
district_fe <- feols(employed_capital_1 ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +distr_year, data = elec18_20)
sector_fe <- feols(employed_capital_1 ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +sector_year, data = elec18_20)

model18_20 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model18_20,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2018-2020 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)





























#Capital group 2----

##12_14----
elec12_14 <- rwa_regress %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )
table(elec12_14$year)

village_fe <- feols(employed_capital_2 ~ p0_2014*elec12_14 + p1_2017*elec12_14 + p2_2020*elec12_14|village_id + year, data = elec12_14)
district_fe <- feols(employed_capital_2 ~ p0_2014*elec12_14 + p1_2017*elec12_14 +p2_2020*elec12_14|village_id + distr_year, data = elec12_14)
sector_fe <- feols(employed_capital_2 ~ p0_2014*elec12_14 + p1_2017*elec12_14 +p2_2020*elec12_14|village_id + sector_year, data = elec12_14)

model12_14 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model12_14,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2012-2014 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)


##15_17----
elec15_17 <- rwa_regress %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  )


village_fe <- feols(employed_capital_2 ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +year, data = elec15_17)
district_fe <- feols(employed_capital_2 ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +distr_year, data = elec15_17)
sector_fe <- feols(employed_capital_2 ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +sector_year, data = elec15_17)

model15_17 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model15_17,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2015-2017 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)


##18_20----
elec18_20 <- rwa_regress %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  )

village_fe <- feols(employed_capital_2 ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +year, data = elec18_20)
district_fe <- feols(employed_capital_2 ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +distr_year, data = elec18_20)
sector_fe <- feols(employed_capital_2 ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +sector_year, data = elec18_20)

model18_20 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model18_20,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2018-2020 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)






#Capital group 3----

##12_14----
elec12_14 <- rwa_regress %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )
table(elec12_14$year)

village_fe <- feols(employed_capital_3 ~ p0_2014*elec12_14 + p1_2017*elec12_14 + p2_2020*elec12_14|village_id + year, data = elec12_14)
district_fe <- feols(employed_capital_3 ~ p0_2014*elec12_14 + p1_2017*elec12_14 +p2_2020*elec12_14|village_id + distr_year, data = elec12_14)
sector_fe <- feols(employed_capital_3 ~ p0_2014*elec12_14 + p1_2017*elec12_14 +p2_2020*elec12_14|village_id + sector_year, data = elec12_14)

model12_14 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model12_14,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2012-2014 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)


##15_17----
elec15_17 <- rwa_regress %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  )


village_fe <- feols(employed_capital_3 ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +year, data = elec15_17)
district_fe <- feols(employed_capital_3 ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +distr_year, data = elec15_17)
sector_fe <- feols(employed_capital_3 ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +sector_year, data = elec15_17)

model15_17 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model15_17,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2015-2017 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)


##18_20----
elec18_20 <- rwa_regress %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  )

village_fe <- feols(employed_capital_3 ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +year, data = elec18_20)
district_fe <- feols(employed_capital_3 ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +distr_year, data = elec18_20)
sector_fe <- feols(employed_capital_3 ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +sector_year, data = elec18_20)

model18_20 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model18_20,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2018-2020 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)











#Capital group 3----

##12_14----
elec12_14 <- rwa_regress %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )
table(elec12_14$year)

village_fe <- feols(employed_capital_4 ~ p0_2014*elec12_14 + p1_2017*elec12_14 + p2_2020*elec12_14|village_id + year, data = elec12_14)
district_fe <- feols(employed_capital_4 ~ p0_2014*elec12_14 + p1_2017*elec12_14 +p2_2020*elec12_14|village_id + distr_year, data = elec12_14)
sector_fe <- feols(employed_capital_4 ~ p0_2014*elec12_14 + p1_2017*elec12_14 +p2_2020*elec12_14|village_id + sector_year, data = elec12_14)

model12_14 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model12_14,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2012-2014 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)


##15_17----
elec15_17 <- rwa_regress %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  )


village_fe <- feols(employed_capital_4 ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +year, data = elec15_17)
district_fe <- feols(employed_capital_4 ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +distr_year, data = elec15_17)
sector_fe <- feols(employed_capital_4 ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +sector_year, data = elec15_17)

model15_17 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model15_17,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2015-2017 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)


##18_20----
elec18_20 <- rwa_regress %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  )

village_fe <- feols(employed_capital_4 ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +year, data = elec18_20)
district_fe <- feols(employed_capital_4 ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +distr_year, data = elec18_20)
sector_fe <- feols(employed_capital_4 ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +sector_year, data = elec18_20)

model18_20 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model18_20,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2018-2020 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)
















#NTL value----

##12_14----
elec12_14 <- rwa_regress %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )
table(elec12_14$year)

village_fe <- feols(value ~ p0_2014*elec12_14 + p1_2017*elec12_14 + p2_2020*elec12_14|village_id + year, data = elec12_14)
district_fe <- feols(value ~ p0_2014*elec12_14 + p1_2017*elec12_14 +p2_2020*elec12_14|village_id + distr_year, data = elec12_14)
sector_fe <- feols(value ~ p0_2014*elec12_14 + p1_2017*elec12_14 +p2_2020*elec12_14|village_id + sector_year, data = elec12_14)

model12_14 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model12_14,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2012-2014 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)


##15_17----
elec15_17 <- rwa_regress %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  )


village_fe <- feols(value ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +year, data = elec15_17)
district_fe <- feols(value ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +distr_year, data = elec15_17)
sector_fe <- feols(value ~ p_2_2011*elec15_17 + p0_2017*elec15_17 + p1_2020*elec15_17|village_id +sector_year, data = elec15_17)

model15_17 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model15_17,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2015-2017 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)


##18_20----
elec18_20 <- rwa_regress %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  )

village_fe <- feols(value ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +year, data = elec18_20)
district_fe <- feols(value ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +distr_year, data = elec18_20)
sector_fe <- feols(value ~ p_3_2011*elec18_20 + p_2_2014*elec18_20 + p0_2020*elec18_20|village_id +sector_year, data = elec18_20)

model18_20 <- list(
  `village_fe` = village_fe,
  `district_fe` = district_fe,
  `sector_fe` = sector_fe
)

modelsummary(
  model18_20,
  output = "huxtable",
  type = "html",
  title = "Villages electrified between 2018-2020 & never electrified ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)













