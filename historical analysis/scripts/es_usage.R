
##########################
#Author: Xiaoming Zhang
#Date: 05102024
#purpose:ES and Difference in difference analysis with the new usage data 
############################



pacman::p_load(raster,exactextractr,dplyr, rgdal,here,ggplot2,sf,tidyr,readxl,writexl, stringr, stargazer, janitor, modelsummary, fixest)

getwd()

#ReadDropbox----

if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX, "Rwanda Energy/datawork/Historical data"
)

output_path <- file.path(
  DROPBOX, "Rwanda Energy/datawork/Historical data/outputs"
)

# Data reshaping----
rwa_wide <- read_xlsx(path = file.path(data_path, "ntl_wide_92_21(connect11&22+rd).xlsx"))
rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp")) 

write_xlsx(rwa_wide.1, path = file.path(data_path, "rwa_wide.xlsx"))
  
rwa_long <- rwa_wide %>% 
  pivot_longer(
    cols = matches("^(19|20)"),  
    names_to = "Year",
    values_to = "Value"
  )


usage_id <- read_xlsx(path = file.path(data_path, "usage_id_0416.xlsx"))
usage_noid <- read_xlsx(path = file.path(data_path, "usage_noid_0416.xlsx"))


usage_year <- usage_id %>% 
  group_by(village_id) %>% 
  summarise(
    year_first = min(year_first)
  )

rwa_wide <- left_join(rwa_wide, usage_year, by = c("Village_ID" = "village_id"))

rwa_long <- rwa_wide %>% 
  pivot_longer(
    cols = matches("^(19|20)"),  
    names_to = "Year",
    values_to = "Value"
  )

#Diff in diff preparation----



rwa_long_did <- rwa_long %>% 
  mutate(
    connect_vt = case_when(
      year_first > Year ~ 0, 
      year_first <= Year ~ 1
    )
  )

sum(is.na(rwa_long_did$connect_vt))

rwa_long_did <- rwa_long_did %>% 
  mutate(
    notreat = ifelse(is.na(connect_vt), 1, 0)
  )

rwa_long_did <- rwa_long_did %>% 
  mutate(connect_vt = ifelse(is.na(connect_vt), 0, connect_vt))

  
##Run include ----

# Fit the fixed effects model
model_fe_did_include <- rwa_long_did %>% 
  filter(year_first > 2010 ) %>% 
  filter(Year >= 2010) %>%
  feols(fml = Value ~  connect_vt  | Village_ID + Year, cluster = "Village_ID") %>% summary()

# View the summary of the model
summary(model_fe_did_include)

## Exclude----
model_fe_did_exclude <- rwa_long_did %>% 
  filter(year_first > 2010 ) %>% 
  filter(Year >= 2010) %>%
  filter(notreat != 1) %>% 
  feols(fml = Value ~  connect_vt  | Village_ID + Year, cluster = "Village_ID") %>% summary()

# View the summary of the model
summary(model_fe_did_exclude)

##output save----
model_list <- list("including never \n electrified villages" = model_fe_did_include, "excluding never \n electrified villages" = model_fe_did_exclude)

modelsummary(
  model_list,
  output = "huxtable",
  title = "Table: Diff in diff Village and Year",
  stars = TRUE,
  note = "excluding already electrified first year(2010)",
  out = file.path(output_path, "did_ntl.html")
)













#Event study preparation-----
rwa_long_es <- rwa_long %>% 
  mutate(
    year_first = case_when(
      year_first == 2010 ~ 1900,
      is.na(year_first) ~ 2300,
      .default = year_first
    )
  )

rwa_long_es <- rwa_long_es %>% 
  mutate(
    Year = as.numeric(Year),
    year_first =as.numeric(year_first)
  ) %>% 
  mutate(time_elec = Year - year_first)

rwa_long_es <- rwa_long_es %>% 
  mutate(
    treatl3 = ifelse( time_elec <= -3 & time_elec>= -30 , 1, 0), # elec 3 or more years in the future
    treatl2 = ifelse( time_elec == -2, 1, 0), #elec 2 years in the future
    treatl1 = ifelse( time_elec == -1, 1, 0), #elec 2 years in the future
    treatp0 = ifelse( time_elec == 0, 1, 0), #elec this year
    treatp1 = ifelse( time_elec == 1, 1, 0), #elec last year
    treatp2 = ifelse( time_elec ==  2, 1, 0), # elec last two years
    treatp3 = ifelse( time_elec >= 3, 1, 0), #elec more than three years ago
    notreat = ifelse(year_first == 2300, 1, 0)
  )

rwa_long_es <- rwa_long_es %>% 
  mutate(
    Sector_ID = substr(Village_ID, 1,4),
    Cell_ID = substr(Village_ID, 1,6)
  )






















#First round regression for es----

##include never electrified villages----
es_include <- rwa_long_es %>% 
  filter(year_first > 1900 ) %>% 
  filter(Year >= 2010) %>%
  feols(fml = Value ~ treatl3 + treatl2 + treatp0 + treatp1 + treatp2 + treatp3 | Village_ID + Year, cluster = "Village_ID") %>% summary()

summary(es_include)



##exclude never electrified villages----

es_exclude <- rwa_long_es %>% 
  filter(year_first > 1900 ) %>% 
  filter(Year >= 2010) %>%
  filter(notreat != 1) %>% 
  feols(fml = Value ~ treatl3 + treatl2 + treatp0 + treatp1 + treatp2 + treatp3 | Village_ID + Year, cluster = "Village_ID") %>% summary()

summary(es_exclude)


##output save----
model_list <- list(`including never \n electrified villages` = es_include, `excluding never \n electrified villages` = es_exclude)

modelsummary(
  model_list,
  output = "huxtable",
  type = "html",
  title = "Table: Events Study Village and Year",
  stars = TRUE,
  note = "excluding already electrified first year(2010)",
  out = file.path(output_path, "es_ntl.html")
)









#Second round regression for es----
# Excluding Ngororero, Nyabihu, Nyamasheke, and Rubavu.

##include never electrified villages----
es_include.1 <- rwa_long_es %>% 
  filter(year_first > 1900 ) %>% 
  filter(Year >= 2010) %>%
  filter(!District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu")) %>% 
  feols(fml = Value ~ treatl3 + treatl2 + treatp0 + treatp1 + treatp2 + treatp3 | Village_ID + Year, cluster = "Village_ID") %>% summary()

summary(es_include.1)



##exclude never electrified villages----

es_exclude.2 <- rwa_long_es %>% 
  filter(year_first > 1900 ) %>% 
  filter(Year >= 2010) %>%
  filter(notreat != 1) %>% 
  filter(!District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu")) %>% 
  feols(fml = Value ~ treatl3 + treatl2 + treatp0 + treatp1 + treatp2 + treatp3 | Village_ID + Year, cluster = "Village_ID") %>% summary()

summary(es_exclude.2)


##output save----
model_list <- list(`including never \n electrified villages` = es_include, 
                   `excluding never \n electrified villages` = es_exclude,
                   `including never \n electrified and \n excluding four district` = es_include.1,
                   `excluding never \n electrified and \n excluding four district` = es_exclude.2
                   )

modelsummary(
  model_list,
  output = "huxtable",
  type = "html",
  title = "Table: Events Study Village and Year",
  stars = TRUE,
  note = "excluding already electrified first year(2010), four districts are Ngororero, Nyabihu, Nyamasheke, Rubavu",
  out = file.path(output_path, "es_ntl_four.html")
)


#Third round regression-----
#progressively stricted FE  province-by-year

##Province by Year----
es_include_province <- rwa_long_es %>%
  filter(year_first > 1900) %>%
  filter(Year >= 2010) %>%
  feols(fml = Value ~ treatl3 + treatl2 + treatp0 + treatp1 + treatp2 + treatp3 + 
          Province:Year| Year + Village_ID,
        cluster = "Village_ID")

# summary(es_include_province)

##District by Year----

es_include_district <- rwa_long_es %>%
  filter(year_first > 1900) %>%
  filter(Year >= 2010) %>%
  feols(fml = Value ~ treatl3 + treatl2 + treatp0 + treatp1 + treatp2 + treatp3 + 
          District:Year| Year + Village_ID,
        cluster = "Village_ID")

# summary(es_include_district)

##sector by Year----

es_include_sector <- rwa_long_es %>%
  filter(year_first > 1900) %>%
  filter(Year >= 2010) %>%
  feols(fml = Value ~ treatl3 + treatl2 + treatp0 + treatp1 + treatp2 + treatp3 + 
          Sector_ID:Year| Year + Village_ID,
        cluster = "Village_ID")

# summary(es_include_sector)



##cell by Year----

es_include_cell <- rwa_long_es %>%
  filter(year_first > 1900) %>%
  filter(Year >= 2010) %>%
  feols(fml = Value ~ treatl3 + treatl2 + treatp0 + treatp1 + treatp2 + treatp3 + 
          Cell_ID:Year| Year + Village_ID,
        cluster = "Village_ID")




##output save----
model_list <- list(`province:year` = es_include_province, 
                   `district:year` = es_include_district,
                   `sector:year` = es_include_sector,
                   `cell:year` = es_include_cell
)

modelsummary(
  model_list,
  type = "html",
  output = "huxtable",
  title = "Table: Events Study Village and Year",
  coef_omit = "^(?!.*treat)", 
  stars = TRUE,
  note = "excluding already electrified first year(2010), only displaying coefficients on treated time",
  out = file.path(output_path, "es_ntl_robust.html")
)



#Usage as outcome----

usage_join <- usage_id %>% 
  group_by(village_id) %>% 
  summarise(
    `2010_usage` = sum(`2010_usage`),
    `2011_usage` = sum(`2011_usage`),
    `2012_usage` = sum(`2012_usage`),
    `2013_usage` = sum(`2013_usage`),
    `2014_usage` = sum(`2014_usage`),
    `2015_usage` = sum(`2015_usage`),
    `2016_usage` = sum(`2016_usage`),
    `2017_usage` = sum(`2017_usage`),
    `2018_usage` = sum(`2018_usage`),
    `2019_usage` = sum(`2019_usage`),
    `2020_usage` = sum(`2020_usage`),
    `2021_usage` = sum(`2021_usage`),
    `2022_usage` = sum(`2022_usage`)
  )


usage_join <- usage_join %>% 
  pivot_longer(
    cols = ends_with("usage"),
    names_to = "year",
    values_to = "usage"
  )%>% 
  mutate(
    year = substr(year, 1,4),
    year = as.double(year)
  )


rwa_long_usage <- left_join(rwa_long_es, usage_join, by = c("Village_ID" = "village_id",
                                                "Year" = "year"))

rwa_long_usage <- rwa_long_usage %>% 
  mutate(usage = ifelse(is.na(usage), 0.0, usage))

check <- rwa_long_usage %>% 
  filter(year_first == 2300 & usage != 0)

rwa_long_usage <- rwa_long_usage %>% 
  mutate(
    treatl4 = ifelse( time_elec <= -4 & time_elec >= -30, 1, 0), 
    treatl3 = ifelse( time_elec == -3, 1, 0), # elec 3 or more years in the future
    treatl2 = ifelse( time_elec == -2, 1, 0), #elec 2 years in the future
    treatl1 = ifelse( time_elec == -1, 1, 0), #elec 2 years in the future
    treatp0 = ifelse( time_elec == 0, 1, 0), #elec this year
    treatp1 = ifelse( time_elec == 1, 1, 0), #elec last year
    treatp2 = ifelse( time_elec ==  2, 1, 0), # elec last two years
    treatp3 = ifelse( time_elec == 3, 1, 0), #elec more than three years ago
    treatp4 = ifelse( time_elec >= 4, 1, 0), #elec more than three years ago
    notreat = ifelse(year_first == 2300, 1, 0)
  )

#First round regression for es----

##include never electrified villages----
es_include_usage <- rwa_long_usage %>% 
  filter(year_first > 1900 ) %>% 
  filter(Year >= 2010) %>%
  feols(fml = usage ~ treatl4 + treatl3 + treatl2 + treatp0 + treatp1 + treatp2 + treatp3 + treatp4 | Village_ID + Year, cluster = "Village_ID") %>% summary()

summary(es_include_usage)



##exclude never electrified villages----

es_exclude_usage <- rwa_long_usage %>% 
  filter(year_first > 1900 ) %>% 
  filter(Year >= 2010) %>%
  filter(notreat != 1) %>% 
  feols(fml = usage ~ treatl4 + treatl3 + treatl2 + treatp0 + treatp1 + treatp2 + treatp3 + treatp4 | Village_ID + Year, cluster = "Village_ID") %>% summary()

summary(es_exclude_usage)


##output save----
model_list <- list(`including never \n electrified villages` = es_include_usage, `excluding never \n electrified villages` = es_exclude_usage)

modelsummary(
  model_list,
  type = "html",
  output = "huxtable",
  title = "Table: Events Study Village and Year, usage as outcomes",
  stars = TRUE,
  note = "excluding already electrified first year(2010)",
  out = file.path(output_path, "es_usage.html")
)


