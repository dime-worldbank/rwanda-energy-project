
##########################
#Author: Xiaoming Zhang
#Date: 04262024
#purpose:Difference in difference analysis with the new usage data 
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

# rwa_wide----
rwa_wide <- read_xlsx(path = file.path(data_path, "ntl_wide_92_21(connect11&22+rd).xlsx"))

rwa_long <- rwa_wide %>% 
  pivot_longer(
    cols = matches("^(19|20)"),  
    names_to = "Year",
    values_to = "Value"
  )

#usage_id----

usage_id <- read_xlsx(path = file.path(data_path, "usage_id_0416.xlsx"))
usage_noid <- read_xlsx(path = file.path(data_path, "usage_noid_0416.xlsx"))


usage_year <- usage_id %>% 
  group_by(village_id) %>% 
  summarise(
    year_first = min(year_first)
  )
  
rwa_wide <- left_join(rwa_wide, usage_year, by = c("Village_ID" = "village_id"))
  
rwa_wide.1 <- rwa_wide %>% 
  mutate(
    connect11 = ifelse(year_first <= 2011, 1, 0),
     connect22 = ifelse(year_first <= 2022, 1, 0)
  ) %>% 
  mutate(
    connect11 = ifelse(is.na(year_first), 0, connect11),
    connect22 = ifelse(is.na(year_first), 0, connect22)
  )

sum(rwa_wide.1$connect11_mv, na.rm = TRUE)
sum(rwa_wide.1$connect11, na.rm = TRUE)

rwa_long.1 <- rwa_wide.1 %>% 
  pivot_longer(
    cols = matches("^(19|20)"),  
    names_to = "Year",
    values_to = "Value"
  )


#did----

rwa_long_did11 <- rwa_long.1 %>% 
  filter(Year %in% c("2011", "2021")) %>% 
  mutate(year_2021 = ifelse(Year == 2021, 1, 0),
         year_2011 =ifelse(Year == 2011, 1, 0))

did_11 <- lm(Value ~ connect11 + year_2021*connect11, data = rwa_long_did11)


summary(did_11)

stargazer(
  did_11,
  type = "html",
  title = "Table: Difference in differences on ntl",
  out = file.path(output_path, "DiD_usage.html"))


#Connect VT----

rwa_long.2 <- rwa_long.1 %>% 
  mutate(
    connect_vt = case_when(
      year_first > Year ~ 0, 
      year_first <= Year ~ 1
    )
  )

##Run regression----

# Fit the fixed effects model
model_fe <- feols(Value ~ connect_vt | as.factor(Village_ID) + as.factor(Year), data = rwa_long.2)

# View the summary of the model
summary(model_fe)


modelsummary(
  model_fe,
  type = "html",
  title = "Table: Fixed Effects Village and Year",
  stars = TRUE,
  out = file.path(output_path, "DiD_fe.html"))



#Event study----

write_xlsx(rwa_long.2, file.path(data_path, "rwa_long_0429.xlsx"))

sum(is.na(rwa_long.2$year_first))

rwa_long.3 <- rwa_long.2 %>% 
  # filter(!is.na(year_first)) %>% 
  mutate(
    year_first = case_when(
      year_first == 2010 ~ 1900,
      is.na(year_first) ~ 2300,
      .default = year_first
    )
  )

sum(is.na(rwa_long.3$year_first))

rwa_long.3 <- rwa_long.3 %>% 
  mutate(
    Year = as.numeric(Year),
    year_first =as.numeric(year_first)
  ) %>% 
  mutate(time_elec = Year - year_first)

rwa_long.3 <- rwa_long.3 %>% 
  mutate(
    
    treatl3 = ifelse( time_elec <= -3 & time_elec>= -30 , 1, 0), # elec 3 or more years in the future
    treatl2 = ifelse( time_elec == -2, 1, 0), #elec 2 years in the future
    treatl1 = ifelse( time_elec == -1, 1, 0), #elec 2 years in the future
    treatp0 = ifelse( time_elec == 0, 1, 0), #elec this year
    treatp1 = ifelse( time_elec == 1, 1, 0), #elec last year
    treatp2 = ifelse( time_elec ==  2, 1, 0), # elec last two years
    treatp3 = ifelse( time_elec >= 3, 1, 0), #elec more than three years ago

  )

rwa_long_check <- rwa_long.3 %>% 
  filter(
    treatl3 == 0 & treatl2 == 0 & treatl1 == 0 & treatp0 == 0 & treatp1 ==0 & treatp2 ==0 & treatp3 == 0
  )

write_xlsx(rwa_long.3, file.path(data_path, "rwa_long_0501.xlsx"))

#Event study regression----

###Fit the fixed effect model----
model_es_fe <- rwa_long.3 %>% 
  filter(year_first > 1900 ) %>% 
  filter(Year >= 2010) %>%
 feols(fml = Value ~ treatl3 + treatl2 + treatp0 + treatp1 + treatp2 + treatp3 | Village_ID + Year, cluster = "Village_ID") %>% summary()



# View the summary of the model
summary(model_es_fe)


modelsummary(
  model_es_fe,
  type = "html",
  title = "Table: Events Study Village and Year(excluding already electrified first year)",
  stars = TRUE,
  out = file.path(output_path, "es_usage_feols.html"))


#Exclude the not electrified ones

rwa_long.4 <- rwa_long.2 %>% 
  # filter(!is.na(year_first)) %>% 
  mutate(
    year_first = case_when(
      year_first == 2010 ~ 1900,
      is.na(year_first) ~ 2300,
      .default = year_first
    )
  )

rwa_long.4 <- rwa_long.4 %>% 
  mutate(
    Year = as.numeric(Year),
    year_first =as.numeric(year_first)
  ) %>% 
  mutate(time_elec = Year - year_first)

rwa_long.4 <- rwa_long.4 %>% 
  mutate(
    treatl3 = ifelse( time_elec <= -3 & time_elec > -30 , 1, 0), # elec 3 or more years in the future
    treatl2 = ifelse( time_elec == -2, 1, 0), #elec 2 years in the future
    treatl1 = ifelse( time_elec == -1, 1, 0), #elec 2 years in the future
    treatp0 = ifelse( time_elec == 0, 1, 0), #elec this year
    treatp1 = ifelse( time_elec == 1, 1, 0), #elec last year
    treatp2 = ifelse( time_elec ==  2, 1, 0), # elec last two years
    treatp3 = ifelse( time_elec >= 3, 1, 0), #elec more than three years ago
    notreat = ifelse(year_first == 2300, 1, 0)
  )

sum(rwa_long.3$year_first == 2300)

#Event study regression----

###Fit the fixed effect model----
model_es_fe.1 <- rwa_long.4 %>% 
  filter(year_first > 1900 ) %>% 
  filter(Year >= 2010) %>%
  feols(fml = Value ~ treatl3 + treatl2 + treatp0 + treatp1 + treatp2 + treatp3 | Village_ID + Year, cluster = "Village_ID") %>% summary()



# View the summary of the model
summary(model_es_fe.1)


modelsummary(
  model_es_fe.1,
  type = "html",
  title = "Table: Events Study Village and Year",
  stars = TRUE,
  note = "excluding already electrified first year, including never electrified villages",
  out = file.path(output_path, "es_usage_feols.html"))



###Fit the fixed effect model----

rwa_long.5 <- rwa_long.4 %>% 
  filter(
    notreat != 1
  ) 
model_es_fe.2 <- rwa_long.5 %>% 
  filter(year_first > 1900) %>% 
  filter(Year >= 2010) %>%
  feols(fml = Value ~ treatl3 + treatl2 + treatp0 + treatp1 + treatp2 + treatp3 | Village_ID + Year, cluster = "Village_ID") %>% summary()



# View the summary of the model
summary(model_es_fe.2)

modelsummary(
  model_es_fe.1, model_es_fe.2,
  type = "html",
  title = "Table: Events Study Village and Year",
  stars = TRUE,
  note = "excluding already electrified first year, excluding never electrified villages",
  out = file.path(output_path, "es_usage_feols(exclude noelec).html"))







# Combine models into a list
model_list <- list(`including never electrified villages` = model_es_fe.1, `excluding never electrified villages` = model_es_fe.2)

# Create the combined table
 modelsummary(
  model_list,
  type = "html",
  title = "Table: Events Study Village and Year",
  stars = TRUE,
  note = "excluding already electrified first year(2010)",
  out = file.path(output_path, "es_feols.html")
)







##graph----
 library(broom)
 coefs_df <- tidy(model_es_fe.1)
 
 new_row <- data.frame(term = "treatl1", estimate = 0, std.error = 0, statistic = 0, p.value = 0)
 
 coefs_df <- coefs_df %>%
   bind_rows(new_row)
 coefs_df <- coefs_df %>% 
   mutate(
     term = factor(term, levels = c("treatl3", "treatl2", "treatl1", "treatp0", "treatp1", "treatp2", "treatp3"))
   ) %>% 
   arrange(term) 
 
 ggplot(coefs_df, aes(x = term, y = estimate, group = 1)) +
   geom_point() +
   geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 *  std.error),
                 colour = "black", width = 0.1, position = position_dodge(0.1)) +
   geom_line() +
   labs(title = "Event Study Plot for usage",
        x = "Year",
        y = "Coefficient")

 
 ggplot(coefs_df, aes(x = term, y = estimate, group = 1)) +
   geom_point() +
   geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 *  std.error),
                 colour = "black", width = 0.1, position = position_dodge(0.1)) +
   geom_line() +
   labs(title = "Event Study on Nighttime Light",
        x = "Year",
        y = "Coefficient",
        caption = "Including never electrified villages and excluding already electrified villages before 2010") +
   annotate("text", x = "treatl1", y = 0, label = "reference point", vjust = -1)



 
#Usage data as the outcome----
 
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
 
 
 usage_long <- usage_join %>% 
   pivot_longer(
     cols = ends_with("usage"),
     names_to = "year",
     values_to = "usage"
   )
 
 usage_long <- usage_long %>% 
   mutate(
     year = substr(year, 1,4),
     year = as.double(year)
   )
 
 
 
 
rwa_long.6 <- left_join(rwa_long.4, usage_long, by = c("Village_ID" = "village_id",
                                                       "Year" = "year"))

rwa_long.6 <- rwa_long.6 %>% 
clean_names() %>% 
  mutate(
    usage = ifelse(is.na(usage), 0, usage)
  )



















##Regression using usage----

###Fit the fixed effect model----

sum(is.na(rwa_long.6$usage))

rwa_long_usage <- rwa_long.6 %>% 
  filter(year >= 2010) %>% 
  mutate(usage = as.numeric(usage))

sum(is.na(rwa_long_usage$connect_vt))
sum(is.na(rwa_long_usage$connect_vt))


check <- rwa_long_usage %>% 
  filter( notreat == 1)

usage_es.1 <- rwa_long_usage %>%
  filter(year_first > 1900) %>%
  feols(usage ~ treatl3 + treatl2 + treatp0 + treatp1 + treatp2 + treatp3 | village_id + year, cluster = "village_id") %>%
  summary()


usage_es.2 <- rwa_long_usage %>%
  filter(year_first > 1900) %>%
  filter(notreat != 1) %>% 
  feols(usage ~ treatl3 + treatl2 + treatp0 + treatp1 + treatp2 + treatp3 | village_id + year, cluster = "village_id") %>%
  summary()

summary(usage_es.2)

# View the summary of the model
summary(usage_es.1)

check_model <- lm(value ~  treatl3 + treatl2 + treatp0 + treatp1 + treatp2 + treatp3, data = rwa_long_usage)

summary(check_model)

check <- rwa_long_usage %>% 
  filter(treatl3 == 1 & usage != 0)

# Combine models into a list
model_list <- list(`including never electrified villages` = usage_es.1, `excluding never electrified villages` = usage_es.2)

# Create the combined table
modelsummary(
  model_list,
  type = "html",
  title = "Table: Events Study Village and Year",
  stars = TRUE,
  note = "excluding already electrified first year(2010)",
  out = file.path(output_path, "usage_feols.html")
)

write_xlsx(rwa_long_usage, path = file.path(data_path, "rwa_long_usage.xlsx"))
