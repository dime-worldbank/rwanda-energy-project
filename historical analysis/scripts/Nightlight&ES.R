##########################
#Author: Xiaoming Zhang
#Date: 01302024
#purpose: Comparing different nightlight data, choosing the harmonized one, and running Event Study 
############################


pacman::p_load(raster,exactextractr,dplyr, rgdal,here,ggplot2,sf,tidyr,readxl,writexl, stringr, stargazer)

getwd()

#ReadDropbox----


if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX, "Rwanda Energy/datawork/Historical data"
)


#read rwa_villages----

rwa_path <- file.path(
  data_path, "rwa_villages/Village.shp"
)

rwa_sp <- st_read(dsn = rwa_path)
rwa_sp <- st_make_valid(rwa_sp)
rwa_sp <- st_transform(rwa_sp, CRS("+init=epsg:4326"))



rwa_df <- as.data.frame(rwa_sp) %>% 
  select(-geometry)






#Harmonized 1992-2020----

# Data citation: Li, Xuecao; Zhou, Yuyu; zhao, Min; Zhao, Xia (2020): Harmonization of DMSP and VIIRS nighttime
# light data from 1992-2020 at the global scale. figshare. Dataset.
# https://doi.org/10.6084/m9.figshare.9828827.v5


rwa_harmonized <- rwa_df %>% 
  select(Village_ID, Province, District, Sector, Cell, Name)

for (year in 1992:2013) {
  # Construct the file path for the raster layer
  file_path <- file.path(data_path, paste0("harmonized1992-2020/Harmonized_DN_NTL_", year, "_calDMSP.tif"))
  
  # Load the raster layer
  night_light <- raster(file_path, 1)
  
  
  # Extract the mean value and add it as a new column in rwa_df
  mean_value <- exact_extract(night_light, rwa_sp, "mean")
  
  # Set the column name to the new date (e.g., "layer_1", "layer_2", ...)
  col_name <-  paste0("year-", year)
  rwa_harmonized[col_name] <- mean_value
}



for (year in 2014:2020) {
  
  # Construct the file path for the raster layer
  file_path <- file.path(data_path, paste0("harmonized1992-2020/Harmonized_DN_NTL_", year, "_simVIIRS.tif"))
  
  # Load the raster layer
  night_light <- raster(file_path, 1)
 
  # Extract the mean value and add it as a new column in rwa_df
  mean_value <- exact_extract(night_light, rwa_sp, "mean")
  
  # Set the column name to the new date (e.g., "layer_1", "layer_2", ...)
  col_name <-  paste0("year-", year)
  rwa_harmonized[col_name] <- mean_value
}


write.csv(rwa_harmonized, file = here("data", "harmonized1992-2020", "harmonized_nightlight_data.csv"), row.names = FALSE)



#Event study on this harmonized data----


#Event year----

MV_2022 <- read_xlsx(path = file.path(data_path, "Rwanda Villages with Existing MV_2022.xlsx"))
MV_2011 <- read_xlsx(path = file.path(data_path, "Rwanda Villages with Existing MV_2011.xlsx"))

MV_2022 <- MV_2022 %>% 
  mutate(connect22 = 1) %>% 
  select(Village_ID, connect22)

rwa_event <- left_join(rwa_harmonized, MV_2022, by = c("Village_ID"))

MV_2011 <- MV_2011 %>% 
  mutate(connect11 = 1) %>% 
  select(Village_ID, connect11)

rwa_event <- left_join(rwa_event, MV_2011, by = c("Village_ID"))


rwa_event_new <- rwa_event %>% 
  mutate(connect22 = ifelse(is.na(connect22), 0, connect22)) %>% 
  mutate(connect11 = ifelse(is.na(connect11), 0, connect11)) 

rwa_event_new <- rwa_event_new %>% 
  mutate(
    connection_time = ifelse(connect11 == 1, "connected 2011", "not connected 2011")
  )






table(rwa_event_new$connection_time)


#Filter out the NAs which are villages that have grid lines in 2011, but not in 2022, noisy
rwa_event_new  <-rwa_event_new%>%
  rename_with(~ gsub("^year-", "", .), starts_with("year-")) %>% 
  filter(!is.na(connection_time))




#pivot long----
rwa_event_long <- rwa_event_new %>%
  pivot_longer(
    cols = matches("^(19|20)"),  # Use a regular expression to match columns starting with "19" or "20"
    names_to = "Year",
    values_to = "Value"
  )


# ALL District ES time series ----


# calculate the means
cohort_means <- rwa_event_long %>% 
  group_by(connection_time, Year) %>% 
  summarize(cohort_mean = mean(Value, na.rm = TRUE)) 

cohort_means <- cohort_means %>% 
  ungroup()

### plot the means----

ggplot(data = cohort_means, aes(x = Year, y = cohort_mean, colour = as.factor(connection_time), group = as.factor(connection_time))) + 
  geom_point() + 
  geom_line() +
  labs(x = "Year", y = "Avg Nightlight Value", color = "connection_time") +
  theme_classic() + 
  geom_vline(xintercept = c(17,24), alpha = 0.3, linetype = "dashed", size = 0.5, color = "red")+
  labs(title = "Event Study Time Series Rwanda 1992- 2020")


#Regression


connect_2011 <- cohort_means%>% 
  mutate(Year = as.numeric(Year)) %>% 
  filter(connection_time == "connected 2011" & Year >=2008 & Year <= 2015)

connect_2011 <- connect_2011 %>% 
  mutate(Year = Year-2008)


model_connect_2011<- lm(as.numeric(cohort_mean) ~ as.numeric(Year), data = connect_2011)

summary(model_connect_2011)



unconnect_2011 <- cohort_means%>% 
  mutate(Year = as.numeric(Year)) %>% 
  filter(connection_time == "not connected 2011" & Year >=2008 & Year <= 2015)

unconnect_2011 <- unconnect_2011 %>% 
  mutate(Year = Year-2008)

model_unconnect_2011<- lm(as.numeric(cohort_mean) ~ as.numeric(Year), data = unconnect_2011)

summary(model_unconnect_2011)

model_rwa <- list(
  "connected_2011" = model_connect_2011,
  "not connected_2011" = model_unconnect_2011
)


library(stargazer)
stargazer(
  model_rwa,
  type = "html",
  title = "Table 1: Nightlight trend from 2008 to 2015 in Rwanda",
  out = "2008-2015.html")

###Event study----

se <- rwa_event_long %>%
  group_by(connection_time, Year) %>%
  summarize(var = var(Value, na.rm = TRUE),
            n = n(),
            .groups = "drop_last") %>%
  mutate(se = sqrt(sum(var * (n - 1)) / (sum(n) - 2)) * sum(1 / n))


#Calculate Mean Difference
mean_diff <- rwa_event_long %>%
  group_by(connection_time, Year) %>%
  summarize(mean_effect = mean(Value, na.rm = TRUE)) %>%
  spread(connection_time, mean_effect) %>%
  summarize(diff = `connected 2011` - `not connected 2011`,
            Year = Year)


# Combine with Standard Errors
mean_diff <- mean_diff %>%
  left_join(se, by = "Year")

# Create Event Study Plot
ggplot(mean_diff, aes(x = as.numeric(Year), y = diff)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = diff - 1.96 * se, ymax = diff + 1.96 * se),
                colour = "black", width = 0.1, position = position_dodge(0.1)) +
  geom_vline(xintercept = c(2008,2015), linetype = "dashed") +
  geom_hline(yintercept = 0.0, linetype = "dashed") +
  labs(title = "Event Study Plot",
       x = "Year",
       y = "Difference and 95% Conf. Int.")











#NOT including Kigali----

# calculate the means
cohort_means <- rwa_event_long %>% 
  filter(!(District %in% c("Gasabo", "Kicukiro", "Nyarugenge"))) %>%
  group_by(connection_time, Year) %>% 
  summarize(cohort_mean = mean(Value, na.rm = TRUE)) 

cohort_means <- cohort_means %>% 
  ungroup()

### plot the means----

ggplot(data = cohort_means, aes(x = Year, y = cohort_mean, colour = as.factor(connection_time), group = as.factor(connection_time))) + 
  geom_point() + 
  geom_line() +
  labs(x = "Year", y = "Avg Nightlight Value", color = "connection_time") +
  theme_classic() + 
  geom_vline(xintercept = c(17,24), alpha = 0.3, linetype = "dashed", size = 0.5, color = "red")+
  labs(title = "Event Study Time Series Rwanda 1992- 2020")


#Regression----


connect_2011 <- cohort_means%>% 
  mutate(Year = as.numeric(Year)) %>% 
  filter(connection_time == "connected 2011" & Year >=2008 & Year <= 2015)

connect_2011 <- connect_2011 %>% 
  mutate(Year = Year-2008)


model_connect_2011<- lm(as.numeric(cohort_mean) ~ as.numeric(Year), data = connect_2011)

summary(model_connect_2011)



unconnect_2011 <- cohort_means%>% 
  mutate(Year = as.numeric(Year)) %>% 
  filter(connection_time == "not connected 2011" & Year >=2008 & Year <= 2015)

unconnect_2011 <- unconnect_2011 %>% 
  mutate(Year = Year-2008)

model_unconnect_2011<- lm(as.numeric(cohort_mean) ~ as.numeric(Year), data = unconnect_2011)

summary(model_unconnect_2011)

model_rwa <- list(
  "connected_2011" = model_connect_2011,
  "not connected_2011" = model_unconnect_2011
)


library(stargazer)
stargazer(
  model_rwa,
  type = "html",
  title = "Table 2: Nightlight trend from 2008 to 2015 in Rwanda(Outside Kigali)",
  out = "2008-2015(Outside Kigali).html")





#All Event study----

se <- rwa_event_long %>%
  filter(!(District %in% c("Gasabo", "Kicukiro", "Nyarugenge"))) %>%
  group_by(connection_time, Year) %>%
  summarize(var = var(Value, na.rm = TRUE),
            n = n(),
            .groups = "drop_last") %>%
  mutate(se = sqrt(sum(var * (n - 1)) / (sum(n) - 2)) * sum(1 / n))


#Calculate Mean Difference
mean_diff <- rwa_event_long %>%
  filter(!(District %in% c("Gasabo", "Kicukiro", "Nyarugenge"))) %>%
  group_by(connection_time, Year) %>%
  summarize(mean_effect = mean(Value, na.rm = TRUE)) %>%
  spread(connection_time, mean_effect) %>%
  summarize(diff = `connected 2011` - `not connected 2011`,
            Year = Year)


# Combine with Standard Errors
mean_diff <- mean_diff %>%
  left_join(se, by = "Year")

# Create Event Study Plot
ggplot(mean_diff, aes(x = as.numeric(Year), y = diff)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = diff - 1.96 * se, ymax = diff + 1.96 * se),
                colour = "black", width = 0.1, position = position_dodge(0.1)) +
  geom_vline(xintercept = c(2008,2015), linetype = "dashed") +
  geom_hline(yintercept = 0.0, linetype = "dashed") +
  labs(title = "Event Study Plot(Outside)",
       x = "Year",
       y = "Difference and 95% Conf. Int.")





#Read roads data----

rd <- file.path(data_path, "Classified_Roads_2022/National_Roads.shp")
national_rd <- st_read(rd)

str(rwa_sp)
national_rd <- st_transform(national_rd, crs = st_crs(rwa_sp))

national_rd <- st_as_sf(national_rd)

national_rd <- st_zm(national_rd)


rwa_sp <- st_as_sf(rwa_sp)

ggplot() + 
  geom_sf(data = rwa_sp, fill = NA) +
  geom_sf(data = national_rd, color = "blue") +
  theme_classic() +
  labs(title = "Rwanda National Road Network")


###overlap with nightlights----

nightlight <- raster(file.path(data_path, "harmonized1992-2020/Harmonized_DN_NTL_2020_simVIIRS.tif"))

rwa_sp <- st_transform(rwa_sp, crs = st_crs(national_rd))
st_crs(rwa_sp)


rwa_road <- st_intersection(rwa_sp, national_rd)

rwa_road <- rwa_road %>% 
  distinct(Village_ID, .keep_all = TRUE)

write_xlsx(rwa_road, path = file.path("C:/Users/wb614406/Dropbox/Rwanda Energy/datawork/Historical data/Rwanda Villages with National Road.xlsx"))

rwa_road_join <- rwa_road %>% 
  st_drop_geometry() %>% 
  mutate(road = 1) %>% 
  select(Village_ID, road)  

#Join with rwa_event_long----

rwa_event_long <- left_join(rwa_event_long, rwa_road_join, by = c("Village_ID"))


rwa_event_long <- rwa_event_long %>% 
  mutate(road = ifelse(is.na(road), 0, road))


#Join LV in 2022 existing----

existing_LV22 <- st_read(dsn = file.path("C:/Users/wb614406/Dropbox/Rwanda Energy/datawork/Historical data/Existing Electrical Network_2022/Existing_LVLine.shp"))


existing_LV22 <- st_transform(existing_LV22, crs = st_crs(rwa_sp))

LV_villages <- st_intersection(existing_LV22, rwa_sp)

LV_villages <- LV_villages %>% 
  distinct(Village_ID, .keep_all = TRUE) %>% 
  select(Village_ID, Name, Cell, Sector, District, Province)

write_xlsx(LV_villages, path = file.path("C:/Users/wb614406/Dropbox/Rwanda Energy/datawork/Historical data/Rwanda Villages with Existing LV_2022.xlsx"))



###Rejoin back to rwa_event_long 

lv_join <- LV_villages %>% 
  select(Village_ID) %>% 
  mutate(connect22_lv = 1) %>% 
  st_drop_geometry()

rwa_event_long <- left_join(rwa_event_long, lv_join, by = c("Village_ID"))


#rwa_long <- rwa_event_long----

rwa_long <- rwa_event_long %>% 
  rename(connect22_mv = connect22,
         connect11_mv = connect11,
         connect22_lv = connect22_lv) 

rwa_long <- rwa_long %>% 
  mutate(connect22_lv = ifelse(is.na(connect22_lv), 0, connect22_lv))


###2022 redo----

after_2011 <- rwa_long %>% 
  filter(connect11_mv != 1)

after_2011 <- after_2011 %>% 
  mutate(
    connection_time = ifelse(connect22_lv == 1, "connected 2022", "not connected 2022")
  )

cohort_means<- after_2011 %>% 
  filter(!(District %in% c("Gasabo", "Kicukiro", "Nyarugenge"))) %>%
  group_by(connection_time, Year, road) %>% 
  summarize(cohort_mean = mean(Value, na.rm = TRUE)) 

cohort_means <- cohort_means %>% 
  ungroup()

cohort_means <- cohort_means %>% 
  mutate(connect_road = case_when(
    connection_time %in% c("connected 2022") & road == 1 ~ "connected 2022-NR1",
    connection_time %in% c("connected 2022") & road == 0 ~ "connected 2022-NR0",
    connection_time %in% c("not connected 2022")& road == 1 ~ "not connected 2022-NR1",
    connection_time %in% c("not connected 2022")& road == 0 ~ "not connected 2022-NR0"
  ))



ggplot(data = cohort_means, aes(x = Year, y = cohort_mean,group = as.factor(connect_road))) + 
  geom_point(aes(color = connect_road)) + 
  geom_line(aes(linetype = connect_road, color = connect_road)) +  
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed" )) +  # Adjust size for specific groups
  scale_color_manual(values = c("blue", "blue", "black", "black")) +
  # Set a default size for all lines
  theme_classic() + 
  geom_vline(xintercept = c(17,24), alpha = 0.3, linetype = "solid", size = 0.5, color = "red") +
  labs(title = "Event Study Time Series Rwanda 1992-2020 (outside Kigali not connected in 2022)") + 
  labs(x = "Year", y = "Avg Nightlight Value", color = "connect_road") 


###2011 redo----

at_2011 <- rwa_long %>% 
  mutate(
    connection_time = ifelse(connect11_mv == 1, "connected 2011", "not connected 2011")
  )


cohort_means <- at_2011 %>% 
  filter(!(District %in% c("Gasabo", "Kicukiro", "Nyarugenge"))) %>%
  group_by(connection_time, Year, road) %>% 
  summarize(cohort_mean = mean(Value, na.rm = TRUE)) 

cohort_means <- cohort_means %>% 
  ungroup()

cohort_means <- cohort_means %>% 
  mutate(connect_road = case_when(
    connection_time %in% c("connected 2011") & road == 1 ~ "connected 2011-NR1",
    connection_time %in% c("connected 2011") & road == 0 ~ "connected 2011-NR0",
    connection_time %in% c("not connected 2011")& road == 1 ~ "not connected 2011-NR1",
    connection_time %in% c("not connected 2011")& road == 0 ~ "not connected 2011-NR0"
  ))


ggplot(data = cohort_means, aes(x = Year, y = cohort_mean,group = as.factor(connect_road))) + 
  geom_point(aes(color = connect_road)) + 
  geom_line(aes(linetype = connect_road, color = connect_road)) +  
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed" )) +  # Adjust size for specific groups
  scale_color_manual(values = c("blue", "blue", "black", "black")) +
  # Set a default size for all lines
  theme_classic() + 
  geom_vline(xintercept = c(17,24), alpha = 0.3, linetype = "solid", size = 0.5, color = "red") +
  labs(title = "Event Study Time Series Rwanda 1992-2020 (outside Kigali not connected in 2011)") + 
  labs(x = "Year", y = "Avg Nightlight Value", color = "connect_road") 



## No roads,2022----

#####All villages----

# calculate the means
cohort_means <- after_2011 %>% 
  group_by(connection_time, Year) %>% 
  summarize(cohort_mean = mean(Value, na.rm = TRUE)) 

cohort_means <- cohort_means %>% 
  ungroup()

ggplot(data = cohort_means, aes(x = Year, y = cohort_mean, colour = as.factor(connection_time), group = as.factor(connection_time))) + 
  geom_point() + 
  geom_line() +
  labs(x = "Year", y = "Avg Nightlight Value", color = "connection_time") +
  theme_classic() + 
  geom_vline(xintercept = c(17,24), alpha = 0.3, linetype = "dashed", size = 0.5, color = "red")+
  labs(title = "Event Study Time Series Rwanda 1992- 2020(not connected in 2011)")


##Not including Kigali----



# calculate the means
cohort_means <- after_2011 %>% 
  filter(!(District %in% c("Gasabo", "Kicukiro", "Nyarugenge"))) %>%
  group_by(connection_time, Year) %>% 
  summarize(cohort_mean = mean(Value, na.rm = TRUE)) 

cohort_means <- cohort_means %>% 
  ungroup()

ggplot(data = cohort_means, aes(x = Year, y = cohort_mean, colour = as.factor(connection_time), group = as.factor(connection_time))) + 
  geom_point() + 
  geom_line() +
  labs(x = "Year", y = "Avg Nightlight Value", color = "connection_time") +
  theme_classic() + 
  geom_vline(xintercept = c(17,24), alpha = 0.3, linetype = "dashed", size = 0.5, color = "red")+
  labs(title = "Event Study Time Series Rwanda 1992- 2020(outside Kigali not connected in 2011)")

write_xlsx(rwa_long, path = file.path("C:/Users/wb614406/Dropbox/Rwanda Energy/datawork/Historical data/ntl_long_92_20.xlsx"))



#Diff in diff----


##2011----
####For all villages2008-2015----

rwa_long_did <- rwa_long %>% 
  filter(!(District %in% c("Gasabo", "Kicukiro", "Nyarugenge")) & Year == c(2008:2015)) %>% 
  mutate(
    treated = ifelse(connection_time== "connected 2011", 1, 0),
    numYear = as.numeric(Year)-2008,
    Year = as.numeric(Year)
  )


###regression----


didrwa <- lm(Value ~ numYear*treated + treated + numYear, data = rwa_long_did)

summary(didrwa)

stargazer(
  didrwa,
  type = "html",
  title = "Table 3: Difference in differences(2011) 2008-2014",
  out = "DiD2011.html")



####graph----


# Create a new variable for the predicted values
rwa_long_did$predicted <- predict(lm(Value ~ treated + Year + treated * Year, data = rwa_long_did))
lm(Value ~ treated + numYear + treated * numYear, data = rwa_long_did)


connected_intercept <- subset(rwa_long_did, treated == 1)$predicted[1]
not_connected_slope <- coef(lm(predicted ~ treated+ Year + treated * Year, 
                               data = subset(rwa_long_did, treated == 0)))[3]

# Plot the interaction term
ggplot(rwa_long_did, aes(x = Year, y = treated * Year, color = connection_time)) +
  geom_line(aes(y = predicted), linetype = "solid", size = 1) +
  geom_segment(aes(x = min(Year), y = connected_intercept,
                   xend = max(Year), yend = connected_intercept + not_connected_slope * (max(Year) - min(Year))),
               linetype = "dashed", color = "black") +
  labs(title = "Diff in Diff(2008-2015 outside Kigali)",
       x = "Year",
       y = "treated*Year",
       color = "Group") +
  theme_minimal()




##2022----
####For all villages2008-2015----

rwa_long_did <- rwa_long %>% 
  filter(!(District %in% c("Gasabo", "Kicukiro", "Nyarugenge")) & Year == c(2008:2015)) %>% 
  mutate(
    treated = ifelse(connect22_lv== 1, 1, 0),
    numYear = as.numeric(Year)-2008,
    Year = as.numeric(Year)
  )


###regression----


didrwa <- lm(Value ~ numYear*treated + treated + numYear, data = rwa_long_did)

summary(didrwa)

stargazer(
  didrwa,
  type = "html",
  title = "Table 3: Difference in differences(2011) 2008-2014",
  out = "DiD2011.html")



####graph----


# Create a new variable for the predicted values
rwa_long_did$predicted <- predict(lm(Value ~ treated + Year + treated * Year, data = rwa_long_did))
lm(Value ~ treated + numYear + treated * numYear, data = rwa_long_did)


connected_intercept <- subset(rwa_long_did, treated == 1)$predicted[1]
not_connected_slope <- coef(lm(predicted ~ treated+ Year + treated * Year, 
                               data = subset(rwa_long_did, treated == 0)))[3]

# Plot the interaction term
ggplot(rwa_long_did, aes(x = Year, y = treated * Year, color = connection_time)) +
  geom_line(aes(y = predicted), linetype = "solid", size = 1) +
  geom_segment(aes(x = min(Year), y = connected_intercept,
                   xend = max(Year), yend = connected_intercept + not_connected_slope * (max(Year) - min(Year))),
               linetype = "dashed", color = "black") +
  labs(title = "Diff in Diff(2008-2015 outside Kigali)",
       x = "Year",
       y = "treated*Year",
       color = "Group") +
  theme_minimal()
