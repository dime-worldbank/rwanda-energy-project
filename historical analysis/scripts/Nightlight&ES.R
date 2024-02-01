##########################
#Author: Xiaoming Zhang
#Date: 01302024
#purpose: Comparing different nightlight data, choosing the harmonized one, and running Event Study 
############################


pacman::p_load(raster,exactextractr,dplyr, rgdal,here,ggplot2,sf,tidyr,readxl,writexl, stringr)

getwd()

#Get the nightlights data


nightlights <- read_xlsx(here("data", "Nightlights Village Average(monthly&yearly).xlsx"))

#rwa_village----

rwa_sp <- readOGR(dsn = here("data", "rwa_villages","Village.shp"))
rwa_sp <- spTransform(rwa_sp, CRS("+init=epsg:4326"))


# 1993-2013 calibrate----
night_light.2 <- raster(here("data", "rw_viirs_corrected_monthly_start_199201_avg_vis.tif"), 35)
# mean <- exact_extract(night_light.2, rwa_sp, "mean")
# rwa_df$`1994` <- mean_1993

rwa_df <- as.data.frame(rwa_sp)

#Loop----

for (layer_number in 1:35) {
  # Construct the file path for the raster layer
  night_light<- raster(here("data", "rw_viirs_corrected_monthly_start_199201_avg_vis.tif"), layer_number)
  
  # Extract the mean value and add it as a new column in rwa_df
  mean_value <- exact_extract(night_light, rwa_sp, "mean")
  
  # Set the column name to the new date (e.g., "layer_1", "layer_2", ...)
  col_name <- paste("layer", layer_number, sep = "_")
  rwa_df[col_name] <- mean_value
}




# 1993-2013 calibrate----
night_light.2 <- raster(here("data", "rw_viirs_corrected_monthly_start_199201_avg_vis.tif"), 35)
# mean <- exact_extract(night_light.2, rwa_sp, "mean")
# rwa_df$`1994` <- mean_1993

rwa_df <- as.data.frame(rwa_sp)

#Loop----

for (layer_number in 1:35) {
  # Construct the file path for the raster layer
  night_light<- raster(here("data", "rw_viirs_corrected_monthly_start_199201_avg_vis.tif"), layer_number)
  
  # Extract the mean value and add it as a new column in rwa_df
  mean_value <- exact_extract(night_light, rwa_sp, "mean")
  
  # Set the column name to the new date (e.g., "layer_1", "layer_2", ...)
  col_name <- paste("layer", layer_number, sep = "_")
  rwa_df[col_name] <- mean_value
}


#CCNL dataset----

#Test
# night_light.2013 <- raster(here("data", "CCNL_DMSP_2013_V1.tif"), 1)
# 
# mean <- exact_extract(night_light.2013, rwa_sp, "mean")
# rwa_df$`2013_bnu` <- mean

rwa_ccnl <- rwa_df %>% 
  select(Village_ID, Province, District, Sector, Cell, Name)


for (year in 1992:2013) {
  # Construct the file path for the raster layer
  night_light<- raster(here("data", "ccnl", paste0("CCNL_DMSP_", year, "_V1.tif" )), 1)
  
  # Extract the mean value and add it as a new column in rwa_df
  mean_value <- exact_extract(night_light, rwa_sp, "mean")
  
  # Set the column name to the new date (e.g., "layer_1", "layer_2", ...)
  col_name <-  paste0("year-", year)
  rwa_ccnl[col_name] <- mean_value
}


write.csv(rwa_ccnl, file = here("data", "ccnl", "ccnl_nightlight_data.csv"), row.names = FALSE)

?write.csv






#Harmonized 1992-2020

# Data citation: Li, Xuecao; Zhou, Yuyu; zhao, Min; Zhao, Xia (2020): Harmonization of DMSP and VIIRS nighttime
# light data from 1992-2020 at the global scale. figshare. Dataset.
# https://doi.org/10.6084/m9.figshare.9828827.v5

rwa_harmonized <- rwa_df %>% 
  select(Village_ID, Province, District, Sector, Cell, Name)


for (year in 1992:2013) {
  # Construct the file path for the raster layer
  night_light<- raster(here("data", "harmonized1992-2020", paste0("Harmonized_DN_NTL_", year, "_calDMSP.tif" )), 1)
  
  # Extract the mean value and add it as a new column in rwa_df
  mean_value <- exact_extract(night_light, rwa_sp, "mean")
  
  # Set the column name to the new date (e.g., "layer_1", "layer_2", ...)
  col_name <-  paste0("year-", year)
  rwa_harmonized[col_name] <- mean_value
}



for (year in 2014:2020) {
  # Construct the file path for the raster layer
  night_light<- raster(here("data", "harmonized1992-2020", paste0("Harmonized_DN_NTL_", year, "_simVIIRS.tif" )), 1)
  
  # Extract the mean value and add it as a new column in rwa_df
  mean_value <- exact_extract(night_light, rwa_sp, "mean")
  
  # Set the column name to the new date (e.g., "layer_1", "layer_2", ...)
  col_name <-  paste0("year-", year)
  rwa_harmonized[col_name] <- mean_value
}


write.csv(rwa_harmonized, file = here("data", "harmonized1992-2020", "harmonized_nightlight_data.csv"), row.names = FALSE)



#Event study on this harmonized data----


#Event year----

MV_2022 <- read_xlsx(here("data", "Rwanda Villages with Existing MV_2022.xlsx"))
MV_2011 <- read_xlsx(here("data", "Rwanda Villages with Existing MV_2011.xlsx"))

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
  summarize(diff = `not connected 2011` - `connected 2011`,
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
  title = "Table 2: Nightlight trend from 2008 to 2015 in Rwanda(Outside Kigali)",
  out = "2008-2015(Outside Kigali).html")





###Event study----

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
  summarize(diff = `not connected 2011` - `connected 2011`,
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



