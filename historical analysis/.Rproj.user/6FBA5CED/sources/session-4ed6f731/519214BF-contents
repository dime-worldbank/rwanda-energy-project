
##########################
#Author: Xiaoming Zhang
#Date: 02072024
#purpose:Difference in difference analysis 
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


# rwa_wide----
rwa_wide <- read_xlsx(path = file.path(data_path, "ntl_wide_92_21(connect11&22+rd).xlsx"))

# rwa_long----

?pivot_longer()

rwa_long <- rwa_wide %>% 
  pivot_longer(
    cols = matches("^(19|20)"),  
    names_to = "Year",
    values_to = "Value"
  )





##2011----
####For all villages2008-2015----

rwa_long_did <- rwa_long %>% 
  filter(!(District %in% c("Gasabo", "Kicukiro", "Nyarugenge")) & Year == c(2008:2015)) %>% 
  mutate(
    treated = ifelse(connect11_mv== 1, 1, 0),
    numYear = as.numeric(Year)-2008,
    Year = as.numeric(Year)
  )


###regression----


didrwa <- lm("Value" ~ numYear*treated + treated + numYear, data = rwa_long_did)

summary(didrwa)

stargazer(
  didrwa,
  type = "html",
  title = "Table 3: Difference in differences(2011) 2008-2015",
  out = "DiD2011.html")



####graph----


# Create a new variable for the predicted values
rwa_long_did$predicted <- predict(lm(Value ~ treated + Year + treated * Year, data = rwa_long_did))
lm(Value ~ treated + numYear + treated * numYear, data = rwa_long_did)


connected_intercept <- subset(rwa_long_did, treated == 1)$predicted[1]
not_connected_slope <- coef(lm(predicted ~ treated+ Year + treated * Year, 
                               data = subset(rwa_long_did, treated == 0)))[3]

# Plot the interaction term
rwa_long_did11 <- rwa_long_did %>% 
  mutate(connection_time = ifelse(connect11_mv == 1, "connected 2011", "not connected 2011"))

ggplot(rwa_long_did11, aes(x = Year, y = treated * Year, color = connection_time)) +
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
  title = "Table 4: Difference in differences(2022) 2008-2015",
  out = "DiD2022.html")



####graph----


# Create a new variable for the predicted values
rwa_long_did$predicted <- predict(lm(Value ~ treated + Year + treated * Year, data = rwa_long_did))
lm(Value ~ treated + numYear + treated * numYear, data = rwa_long_did)


connected_intercept <- subset(rwa_long_did, treated == 1)$predicted[1]
not_connected_slope <- coef(lm(predicted ~ treated+ Year + treated * Year, 
                               data = subset(rwa_long_did, treated == 0)))[3]

# Plot the interaction term
rwa_long_did22 <- rwa_long_did %>% 
  mutate(connection_time = ifelse(connect22_lv == 1, "connected 2022", "not connected 2022"))


ggplot(rwa_long_did22, aes(x = Year, y = treated * Year, color = connection_time)) +
  geom_line(aes(y = predicted), linetype = "solid", size = 1) +
  geom_segment(aes(x = min(Year), y = connected_intercept,
                   xend = max(Year), yend = connected_intercept + not_connected_slope * (max(Year) - min(Year))),
               linetype = "dashed", color = "black") +
  labs(title = "Diff in Diff(2008-2015 outside Kigali)",
       x = "Year",
       y = "treated*Year",
       color = "Group") +
  theme_minimal()



#Filter out the connected 2022 but not 2011

#Let's try the MV again

existing22_mv <- st_read(dsn = file.path(data_path, "Existing Electrical Network_2022/Existing_MVLine.shp"))

existing22_mv <- st_transform(existing22_mv, crs = st_crs(rwa_sp))

existing22_mv <- st_intersection(existing22_mv, rwa_sp)



existing22_mv <- existing22_mv %>% 
  distinct(Village_ID) %>% 
  mutate(connect22_mv.2 = 1) %>% 
  select(Village_ID, connect22_mv.2)


rwa_long.2 <- left_join(rwa_long, existing22_mv, by = c("Village_ID"))

rwa_long.2 <- rwa_long.2 %>% 
  mutate(connect22_mv.2 = ifelse(is.na(connect22_mv.2), 0, connect22_mv.2))

#Check if the new one is the same with the old one
all(rwa_long.2$connect22_mv == rwa_long.2$connect22_mv.2)



#Redo graph----
rwa_long.1 <- rwa_long %>% 
  mutate(connection_time = case_when(
    connect11_mv == 0 & connect22_mv == 1 ~ "connected 2011-2022",
    connect11_mv == 0 & connect22_mv == 0 ~ "not connected"
  ))
  
  
cohort_means <- rwa_long.1 %>% 
  filter(!(District %in% c("Gasabo", "Kicukiro", "Nyarugenge")) & !is.na(connection_time)) %>%
  group_by(connection_time, Year, road) %>% 
  summarize(cohort_mean = mean(Value[Value !=0], na.rm = TRUE))

cohort_means <- cohort_means %>% 
  ungroup()

cohort_means <- cohort_means %>% 
  mutate(connect_road = case_when(
    connection_time %in% c("connected 2011-2022") & road == 1 ~ "connected-NR1",
    connection_time %in% c("connected 2011-2022") & road == 0 ~ "connected-NR0",
    connection_time %in% c("not connected")& road == 1 ~ "not connected-NR1",
    connection_time %in% c("not connected")& road == 0 ~ "not connected-NR0"
  ))




ggplot(data = cohort_means, aes(x = Year, y = cohort_mean,group = as.factor(connect_road))) + 
  geom_point(aes(color = connect_road)) + 
  geom_line(aes(linetype = connect_road, color = connect_road)) +  
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed" )) +  # Adjust size for specific groups
  scale_color_manual(values = c("blue", "blue", "black", "black")) +
  # Set a default size for all lines
  theme_classic() + 
  geom_vline(xintercept = c(17,24), alpha = 0.3, linetype = "solid", size = 0.5, color = "red") +
  labs(title = "Event Study Villages outside Kigali that connected between 2011 and 2022") + 
  labs(x = "Year", y = "Avg Nightlight Value", color = "connect_road") 



#0227 diff in diff----

rwa_long <- rwa_wide %>% 
  pivot_longer(
    cols = matches("^(19|20)"),  
    names_to = "Year",
    values_to = "Value"
  )

rwa_long_did <- rwa_long %>% 
  filter(Year >= 2011) %>% 
  mutate(year_2021 = ifelse(Year == 2021, 1, 0))

did <- lm(Value ~ connect11_mv + year_2021*connect11_mv, data = rwa_long_did)


summary(did)





2011 value ~ dummy 2022

interaction with year 2022 

interaction between 2022 values and treatment 


ntl ~ constant + interaction T * 2022 +  T 

















