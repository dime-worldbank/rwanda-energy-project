
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


didrwa <- lm(Value ~ numYear*treated + treated + numYear, data = rwa_long_did)

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
