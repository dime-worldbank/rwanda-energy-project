##############
#Author: Xiaoming Zhang
#Date: 1.27.2025
#Purpose: EDCL List
#############


pacman::p_load(knitr, stargazer, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr, lfe)
library(googlesheets4)
getwd()


# Import Data ----
dropbox <- 'C:/Users/wb614406/Dropbox'

hfc_data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/HFC/data"
)

output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/baseline analysis/output"
)

data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/baseline analysis/data"
)

hfc_constr <- read_xlsx(file.path(hfc_data_path, "hfc_constr.xlsx"))

hfc_constr <- hfc_constr %>% 
  filter(!is.na(A1_1))  %>% 
  filter(finish == 1) %>% 
  distinct(hh_head_name, hh_id, A1_2, A1_3, .keep_all = TRUE)



wtp <- hfc_constr %>%
  mutate(
    wtp_12 = J4_2*12,
    wtp_24 = J5_2*24
  )


wtp_var <- c(
  "J1_final", # wtp_fixed
  "J2_1",     # wtp_fixed_appliance
  "J3_1",     # wtp_fixed_low_reliability
  "J4_2",     # wtp_paygo_12
  "J5_2",     # wtp_paygo_24
  "wtp_12",
  "wtp_24",
  "J6_1")

wtp_selected <- wtp %>%
  select(hh_id, all_of(wtp_var), J6_1_picture, J6_4) %>% 
  rename(
    fixed_system = J1_final,
    appliance = J2_1,
    low_reliability = J3_1,
    lightbulb = J6_1
  ) %>% 
  mutate(across(- c(hh_id, lightbulb, J6_1_picture, J6_4), ~ pmax(.x, 1000)))  %>% 
  mutate(across(- c(hh_id, lightbulb, J6_1_picture, J6_4), ~pmin(.x, 100000))) %>% 
  mutate(
    lightbulb = pmax(lightbulb, 100)
  )



#Fixed system low reliability----

fixedsystem_lowreliability <-
  ggplot(wtp_selected, aes(x = fixed_system, y = low_reliability)) +  # Use raw values
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Add 45-degree line
  geom_point() +  # Add points
  geom_smooth(method = "loess", se = TRUE) +  # Add smooth curve
  labs(title = "Impact of Decreased Reliability of Electricity System \n on Household Willingness to Pay",
       x = "Fixed System (RwF)",
       y = "Low Reliability (RwF)") +
  scale_x_log10() +  # Logarithmic scale for x-axis (keeping values in RwF)
  scale_y_log10() +  # Logarithmic scale for y-axis (keeping values in RwF)
  coord_equal() +  # Equal scaling for both axes
  theme(
    plot.title = element_text(size = 35, face = "bold", hjust = 0.5),  # Bigger & centered title
    axis.text.x = element_text(size = 25),  # Bigger x-axis labels
    axis.text.y = element_text(size = 25),  # Bigger y-axis labels
    axis.title.x = element_text(size = 30), # Bigger x-axis title
    axis.title.y = element_text(size = 30)  # Bigger y-axis title
  )



# Save the Fixed System vs Low Reliability plot
ggsave(filename = file.path(output_path, "fixedsystem_lowreliability_plot.png"), 
       plot = fixedsystem_lowreliability, scale = 3)






# Fixed System vs Appliances ----
fixedsystem_appliances <-
  ggplot(wtp_selected, aes(y = fixed_system, x = appliance)) +  # Use raw values
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Add 45-degree line
  geom_point() +  # Add points
  geom_smooth(method = "loess", se = TRUE) +  # Add smooth curve
  labs(title = "Impact of Fixed Appliance Capacity \n on Household Willingness to Pay for Electrification",
       y = "Fixed System (RwF)",
       x = "Appliances (RwF)") +
  scale_x_log10() +  # Logarithmic scale for x-axis (keeping values in RwF)
  scale_y_log10() +  # Logarithmic scale for y-axis (keeping values in RwF)
  coord_equal() +  # Equal scaling for both axes
  theme(
    plot.title = element_text(size = 35, face = "bold", hjust = 0.5),  # Bigger & centered title
    axis.text.x = element_text(size = 25),  # Bigger x-axis labels
    axis.text.y = element_text(size = 25),  # Bigger y-axis labels
    axis.title.x = element_text(size = 30), # Bigger x-axis title
    axis.title.y = element_text(size = 30)  # Bigger y-axis title
  )

# Save the Fixed System vs Appliances plot
ggsave(filename = file.path(output_path, "fixedsystem_appliances_plot.png"), 
       plot = fixedsystem_appliances, scale = 3)

# Appliances vs Low Reliability ----
appliances_lowreliability <-
  ggplot(wtp_selected, aes(x = appliance, y = low_reliability)) +  # Use raw values
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Add 45-degree line
  geom_point() +  # Add points
  geom_smooth(method = "loess", se = TRUE) +  # Add smooth curve
  labs(title = "Impact of Added Appliances and \n Added Reliability for Electricity \n System on Household Willingness to Pay",
       x = "Grid system willingness to pay (RwF)",
       y = "Solar system willingness to pay (RwF)") +
  scale_x_log10() +  # Logarithmic scale for x-axis (keeping values in RwF)
  scale_y_log10() +  # Logarithmic scale for y-axis (keeping values in RwF)
  coord_equal() +  # Equal scaling for both axes
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),  # Bigger & centered title
    axis.text.x = element_text(size = 18),  # Bigger x-axis labels
    axis.text.y = element_text(size = 18),  # Bigger y-axis labels
    axis.title.x = element_text(size = 20), # Bigger x-axis title
    axis.title.y = element_text(size = 20)  # Bigger y-axis title
  )
appliances_lowreliability 
# Save the Appliances vs Low Reliability plot
ggsave(filename = file.path(output_path, "appliances_lowreliability_plot.png"), 
       plot = appliances_lowreliability, scale = 3)



# Regression----

log_fixed <- lm(log(lightbulb) ~ log(fixed_system), data = wtp_selected)

log_fixed
stargazer(log_fixed, type = "text", 
          title = "Regression Results", 
          out = "regression_output.txt")


log_reliability <- lm(log(lightbulb) ~ log(low_reliability), data = wtp_selected)

stargazer(log_reliability,
          type = "latex", 
          title = "Regression Results", 
          out = "regression_output.latex")


log_appliance <- lm(log(lightbulb) ~ log(appliance), data = wtp_selected)

log_reg <- lm(log(lightbulb) ~ log(fixed_system) + log(low_reliability) + log(appliance) , data = wtp_selected)



wtp_selected <- wtp_selected %>%
  mutate(
    `log(appliance) - log(fixed_system)` = log(appliance) - log(fixed_system),
    `log(fixed_system) - log(low_reliability)` = log(fixed_system) - log(low_reliability)
  )


log_diff <- lm(log(lightbulb) ~ log(low_reliability) + `log(appliance) - log(fixed_system)` + `log(fixed_system) - log(low_reliability)`  , data = wtp_selected)

log_diff

stargazer(log_fixed,log_appliance, log_reliability,log_reg,log_diff,
          
          type = "latex", 
          title = "Regression Results", 
          out = "regression_output.latex")




  ggplot(wtp_selected, aes(x = lightbulb , y = appliance)) +  # Use raw values
  geom_point() +  # Add points
  geom_smooth(method = "loess", se = TRUE) +  # Add smooth curve
  labs(
       x = "Energy demand",
       y = "Grid") +
  scale_x_log10() +  # Logarithmic scale for x-axis (keeping values in RwF)
  scale_y_log10() +  # Logarithmic scale for y-axis (keeping values in RwF)
  coord_equal() +  # Equal scaling for both axes
  theme(
    axis.text.x = element_text(size = 25),  # Bigger x-axis labels
    axis.text.y = element_text(size = 25),  # Bigger y-axis labels
    axis.title.x = element_text(size = 30), # Bigger x-axis title
    axis.title.y = element_text(size = 30)  # Bigger y-axis title
  )





#Reg asset index----
  
hfc_regress <- hfc_constr %>% 
    mutate(
      asset_index = C3_1 + C3_2 + C3_3
      + C3_4 + C3_5 + C3_6 + C3_7 + C3_8
      + C3_9 + C3_10 +C3_11 + C3_12 + C3_13
    ) %>% 
    mutate(
      wtp_12 = J4_2*12,
      wtp_24 = J5_2*24
    ) %>% 
    mutate(across(c("J1_final", # wtp_fixed
                    "J2_1",     # wtp_fixed_appliance
                    "J3_1"     # wtp_fixed_low_reliability
                    ), ~ pmax(.x, 1000)))  %>% 
    mutate(across(c("J1_final", # wtp_fixed
                    "J2_1",     # wtp_fixed_appliance
                    "J3_1"     # wtp_fixed_low_reliability
                    ), ~pmin(.x, 100000))) %>% 
    
    rename(
      fixed_system = J1_final,
      appliance = J2_1,
      low_reliability = J3_1,
      lightbulb = J6_1
    ) %>%  mutate(
      lightbulb = pmax(lightbulb, 100)
    )
  

                          
  # Regression----
  
  log_fixed <- lm(log(lightbulb) ~ log(fixed_system) + asset_index, data = hfc_regress)
  
  summary(log_fixed)  
  
  
  log_reliability <- lm(log(lightbulb) ~ log(low_reliability) + asset_index, data = hfc_regress)
  
  summary(log_reliability)
  
  
  

  
  
  log_appliance <- lm(log(lightbulb) ~ log(appliance) + asset_index, data = hfc_regress)
  
  log_reg <- lm(log(lightbulb) ~ log(fixed_system) + log(low_reliability) + log(appliance) +asset_index, data = hfc_regress)
  
  
  
  hfc_regress <- hfc_regress %>%
    mutate(
      `log(appliance) - log(fixed_system)` = log(appliance) - log(fixed_system),
      `log(fixed_system) - log(low_reliability)` = log(fixed_system) - log(low_reliability)
    )
  
  
  log_diff <- lm(log(lightbulb) ~ log(low_reliability) + `log(appliance) - log(fixed_system)` + `log(fixed_system) - log(low_reliability)` + asset_index  , data = hfc_regress)
  
  log_diff
  
  stargazer(log_fixed,log_appliance, log_reliability,log_reg,log_diff,
            
            type = "latex", 
            title = "Regression Results with Asset Index", 
            out = "regression_output.latex")
  

 asset_index <- hfc_regress %>% 
    select(asset_index) %>% 
    group_by(asset_index) %>% 
    summarise(n = n()) %>% 
    arrange(n)
  
  # Create bar plot with continuous line for distribution
  ggplot(asset_index, aes(x = asset_index, y = n)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    geom_smooth(aes(x = asset_index, y = n), method = "loess", se = FALSE, color = "red") +
    labs(title = "Distribution of Asset Index", x = "Asset Index", y = "Frequency") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 14),  # Increase size of x and y axis labels
      axis.text = element_text(size = 12),   # Increase size of x and y axis tick labels
      plot.title = element_text(size = 16, hjust = 0.5),  # Increase plot title size and center it
      legend.title = element_text(size = 14),  # Increase legend title size (if applicable)
      legend.text = element_text(size = 12)    # Increase legend text size (if applicable)
    )
    
#Household size----
  
hfc_regress <- hfc_regress %>% 
    rename(household_size = A1_1)
  
  log_fixed <- lm(log(lightbulb) ~ log(fixed_system) + household_size, data = hfc_regress)
  
  log_reliability <- lm(log(lightbulb) ~ log(low_reliability) + household_size, data = hfc_regress)
  
  log_appliance <- lm(log(lightbulb) ~ log(appliance) + household_size, data = hfc_regress)
  
  log_reg <- lm(log(lightbulb) ~ log(fixed_system) + log(low_reliability) + log(appliance) +household_size, data = hfc_regress)
  
  
  
   hfc_regress <- hfc_regress %>%
    mutate(
      `log(appliance) - log(fixed_system)` = log(appliance) - log(fixed_system),
      `log(fixed_system) - log(low_reliability)` = log(fixed_system) - log(low_reliability)
    )
  
  
  log_diff <- lm(log(lightbulb) ~ log(low_reliability) + `log(appliance) - log(fixed_system)` + `log(fixed_system) - log(low_reliability)` + household_size  , data = hfc_regress)
  
  
  
  stargazer(log_fixed,log_appliance, log_reliability,log_reg,log_diff,
            
            type = "latex", 
            title = "Regression Results with Household Size", 
            out = "regression_output.latex")
  
  
  
  #Household size----
  
hfc_regress <- hfc_regress %>% 
    rename(household_size = A1_1)
  
  log_fixed <- lm(log(lightbulb) ~ log(fixed_system) + household_size, data = hfc_regress)
  
  log_reliability <- lm(log(lightbulb) ~ log(low_reliability) + household_size, data = hfc_regress)
  
  log_appliance <- lm(log(lightbulb) ~ log(appliance) + household_size, data = hfc_regress)
  
  log_reg <- lm(log(lightbulb) ~ log(fixed_system) + log(low_reliability) + log(appliance) +household_size, data = hfc_regress)
  
  
  
   hfc_regress <- hfc_regress %>%
    mutate(
      `log(appliance) - log(fixed_system)` = log(appliance) - log(fixed_system),
      `log(fixed_system) - log(low_reliability)` = log(fixed_system) - log(low_reliability)
    )
  
  
  log_diff <- lm(log(lightbulb) ~ log(low_reliability) + `log(appliance) - log(fixed_system)` + `log(fixed_system) - log(low_reliability)` + household_size  , data = hfc_regress)
  
  
  
  stargazer(log_fixed,log_appliance, log_reliability,log_reg,log_diff,
            
            type = "latex", 
            title = "Regression Results with Household Size", 
            out = "regression_output.latex")
  

  
  
#Household income----
  
  hfc_regress <- hfc_regress %>% 
    rename(
      primary_week = A2_7,
      primary_day = A2_8,
      primary_hour = A2_9,
      secondary_week = A3_8,
      secondary_day = A3_9,
      secondary_hour = A3_10
    )
  
  hfc_regress <- hfc_regress %>%
    mutate(
      A2_4_week = case_when(
        A2_5_label == "Hour" ~ A2_4 * primary_hour * primary_day,
        A2_5_label == "Day"  ~ A2_4 * primary_week,
        A2_5_label == "Week" ~ A2_4 ,
        A2_5_label == "2 Weeks" ~ round(A2_4/2,2),
        A2_5_label == "Month" ~ round(A2_4/4,2),
        A2_5_label == "Quarter" ~ round(A2_4/(3*4),2),
        A2_5_label == "Agricultural season" ~ round(A2_4/(4*4),2),
        A2_5_label == "Half Year" ~ round(A2_4/(6*4),2),
        A2_5_label == "Year" ~ round(A2_4/(12*4),2),
        TRUE ~ A2_4
      )
    ) %>% 
    mutate(
      A2_4_week = ifelse(
        A2_4_week > quantile(A2_4_week, 0.99, na.rm = TRUE),
        quantile(A2_4_week, 0.99, na.rm = TRUE), A2_4_week
      ) 
    ) %>% 
    rename(
      weekly_income = A2_4_week
    ) 
  
  hfc_regress<- hfc_regress%>% 
    mutate(
      weekly_income = ifelse(is.na(weekly_income), 0, weekly_income)
    )
  
  
  log_fixed <- lm(log(lightbulb) ~ log(fixed_system) + weekly_income, data = hfc_regress)
  
  summary(log_fixed)
  log_reliability <- lm(log(lightbulb) ~ log(low_reliability) + weekly_income, data = hfc_regress)
  summary(log_reliability)
  log_appliance <- lm(log(lightbulb) ~ log(appliance) + weekly_income, data = hfc_regress)
  summary(log_appliance)
  log_reg <- lm(log(lightbulb) ~ log(fixed_system) + log(low_reliability) + log(appliance) + weekly_income, data = hfc_regress)
  summary(log_reg)
  
  log_diff <- lm(log(lightbulb) ~ log(low_reliability) + `log(appliance) - log(fixed_system)` + `log(fixed_system) - log(low_reliability)` + weekly_income , data = hfc_regress)
  
  
  
  stargazer(log_fixed,log_appliance, log_reliability,log_reg,log_diff,
            
            type = "latex", 
            title = "Regression Results with Household Head Weekly Income", 
            out = "regression_output.latex")
  
  
#Ratio with the wtp 12 to 24----
  

  log_fixed12 <- lm(log(fixed_system) ~ log(wtp_12) , data = hfc_regress)
  summary(log_fixed12)
  log_fixed24<- lm(log(fixed_system) ~ log(wtp_24) , data = hfc_regress)
  summary(log_fixed24)
  
  stargazer(log_fixed12, log_fixed24,
            
            type = "latex", 
            title = "Regression Results credit 12 and 24 month", 
            out = "regression_output.latex"
  )
  
  
  
 ##12 month----
  
  
  log_reliability <- lm(log(lightbulb) ~ log(low_reliability) + log(wtp_12), data = hfc_regress)
  summary(log_reliability)
  
  log_appliance <- lm(log(lightbulb) ~ log(appliance) + log(wtp_12), data = hfc_regress)
  summary(log_appliance)
  
  log_reg <- lm(log(lightbulb) ~ log(fixed_system) + log(low_reliability) + log(appliance) + log(wtp_12), data = hfc_regress)
  summary(log_reg)
  
  log_diff <- lm(log(lightbulb) ~ log(low_reliability) + `log(appliance) - log(fixed_system)` + `log(fixed_system) - log(low_reliability)` + log(wtp_12) , data = hfc_regress)
  
  
  
  stargazer(log_fixed,log_appliance, log_reliability,log_reg,log_diff,
            
            type = "latex", 
            title = "Regression Results with WTP 12 month", 
            out = "regression_output.latex"
            )
  
  
#24 month----

  
  
  log_fixed <- lm(log(lightbulb) ~ log(fixed_system) + wtp_ratio24, data = hfc_regress)
  summary(log_fixed)
  
  log_reliability <- lm(log(lightbulb) ~ log(low_reliability) + wtp_ratio24, data = hfc_regress)
  summary(log_reliability)
  
  log_appliance <- lm(log(lightbulb) ~ log(appliance) + wtp_ratio24, data = hfc_regress)
  summary(log_appliance)
  
  log_reg <- lm(log(lightbulb) ~ log(fixed_system) + log(low_reliability) + log(appliance) + wtp_ratio24, data = hfc_regress)
  summary(log_reg)
  
  log_diff <- lm(log(lightbulb) ~ log(low_reliability) + `log(appliance) - log(fixed_system)` + `log(fixed_system) - log(low_reliability)` + wtp_ratio24 , data = hfc_regress)
  
  
  
  stargazer(log_fixed,log_appliance, log_reliability,log_reg,log_diff,
            
            type = "latex", 
            title = "Regression Results with WTP fixed to WTP 24 month ratio", 
            out = "regression_output.latex"
  )
  
  
#All control at once------

  
  
  hfc_regress <- hfc_regress %>%
    mutate(
      log_head_weekly_income = log(weekly_income / 1000 + 1),  
      `log(wtp_24)-log(fixed_system)` = log(wtp_24) - log(fixed_system) ,
      `log(wtp_12)-log(fixed_system)` = log(wtp_12) - log(fixed_system) ,
    )
  

  full_model12 <- lm(log(lightbulb) ~log(low_reliability) 
                   +  `log(appliance) - log(fixed_system)` + `log(fixed_system) - log(low_reliability)` +
                     `log(wtp_12)-log(fixed_system)` + log_head_weekly_income + asset_index + household_size, data = hfc_regress)
  
  summary(full_model12)
  stargazer(full_model12,
            
            type = "latex", 
            title = "Regression Results with control", 
            out = "regression_output.latex"
  )
  

  full_model12_fe <- felm(log(lightbulb) ~log(low_reliability) 
                     +  `log(appliance) - log(fixed_system)` + `log(fixed_system) - log(low_reliability)` +
                       `log(wtp_12)-log(fixed_system)` + log_head_weekly_income + asset_index + household_size, data = hfc_regress )
  
  summary(full_model12)
  stargazer(full_model12,
            
            type = "latex", 
            title = "Regression Results with control", 
            out = "regression_output.latex"
  )
  

  
  full_model24 <- lm(log(lightbulb) ~log(low_reliability) 
                     +  `log(appliance) - log(fixed_system)` + `log(fixed_system) - log(low_reliability)` +
                       `log(wtp_24)-log(fixed_system)` + log_head_weekly_income + asset_index + household_size, data = hfc_regress)
  
  summary(full_model24)
  stargazer(full_model24,
            
            type = "latex", 
            title = "Regression Results with control", 
            out = "regression_output.latex"
  )
  
  
  
#Four separate &Control Village fe----
  
reg_low_reliability <- felm(log(low_reliability) ~ log_head_weekly_income + asset_index + household_size|village, data = hfc_regress)
summary(reg_low_reliability)
reg_fixed <- felm(log(fixed_system) ~ log_head_weekly_income + asset_index + household_size|village, data = hfc_regress)
summary(reg_fixed)
reg_appliance_fix <- felm(`log(appliance) - log(fixed_system)`~ log_head_weekly_income + asset_index + household_size|village, data = hfc_regress)
summary(reg_appliance_fix)
reg_fix_low_reliability <- felm(`log(fixed_system) - log(low_reliability)` ~ log_head_weekly_income + asset_index + household_size|village, data = hfc_regress)
summary(reg_fix_low_reliability)
reg_12 <- felm(`log(wtp_12)-log(fixed_system)` ~ log_head_weekly_income + asset_index + household_size|village, data = hfc_regress)
summary(reg_12)

reg_lightbulb <- felm(log(lightbulb) ~ log_head_weekly_income + asset_index + household_size|village, data = hfc_regress)
summary(reg_lightbulb)

regs <- list(
  "low_reliability" = reg_low_reliability,
  "fixed_system" = reg_fixed,
  "appliance_fix" = reg_appliance_fix,
  "fix_reliability" = reg_fix_low_reliability,
  "reg_12" = reg_12,
  "lightbulb" = reg_lightbulb
)
stargazer( 
  regs,
  type = "latex", 
  title = "Regression Results", 
  out = "regression_output.latex"
)




reg_fixed <- lm(fixed_system ~ log_head_weekly_income + asset_index + household_size, data = hfc_regress)
reg_appliance <- lm(appliance ~ log_head_weekly_income + asset_index + household_size, data = hfc_regress)
reg_low_reliability <- lm(low_reliability ~ log_head_weekly_income + asset_index + household_size, data = hfc_regress)




reg_lightbulb <- lm(log(lightbulb) ~ log_head_weekly_income + asset_index + household_size, data = hfc_regress)
summary(reg_lightbulb)


#Distance to the household and the surveyed poles

baseline_data <- file.path(dropbox,  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/data")

hfc_sf <- st_as_sf(hfc_regress, coords = c("coordinate.Longitude", "coordinate.Latitude"), crs = st_crs(karongi_lv))

##Karongi----
karongi_lv <- st_read(dsn = file.path(baseline_data, "Karongi Surveyed 0116", "Surveyed_LV_Lines.shp"))

hfc_karongi <- hfc_sf %>% 
  filter(district_key == "Karongi")

karongi_lv <- st_transform(karongi_lv, crs = 4326)
karongi_villages <- st_transform(karongi_villages, crs = 4326)

ggplot(data = karongi_villages) +
  geom_sf(fill = NA, color = "lightgrey") +  
  geom_sf(data = karongi_lv, color = "blue", size = 0.5) + 
  geom_sf(data = hfc_karongi, color = "red", size = 0.1) +
  labs(title = "Electrification Network in Karongi") +
  theme_minimal()



ggplot(data = karongi_villages) +
  geom_sf(fill = NA, color = "black") +  
  # geom_sf(data = karongi_lv, color = "blue", size = 0.5) + 
  geom_sf(data = hfc_karongi, color = "red", size = 0.5) +
  # labs(title = "Electrification Network in Karongi") +
  theme_void()




##Rulindo----

rulindo_lv <- st_read(dsn = file.path(baseline_data, "Rulindo Surveyed 0116", "Surveyed_LV_Lines.shp"))

hfc_rulindo <- hfc_sf %>% 
  filter(district_key == "Rulindo")

rulindo_lv <- st_transform(rulindo_lv, crs = 4326)
rulindo_villages <- st_transform(rulindo_villages, crs = 4326)
hfc_rulindo <- st_transform(hfc_rulindo, crs = st_crs(rulindo_villages))
hfc_rulindo <- st_intersection(rulindo_villages, hfc_rulindo)

ggplot(data = rulindo_villages) +
  geom_sf(fill = NA, color = "lightgrey") +  
  geom_sf(data = rulindo_lv, color = "blue", size = 0.5) + 
  geom_sf(data = hfc_rulindo, color = "red", size = 0.1) +
  labs(title = "Electrification Network in Rulindo") +
  theme_minimal()



ggplot(data = rulindo_villages) +
  geom_sf(fill = NA, color = "black") +  
  # geom_sf(data = rulindo_lv, color = "blue", size = 0.5) + 
  geom_sf(data = hfc_rulindo, color = "red", size = 0.5) +
  # labs(title = "Electrification Network in Rulindo") +
  theme_void()



##Rutsiro----
rutsiro_lv <- st_read(dsn = file.path(baseline_data, "Rutsiro Surveyed 0116", "Surveyed_LV_Lines.shp"))

hfc_rutsiro <- hfc_sf %>% 
  filter(district_key == "Rutsiro")


rutsiro_villages <- rwa_villages %>% 
  filter(District == "Rutsiro")
plot(rutsiro_villages)

rutsiro_lv <- st_transform(rutsiro_lv, crs = 4326)
rutsiro_villages <- st_transform(rutsiro_villages, crs = 4326)

rutsiro_villages <- st_zm(rutsiro_villages)

ggplot() +
  geom_sf(data = rutsiro_villages, fill = NA, color = "lightgrey", size = 0.3) +  
  geom_sf(data = rutsiro_lv, color = "blue", size = 0.5) +  
  geom_sf(data = hfc_rutsiro, color = "red", size = 0.1) +  
  labs(title = "Electrification Network in Rutsiro") +
  theme_minimal()

ggplot() +
  geom_sf(data = rutsiro_villages, fill = NA, color = "black", size = 0.3) +  
  # geom_sf(data = rutsiro_lv, color = "blue", size = 0.5) +  
  geom_sf(data = hfc_rutsiro, color = "red", size = 0.5) +  
  # labs(title = "Electrification Network in Rutsiro") +
  theme_void()


#Rusizi----


hfc_rusizi <- hfc_sf %>% 
  filter(district_key == "Rusizi")


rusizi_villages <- rwa_villages %>% 
  filter(District == "Rusizi")
plot(rusizi_villages)


rusizi_villages <- st_transform(rusizi_villages, crs = 4326)

rusizi_villages <- st_zm(rusizi_villages)

hfc_rusizi <- st_intersection(hfc_rusizi, rusizi_villages)

ggplot() +
  geom_sf(data = rusizi_villages, fill = NA, color = "black", size = 0.3) +  
  # geom_sf(data = rusizi_lv, color = "blue", size = 0.5) +  
  geom_sf(data = hfc_rusizi, color = "red", size = 0.5) +  
  # labs(title = "Electrification Network in rusizi") +
  theme_void()





#LV line----
karongi_lv <- karongi_lv %>% rename(length = SHAPE_Leng)
rutsiro_lv <- rutsiro_lv %>% select(length, geometry)

lv_line <- rbind(karongi_lv, rulindo_lv, rutsiro_lv)

lv_line <-st_zm(lv_line)

dist_matrix <- st_distance(hfc_sf, lv_line)

dist_matrix <- as.data.frame(dist_matrix)


dist_matrix$min_meter <- apply(dist_matrix, 1, min, na.rm = TRUE)

dist <- dist_matrix %>% 
  select(min_meter) %>% 
  mutate(min_meter = round(min_meter,2))



hfc_sf_regress <- hfc_sf %>%
  mutate(distance_to_lv = dist) %>% 
  st_drop_geometry() %>% 
  mutate(
    distance_to_lv = as.numeric(unlist(distance_to_lv)),
    distance_to_lv = distance_to_lv / 1000  # Convert meters to kilometers
  )

#felm village-----

# Regression 1: Low Reliability
reg_low_reliability <- felm(
  log(low_reliability) ~ log_head_weekly_income + asset_index + household_size + distance_to_lv | village,
  data = hfc_sf_regress
)
summary(reg_low_reliability)

# Regression 1: Low Reliability
reg_fixed_system <- felm(
  log(fixed_system) ~ log_head_weekly_income + asset_index + household_size + distance_to_lv | village,
  data = hfc_sf_regress
)
summary(reg_fixed_system)

# Regression 2: Appliance Fix (including village FE)
reg_appliance_fix <- felm(
  `log(appliance) - log(fixed_system)` ~ log_head_weekly_income + asset_index + household_size + distance_to_lv | village,
  data = hfc_sf_regress
)
summary(reg_appliance_fix)

# Regression 3: Fixed System vs Low Reliability (including village FE)
reg_fix_low_reliability <- felm(
  `log(fixed_system) - log(low_reliability)` ~ log_head_weekly_income + asset_index + household_size + distance_to_lv | village,
  data = hfc_sf_regress
)
summary(reg_fix_low_reliability)

# Regression 4: WTP 12 vs Fixed System (including village FE)
reg_12 <- felm(
  `log(wtp_12)-log(fixed_system)` ~ log_head_weekly_income + asset_index + household_size + distance_to_lv | village,
  data = hfc_sf_regress
)
summary(reg_12)

# Regression 5: Lightbulb (including village FE)
reg_lightbulb <- felm(
  log(lightbulb) ~ log_head_weekly_income + asset_index + household_size + distance_to_lv | village,
  data = hfc_sf_regress
)
summary(reg_lightbulb)

regs <- list(
  "low_reliability" = reg_low_reliability,
  "fixed_system" = reg_fixed_system,
  "appliance_fix" = reg_appliance_fix,
  "fix_reliability" = reg_fix_low_reliability,
  "reg_12" = reg_12,
  "lightbulb" = reg_lightbulb
)
stargazer(
  regs,
  type = "latex",
  title = "Regression Results",
  out = "regression_output.latex"
)



#With all the other ones

distance_hhsize <- felm(
  distance_to_lv ~  household_size  | village,
  data = hfc_sf_regress
)

summary(distance_hhsize)

distance_asset <- felm(
  distance_to_lv ~ asset_index  | village,
  data = hfc_sf_regress
)

summary(distance_asset)

distance_income <- felm(
  distance_to_lv ~ log_head_weekly_income  | village,
  data = hfc_sf_regress
)

summary(distance_income)



distance_all <- felm(
  distance_to_lv ~ log_head_weekly_income + asset_index + household_size  | village,
  data = hfc_sf_regress
)

summary(distance_all)

regs <- list(
  "household size" = distance_hhsize,
  "household asset" = distance_asset,
  "household income" = distance_income,
  "combined" = distance_all
  )


stargazer(
  regs,
  type = "latex",
  title = "Regression Results",
  out = "regression_output.latex"
)



##Four separate and control----


reg_low_reliability <- felm(log(low_reliability) ~ log_head_weekly_income + asset_index + household_size|village, data = hfc_regress)
summary(reg_low_reliability)
reg_fixed <- felm(log(fixed_system) ~ log_head_weekly_income + asset_index + household_size|village, data = hfc_regress)
summary(reg_fixed)
reg_appliance_fix <- felm(`log(appliance) - log(fixed_system)`~ log_head_weekly_income + asset_index + household_size|village, data = hfc_regress)
summary(reg_appliance_fix)
reg_fix_low_reliability <- felm(`log(fixed_system) - log(low_reliability)` ~ log_head_weekly_income + asset_index + household_size|village, data = hfc_regress)
summary(reg_fix_low_reliability)
reg_12 <- felm(`log(wtp_12)-log(fixed_system)` ~ log_head_weekly_income + asset_index + household_size|village, data = hfc_regress)
summary(reg_12)

reg_lightbulb <- felm(log(lightbulb) ~ log_head_weekly_income + asset_index + household_size|village, data = hfc_regress)
summary(reg_lightbulb)

regs <- list(
  "low_reliability" = reg_low_reliability,
  "fixed_system" = reg_fixed,
  "appliance_fix" = reg_appliance_fix,
  "fix_reliability" = reg_fix_low_reliability,
  "reg_12" = reg_12,
  "lightbulb" = reg_lightbulb
)
stargazer( 
  regs,
  type = "latex", 
  title = "Regression Results", 
  out = "regression_output.latex"
)






log_fixed <- felm(log(lightbulb) ~ log(fixed_system) +  log_head_weekly_income + asset_index + household_size|village, data = hfc_regress)

summary(log_fixed)  


log_reliability <- felm(log(lightbulb) ~ log(low_reliability)  +  log_head_weekly_income + asset_index + household_size|village, data = hfc_regress)

summary(log_reliability)



write_xlsx(hfc_regress, path = file.path(data_path, "hfc_regress.xlsx"))
