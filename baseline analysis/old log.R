
log_fixed <- lm(log(lightbulb) ~ log(fixed_system), data = wtp_selected)

summary(log_fixed)


log_reliability <- lm(log(lightbulb) ~ log(low_reliability), data = wtp_selected)

summary(log_reliability)

log_appliance <- lm(log(lightbulb) ~ log(appliance), data = wtp_selected)

summary(log_appliance)



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






# Regression----

log_fixed <- lm(log(lightbulb) ~ log(fixed_system) + asset_index, data = hfc_regress)

summary(log_fixed)  


log_reliability <- lm(log(lightbulb) ~ log(low_reliability) + asset_index, data = hfc_regress)

summary(log_reliability)






log_appliance <- lm(log(lightbulb) ~ log(appliance) + asset_index, data = hfc_regress)

log_reg <- lm(log(lightbulb) ~ log(fixed_system) + log(low_reliability) + log(appliance) +asset_index, data = hfc_regress)




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





#Household income----


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



stargazer(
  log_fixed,
  log_appliance, 
  log_reliability,
  log_reg,
  log_diff,
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
summary(log_diff)


stargazer(
  log_fixed,
  log_appliance, 
  log_reliability,
  log_reg,
  log_diff,
  type = "latex", 
  title = "Regression Results with WTP fixed to WTP 24 month ratio", 
  out = "regression_output.latex"
)


#All control at once------





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



reg_fixed <- lm(fixed_system ~ log_head_weekly_income + asset_index + household_size, data = hfc_regress)
reg_appliance <- lm(appliance ~ log_head_weekly_income + asset_index + household_size, data = hfc_regress)
reg_low_reliability <- lm(low_reliability ~ log_head_weekly_income + asset_index + household_size, data = hfc_regress)




reg_lightbulb <- lm(log(lightbulb) ~ log_head_weekly_income + asset_index + household_size, data = hfc_regress)
summary(reg_lightbulb)
