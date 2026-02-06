
##############################################
#Author: Xiaoming Zhang
#Purpose: Plots and graphs for BBL Nov 2025
#Date: November 5th 2025
#####################################################################



# Primary light plot------
primary_light_energy <- hfc_constr %>% 
  group_by(H7_1_label) %>% 
  summarise(
    n = n(),
    .groups = "drop"
  ) %>% 
  arrange(desc(n)) %>% 
  rename(primary_lighting_energy = H7_1_label) %>% 
  mutate(
    percentage = round((n / sum(n)) * 100, 2)  # 2 decimal places
  )


library(dplyr)
library(ggplot2)








# Create grouped dataset
plot_df <- primary_light_energy %>%
  mutate(
    category = case_when(
      primary_lighting_energy == "Dry cell batteries/Electric Torch" ~ "Torch",
      primary_lighting_energy == "Biomass(Charcoal/Wood）" ~ "Biomass",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(category) %>%
  summarise(
    n = sum(n),
    percentage = sum(percentage)/100,
    .groups = "drop"
  ) %>%
  arrange(desc(n))




library(scales)  # for label_percent

primary_lighting_plot <- ggplot(plot_df, aes(x = reorder(category, -percentage), y = percentage)) +
  geom_col(fill = "grey70", color = "black",width = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", percentage * 100)),
            vjust = -0.3) +
  labs(
    title = "",
    x = "% of Households Using Primary Lighting Source",
    y = "Expenditures (RwF/month)"
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.6, 0.2),
    limits = c(0, 0.65),
    labels = label_percent(accuracy = 1)  # shows 0%, 20%, etc.
  ) +
  # theme_minimal(base_size = 12) +
  theme(
    plot.title = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    # plot.caption = element_text(size = 10, color = "gray30", hjust = 0),
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(t = 30, r = 10, b = 10, l = 10)  # add space on top (t)
    
  )

primary_lighting_plot

ggsave(
  file.path(output_path, "figures", "primary_lighting_bar.png"),   # file name
  plot = primary_lighting_plot,                      # which plot to save
  width = 8,                                         # width in inches
  height = 8,                                        # height in inches
  dpi = 300,                                         # high quality
  scale = 0.5                                   # scale down by 60%
)


#Mobile Pay------


#Mobile roster----

mobile_own <- hfc_constr %>%
  mutate(
    own_mobile = ifelse(F1_1 > 0, 1, 0)
  )  


mobile <- hfc_constr %>% 
  filter(F1_1 != 0) %>% 
  select(hh_id, starts_with("F")) %>% 
  pivot_longer(
    cols = starts_with("F"),  # Pivot all member columns
    names_to = c(".value", "mobile_id"),  # Extract the 'member' part and assign the 'member_id'
    names_pattern = "(.*)_(\\d+)",  # Match the pattern of variable name and member number
    values_drop_na = TRUE  # Drop NAs if there are missing values for some members
  ) %>% 
  mutate(F2_4 = ifelse(is.na(F2_4), 0, F2_4))

mobile_plot <- mobile %>%
  filter(F2_4 > 0) %>%                # keep households with positive phone-charging spending
  group_by(hh_id) %>%                 # group by household ID
  summarise(
    mobile_charge_rwf = sum(F2_4, na.rm = TRUE),     # total monthly phone charging cost (RWF)
    airtime_data_rwf = sum(F1_7 + F1_9, na.rm = TRUE) # combined airtime + data spending
  ) %>%
  ungroup()


# Prepare data for plotting
plot_df <- mobile_plot %>%
  summarise(
    `Phone charging` = mean(mobile_charge_rwf, na.rm = TRUE),
    `Airtime & data` = mean(airtime_data_rwf, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(),
               names_to = "category",
               values_to = "mean_rwf")

# Plot



mobile_charge_plot <- ggplot(plot_df, aes(x = category, y = mean_rwf)) +
  geom_col(fill = "grey70", color = "black", width = 0.8) +
  geom_text(aes(label = paste0(round(mean_rwf, 0), " RwF")),
            vjust = -0.3) +
  labs(
    x = "",
    y = "RwF per month",
    caption = ""
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # ← adds 10% space on top
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_blank(),
    axis.text = element_text(color = "black"),
    # axis.text.x = element_text(size = 14, color = "black"),  # ← increased x-axis label size
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    # plot.caption = element_text(size = 10, color = "gray30", hjust = 0),
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(t = 30, r = 10, b = 10, l = 10)  # add space on top (t)
  )

mobile_charge_plot

ggsave(
  file.path(output_path, "figures", "mobile_charge_bar(restricted to $charging).png"),   # file name
  plot = mobile_charge_plot,                      # which plot to save
  width = 8,                                         # width in inches
  height = 8,                                        # height in inches
  dpi = 300,                                         # high quality
  scale = 0.5                                  # scale down by 60%
)



#WTP log output---------


hfc_plot<- hfc_constr.1 %>% 
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
  rename(
    fixed_system = J1_final,
    appliance = J2_1,
    low_reliability = J3_1,
    lightbulb = J6_1
  ) %>%  mutate(
    lightbulb = pmax(lightbulb, 100)
  ) %>%   
  mutate(
    lightbulb_win = ifelse(lightbulb >= 2000, 2000, lightbulb),
    appliance_win = ifelse(appliance >= 125000, 125000, appliance),
    wtp_12_win = ifelse(wtp_12 >= 125000, 125000, wtp_12)
  ) 


wtp_regress <- hfc_plot %>% 
  mutate(hh = row_number()) %>% 
  select(hh, appliance, low_reliability, lightbulb, village) %>% 
  pivot_longer(
    cols = c("appliance", "low_reliability"),
    values_to = "wtp",
    names_to = "grid"
  ) %>% 
  mutate(
    grid  = ifelse(grid == "appliance", 1, 0)  ) %>% 
  rename(
    `energy demand` = lightbulb
  ) 


wtp_nov <- lm(log(wtp) ~ grid +  (1-grid)*log(`energy demand`) + grid * log(`energy demand`), data = wtp_regress)
summary(wtp_nov)

options(scipen = 999)  # turn off scientific notation globally


# Recode grid factor with levels in desired order: Grid (1) on top, SHS (0) below

wtp_regress <- wtp_regress %>%
  mutate(
    log_wtp = log(wtp),
    log_demand = log(`energy demand`),
    grid_factor = factor(grid, levels = c(1, 0))  # Grid first
  )

wtp_regress <- wtp_regress %>%
  mutate(
    log_demand = log(`energy demand`),
    log_wtp = log(wtp),
    grid_factor = factor(grid, levels = c(1, 0))  # Grid first
    
  )

# # Plot
# energy_demand_plot <- ggplot(wtp_regress, aes(
#   x = log_demand,
#   y = log_wtp,
#   color = grid_factor
# )) +
#   geom_point(alpha = 0.6, size = 2) +
#   geom_smooth(method = "lm", se = TRUE) +
#   scale_x_continuous(
#     name = "Energy Demand (RwF/month)",
#     breaks = c(log(100), log(1000), log(10000), log(100000)),
#     labels = c(100, 1000, 10000, 10000)
#   ) +
#   scale_y_continuous(
#     name = "Willingness to Pay (RwF)",
#     breaks = c(log(100), log(1000), log(10000), log(100000)),
#     labels = c(100, 1000, 10000, 10000)
#   ) +
#   scale_color_manual(
#     values = c("1" = "darkorange", "0" = "steelblue"),
#     labels = c("Grid", "SHS")
#   ) +
#   guides(color = guide_legend(title = NULL)) +  # remove legend title
#   theme_minimal(base_size = 13) +
#   theme(
#     plot.title = element_text(face = "bold", hjust = 0.5),
#     axis.text = element_text(color = "black"),
#     panel.background = element_rect(fill = "white", color = NA),
#     plot.background = element_rect(fill = "white", color = NA)
#   )
# 
# # Show the plot
# energy_demand_plot
# 
# ggsave(
#   file.path(output_path, "figures", "energy demand(scatter).png"),   # file name
#   plot = energy_demand_plot,                      # which plot to save
#   width = 8,                                         # width in inches
#   height = 6,                                        # height in inches
#   dpi = 300,                                         # high quality
#   scale = 0.7                                      # scale down by 60%
# )





library(ggExtra)
library(patchwork)


# 1. Prep the data
wtp_regress <- wtp_regress %>%
  mutate(
    log_demand = log(`energy demand`),
    log_wtp = log(wtp),
    grid_factor = factor(grid, levels = c(1, 0), labels = c("Grid", "SHS"))
  )

# Main scatter plot
main_plot <- ggplot(wtp_regress, aes(x = log_demand, y = log_wtp, color = grid_factor)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("Grid" = "darkorange", "SHS" = "steelblue")) +
  scale_x_continuous(
    name = "Energy Demand (RwF/month)",
    breaks = log(c(100, 1000, 10000, 100000)),
    labels = c(100, 1000, 10000, 100000)
  ) +
  scale_y_continuous(
    name = "Willingness to Pay (RwF)",
    breaks = log(c(100, 1000, 10000, 100000)),
    labels = c(100, 1000, 10000, 100000)
  ) +
  guides(color = guide_legend(title = NULL)) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.8, 0.4)
    # panel.background = element_rect(fill = "white"),
    # axis.line = element_line(color = "black")
  )

# Top marginal (black x-axis density)
top_density <- ggplot(wtp_regress, aes(x = log_demand)) +
  geom_density(color = "black", fill = NA, size = 0.7) +
  theme_void() 
# theme(plot.margin = margin(0, 5, 0, 35))

# Right marginal (colored y-axis density, no legend)
right_density <- ggplot(wtp_regress, aes(x = log_wtp, color = grid_factor)) +
  geom_density(size = 0.7, fill = NA) +
  scale_color_manual(values = c("Grid" = "darkorange", "SHS" = "steelblue")) +
  coord_flip() +
  theme_void() +
  theme(
    legend.position = "none"
    # plot.margin = margin(5, 0, 30, 0)
  )

# Combine with patchwork
energy_demand_plot_marginal <- (
  top_density + plot_spacer() +
    main_plot + right_density
) +
  plot_layout(
    ncol = 2,
    nrow = 2,
    widths = c(4, 1),
    heights = c(1, 4)
  )

# Show final plot
energy_demand_plot_marginal
ggsave(
  file.path(output_path, "figures", "energy demand(marginal).png"),   # file name
  plot = energy_demand_plot_marginal,                      # which plot to save
  width = 8,                                         # width in inches
  height = 6,                                        # height in inches
  dpi = 300,                                         # high quality
  scale = 0.7                                      # scale down by 60%
)


