##############
#Author: Xiaoming Zhang
#Date: 2.25.2025
#Purpose: Baseline analysis presentaiton
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
  "Rwanda Energy/EAQIP/datawork/baseline analysis/output"
)

data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/baseline analysis/data"
)

hfc_constr <- read_xlsx(file.path(hfc_data_path, "hfc_constr.xlsx"))

hfc_plot <- hfc_constr %>% 
  filter(!is.na(A1_1))  %>% 
  filter(finish == 1) %>% 
  distinct(hh_head_name, hh_id, A1_2, A1_3, .keep_all = TRUE)


hfc_plot<- hfc_plot %>% 
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
  # mutate(across(c("J1_final", # wtp_fixed
  #                 "J2_1",     # wtp_fixed_appliance
  #                 "J3_1"     # wtp_fixed_low_reliability
  # ), ~pmin(.x, 100000))) %>% 
  
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






#Appliances----

appliance <- ggplot(hfc_plot, aes(x = appliance_win)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
                 binwidth = 2500,       # Each bin covers 2,500
                 boundary = 0,         # Ensures the first bin starts at 0
                 fill = "lightblue",
                 color = "black",
                 alpha = 0.7) +
  geom_vline(xintercept = 96000, 
             color = "blue", 
             linetype = "dashed", 
             size = 1.5) +
  labs(
    x = "Grid WTP (RwF)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 28, face = "bold"),
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    axis.text.x = element_text(size = 24),
    axis.text.y = element_text(size = 24)
  )

appliance

ggsave(file.path(output_path, "lightning seminar plot", "plot1_appliance.jpeg"), 
       plot = appliance, 
       device = "jpeg", 
       width = 16, 
       height = 8, 
       dpi = 300,
       scale = 0.5)




#wtp_12----


wtp_12 <- ggplot(hfc_plot, aes(x = wtp_12_win)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
                 binwidth = 2500,       # Each bin covers 2,500
                 boundary = 0,         # Ensures the first bin starts at 0
                 fill = "lightblue",
                 color = "black",
                 alpha = 0.7) +
  geom_vline(xintercept = 96000, 
             color = "blue", 
             linetype = "dashed", 
             size = 1.5) +
  labs(
    x = "Grid WTP over 12 month (RwF)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 28, face = "bold"),
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    axis.text.x = element_text(size = 24),
    axis.text.y = element_text(size = 24)
  )

wtp_12

ggsave(file.path(output_path, "lightning seminar plot", "plot2_wtp_12.jpeg"), 
       plot = wtp_12, 
       device = "jpeg", 
       width = 16, 
       height = 8, 
       dpi = 300,
       scale = 0.5)



#lightbulb----


lightbulb <- ggplot(hfc_plot, aes(x = lightbulb_win)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
                 binwidth = 100,       # Each bin covers 2,500
                 boundary = 0,         # Ensures the first bin starts at 0
                 fill = "lightblue",
                 color = "black",
                 alpha = 0.7) +
  geom_vline(xintercept = 300, 
             color = "blue", 
             linetype = "dashed", 
             size = 1.5) +
  labs(
    x = "Stated Lighting Demand \n (RwF/month of elec.)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 28, face = "bold"),
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    axis.text.x = element_text(size = 24),
    axis.text.y = element_text(size = 24)
  )

lightbulb

ggsave(file.path(output_path, "lightning seminar plot", "plot3_lightbulb.jpeg"), 
       plot = lightbulb, 
       device = "jpeg", 
       width = 16, 
       height = 8, 
       dpi = 300,
       scale = 0.5)


#Regression analysis----

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
  ) %>% 
  mutate(
    `energy demand/100` = `energy demand` /100
  )



wtp_nov <- lm(log(wtp) ~ grid + log(`energy demand/100`) + grid * log(`energy demand/100`), data = wtp_regress)

summary(wtp_nov)
wtp_v <- felm(log(wtp) ~ grid + log(`energy demand/100`) + grid * log(`energy demand/100`)|village, data = wtp_regress)

summary(wtp_v)

stargazer(wtp_nov, wtp_v,
          type = "latex",
          title = "Regression Results",
          column.labels = c("OLS", "Fixed Effects (Village)"),
          dep.var.labels = "log(wtp)",
          covariate.labels = c("grid", "log(energy demand)", "grid:log(energy demand)"),
          out = file.path(output_path, "lightning seminar tex", "regression_results.tex"))

regs <- list(
  "willingness to pay\n (without village FE)" = wtp_nov,
  "willingness to pay\n (with village FE)" = wtp_v
)

stargazer( 
  regs,
  type = "latex", 
  title = "Regression Results", 
  out = "regression_output.latex"
)

#Bar plot----

#general well being----



# Pivot the four variables of interest into long format
well_long <- hfc_plot %>% 
  select(B5_1, B5_5, B5_6, B5_8) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "var",
    values_to = "wellbeing"
  ) %>% 
  mutate(
    var = case_when(
      var == "B5_1" ~ "Satisfaction with current economic status",
      var == "B5_5" ~ "Satisfaction with energy use in phone charging",
      var == "B5_6" ~ "Satisfaction with energy use in lighting",
      var == "B5_8" ~ "Satisfaction with current energy status"
    )
  ) %>% 
  filter(!is.na(wellbeing))

# Calculate the mean for each variable
mean_table <- well_long %>% 
  group_by(var) %>% 
  summarise(mean_val = mean(wellbeing, na.rm = TRUE))




well_being <- ggplot(well_long, aes(x = wellbeing, , y = ..count.. / sum(..count..))) +
  geom_bar(fill = "lightblue", color = "black", binwidth = 1) +
  facet_wrap(~var, scales = "free_x") +
  geom_vline(data = mean_table, aes(xintercept = mean_val),
             color = "red", linetype = "dashed", size = 1) +
  geom_text(data = mean_table, aes(x = mean_val, y = 650, label = round(mean_val, 2)),
            color = "red", hjust = -0.01, size = 5) +
  labs(
    x = "Household's satisfaction on Cantril's ladder  (1-10)",
    y = "Count",
    # title = "Distribution of Wellbeing Scores"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    strip.text = element_text(size = 20) 
  )

well_being

ggsave(file.path(output_path, "lightning seminar plot", "plot4_wellbeing_all4.jpeg"), 
       plot = well_being, 
       device = "jpeg", 
       width = 16, 
       height = 8, 
       dpi = 300,
       scale = 0.8)


#Separate graphs-----



# Get the distinct variable names from well_long
var_list <- unique(well_long$var)




for(v in var_list) {
  # Filter the data for the current variable
  data_v <- well_long %>% filter(var == v)
  
  # Compute the mean for the current variable
  mean_v <- mean(data_v$wellbeing, na.rm = TRUE)
  
  # Create the plot with proportion on the y-axis, bins starting at 0, and x-axis breaks every 2
  p <- ggplot(data_v, aes(x = wellbeing)) +
    geom_histogram(aes(y = ..count.. / sum(..count..)),
                   fill = "lightblue", color = "black", 
                   binwidth = 1) +
    geom_vline(xintercept = mean_v,
               color = "red", linetype = "dashed", size = 1) +
    annotate("text", x = mean_v, y = 0.65, label = round(mean_v, 2),
             color = "red", hjust = -0.03, size = 8) +
    scale_x_continuous(breaks = seq(0, 10, by = 2)) +
    labs(
      x = v,
      y = ""
    ) +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 22, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 18)
    )
  
  # Create a filename using the variable name (removing spaces)
  filename <- paste0("plot_wellbeing_", gsub(" ", "_", v), ".jpeg")
  
  # Save the plot with the desired scale
  ggsave(file.path(output_path, "lightning seminar plot", filename),
         plot = p, device = "jpeg", 
         width = 16, 
         height = 8, 
         dpi = 300,
         scale = 0.5)
}