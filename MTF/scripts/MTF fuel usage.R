
pacman::p_load(tidyverse, dplyr, here, sf, 
               stringr, haven, lmtest, fixest, kableExtra,
               ggplot2, readxl, writexl, janitor, randomizr,
               stargazer, modelsummary, googlesheets4)

getwd()

# Import Data ----
dropbox <- 'C:/Users/wb614406/Dropbox'

data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/MTF Data"
)

output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/MTF Data/output"
)

library(dplyr)
library(tidyr)
library(ggplot2)

fuel_rural <- sectionI %>% 
  mutate(HI04 = as_factor(HI04)) %>% 
  filter(HI04 == "Rural") %>% 
  filter(I05 == 1) %>% 
  filter(I03 == 1) %>% 
  mutate(I02 = as_factor(I02)) %>% 
  mutate(fuel_group = case_when(
    I02 == "LPG/ cooking gas" ~ "LPG",
    I02 == "Charcoal" ~ "Charcoal",
    I02 %in% c("Wood Collected", "Wood Purchased") ~ "Firewood",
    TRUE ~ "Others"
  ))

hh_lpg_rural <- fuel_rural %>% 
  group_by(HHID) %>% 
  summarise(
    LPG_use = as.integer(any(fuel_group == "LPG"))
  )
table(hh_lpg_rural$LPG_use)


# 2. Convert frequency to monthly

fuel_rural <- fuel_rural %>% 
  mutate(freq_month = case_when(
    I13 == 1 ~ 30,      # Daily
    I13 == 2 ~ 4.3,     # Weekly
    I13 == 3 ~ 8.6,     # Twice a week
    I13 == 4 ~ 1,       # Monthly
    TRUE ~ NA_real_
  ))

# 3. Compute monthly expenditure

fuel_rural <- fuel_rural %>% 
  mutate(monthly_exp = I16 * freq_month)

# 4. Household total energy expenditure

hh_exp <- fuel_rural %>% 
  group_by(HHID) %>% 
  summarise(total_energy_exp = sum(monthly_exp, na.rm = TRUE))


# 5. Expenditure by fuel type

fuel_exp <- fuel_rural %>% 
  group_by(fuel_group) %>% 
  summarise(total_exp = sum(monthly_exp, na.rm = TRUE)) %>% 
  mutate(share = total_exp / sum(total_exp))



# 6. Average household expenditure by fuel

fuel_exp_hh <- fuel_rural %>% 
  group_by(HHID, fuel_group) %>% 
  summarise(exp = sum(monthly_exp, na.rm = TRUE), .groups = "drop") %>% 
  group_by(fuel_group) %>% 
  summarise(avg_exp = round(mean(exp, na.rm = TRUE), 2)) %>% 
  mutate(avg_exp = paste0(avg_exp, " RwF"))

View(fuel_exp_hh)


latex_table <- kable(
  fuel_exp_hh,
  format = "latex",
  booktabs = TRUE,
  col.names = c("Fuel Type", "Average Monthly Expenditure (RwF)"),
  caption = "Average Household Energy Expenditure by Fuel Type"
)

writeLines(
  latex_table,
  file.path(output_path, "fuel_exp_table.tex")
)

# 7. Create wide dataset (for usage shares)

fuel_wide <- fuel_rural %>% 
  distinct(HHID, fuel_group) %>% 
  mutate(value = 1) %>% 
  pivot_wider(
    names_from = fuel_group,
    values_from = value,
    values_fill = 0
  )


# 8. Compute usage shares

plot_data <- fuel_wide %>% 
  summarise(
    LPG = mean(LPG),
    Charcoal = mean(Charcoal),
    Firewood = mean(Firewood),
    Others = mean(Others)
  ) %>% 
  pivot_longer(everything(),
               names_to = "fuel_group",
               values_to = "share")

# order for plotting
plot_data$fuel_group <- factor(
  plot_data$fuel_group,
  levels = c("LPG", "Charcoal", "Firewood", "Others")
)


# 9. Plot: fuel usage (bar plot)

p <- ggplot(plot_data, aes(x = fuel_group, y = share)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(share, accuracy = 0.1)),
            vjust = -0.3) +
  scale_y_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = "",
    y = "Share of households",
    title = "Household Fuel Use by Type"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_path, "fuel_usage_barplot.png"),
  plot = p,
  width = 4,
  height = 3,
  dpi = 300,
  scale = 0.8
)

#10/ Share of LPG

lpg_data <- fuel_wide %>% 
  mutate(LPG_use = if_else(LPG == 1, "Uses LPG", "Does not use LPG"))

pie_data <- lpg_data %>% 
  count(LPG_use) %>% 
  mutate(share = n / sum(n))


p_pie <- ggplot(pie_data, aes(x = "", y = share, fill = LPG_use)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = scales::percent(share, accuracy = 0.1)),
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Share of Households Using LPG",
    fill = ""
  ) +
  theme_void()

p_pie
ggsave(
  filename = file.path(output_path, "lpg_usage_pie.png"),
  plot = p_pie,
  width = 6,
  height = 6,
  dpi = 300,
  scale = 0.8
)


#What they use LPG to do----




#Regression-----
hh_exp <- fuel_rural %>% 
  group_by(HHID) %>% 
  summarise(
    total_energy_exp = sum(monthly_exp, na.rm = TRUE)
  )

fuel_rural %>%
  group_by(HHID) %>%
  summarise(LPGexp = sum(monthly_exp * (I02 == "LPG/ cooking gas"), na.rm = T),
            Charcoalexp = sum(monthly_exp * (I02 == "Charcoal"), na.rm = T)) %>%
  ungroup %>%
  mutate(anylpg = LPGexp > 0,
         anycharcoal = Charcoalexp > 0) %>%
  group_by(anylpg, anycharcoal) %>%
  summarise(LPGexp = mean(LPGexp),
            Charcoalexp = mean(Charcoalexp))

hh_lpg <- fuel_rural %>% 
  filter(fuel_group == "LPG") %>% 
  select(HHID) %>% 
  mutate(`LPG` = 1)

hh_data <- hh_exp %>% 
  left_join(hh_lpg, by = "HHID") %>% 
  mutate(LPG = ifelse(is.na(LPG), 0, LPG))

hh_data <- hh_data %>% 
  mutate(
    log_exp = log(total_energy_exp)   # handles zeros
  )

model1 <- lm(log_exp ~ LPG, data = hh_data %>% subset(total_energy_exp > 0))
summary(model1)

#Time spent----



time_plot <- sectionI2J %>% 
  filter(HI04 == 2) %>% 
  filter(HHID %in% fuel_rural$HHID) %>% 
  mutate(
    women = I17A + I19A,
    girls = I17B + I19B,
    men   = I17C + I19C,
    boys  = I17D + I19D
  ) %>% 
  summarise(
    women = mean(women %>% (function(x) ifelse(is.na(x), 0, x)), na.rm = TRUE),
    girls = mean(girls %>% (function(x) ifelse(is.na(x), 0, x)), na.rm = TRUE),
    men   = mean(men %>% (function(x) ifelse(is.na(x), 0, x)), na.rm = TRUE),
    boys  = mean(boys %>% (function(x) ifelse(is.na(x), 0, x)), na.rm = TRUE)
  ) %>% 
  pivot_longer(everything(), names_to = "group", values_to = "minutes")


p_time <- ggplot(time_plot, aes(x = group, y = minutes)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(
    aes(label = paste0(round(minutes, 1), " min")),
    vjust = -0.3,
    size = 4
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = "",
    y = "Average minutes per day",
    title = "Time Spent on Fuel Collection and Preparation"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_path, "time_burden_barplot.png"),
  plot = p_time,
  width = 8,
  height = 6,
  dpi = 300,
  scale = 0.6
)


#Regression----


time_spent <-  sectionI2J %>% 
  filter(HI04 == 2) %>% 
  filter(HHID %in% fuel_rural$HHID) %>% 
  mutate(
    women = I17A + I19A,
    girls = I17B + I19B,
    men   = I17C + I19C,
    boys  = I17D + I19D
  ) %>% 
  select(HHID, women, girls, men, boys)


hh_data <- left_join(hh_data, time_spent, by = "HHID") %>% 
  mutate(across(c(women, girls, men, boys), ~ ifelse(is.na(.), 0, .)))

m_women <- lm(women ~ LPG, data = hh_data)
m_girls <- lm(girls ~ LPG, data = hh_data)
m_men   <- lm(men   ~ LPG, data = hh_data)
m_boys  <- lm(boys  ~ LPG, data = hh_data)

stargazer(
  m_women, m_girls, m_men, m_boys,
  type = "latex",
  title = "Effect of LPG Use on Time Spent on Fuel Activities",
  covariate.labels = "Uses LPG",
  column.labels = c("Women", "Girls", "Men", "Boys"),
  out = file.path(output_path, "time_regression_table.tex")
)










