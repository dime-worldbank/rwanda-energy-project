#################################################
#Author: Xiaoming zhang
#Date 2026 APril
#Purpose: EICV data on hosuehold fuel ues
#########################################################


pacman::p_load(tidyverse, dplyr, here, sf, 
               stringr, haven, lmtest, fixest, kableExtra,
               ggplot2, readxl, writexl, janitor, randomizr,
               stargazer, modelsummary, googlesheets4)

getwd()

dropbox <- 'C:/Users/wb614406/Dropbox'

data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/EICV/data"
)

output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/EICV/output"
)

#Household fuel-----
household_5 <- read_dta(file.path(data_path, "Cross_Section_5","cs_S0_S5_Household.dta"))
# Replace column names with variable labels if available

household_5 <- household_5 |> 
  mutate(
  main_fuel = as_factor(s5cq18),
  ur = as_factor(ur)
  ) |> 
  #remove all punctuation from the fuel type variables
  mutate(
    main_fuel = str_replace_all(main_fuel, "[[:punct:]]", "")
  )  
library(xtable)

# Create the frequency table
fuel_tab <- as.data.frame(table(household_5$main_fuel))
colnames(fuel_tab) <- c("Main Fuel Type", "Count")

# Write LaTeX table to file
latex_table <- xtable(fuel_tab, caption = "Distribution of Main Fuel Types", label = "tab:mainfuel")
print(
  latex_table,
  include.rownames = FALSE,
  file = file.path(output_path, "main_fuel_table_5.tex")
)




household_5 <- household_5 |> 
  mutate(
    main_fuel = case_when(
      main_fuel == "Gas" ~ "Gas",
      main_fuel == "Charcoal" ~ "Charcoal",
      main_fuel == "Firewood" ~ "Firewood",
      TRUE ~ "Other"
    )
  )


#Graph pie chart of main fuel types and table for secondary and tertiary fuel types
main_fuel_counts <- household_5 %>%
  count(main_fuel) %>%
  mutate(percentage = n / sum(n) * 100)

#household that use gas as main fuel
gas_main_fuel <- household_5 %>%
  filter(main_fuel == "Gas") |> 
  mutate(
    main_fuel_gas =1 
  )

household_id <- household_5 |> 
  select(hhid, clust, province, district, ur, main_fuel)

fuel_data.1 <- left_join(household_id, gas_main_fuel |> select(hhid, main_fuel_gas), by = "hhid") |> 
  mutate(main_fuel_gas = ifelse(is.na(main_fuel_gas), 0, main_fuel_gas))



#Graph pie chart of main fuel types

main_fuel_counts <- main_fuel_counts |>
  mutate(
    legend_label = paste0(main_fuel, " (", round(percentage, 1), "%)")
  )

p <- ggplot(main_fuel_counts, aes(x = "", y = percentage, fill = legend_label)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Distribution of Main Fuel Types",
    fill = "Main Fuel Type"
  ) +
  theme_void() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

p




ggsave(file.path(output_path, "main_fuel_pie_chart_5.png"), plot = p, width = 6, height = 6, dpi = 300, scale = 0.8)



#household energy expenditure------


expenditure_wide <- read_dta(file.path(data_path, "Cross_Section_5", "cs_S8A3_expenditure.dta")) |>
  mutate(
    fuel = as_factor(s8a3q0),
    purchase = as_factor(s8a3q2),
    expenditure = s8a3q3,
    ur = as_factor(ur)
  ) |>
  filter(fuel %in% c("Charcoal _cooking_", "Gas _propane_")) |>
  mutate(
    fuel = case_when(
      fuel == "Charcoal _cooking_" ~ "charcoal",
      fuel == "Gas _propane_" ~ "gas"
    )
  ) |>
  select(hhid, fuel, expenditure, purchase) |> 
  mutate(
    expenditure = ifelse(purchase == "None", 0, expenditure)
  ) |>
  mutate(expenditure = expenditure * 4) |>
  select(-purchase) |>
  pivot_wider(
    names_from = fuel,
    values_from = expenditure,
    names_prefix = "expenditure_"
  )


fuel_data.2 <- left_join(fuel_data.1, expenditure_wide, by = "hhid") 



#Calculate expediture including zeros
#Two versions 1. sample households primary charcoal vs gas 
#treatment is primary gas



mean_expenditure <- expenditure_wide%>%
  pivot_longer(cols = starts_with("expenditure_"), names_to = "fuel", values_to = "expenditure") %>%
  group_by(fuel) %>%
  summarise(mean_expenditure = mean(expenditure , na.rm = TRUE))

#graph bar plot of mean expenditure by fuel type

p2 <- ggplot(mean_expenditure, aes(x = fuel, y = mean_expenditure, fill = fuel)) +
  geom_col(width = 0.5) +
  geom_text(
    aes(label = round(mean_expenditure, 0)),
    vjust = -0.5, size = 4,
    color = "black",
    stroke = 0.3, # white outline if using ggtext or shadowtext, see note below
    fontface = "bold"
  ) +
  labs(x = "Fuel Type", y = "Mean Expenditure (RwF last 4 weeks)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

p2

ggsave(
  file.path(output_path, "mean_expenditure_bar_plot_5.png"),
  plot = p2,
  width = 7, height = 4, dpi = 300, scale = 0.8
)


#Person-------


person <- read_dta(file.path(data_path, "Cross_Section_5","cs_S1_S2_S3_S4_S6A_S6E_Person.dta"))

person_time.1 <- person |> 
  select(hhid, pid,  s1q1, s1q3y, starts_with("s6e")) |> 
  mutate(
    gender = as_factor(s1q1),
    age = s1q3y,
    #Over the last 7 days collect fuel
    collect_fuel = as_factor(s6eq1),
    #s6eq2 number of hour in the past 7 days collect fuel
    collect_fuel_hours = s6eq2,
    # Over the last 7 days main fuel main cook stove
    main_fuel_cookstove = as_factor(s6eq3),
    #s6eq4 number of hour main fuel cook stove
    main_fuel_cookstove_hours = s6eq4,
    #Over the last 7 days cooking
    cooking = as_factor(s6eq5),
    cooking_hours = s6eq6,  
  ) |>  
  mutate(
     collect_fuel_hours = ifelse(collect_fuel == "No", 0, collect_fuel_hours),
      main_fuel_cookstove_hours = ifelse(main_fuel_cookstove == "No", 0, main_fuel_cookstove_hours),
      cooking_hours = ifelse(cooking == "No", 0, cooking_hours)
  ) |>
  select(-starts_with("s6e"), -s1q1, -s1q3y)  


person_time <- person_time.1 |> 
  group_by(hhid, gender) |>
  summarise(
    collect_fuel_hours = sum(collect_fuel_hours, na.rm = TRUE),
    main_fuel_cookstove_hours = sum(main_fuel_cookstove_hours, na.rm = TRUE),
    cooking_hours = sum(cooking_hours, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(gender = str_to_lower(gender)) |> 
  pivot_wider(names_from = gender, values_from = c(collect_fuel_hours, main_fuel_cookstove_hours, cooking_hours), values_fill = 0)


fuel_data.3<- left_join(fuel_data.2, person_time, by = "hhid")


#plot data of time spent on collecting fuel and cooking hours by gender in bar plot


time_data <- fuel_data.3 |>
  # filter(main_fuel %in% c("Charcoal", "Gas")) |>
  select(starts_with("collect_fuel_hours"), starts_with("cooking_hours")) |>
  pivot_longer(cols = everything(), names_to = "activity_gender", values_to = "minutes") |>
  mutate(
    gender = case_when(
      str_detect(activity_gender, "female") ~ "female",
      str_detect(activity_gender, "male") ~ "male",
      TRUE ~ NA_character_
    ),
    activity = case_when(
      gender == "female" ~ str_remove(activity_gender, "_female$"),
      gender == "male" ~ str_remove(activity_gender, "_male$"),
      TRUE ~ activity_gender
    )
  ) |> 
  group_by(activity, gender) |>
  summarise(mean_hours = mean(round(minutes/60, 1), na.rm = TRUE), .groups = "drop")

#Make the bar plot slimmer

#6 and older
p3 <- ggplot(time_data, aes(x = activity, y = mean_hours, fill = gender)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5) +
  geom_text(
    aes(label = round(mean_hours, 1)),
    position = position_dodge(width = 0.6),
    vjust = -0.5, size = 4,
    color = "black",
    stroke = 0.3, # white outline if using ggtext or shadowtext, see note below
    fontface = "bold"
  ) +
  labs(x = "Activity", y = "Mean Hours") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
p3

ggsave(
  file.path(output_path, "mean_hours_bar_plot_5.png"),
  plot = p3,
  width = 7, height = 4, dpi = 300, scale = 0.8
)


#Regression-----

consumption <- read_dta(file.path(data_path, "Cross_Section_5","cs_S8B_expenditure.dta")) |>
  select(hhid, s8bq2:s8bq13)  |> 
  mutate(across(s8bq3:s8bq13, ~ ifelse(s8bq2 == 0 , 0, .))) |>
  mutate(mean_expenditure = rowMeans(across(s8bq3:s8bq13), na.rm = TRUE)) |>
  group_by(hhid) |>
  summarise(food_consumption = mean(mean_expenditure, na.rm = TRUE), .groups = "drop")

fuel_data_sub <- fuel_data.3 |> 
  left_join(consumption, by = "hhid") 


library(lfe)

# Filter to charcoal or LPG households
fuel_data_sub <- fuel_data_sub|>
  filter(main_fuel %in% c("Charcoal", "Gas")) |>
  mutate(use_lpg = as.integer(main_fuel == "Gas"))

#LM regression of main fuel gas on expenditure

lm_model_gas <- felm(
  expenditure_gas ~ use_lpg + log1p(food_consumption) | clust,
  data = fuel_data_sub
)

summary(lm_model_gas)


lm_model_charcoal <- felm(
  expenditure_charcoal ~ use_lpg + log1p(food_consumption) | clust,
  data = fuel_data_sub
)

summary(lm_model_charcoal)


models_expenditure <- list(
  "Gas Expenditure" = lm_model_gas,
  "Charcoal Expenditure" = lm_model_charcoal
)


stargazer(
  models_expenditure,
  type = "latex",
  title = "Linear Regression of Expenditure on Main Fuel Gas (RwF last 4 weeks)",
  covariate.labels = c("Main Fuel is Gas"),
  out = file.path(output_path, "lm_expenditure_main_fuel_gas_5.tex"),
)

# Extract only the tabular environment
tex_file <- file.path(output_path, "lm_expenditure_main_fuel_gas_5.tex")
lines <- readLines(tex_file)

# Find the start and end of tabular environment
start_idx <- grep("\\\\begin\\{tabular\\}", lines)
end_idx <- grep("\\\\end\\{tabular\\}", lines)

# Keep only lines from tabular start to end
if (length(start_idx) > 0 && length(end_idx) > 0) {
  lines <- lines[start_idx:end_idx]
  writeLines(lines, tex_file)
}





#LM regression of main fuel gas on time spent on collecting firewood and'cooking hours-----
#Number of hours spent in the last 7 days
lm_firewood_female <- felm(collect_fuel_hours_female ~ use_lpg + log1p(food_consumption) | clust, data = fuel_data_sub)
lm_firewood_male <- felm(collect_fuel_hours_male ~ use_lpg + log1p(food_consumption) | clust, data = fuel_data_sub)
lm_cooking_female <- felm(cooking_hours_female ~ use_lpg + log1p(food_consumption) | clust, data = fuel_data_sub)
lm_cooking_male <- felm(cooking_hours_male ~ use_lpg + log1p(food_consumption) | clust, data = fuel_data_sub)

summary(lm_firewood_female)
summary(lm_firewood_male)
summary(lm_cooking_female)
summary(lm_cooking_male)


models <- list(
  "Firewood(Female)" = lm_firewood_female,
  "Firewood(Male)" = lm_firewood_male,
  "Cooking(Female)" = lm_cooking_female,
  "Cooking(Male)" = lm_cooking_male
)

stargazer(
  models,
  type = "latex",
  dep.var.labels.include = FALSE,
  covariate.labels = c("Main Fuel is Gas"),
  column.labels = c("Firewood (Female)", "Firewood (Male)", "Cooking (Female)", "Cooking (Male)"),
  omit.stat = c("f", "ser"),  # optional cleanup
  out = file.path(output_path, "lm_time_spent_activities_main_fuel_gas_5.tex")
)


tex_file <- file.path(output_path, "lm_time_spent_activities_main_fuel_gas_5.tex")
lines <- readLines(tex_file)

# Find the start and end of tabular environment
start_idx <- grep("\\\\begin\\{tabular\\}", lines)
end_idx <- grep("\\\\end\\{tabular\\}", lines)

# Keep only lines from tabular start to end
if (length(start_idx) > 0 && length(end_idx) > 0) {
  lines <- lines[start_idx:end_idx]
  writeLines(lines, tex_file)
}