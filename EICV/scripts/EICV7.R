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
household <- read_dta(file.path(data_path, "Cross_Section_7","CS_S01_S5_S7_Household.dta"))
# Replace column names with variable labels if available

household <- household |> 
  mutate(
  main_fuel = as_factor(s5cq22a),
  secondary_fuel = as_factor(s5cq22b),
  tertiary_fuel = as_factor(s5cq22c),
  cook_stove = as_factor(s5cq21),
  ur = as_factor(ur)
  ) |> 
  #remove all punctuation from the fuel type variables
  mutate(
    main_fuel = str_replace_all(main_fuel, "[[:punct:]]", ""),
    secondary_fuel = str_replace_all(secondary_fuel, "[[:punct:]]", ""),
    tertiary_fuel = str_replace_all(tertiary_fuel, "[[:punct:]]", "")
  )  

library(xtable)

# Create the frequency table
fuel_tab <- as.data.frame(table(household$main_fuel))
colnames(fuel_tab) <- c("Main Fuel Type", "Count")

# Write LaTeX table to file
latex_table <- xtable(fuel_tab, caption = "Distribution of Main Fuel Types", label = "tab:mainfuel")
print(
  latex_table,
  include.rownames = FALSE,
  file = file.path(output_path, "main_fuel_table.tex")
)




household <- household |> 
  mutate(
    main_fuel = case_when(
      main_fuel == "Gas" ~ "Gas",
      main_fuel == "Charcoal" ~ "Charcoal",
      main_fuel == "Firewood" ~ "Firewood",
      TRUE ~ "Other"
    ),
    secondary_fuel = case_when(
      secondary_fuel == "Gas" ~ "Gas",
      secondary_fuel == "Charcoal" ~ "Charcoal",
      secondary_fuel == "Firewood" ~ "Firewood",
      TRUE ~ "Other"
    ),
    tertiary_fuel = case_when(
      tertiary_fuel == "Gas" ~ "Gas",
      tertiary_fuel == "Charcoal" ~ "Charcoal",
      tertiary_fuel == "Firewood" ~ "Firewood",
      TRUE ~ "Other"
    )
  )


#Graph pie chart of main fuel types and table for secondary and tertiary fuel types
main_fuel_counts <- household %>%
  count(main_fuel) %>%
  mutate(percentage = n / sum(n) * 100)

#household that use gas as main fuel
gas_main_fuel <- household %>%
  filter(main_fuel == "Gas") |> 
  mutate(
    main_fuel_gas =1 
  )

household_id <- household |> 
  select(hhid, clust, province, district, ur, main_fuel, secondary_fuel, tertiary_fuel)

fuel_data.1 <- left_join(household_id, gas_main_fuel |> select(hhid, main_fuel_gas), by = "hhid") |> 
  mutate(main_fuel_gas = ifelse(is.na(main_fuel_gas), 0, main_fuel_gas))



#Graph pie chart of main fuel types

p <- ggplot(main_fuel_counts, aes(x = "", y = percentage, fill = main_fuel)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(main_fuel, "\n", round(percentage, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 4
  ) +
  labs(title = "Distribution of Main Fuel Types") +
  theme_void() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

ggsave(file.path(output_path, "main_fuel_pie_chart.png"), plot = p, width = 6, height = 6, dpi = 300, scale = 0.8)



#household energy expenditure------


gas_expenditure <- read_dta(file.path(data_path,"Cross_Section_7", "CS_S8A2_Expenditure.dta")) |>
  mutate(
    fuel = as_factor(s8a2q0),
    purchase = as_factor(s8a2q2),
    expenditure = s8a2q3,
    ur = as_factor(ur)
  ) |> 
  filter(str_detect(tolower(as.character(fuel)), "gas")) |> 
  mutate(expenditure_gas = expenditure) |> 
  mutate(expenditure_gas = ifelse(purchase == "No", 0, expenditure_gas)) |> 
  select(hhid, purchase,  expenditure_gas)


charcoal_expenditure <- read_dta(file.path(data_path, "Cross_Section_7", "CS_S8A3_Expenditure.dta")) |> 
  mutate(
    fuel = as_factor(s8a3q0),
    purchase = as_factor(s8a3q2),
    expenditure = s8a3q3,
    ur = as_factor(ur)
  ) |> 
  filter(str_detect(tolower(as.character(fuel)), "charcoal")) |> 
  mutate(expenditure_charcoal = expenditure) |> 
  mutate(expenditure_charcoal = ifelse(purchase == "No", 0, expenditure_charcoal)) |> 
  select(hhid, expenditure_charcoal)

expenditure_wide <- left_join(gas_expenditure, charcoal_expenditure, by = "hhid")

fuel_data.2 <- left_join(fuel_data.1, expenditure_wide, by = "hhid") 



#Calculate expediture including zeros
#Two versions 1. sample households primary charcoal vs gas 
#treatment is primary gas



mean_expenditure <- expenditure_wide%>%
  pivot_longer(cols = starts_with("expenditure_"), names_to = "fuel", values_to = "expenditure") %>%
  group_by(fuel) %>%
  summarise(mean_expenditure = mean(expenditure, na.rm = TRUE))

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
  file.path(output_path, "mean_expenditure_bar_plot.png"),
  plot = p2,
  width = 7, height = 4, dpi = 300, scale = 0.8
)


#Person-------


person <- read_dta(file.path(data_path, "Cross_Section_7", "CS_S0_S1_S2_S3_S4_S6A_S6B_S6C_Person.dta"))

person_time.1 <- person |> 
   mutate(ur = as_factor(ur)) |>
  select(hhid, pid,  s1q1, s1q3y, starts_with("s6c")) |> 
  mutate(
    gender = as_factor(s1q1),
    age = s1q3y,
    #s6cq1 Over the last 7 days fetch water
    fetch_water = as_factor(s6cq1),
    #s6cq2 number of hour in the past 7 days fetch water
    fetch_water_hours = s6cq2,
    #s6cq3 Over the last 7 days collect firewood
    collect_firewood = as_factor(s6cq3),
    #s6cq4 number of hour collect firewood
    collect_firewood_hours = s6cq4,
    #grazing for household animals
    grazing_animals = as_factor(s6cq5),
    grazing_animals_hours = s6cq6,
    #shopping for household needs
    shopping = as_factor(s6cq7),
    shopping_hours = s6cq8,
    #cooking for household
    cooking = as_factor(s6cq9),
    cooking_hours = s6cq10,
    #household chores
    chores = as_factor(s6cq11),
    chores_hours = s6cq12
  ) |> 
  mutate(
    fetch_water_hours = ifelse(fetch_water == "No", 0, fetch_water_hours),
    collect_firewood_hours = ifelse(collect_firewood == "No", 0, collect_firewood_hours),
    grazing_animals_hours = ifelse(grazing_animals == "No", 0, grazing_animals_hours),
    shopping_hours = ifelse(shopping == "No", 0, shopping_hours),
    cooking_hours = ifelse(cooking == "No", 0, cooking_hours),
    chores_hours = ifelse(chores == "No", 0, chores_hours)
  ) |>
  select(-starts_with("s6c"), -s1q1, -s1q3y)  


person_time <- person_time.1 |> 
  group_by(hhid, gender) |>
  summarise(
    fetch_water_hours = sum(fetch_water_hours, na.rm = TRUE),
    collect_firewood_hours = sum(collect_firewood_hours, na.rm = TRUE),
    grazing_animals_hours = sum(grazing_animals_hours, na.rm = TRUE),
    shopping_hours = sum(shopping_hours, na.rm = TRUE),
    cooking_hours = sum(cooking_hours, na.rm = TRUE),
    chores_hours = sum(chores_hours, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(gender = str_to_lower(gender)) |> 
  pivot_wider(names_from = gender, values_from = c(fetch_water_hours, collect_firewood_hours, grazing_animals_hours, shopping_hours, cooking_hours, chores_hours), values_fill = 0)


fuel_data.3<- left_join(fuel_data.2, person_time, by = "hhid")


#plot data of time spent on colleting fire wook and cooking hours by gender in bar plot

time_data <- fuel_data.3 |>
  select(starts_with("collect_firewood_hours"), starts_with("cooking_hours")) |>
  pivot_longer(cols = everything(), names_to = "activity_gender", values_to = "hours") |>
  separate(activity_gender, into = c("activity", "gender"), sep = "(_[^_]*)$", remove = TRUE, extra = "merge") |> 
  group_by(activity, gender) |>
  summarise(mean_hours = mean(hours, na.rm = TRUE), .groups = "drop")

time_data <- fuel_data.3 |>
  select(starts_with("collect_firewood_hours"), starts_with("cooking_hours")) |>
  pivot_longer(cols = everything(), names_to = "activity_gender", values_to = "hours") |>
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
  summarise(mean_hours = mean(hours, na.rm = TRUE), .groups = "drop")

#Make the bar plot slimmer

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
  file.path(output_path, "mean_hours_bar_plot.png"),
  plot = p3,
  width = 7, height = 4, dpi = 300, scale = 0.8
)


#Regression-----
consumption <- read_dta(file.path(data_path, "Cross_Section_7", "CS_S8B_Food_Expenditure_Consumption.dta")) |>
  select(hhid, starts_with("s8bq4")) |>
  mutate(mean_expenditure = rowMeans(across(starts_with("s8bq4")), na.rm = TRUE)) |>
  group_by(hhid) |>
  summarise(food_consumption = mean(mean_expenditure*7, na.rm = TRUE), .groups = "drop") 



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
  out = file.path(output_path, "lm_expenditure_main_fuel_gas.tex")
)

# Extract only the tabular environment
tex_file <- file.path(output_path, "lm_expenditure_main_fuel_gas.tex")
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
lm_firewood_female <- felm(collect_firewood_hours_female ~ use_lpg + log1p(food_consumption) | clust, data = fuel_data_sub)
lm_firewood_male <- felm(collect_firewood_hours_male ~ use_lpg + log1p(food_consumption) | clust, data = fuel_data_sub)
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
  out = file.path(output_path, "lm_time_spent_activities_main_fuel_gas.tex")
)


tex_file <- file.path(output_path, "lm_time_spent_activities_main_fuel_gas.tex")
lines <- readLines(tex_file)

# Find the start and end of tabular environment
start_idx <- grep("\\\\begin\\{tabular\\}", lines)
end_idx <- grep("\\\\end\\{tabular\\}", lines)

# Keep only lines from tabular start to end
if (length(start_idx) > 0 && length(end_idx) > 0) {
  lines <- lines[start_idx:end_idx]
  writeLines(lines, tex_file)
}