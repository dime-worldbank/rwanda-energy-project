##############
#Author: Xiaoming Zhang
#Date: 1.21.2025
#Purpose: Analysis
#############


pacman::p_load(knitr, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr)

getwd()

library(haven)

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


# select----
hfc_admin <- read_xlsx(file.path(data_path, "hfc_admin.xlsx"))

#Do some cleaning
hfc_admin <- hfc_admin %>% 
  filter(!is.na(hh_head_name)) %>% 
  mutate(
    wtp_12 = J4_2*12,
    wtp_24 = J5_2*24
  )

#Unit uniformation----

##Primary salary other----

primary_salary <- hfc_admin %>% 
  filter(A2_5_label == "Other" & A2_4 !=0) %>% 
  select(hh_id, A2_4, A2_5, A2_5_label, A2_6)


write_xlsx(primary_salary, path = file.path(data_path, "primary_salary.xlsx"))

primary_edit <- read_xlsx()






##Make changes in hfc_admin----

hfc_admin <- hfc_admin %>% 
mutate(
  A2_4_month = case_when(
    A2_5_label == "Hour" ~ A2_4 *   *8,
    A2_5_label == "Day"  ~ A2_4 * 30,
    A2_5_label == "Week" ~ A2_4 * 4, 
    A2_5_label == "2 Weeks" ~ A2_4 * 2,
    A2_5_label == "Month" ~ A2_4*1, 
    A2_5_label == "Quarter" ~ A2_4/3,
    A2_5_label == "Agricultural season" ~ A2_4/4,
    A2_5_label == "Half Year" ~ A2_4/6,
    A2_5_label == "Year" ~ A2_4/12,
    A2_5_label == "Other" ~ A2_4_other,
    
    .default = A2_4
  )
)

#1. General Descriptive-----
desc_stats_variables <- c(
  "A2_4", #primary occupation salary
  "A3_5", #secondary occupation salary
  "H4_2", #candle expenditure
  "H5_4", #biomass expenditure
  "H6_2", #kerosene
  "B4_1", #energy reliability
  "B4_2", #energy efficiency
  "B4_3", #energy accessibility
  "B4_4", #light accessbility
  "B4_5", #energy service satisfaction
  "B4_6", #anxiety related to energy
  "B4_7", #energy challenges
  "B4_8", #energy and peace of mind
  
  "B5_1", #personal ladder
  "B5_2", #ladder one year back
  "B5_3", #financial situation
  "B5_4", #cooking ladder
  "B5_5", #mobile phone charging ladder
  "B5_6", #lighting ladder
  "B5_7", #energy satisfaction
  "B5_8", #energy general
  
  "J1_final", #wtp_fixed
  "J2_1", #wtp_fixed_appliance
  "J3_1", #wtp_fixed_low_reliability
  "J4_2", #wtp_paygo_12
  "J5_2", #wtp_paygo_24
  "wtp_12",
  "wtp_24",
  "J6_1", #wtp_lightbulb
  
  "E2_3", #formal savings
  "E3_3", #informal savings
  
  
  "I4_1", #fuel preparation women
  "I4_2" #fuel preparation men
  
  
  #general wellbeing
  #social desirability
  
)




###Summarise mean----


desc_stats_check <- hfc_admin %>%
  summarize(
    across(
      all_of(desc_stats_variables),
      list(
        mean     = ~ mean(.x, na.rm = TRUE),
        sd       = ~ sd(.x, na.rm = TRUE),
        min      = ~ min(.x, na.rm = TRUE),
        max      = ~ max(.x, na.rm = TRUE),
        median   = ~ median(.x, na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(
    cols          = everything(),
    names_to      = c("var", "stat"),
    names_pattern = "^(.*)_(mean|sd|min|max|median)$"
  ) %>%
  pivot_wider(
    names_from  = "stat",
    values_from = "value"
  )  %>% 
  mutate(across(everything(), ~ ifelse(is.nan(.x) | .x == -Inf | .x == Inf, NA, .x)))

desc_stats_check <- desc_stats_check %>% 
  mutate(
    var = case_when(
      var == "A2_4" ~ "primary_salary",
      var == "A3_5" ~ "secondary_salary",
      var == "H4_2" ~ "candle_exp",
      var == "H5_4" ~ "biomass_exp",
      var == "H6_2" ~ "kerosene_exp", 
      
      var == "B4_1" ~ "reliable_energy",
      var == "B4_2" ~ "efficient_energy",
      var == "B4_3" ~ "accessible_energy",
      var == "B4_4" ~ "accessible_light",
      var == "B4_5" ~ "satisfactory_energy",
      var == "B4_6" ~ "no_energy_anxiety",
      var == "B4_7" ~ "no_energy_challenge",
      var == "B4_8" ~ "energy_peace_of_mind",
      
      var == "B5_1" ~ "personal_ladder",
      var == "B5_2" ~ "ladder_year_back",
      var == "B5_3" ~ "financial_situation",
      var == "B5_4" ~ "cooking_ladder",
      var == "B5_5" ~ "mobile_ladder",
      var == "B5_6" ~ "lighting_ladder",
      var == "B5_7" ~ "energy_satisfaction",
      var == "B5_8" ~ "energy_general",
      
      var == "J1_final" ~ "wtp_fixed_system",
      var == "J2_1" ~ "wtp_fixed_appliance",
      var == "J3_1" ~ "wtp_fixed_low_reliability",
      var == "J4_2" ~ "wtp_fixed_paygo_12",
      var == "J5_2" ~ "wtp_fixed_paygo_24",
      var == "wtp_12" ~ "wtp_12", 
      var == "wtp_24" ~ "wtp_24",
      var == "J6_1" ~ "wtp_lightbulb",
      
      var == "E2_3" ~ "formal_savings", 
      var == "E3_3" ~ "informal_savings",
      
      var == "I4_1" ~ "fuel_prep_women",
      var == "I4_2" ~ "fuel_prep_men",
      
      TRUE ~ var  # Retain original name if no match
    )
  )


#Add units----


units_df <- data.frame(
  var = c(
    "primary_salary", "secondary_salary", "candle_exp", "biomass_exp", "kerosene_exp",
    "reliable_energy", "efficient_energy", "accessible_energy", "accessible_light",
    "satisfactory_energy", "no_energy_anxiety", "no_energy_challenge", "energy_peace_of_mind",
    "personal_ladder", "ladder_year_back", "financial_situation", "cooking_ladder",
    "mobile_ladder", "lighting_ladder", "energy_satisfaction", "energy_general",
    "wtp_fixed_system", "wtp_fixed_appliance", "wtp_fixed_low_reliability", "wtp_fixed_paygo_12",
    "wtp_fixed_paygo_24", "wtp_12", "wtp_24", "wtp_lightbulb", "formal_savings", "informal_savings",
    "fuel_prep_women", "fuel_prep_men"
  ),
  unit = c(
    NA, NA, "Rwf/month", "Rwf/month", "Rwf/month",
    "1(strongly agree) to 5(strongly disagree)", "1(strongly agree) to 5(strongly disagree)", "1(strongly agree) to 5(strongly disagree)", "1(strongly agree) to 5(strongly disagree)",
    "1(strongly agree) to 5(strongly disagree)", "1(strongly agree) to 5(strongly disagree)", "1(strongly agree) to 5(strongly disagree)", "1(strongly agree) to 5(strongly disagree)",
    "1(lowest ladder) to 10 (top of ladder) scale", "1(lowest ladder) to 10 (top of ladder) scale", "1(lowest ladder) to 10 (top of ladder) scale", "1(lowest ladder) to 10 (top of ladder) scale",
    "1(lowest ladder) to 10 (top of ladder) scale", "1(lowest ladder) to 10 (top of ladder) scale", "1(lowest ladder) to 10 (top of ladder) scale", "1(lowest ladder) to 10 (top of ladder) scale",
    "Rwf", "Rwf", "Rwf", "Rwf per month for 12 months",
    "Rwf per month for 24 months", "Rwf", "Rwf","Rwf per month", "Rwf", "Rwf",
    "minutes per day", "minutes per day"
  )
)


desc_stats_check <- desc_stats_check %>%
  left_join(units_df, by = "var") %>%  # Add units based on the "var" column
  mutate(
    across(!c("var", "unit"), ~round(.x, 2))  # Round all numeric columns except "var" and "unit"
  ) %>% 
  select(var, unit, everything())
# 
# 
# 
# 
# 

desc_stats_check <- as.data.frame(desc_stats_check)

desc_stats_check <- desc_stats_check %>% 
  mutate(
    across(c(mean, sd, min, max, median), ~round(.x, 2))
  )

write_xlsx(
  desc_stats_check,
  path = file.path(output_path, "descriptives.xlsx")
)






# 
# ###Treatment mean----
# 
# treatment_summary <- hfc_admin %>%
#   group_by(treatment) %>% 
#   summarize(
#     across(
#       all_of(desc_stats_variables),
#       ~ mean(.x, na.rm = TRUE)
#     )
#   ) %>% 
#   
#   
#   pivot_longer(
#     cols = -treatment,
#     names_to = "var",
#     values_to = "mean"
#   ) %>%
#   mutate(
#     var = case_when(
#       var == "A2_4" ~ "primary_salary", 
#       var == "A3_5" ~ "secondary_salary",
#       var == "H4_2" ~ "candle_exp",
#       var == "H5_4" ~ "biomass_exp",
#       var == "H6_2" ~ "kerosene_exp", 
#       
#       var == "B4_1" ~ "energy_reliability",
#       var == "B4_2" ~ "energy_efficiency",
#       var == "B4_3" ~ "energy_accessibility",
#       var == "B4_4" ~ "light_accessibility",
#       var == "B4_5" ~ "service_satisfaction",
#       var == "B4_6" ~ "anxiety_energy",
#       var == "B4_7" ~ "energy_challenges",
#       var == "B4_8" ~ "energy_peace_mind",
#       
#       var == "B5_1" ~ "personal_ladder",
#       var == "B5_2" ~ "ladder_year_back",
#       var == "B5_3" ~ "financial_situation",
#       var == "B5_4" ~ "cooking_ladder",
#       var == "B5_5" ~ "mobile_ladder",
#       var == "B5_6" ~ "lighting_ladder",
#       var == "B5_7" ~ "energy_satisfaction",
#       var == "B5_8" ~ "energy_general",
#       
#       var == "J1_final" ~ "wtp_fixed",
#       var == "J2_1" ~ "wtp_fixed_appliance",
#       var == "J3_1" ~ "wtp_fixed_low_reliability",
#       var == "J4_2" ~ "wtp_paygo_12",
#       var == "J5_2" ~ "wtp_paygo_24",
#       var == "J6_1" ~ "wtp_lightbulb",
#       
#       var == "E2_3" ~ "formal_savings", 
#       var == "E3_3" ~ "informal_savings",
#       
#       var == "I4_1" ~ "fuel_prep_women",
#       var == "I4_2" ~ "fuel_prep_men",
#       
#       TRUE ~ var  # Retain original name if no match
#     )
#   ) %>%
#   pivot_wider(
#     names_from = "treatment",
#     values_from = "mean"
#   )
# 
# 
# write_xlsx(
#   list(
#     Treatment_Summary = treatment_summary,
#     Desc_Stats_Check = desc_stats_check
#   ),
#   path = file.path(output_path, "data_summary.xlsx")
# )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ###Treatment table----
# 
# balance_table <- hfc_admin %>% 
#   select(
#     treatment, all_of(desc_stats_variables)
#   ) %>% 
#   rename(
#       primary_salary = A2_4,
#       secondary_salary = A3_5,
#       candle_exp = H4_2,
#       biomass_exp = H5_4,
#       kerosene_exp = H6_2,
#       
#       energy_reliability = B4_1,
#       energy_efficiency = B4_2,
#       energy_accessibility = B4_3,
#       light_accessibility = B4_4,
#       service_satisfaction = B4_5,
#       anxiety_energy = B4_6,
#       energy_challenges = B4_7,
#       energy_peace_mind = B4_8,
#       
#       personal_ladder = B5_1,
#       ladder_year_back = B5_2,
#       financial_situation = B5_3,
#       cooking_ladder = B5_4,
#       mobile_ladder = B5_5,
#       lighting_ladder = B5_6,
#       energy_satisfaction = B5_7,
#       energy_general = B5_8,
#       
#       wtp_fixed = J1_final,
#       wtp_fixed_appliance = J2_1,
#       wtp_fixed_low_reliability = J3_1,
#       wtp_paygo_12 = J4_2,
#       wtp_paygo_24 = J5_2,
#       wtp_lightbulb = J6_1,
#       
#       formal_savings = E2_3,
#       informal_savings = E3_3,
#       
#       fuel_prep_women = I4_1,
#       fuel_prep_men = I4_2
#     )
#     
# 
# 
# write_dta(balance_table, path = file.path(data_path, "balance_table.dta"))
# 
# 
# 
# 
# 

