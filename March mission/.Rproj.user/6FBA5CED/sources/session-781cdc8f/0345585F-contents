#Project name: Data Tidying
#Author: Xiaoming
#Date: 09262023
#Modifying date: 

library(tidyverse)
library(haven)
library(here)
library(dplyr)

#Read datafiles----

households <- read_stata(here("data", "LWH_FUP2_households_clean.dta"))
plot <- read_stata(here( "data", "LWH_FUP2_plot_clean.dta"))


households_clean <- forcats::as_factor(households)
plot_clean <- forcats::as_factor(plot)

#For the table outputs
#I will have to create 2 datasets to do it. 
#The unit of observation for 1/3/4 are all households, 
#and the unit of observation for 2 is village.

#Household income----
household_income <- households_clean %>%
  group_by(ID_05) %>%
  mutate(total_income = rowSums(across(starts_with("INC")))) %>%
  ungroup()

?across()

#Harvest of dry beans----

table(plot_clean$CRP08UA)

plot_clean <- plot_clean%>% 
  mutate(kg_unit = case_when( CRP08UA %in% "15 kg basket: 15 kg Agatebo" ~ 15,
                              CRP08UA %in% "10 kg basket: 10 kg Agatebo" ~ 10,
                              CRP08UA %in% "Bucket (5 kg): Imbegeti (5kg)" ~ 5,
                              CRP08UA %in% "Bucket (2.5kg): Imbegeti (2.5kg)" ~ 2.5,
                              CRP08UA %in% "Mironko (1.5 kg)" ~ 1.5,
                              CRP08UA %in% "tons" ~ 1000,
                              CRP08UA %in% "100 Kg sack" ~ 100,
                              CRP08UA %in% "50 Kg sack" ~ 50,
                              CRP08UA %in% "25 Kg sack" ~ 25,
                              CRP08UA %in% "kg" ~ 1),
         harvest = CRP08QA*kg_unit) 

dry_beans <- plot_clean %>% 
  filter(A_CROP %in% c("Dry Beans"))


#Average crop sales per village----

table(plot_clean$CRP09UA)

plot_clean <- plot_clean%>% 
  mutate(CRP09_unit = case_when( CRP09UA %in% "15 kg basket: 15 kg Agatebo" ~ 15,
                              CRP09UA %in% "10 kg basket: 10 kg Agatebo" ~ 10,
                              CRP09UA %in% "Bucket (5 kg): Imbegeti (5kg)" ~ 5,
                              CRP09UA %in% "Bucket (2.5kg): Imbegeti (2.5kg)" ~ 2.5,
                              CRP09UA %in% "Mironko (1.5 kg)" ~ 1.5,
                              CRP09UA %in% "tons" ~ 1000,
                              CRP09UA %in% "100 Kg sack" ~ 100,
                              CRP09UA %in% "50 Kg sack" ~ 50,
                              CRP09UA %in% "25 Kg sack" ~ 25,
                              CRP09UA %in% "kg" ~ 1),
         CRP09_sales = CRP09QA*CRP09_unit) 

####by village----

plot_clean_village <- plot_clean %>% 
  group_by(ID_06, ID_07, ID_08, ID_09, ID_10) %>% 
  summarize(avg_crop_sales = round(mean(CRP09_sales, na.rm = TRUE), 2)) %>% 
  ungroup()

# View the resulting data frame
#earnings_households owns crops----

plot_earnings <- plot_clean %>% 
  group_by(ID_05,ID_06, ID_07, ID_08, ID_09, ID_10) %>% 
  summarize(earnings = sum(CRP10A, na.rm = TRUE)) %>% 
  ungroup()


#Winsorization function----

winsor_function <- function(dataset, var, min = 0.00, max = 0.99){
  percentiles <- quantile(
    dataset %>%  select(var), probs = c(min, max), na.rm = TRUE
  )
  dataset %>% 
    mutate(
      var_winsorized = case_when(
        is.na(var)~ NA_numeric_,
        var <= min_percentile ~ percentiles[1],
        var >= max_percentile ~ percentiles[2],
        TRUE ~ var
      )
    )
}





#Select variables that have more than 100 non-missing observations
# Filter columns that are income type variables
income_data <- households_clean %>%
  mutate(across(starts_with("INC"), as.numeric)) %>%
  select(names(.)[colSums(!is.na(.))>100])


# income_data <- 
#   income_data %>%
#   mutate(
#     across(
#       starts_with("INC"), 
#       ~winsor_function(.)
#       )
#     )





