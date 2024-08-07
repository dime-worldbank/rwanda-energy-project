########################################################################################################
#                                                                                                      #
#            HIGH-FREQUENCY CHECKS AND PHONE TRACKING CODE -- OUTLIER HIGH-FREQUENCY CHECKS            #
#                                                                                                      #
########################################################################################################

## PURPOSE      Create an outlier high-frequency check sheet in the HFC dashboard.



## AUTHOR      Xiaoming Zhang (adapted from Adrien Ciret & Marc-Andrea Fiorina & Juliana Guerrero)

## LAST UPDATE August 1st, 2024

########################################################################################################

#Import Data ----


#1. Outliers ----

# In our case, want to identify values that are more than 3 standard deviations away from the variable's mean

##Identify variables to check----

outlier_variables <- c(
  "A1_1", #household size
  "H4_2", "H5_4", "H6_2", "H8_7", #energy spending and lightbulb usage
  "J1_final", "J2_1", "J3_1", "J4_2", "J5_2"#WTP
 "F1_1"#mobile phone ownership
)


# Note — This can also be done by running:
#   outlier_variables <- survey_data %>%
#       select(
#           starts_with("inc_"), starts_with("crp10a_c1_")
#       ) %>%
#       names()



##Check for variables outside of expected range----
outlier_check <- outlier_variables %>%
  map_dfr(
    ~ {
      mean_value <- hfc_constr %>%
        summarise(mean = mean(!!sym(.x), na.rm = TRUE)) %>%
        pull(mean)
      
      sd_value <- hfc_constr %>%
        summarise(sd = sd(!!sym(.x), na.rm = TRUE)) %>%
        pull(sd)
      
      low_limit <- mean_value - 3 * sd_value
      high_limit <- mean_value + 3 * sd_value
      
      hfc_constr %>%
        filter(!!sym(.x) < low_limit | !!sym(.x) > high_limit) %>%
        mutate(
          issue_var = .x,
          mean = round(mean_value, digits = 0),
          sd = round(sd_value, digits = 0),
          low_limit = round(low_limit, digits = 0),
          high_limit = round(high_limit, digits = 0)
        ) %>%
        select(
          hh_id, village, enumerator, enumerator_key, issue_var, value = !!sym(.x), mean, sd, low_limit, high_limit #might need to add village_key
        )
    }
  )


outlier_check <- outlier_check %>% 
  mutate(
    issue_var = case_when(
      issue_var == "F1_1" ~ "n_tablet",
      issue_var == "J1_final" ~ "wtp_fixed",
      issue_var == "J2_1" ~ "wtp_add_appliance",
      issue_var == "J3_1" ~ "wtp_low_stability",
      issue_var == "J4_2" ~ "wtp_paygo_12",
      issue_var == "J5_2" ~ "wtp_pay_24",
      issue_var == "H4_2" ~ "candle_spending",
      issue_var == "H5_4" ~ "biomass_spending",
      issue_var == "H6_2" ~ "kerosene_spending",
      issue_var == "H8_7" ~ "n_lightbulb",
      TRUE ~ NA_character_
    )
  )

#Export Data

hfc_sheet %>%
  sheet_write(data = outlier_check, sheet = "outlier_data")

# In case googlesheets4 asks me to enter '1' to continue

1



