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
## Define outlier variables ----
outlier_variables <- c(
  "A1_1", #household size
  "H4_2", "H5_4", "H6_2", "H8_7", #energy spending and lightbulb usage
  "J1_final", "J2_1", "J3_1", "J4_2", "J5_2", #WTP
  "F1_1" #mobile phone ownership
)

## Check for variables outside of expected range using log-transformation ----
outlier_check <- outlier_variables %>%
  map_dfr(
    ~ {
      # Log-transform variable, add 1 to avoid log(0) issues
      log_values <- log1p(hfc_constr[[.x]]) # log1p(x) calculates log(1 + x), handling zeros
      
      # Calculate log-transformed mean and SD
      mean_log <- mean(log_values, na.rm = TRUE)
      sd_log <- sd(log_values, na.rm = TRUE)
      
      # Calculate limits in log scale, then convert back to original scale
      low_limit <- exp(mean_log - 3 * sd_log) - 1
      high_limit <- exp(mean_log + 3 * sd_log) - 1
      
      # Filter and select variables outside of these limits
      hfc_constr %>%
        filter(hfc_constr[[.x]] < low_limit | hfc_constr[[.x]] > high_limit) %>%
        mutate(
          issue_var = .x,
          mean = round(exp(mean_log) - 1, digits = 0), # Convert mean back to original scale
          sd = round(exp(sd_log) - 1, digits = 0), # Convert SD back to original scale
          low_limit = round(low_limit, digits = 0),
          high_limit = round(high_limit, digits = 0)
        ) %>%
        select(
          hh_id, village, enumerator, enumerator_key, issue_var, value = !!sym(.x), mean, sd, low_limit, high_limit # might need to add village_key
        )
    }
  )

## Update variable names in outlier_check ----
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



