########################################################################################################
#                                                                                                      #
#            HIGH-FREQUENCY CHECKS AND PHONE TRACKING CODE -- OUTLIER HIGH-FREQUENCY CHECKS            #
#                                                                                                      #
########################################################################################################

## PURPOSE      Create an outlier high-frequency check sheet in the HFC dashboard.



## AUTHOR      Xiaoming Zhang (adapted from Adrien Ciret & Marc-Andrea Fiorina & Juliana Guerrero)

## LAST UPDATE April 17, 2024

########################################################################################################

#Import Data ----


#1. Outliers ----

# In our case, want to identify values that are more than 3 standard deviations away from the variable's mean

##Identify variables to check----

outlier_variables <- c(
  "A1_1", #household size
  "H4_2", "H5_4", "H6_2", "H8_7", #energy spending and lightbulb usage
  "J1_final", "J2_1", "J3_1", "J4_2", "J5_2", #WTP
  "F1_1" #mobile phone ownership
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
    ~ hfc_constr %>%
      mutate(
        across(
          matches(.x), ~ mean(.x, na.rm = TRUE), .names = "mean"
        ),
        across(
          matches(.x), ~ sd(.x, na.rm = TRUE), .names = "sd"
        ),
        low_limit  = mean - 3 * sd,
        high_limit = mean + 3 * sd
      ) %>%
      filter(
        !!sym(.x) < low_limit | !!sym(.x) > high_limit
      ) %>%
      mutate(
        issue_var = .x,
        across(
          mean:high_limit, ~ round(.x, digits = 0)
        )
      ) %>%
      select(
        hh_id, village, enumerator, enumerator_key, issue_var, value = matches(.x), mean, sd, low_limit, high_limit #might need to add village_key
      )
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
?case_when()

#Export Data

hfc_sheet %>%
  sheet_write(data = outlier_check, sheet = "outlier_data")

# In case googlesheets4 asks me to enter '1' to continue

1





# 
# ########################################################################################################
# 
# ## 5. Export Data for household without permanent crops or seasonal crops----
# 
# # 
# crops <-  hfc_data %>%
#   
#   select(ID_05, ID_03,enumerator_name, ID_04, SubmissionDate, id_24,
#          pl_id_06,pl_id_07,pl_id_08,pl_id_09,
#          #village, cell, sector, district,
#          d_n_1, d_01,d_61_a,b_01_1_1,b_01_1_2,b_01_1_3,
#          b_01_oth_1_1,b_01_oth_1_2,b_01_oth_1_3) %>% 
#   
#   filter(d_n_1 == 0 | d_01 == 0) %>%
#   
#   rename(permanent_crops = d_n_1,seasonal_crops = d_01,phone_number =id_24,
#          village = pl_id_06,
#          cell = pl_id_07,
#          sector = pl_id_08,
#          district = pl_id_09) %>% 
#   mutate(permanent_crops=ifelse(permanent_crops==0,'No','Yes'),
#          seasonal_crops=ifelse(seasonal_crops==0,'No','Yes'),
#          livestock = ifelse(d_61_a==0,'No','Yes')) %>% 
#   mutate(occupation_s1= case_when(b_01_1_1==1~'Agriculture on own farm',
#                                   b_01_1_1==2~'Wage farm/livestock',
#                                   b_01_1_1==3~'Petty/retail trade',
#                                   b_01_1_1==4~'Other private sector',
#                                   b_01_1_1==5~'Public works',
#                                   b_01_1_1==6~'Government/public sector',
#                                   b_01_1_1==7~'Services (incl. tourism)',
#                                   b_01_1_1==8~'Household chores/ taking care of dependants',
#                                   b_01_1_1==9~'School',
#                                   b_01_1_1==10~'Livestock keeping',
#                                   b_01_1_1==11~'casual work (non-agricultural)',
#                                   b_01_1_1==-77~b_01_oth_1_1,
#                                   TRUE~'None/Dont know'),
#          occupation_s2= case_when(b_01_1_2==1~'Agriculture on own farm',
#                                   b_01_1_2==2~'Wage farm/livestock',
#                                   b_01_1_2==3~'Petty/retail trade',
#                                   b_01_1_2==4~'Other private sector',
#                                   b_01_1_2==5~'Public works',
#                                   b_01_1_2==6~'Government/public sector',
#                                   b_01_1_2==7~'Services (incl. tourism)',
#                                   b_01_1_2==8~'Household chores/ taking care of dependants',
#                                   b_01_1_2==9~'School',
#                                   b_01_1_2==10~'Livestock keeping',
#                                   b_01_1_2==11~'casual work (non-agricultural)',
#                                   b_01_1_2==-77~b_01_oth_1_2,
#                                   TRUE~'None/Dont know'),
#          occupation_s3= case_when(b_01_1_3==1~'Agriculture on own farm',
#                                   b_01_1_3==2~'Wage farm/livestock',
#                                   b_01_1_3==3~'Petty/retail trade',
#                                   b_01_1_3==4~'Other private sector',
#                                   b_01_1_3==5~'Public works',
#                                   b_01_1_3==6~'Government/public sector',
#                                   b_01_1_3==7~'Services (incl. tourism)',
#                                   b_01_1_3==8~'Household chores/ taking care of dependants',
#                                   b_01_1_3==9~'School',
#                                   b_01_1_3==10~'Livestock keeping',
#                                   b_01_1_3==11~'casual work (non-agricultural)',
#                                   b_01_1_3==-77~b_01_oth_1_3,
#                                   TRUE~'None/Dont know')) %>% 
#   select(-c(d_61_a,b_01_1_1,b_01_1_2,b_01_1_3,b_01_oth_1_3,b_01_oth_1_2,b_01_oth_1_1))
# 
# 
# 
# 
# hfc_sheet %>%
#   
#   sheet_write(data = crops, sheet = "nocrops_data")
# 
# 1
# 
# livestock = hfc_data %>% 
#   select(ID_05, ID_03,enumerator_name, ID_04, SubmissionDate, id_24,
#          pl_id_06,pl_id_07,pl_id_08,pl_id_09,
#          #village, cell, sector, district,
#          d_61_a) %>% 
#   
#   filter(d_61_a == 0) %>%
#   
#   rename(livestock = d_61_a,phone_number =id_24,
#          village = pl_id_06,
#          cell = pl_id_07,
#          sector = pl_id_08,
#          district = pl_id_09) %>% 
#   mutate(livestock = ifelse(livestock==0,'No','Yes'))
# 
# 
# hfc_sheet %>%
#   
#   sheet_write(data = livestock, sheet = "nolivestock_data")
# 
# 1
