

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


######################################################################
#Basic Cleaning
#########################################################################

##This is the dta version, with the number
rosterA1 <- read_dta(file.path(data_path, "Household Data", "ROSTER_A1.dta"))
sectionA2 <- read_dta(file.path(data_path, "Household Data", "SECTION_A2.dta"))
sectionB <- read_dta(file.path(data_path, "Household Data", "SECTION_B.dta"))
sectionC1 <- read_dta(file.path(data_path, "Household Data", "SECTION_C1.dta"))
sectionC2 <- read_dta(file.path(data_path, "Household Data", "SECTION_C2.dta"))
sectionD <- read_dta(file.path(data_path, "Household Data", "SECTION_D.dta"))
sectionDE <- read_dta(file.path(data_path, "Household Data", "SECTION_DE.dta"))
sectionEF1 <- read_dta(file.path(data_path, "Household Data", "SECTION_EF1.dta"))
sectionF1 <- read_dta(file.path(data_path, "Household Data", "SECTION_F1.dta"))
sectionF2 <- read_dta(file.path(data_path, "Household Data", "SECTION_F2.dta"))
sectionG1 <- read_dta(file.path(data_path, "Household Data", "SECTION_G1.dta"))
sectionG2 <- read_dta(file.path(data_path, "Household Data", "SECTION_G2.dta"))
sectionH <- read_dta(file.path(data_path, "Household Data", "SECTION_H.dta"))
sectionI <- read_dta(file.path(data_path, "Household Data", "SECTION_I.dta"))
sectionI2J <- read_dta(file.path(data_path, "Household Data", "SECTION_I2J.dta"))
sectionKL <- read_dta(file.path(data_path, "Household Data", "SECTION_KL.dta"))
sectionMN <- read_dta(file.path(data_path, "Household Data", "SECTION_MN.dta"))

##Creat household ID for all of them
rosterA1 <- rosterA1 %>%
  mutate(HHID = HI05 * 100 + Cluster * 100 * 1000 + DISTRICT * 100 * 1000 * 100 + PROVINCE)
sectionA2 <- sectionA2 %>%
  mutate(HHID = HI05 * 100 + Cluster * 100 * 1000 + DISTRICT * 100 * 1000 * 100 + PROVINCE)
sectionB <- sectionB %>%
  mutate(HHID = HI05 * 100 + Cluster * 100 * 1000 + DISTRICT * 100 * 1000 * 100 + PROVINCE)
sectionC1 <- sectionC1 %>%
  mutate(HHID = HI05 * 100 + Cluster * 100 * 1000 + DISTRICT * 100 * 1000 * 100 + PROVINCE)
sectionC2 <- sectionC2 %>%
  mutate(HHID = HI05 * 100 + Cluster * 100 * 1000 + DISTRICT * 100 * 1000 * 100 + PROVINCE)
sectionD <- sectionD %>%
  mutate(HHID = HI05 * 100 + Cluster * 100 * 1000 + DISTRICT * 100 * 1000 * 100 + PROVINCE)
sectionDE <- sectionDE %>%
  mutate(HHID = HI05 * 100 + Cluster * 100 * 1000 + DISTRICT * 100 * 1000 * 100 + PROVINCE)
sectionEF1 <- sectionEF1 %>%
  mutate(HHID = HI05 * 100 + Cluster * 100 * 1000 + DISTRICT * 100 * 1000 * 100 + PROVINCE)
sectionF1 <- sectionF1 %>%
  mutate(HHID = HI05 * 100 + Cluster * 100 * 1000 + DISTRICT * 100 * 1000 * 100 + PROVINCE)
sectionF2 <- sectionF2 %>%
  mutate(HHID = HI05 * 100 + Cluster * 100 * 1000 + DISTRICT * 100 * 1000 * 100 + PROVINCE)
sectionG1 <- sectionG1 %>%
  mutate(HHID = HI05 * 100 + Cluster * 100 * 1000 + DISTRICT * 100 * 1000 * 100 + PROVINCE)
sectionG2 <- sectionG2 %>%
  mutate(HHID = HI05 * 100 + Cluster * 100 * 1000 + DISTRICT * 100 * 1000 * 100 + PROVINCE)
sectionH <- sectionH %>%
  mutate(HHID = HI05 * 100 + Cluster * 100 * 1000 + DISTRICT * 100 * 1000 * 100 + PROVINCE)
sectionI <- sectionI %>%
  mutate(HHID = HI05 * 100 + Cluster * 100 * 1000 + DISTRICT * 100 * 1000 * 100 + PROVINCE)
sectionI2J <- sectionI2J %>%
  mutate(HHID = HI05 * 100 + Cluster * 100 * 1000 + DISTRICT * 100 * 1000 * 100 + PROVINCE)
sectionKL <- sectionKL %>%
  mutate(HHID = HI05 * 100 + Cluster * 100 * 1000 + DISTRICT * 100 * 1000 * 100 + PROVINCE)
sectionMN <- sectionMN %>%
  mutate(HHID = HI05 * 100 + Cluster * 100 * 1000 + DISTRICT * 100 * 1000 * 100 + PROVINCE)



#Household business-----


business <- sectionA2 %>% 
  filter(HI04 == 2) %>% 
  group_by(A26A) %>% 
  summarise(n = n())

business_activity <- sectionA2 %>%
  mutate(A30 = as_factor(A30)) %>% 
  filter(!is.na(A30)) %>% # uses haven::as_factor
  group_by(A30) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = paste(round((n / sum(n)),4) * 100, "%"))


#Household saving----
total_households <- 5706

# Example for B06 (formal institutions)
 
# Count 1s in each column
formal_savings_summary <- sectionB %>%
  select(B06_1, B06_2, B06_3, B06_4, B06_5) %>%
  summarise(across(everything(),
                   ~sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "count") %>%
  mutate(
    percentage = round(100 * count / 5706, 2),
    label = sapply(variable, function(v) attr(sectionB[[v]], "label"))
  ) %>%
  select(label, variable, count, percentage)


# Count 1s in each B07 column (informal savings)
informal_savings_summary <- sectionB %>%
  select(B07_1, B07_2, B07_3, B07_4) %>%
  summarise(across(everything(),
                   ~sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "count") %>%
  mutate(
    percentage = round(100 * count / 5706, 2),
    label = sapply(variable, function(v) attr(sectionB[[v]], "label"))
  ) %>%
  select(label, variable, count, percentage)



borrowing_summary <- sectionB %>%
  select(B08_01:B08_15) %>%
  summarise(across(everything(),
                   ~sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "count") %>%
  mutate(
    percentage = round(100 * count / 5706, 2),
    label = sapply(variable, function(v) attr(sectionB[[v]], "label"))
  ) %>%
  select(label, variable, count, percentage)


#Mobile phones----
mobile_owners_summary <- sectionC1 %>%
  filter(HI04 == 2 ) %>% 
  summarise(
    count = sum(C172 > 0, na.rm = TRUE),
    percentage = round(100 * count / total_households, 2)
  )

charingspending_summary <- sectionC1 %>%
  filter(HI04 == 2 ) %>% 
  summarise(
    mean = mean(C176, na.rm = TRUE),
    sd   = sd(C176, na.rm = TRUE),
    min  = min(C176, na.rm = TRUE),
    max  = max(C176, na.rm = TRUE),
    median = median(C176, na.rm = TRUE),  )

mobile_charge_summary <- sectionC1 %>%
  filter(HI04 == 2 ) %>% 
  summarise(
    count = sum(C175 > 0, na.rm = TRUE),
    percentage = round(100 * count / total_households, 2)
  )

#Electricity./ energy usage----

main_source <- sectionC1 %>% 
  filter(HI04 == 2 ) %>% 
  mutate(C171 = as_factor(C171)) %>% 
  group_by(C171) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = paste(round((n / sum(n)),4) * 100, "%"))


#Candel and fuel-====

fuel_spending <- sectionF2 %>% 
  filter(HI04 == 2 ) %>% 
  mutate(
    F11 = as_factor(F11),  # F11 = fuel type
    F14 = as.numeric(F14), # ensure F14 is numeric (days between purchases)
    F15 = as.numeric(F15), # spending per purchase
    spending_month = ifelse(F14 > 0, F15 * (30 / F14), NA_real_)
  ) %>% 
  filter(F12 == 1) %>%  # filter for fuels used for lighting
  group_by(F11) %>% 
  summarise(
    n = n(),
    mean_spending_month = round(mean(spending_month, na.rm = TRUE), 2)
  ) %>% 
  arrange(desc(n)) %>% 
  mutate(
    percentage = paste0(round((n / 5706) * 100, 2), "%")
  )

biomass_spending <- sectionI %>%
  filter(I02 %in% c("10", "13", "15")) %>%  # Select biomass fuels
  mutate(
    I02 = as_factor(I02),
    frequency_month = case_when(
      I13 == 1 ~ 30,
      I13 == 2 ~ 4,
      I13 == 3 ~ 8,
      I13 == 4 ~ 1,
      I13 == 7 ~ NA_real_,  # Optional: handle 'Other'
      TRUE ~ NA_real_
    )
  ) %>%
  filter(I03 == 1) %>%   # Used for lighting
  mutate(
    total_amount = frequency_month * I16  # Replace I15 with your amount column
  ) %>% 
  group_by(HHID) %>% 
  summarise(
    total_amount = sum(total_amount, na.rm = TRUE)
  )


fuel_use <- sectionI %>%
  filter(HI04 == 2 ) %>% 
  mutate(I02 = as_factor(I02))  
  summarise(
    candle_count = sum(str_detect(I02, regex("candle", ignore_case = TRUE)), na.rm = TRUE),
    kerosene_count = sum(str_detect(I02, regex("kerosene", ignore_case = TRUE)), na.rm = TRUE),
    biomass_count = sum(str_detect(
      I02,
      regex("wood purchased|wood collected|animal waste|crop residue|plant biomass|sawdust|biomass briquette|pellets|processed biomass|wood chips", ignore_case = TRUE)
    ), na.rm = TRUE)
  )

#Primary lighting-------


lighting <- sectionB %>% 
    filter(HI04 == 2 ) %>% 
  mutate(B02 = as_factor(B02)) %>% 
  group_by(B02) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = paste(round((n / sum(n)),4) * 100, "%"))


#Main cookstove----

cookstove_summary <- sectionG1 %>%
    filter(HI04 == 2 ) %>% 
  filter(G26 == 1) %>%  # Only keep the main cookstove
  mutate(G03 = as_factor(G03)) %>%  # Apply labels from .dta if read with haven
  group_by(G03) %>%
  summarise(n = n()) %>%
  mutate(
    percentage = round(100 * n / 5706, 2)
  )


secondary_cookstove_summary <- sectionG1 %>%
  filter(HI04 == 2 ) %>% 
  
  filter(G26 != 1) %>%  # Only keep the main cookstove
  mutate(G03 = as_factor(G03)) %>%  # Apply labels from .dta if read with haven
  group_by(G03) %>%
  summarise(n = n()) %>%
  mutate(
    percentage = round(100 * n / 5706, 2)
  )

#Cooking time use-------

# Summarize cooking time
cooking_time_summary <- sectionKL %>%
  filter(HI04 == 2 ) %>% 
  
  mutate(
    women = K2A + K2B,
    men   = K2C + K2D
  ) %>%
  summarise(
    women_mean   = round(mean(women, na.rm = TRUE), 2),
    women_sd     = round(sd(women, na.rm = TRUE), 2),
    women_min    = min(women, na.rm = TRUE),
    women_max    = max(women, na.rm = TRUE),
    women_median = median(women, na.rm = TRUE),
    
    men_mean     = round(mean(men, na.rm = TRUE), 2),
    men_sd       = round(sd(men, na.rm = TRUE), 2),
    men_min      = min(men, na.rm = TRUE),
    men_max      = max(men, na.rm = TRUE),
    men_median   = median(men, na.rm = TRUE)
  )



#######################################################################
#Electricity and lighting
########################################################################

##Whether the household is connected to the electricity grid
table(sectionC1$C002)

# formatted_table <- join_elmobst %>%
#   count(electricity) %>%
#   mutate(electricity = ifelse(electricity == 0, "No", "Yes"),
#          frequency = n) %>%
#   select(electricity, frequency) %>%
#   kable(format = "html", table.attr = "style='border-collapse: collapse;'",
#         caption = "Household Connection to Electricity") %>%
#   kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
# 
# print(formatted_table)

##Whether the household uses electricity
table(sectionC1$C171)

# # Check for duplicated values in the HHID column
# duplicated_HHID <- duplicated(sectionC1$HHID)
# 
# # Check if there are any duplicates (i.e., any TRUE values in the duplicated_HHID vector)
# if (any(duplicated_HHID)) {
#   # If there are duplicates, print a message indicating that duplicates exist
#   cat("The HHID column contains duplicates.\n")
# } else {
#   # If there are no duplicates, print a message indicating all values are unique
#   cat("The HHID column contains only unique values.\n")
# }

##Now we are aggregating on the household_head level
household_head <- rosterA1 %>% 
  filter(A04 == 1) %>% 
  select(PROVINCE, DISTRICT, Cluster, HHID, HI04, A03, A05, A22, A07, A10, HH_WT, C_hhweight,) %>% 
  rename(province_ID = PROVINCE,
         district_ID = DISTRICT,
         urban_rural = HI04,
         sex = A03,
         age = A05,
         paid_work = A22,
         school = A07)
###Electricity cleaning
electricity <- sectionC1 %>% 
  select(C171, C002, HHID) %>% 
  rename(source = C171,
         grid = C002) %>% 
  mutate(connection = ifelse(source == 111, "0", "1"))

join_elec1 <- left_join(household_head, electricity, by= c("HHID"= "HHID"))

##Recode them out of the dta codes
join_elec <- join_elec1 %>%
  mutate(province = forcats::as_factor(province_ID),
         district = forcats::as_factor(district_ID),
         marital = forcats :: as_factor(A10),
         source = forcats ::as_factor(source)) %>% 
  rename(electricity = connection,
         cluster = Cluster,
         marital_code = A10) %>% 
  select(province, province_ID, district, district_ID, cluster, HHID, sex, age, grid, electricity, everything())

table(join_elec$electricity) %>% 
  knitr::kable()

##Lighting
#F10 main source of lighting that children Use
#F11, F12, G13, G14,G15,G16 Fuel usage for each household

fuel_exp <- sectionF2 %>% 
  select(HHID, F11, F12, F13, F14, F15, F16) %>% 
  mutate(type = forcats::as_factor(F11)) %>% 
  rename(quantity = F13,
         frequency = F14,
         expenditure = F15,
         light_percentage = F16) %>% 
  filter(F12 != 0) %>% 
  select(HHID, type, everything()) %>% 
  select(-c(F11, F12))

join_fuel <- left_join(join_elec, fuel_exp, by= c("HHID"= "HHID"))

# mutate(total_fuel = sum(F15), na.rm= TRUE)

####Calculation on frequencies

join_fuel <- join_fuel %>% 
  mutate(total_expenditure = (30/frequency)*expenditure) %>% 
  rename(monthly_expenditure = total_expenditure)

join_fuel<- join_fuel %>% 
  mutate(annual_expenditure = (expenditure/frequency)*365)

formatted1 <-  join_fuel %>%
  count(type) %>%
  kable(format = "html", table.attr = "style='border-collapse: collapse;'") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

formatted1

join_fuel <- join_fuel %>% 
  mutate(annual_expenditure_dollar = annual_expenditure*0.00086)



# Filter the data for the specific fuel_type (Candle)
candle_data <- join_fuel %>%
  filter(type == "Candle")  # Assuming '1' represents Candle
# Filter the data for the specific fuel_type (Kerosene)
kerosene_data <- join_fuel %>%
  filter(type == "Kerosene")  



#############################################################################
##Regression Analysis
###############################################

model_elec_fuel <- lm(monthly_expenditure ~ electricity + district + urban_rural + paid_work + marital_code, data = join_fuel)
summary(model_elec_fuel)

annual_elec_fuel <- lm(annual_expenditure_dollar ~ electricity + urban_rural + paid_work + marital_code, data = join_fuel)
candle_model <- lm(annual_expenditure_dollar ~ electricity + urban_rural + paid_work + marital_code, data = candle_data)
kerosene_model <- lm(annual_expenditure_dollar ~ electricity + urban_rural + paid_work + marital_code, data = kerosene_data)

stargazer(annual_elec_fuel,
          candle_model,
          kerosene_model,
          type = "html", title = "Table 1: Annual Fuel Expenditure", out = "table1.html")

feols_annual_fuel <- feols(annual_expenditure_dollar ~ electricity + paid_work + marital_code|district, data = join_fuel, cluster = "district")
feols_candle_fuel <- feols(annual_expenditure_dollar ~ electricity + paid_work + marital_code|district, data = candle_data, cluster = "district")
feols_kerosene_fuel <- feols(annual_expenditure_dollar ~ electricity + paid_work + marital_code|district, data = kerosene_data, cluster = "district")

models <- list(feols_annual_fuel <- feols(annual_expenditure_dollar ~ electricity + paid_work + marital_code|district, data = join_fuel, cluster = "district"),
               feols_candle_fuel <- feols(annual_expenditure_dollar ~ electricity + paid_work + marital_code|district, data = candle_data, cluster = "district"),
               feols_kerosene_fuel <- feols(annual_expenditure_dollar ~ electricity + paid_work + marital_code|district, data = kerosene_data, cluster = "district")
)
modelsummary(models, output = "table2.html" )





#########################################################
###Cookstove
##########################################################

cookstove <- sectionG1 %>% 
  filter(G26 == 1) %>% 
  select(HHID, G03, G04, G14, G14_OTHER, G25, G25_OTHER, G07, G16A, G17, G18, G23, starts_with("G24_"), G26)
cookstove <- cookstove %>% 
  mutate(G14.fac = forcats::as_factor(G14),
         G25.fac = forcats::as_factor(G25),
         G03.fac = forcats::as_factor(G03))

cookstove <- cookstove %>% 
  select(HHID, G03, G03.fac, G04, G14, G14.fac,G25, G25.fac, G07, G16A, G17, G18, G23)

cookstove.1 <- cookstove %>%
  mutate(
    clean_cooking = ifelse( G03 %in% c(3, 5, 6, 7, 8),1,0)
  ) 

cookstove.1 <- cookstove.1 %>% 
  mutate( injury = ifelse(as.factor(G25) != "NA", 1, 0))

cookstove.1 <- cookstove.1 %>% 
  mutate(G03.fac = forcats :: as_factor(G03)) %>% 
  rename(cookstove_ID = G03,
         cookstove_type = G03.fac,
         fuel_availability = G16A,
         minute_spent = G17
         )

cookstove.2 <- cookstove.1 %>%
  select(-matches("^G"))

cookstove.2 <- cookstove.2 %>% 
  mutate(availability.fac = forcats::as_factor(fuel_availability))

cookstove.2 <- cookstove.2 %>% 
  select(HHID, clean_cooking, cookstove_type, injury, minute_spent, fuel_availability, availability.fac)


# # Check for duplicated values in the HHID column
# duplicated_HHID <- duplicated(cookstove.1$HHID)
# 
# # Check if there are any duplicates (i.e., any TRUE values in the duplicated_HHID vector)
# if (any(duplicated_HHID)) {
#   # If there are duplicates, print a message indicating that duplicates exist
#   cat("The HHID column contains duplicates.\n")
# } else {
#   # If there are no duplicates, print a message indicating all values are unique
#   cat("The HHID column contains only unique values.\n")
# }

join_fuel_stove <- left_join(join_fuel, cookstove.2, by = c("HHID" = "HHID"))


table(join_fuel_stove$clean_cooking)
table(join_fuel_stove$cookstove_type)


#############################################################################
##Regression Analysis
###############################################

model_elec_stove <- lm(clean_cooking ~ electricity +  urban_rural + paid_work + marital_code, data = join_fuel_stove)
stargazer(model_elec_stove, type = "html", title = "Table 4: Clean cooking stove", out = "table4-cleanstove.html")

# # The coefficient for the electricity1 predictor is 0.221335. This means that, holding other variables constant, having electricity1
# # is associated with 22.13% more possibility to have clean cooking stove.  
# 
# stove_injury <- lm(injury ~ clean_cooking + cookstove_type + electricity + district + urban_rural + paid_work + marital_code, data = join_elec_stove)
# summary(stove_injury)
# 
# stove_injury.1 <- lm(injury ~ clean_cooking + cookstove_type, data = cookstove.2)
# summary(stove_injury.1)
# #If the household have clean_cooking stove, it doesn't seem to be closely related to injury 


time_spent <- lm(minute_spent ~ electricity + cookstove_type + district + urban_rural + paid_work + marital_code, data = join_elec_stove)
summary(time_spent)

#With electricity, household members spend 2.05 more minute preparing this stove and fuel
#This is statistically significant??

stove_time <- lm(minute_spent ~ clean_cooking, data = cookstove.2)
summary(stove_time)

##Clean cooking shows 1.69 more minute spent on household member preparing this stove and fuel. 
#This is statistically significant??








##############################################################################
##Mobile phone usage
##################################################################################

mobile_usage <- sectionC1 %>% 
  select(C172, C173, C174, C175, C176, C177, C058, C019, HHID) %>% 
  rename(mobile_num = C172,
         smart_phone = C173,
         charge_home = C174,
         charge_outside = C175,
         charge_expenditure = C176,
         internet = C177,
         minigrid_pay = C058,
         nationalgrid_pay = C019)

join_elmobst <- left_join(join_fuel_stove, mobile_usage, by = c("HHID" = "HHID"))


###########################################################################
##Descriptive statistics
############################################################################

mean_by_electricity <- join_elmobst %>%
  group_by(electricity) %>%
  summarise(mean_mobile_num = mean(mobile_num, na.rm = TRUE))

print(mean_by_electricity)


mean(join_elmobst$charge_expenditure, na.rm = TRUE)

mean_charge_by_electricity <- join_elmobst %>%
  group_by(electricity) %>%
  summarise(mean_charge_outside = mean(charge_outside, na.rm = TRUE))

print(mean_charge_by_electricity)

mean_expenditure_by_electricity <- join_elmobst %>%
  group_by(electricity) %>%
  summarise(mean_charge_expenditure = mean(charge_expenditure, na.rm = TRUE))

print(mean_expenditure_by_electricity)



table(join_elmobst$minigrid_pay) %>% 
  knitr :: kable()

table(join_elmobst$nationalgrid_pay) %>% 
  knitr::kable()


join_elmobst <- join_elmobst %>% 
  rename(charge_home.code = charge_home,
         charge_outside.code = charge_outside,
         minigrid_pay.code = minigrid_pay,
         nationalgrid_pay.code = nationalgrid_pay) %>% 
  mutate(charge_home = forcats::as_factor(charge_home.code),
         charge_outside = forcats :: as_factor(charge_outside.code),
         minigrid_pay = forcats :: as_factor(minigrid_pay.code),
         nationalgrid_pay = forcats :: as_factor(nationalgrid_pay.code))

join_elmobst <- join_elmobst %>% 
  mutate(charge_outside = as.numeric(charge_outside))

#############################################################################
##Regression Analysis
###############################################################################

join_elmobst <- join_elmobst %>% 
  mutate(charge_expenditure_dollar = charge_expenditure*0.00086*12)
mobile <- lm(charge_expenditure_dollar ~ electricity + urban_rural + paid_work + marital_code, data = join_elmobst)
stargazer(mobile, type = "html", title = "Table 3: Outside Phone Charging Annual Expenditure and Electricity", out = "table3.html")

#electricity1        390.514    252.082   1.549   0.1216  





###############################################################
#Time use
#######################################################################

time_use.1 <- sectionKL  %>% 
  select(HHID, ends_with("A"), ends_with("C"))

time_use <- time_use.1 %>% 
  rename(spaceheater_women = K1A,
         spaceheater_men = K1C,
         fuel_prep_women = K2A,
         fuel_prep_men = K2C,
         children_women= K3A,
         children_men = K3C,
         chlidschool_women = K4A,
         childschool_men = K4C,
         workoutside_women = K5A,
         workdoutside_men = K5C,
         entertainment_women = K6A,
         entertainment_men= K6C,
         study_women = K7A,
         study_men = K7C,
         news_women = K8A,
         news_men = K8C,
         TV_women = K9A,
         TV_men = K9C,
         back_women = L9A,
         cookinj_women = L7A,
         poison_women = L6A,
         severeburn_women = L8A,
         back_men = L9C,
         cookinj_men = L7C,
         poison_men = L6C,
         severeburn_men = L8C
         )

time_use <- time_use %>% 
  select(HHID, spaceheater_women,spaceheater_men,
         fuel_prep_women, fuel_prep_men,
         children_women, children_men,
         chlidschool_women, childschool_men,
         workoutside_women, workdoutside_men,
         entertainment_women, entertainment_men,
         study_women, study_men,
         news_women, news_men,
         TV_women, TV_men,
         back_women, back_men,
         cookinj_women,cookinj_men,
         poison_women, poison_men,
         severeburn_women, severeburn_men) 
  



summarize_table <- time_use %>% 
  summarize(spaceheater_women = round(mean(spaceheater_women, na.rm = TRUE), 2),
            spaceheater_men = round(mean(spaceheater_men, na.rm = TRUE), 2),
            fuel_prep_women = round(mean(fuel_prep_women, na.rm = TRUE), 2), 
            fuel_prep_men = round(mean(fuel_prep_men, na.rm = TRUE), 2),
            children_women = round(mean(children_women, na.rm = TRUE), 2), 
            children_men = round(mean(children_men, na.rm = TRUE), 2),
            chlidschool_women = round(mean(chlidschool_women, na.rm = TRUE), 2), 
            childschool_men = round(mean(childschool_men, na.rm = TRUE), 2),
            workoutside_women = round(mean(workoutside_women, na.rm = TRUE), 2), 
            workdoutside_men = round(mean(workdoutside_men, na.rm = TRUE), 2),
            entertainment_women = round(mean(entertainment_women, na.rm = TRUE), 2), 
            entertainment_men = round(mean(entertainment_men, na.rm = TRUE), 2),
            study_women = round(mean(study_women, na.rm = TRUE), 2), 
            study_men = round(mean(study_men, na.rm = TRUE), 2),
            news_women = round(mean(news_women, na.rm = TRUE), 2), 
            news_men = round(mean(news_men, na.rm = TRUE), 2),
            TV_women = round(mean(TV_women, na.rm = TRUE), 2), 
            TV_men = round(mean(TV_men, na.rm = TRUE), 2),
            back_women = round(sum(back_women, na.rm = TRUE), 2), 
            back_men = round(sum(back_men, na.rm = TRUE), 2),
            cookinj_women = round(sum(cookinj_women, na.rm = TRUE), 2),
            cookinj_men = round(sum(cookinj_men, na.rm = TRUE), 2),
            poison_women = round(sum(poison_women, na.rm = TRUE), 2),
            poison_men = round(sum(poison_men, na.rm = TRUE), 2),
            severeburn_women = round(sum(severeburn_women, na.rm = TRUE), 2),
            severeburn_men = round(sum(severeburn_men, na.rm = TRUE), 2))


sum_table <- summarize_table %>%
  pivot_longer(cols = everything(), names_to = "column_name", values_to = "mean_value")

# Display the long format sum table
print(sum_table)
summary(summarized_data$cookinj_women)




##########################################################################
##Account Ownership
################################################################################

account_ownership <- sectionMN %>% 
  select(HHID,N01, N02)

account_ownership <- account_ownership %>% 
  mutate(ownership = forcats::as_factor(N02))

join_elmobstac <- left_join(join_elmobst, account_ownership, by = c("HHID" = "HHID"))

table(join_elmobstac$ownership)













#################################################################################
###Ubudehe
##################################################################

ubudehe_categories <- sectionB %>% 
  select(HHID, B13)

join.1 <- left_join(join_elmobstac, ubudehe_categories, by = c("HHID" = "HHID"))

join_clean <- join.1 %>% 
  select(province, district, cluster, HHID, B13, everything()) %>% 
  rename(ubudehe = B13)

join_clean <- join_clean %>% 
  mutate(electricity = as.numeric(electricity))

model_elec <- feols(electricity ~ ubudehe |cluster, data = join_clean)

join_clean$ubudehe <- as.factor(join_clean$ubudehe)


#################################################################################
#Solar as the main source join the main source
############################################################################

sectionC.1<- sectionC1 %>% 
  select(HHID, C171) %>% 
  mutate(main_energy.code = C171,
         main_energy = forcats::as_factor(C171)) %>% 
  select(HHID, main_energy.code, main_energy)


join_clean.1 <- left_join(join_clean, sectionC.1, by = c("HHID" = "HHID"))

join_clean <- join_clean.1

join_clean <- join_clean %>% 
  mutate(Solar = ifelse(str_detect(main_energy, "Solar"), 1, 0))

join_clean_inter <- join_clean%>% 
  select(HHID, Solar)

join_clean <- left_join(join_clean.1, join_clean_inter, by = c("HHID" = "HHID"))

#############################################
#Regression solar and ubudehe
####################################

solar <- join_clean %>% filter(Solar == 1)

# Perform linear regression
regression_model <- lm(Solar ~ ubudehe, data = join_clean)

# Print regression summary
summary(regression_model)

stargazer(regression_model,
          type = "html", title = "Table 10: Ubudehe and Solar", out = "table10.html")

fixed_model <- feols(Solar ~ ubudehe|cluster, data = join_clean)

summary(fixed_model)

modelsummary(fixed_model, 
             stars = TRUE,
             type = "html", 
             title = "Table 11: Ubudehe and Solar with FE", out = "table11.html")







##############################################################################
#Expenditure Clean
#####################################################################

join_clean.2 <- join_clean %>% 
  mutate(candle_expenditure = round(ifelse(type == "Candle", annual_expenditure_dollar, 0),2),
         kerosene_expenditure = round(ifelse(type == "Kerosene", annual_expenditure_dollar, 0),2),
         annual_expenditure_dollar = round(annual_expenditure_dollar,2))

join_clean <- join_clean.2 %>% 
  mutate(candle_expenditure = ifelse(is.na(candle_expenditure), 0, candle_expenditure),
         kerosene_expenditure = ifelse(is.na(kerosene_expenditure), 0, kerosene_expenditure),
         annual_expenditure_dollar = ifelse(is.na(annual_expenditure_dollar), 0, annual_expenditure_dollar),
         annual_expenditure = ifelse(is.na(annual_expenditure), 0, annual_expenditure))

join_clean <- join_clean %>% 
  mutate(charge_expenditure_dollar = ifelse(is.na(charge_expenditure), 0, charge_expenditure*0.00086))

join_clean <- join_clean %>% 
  rename(all_fuel_exp = annual_expenditure_dollar)

         
join_clean <- join_clean %>%          
  rename(candle_exp = candle_expenditure,
         kerosene_exp = kerosene_expenditure)







####################################
#Regression Analysis
########################################


#####Electricity status
model <- lm(electricity ~ ubudehe, data = join_clean)
summary(model)

stargazer(model,
          type = "html", title = "Table 5: Ubudehe categories on electricity", out = "table5.html")

model_fe <- feols(electricity ~ ubudehe|cluster, data = join_clean)

modelsummary(model_fe, stars = TRUE, title = "Table 6. Ubudehe categories on electricity with cluster FE", output = "table6.html" )

model_fe_interaction <- feols(electricity ~ ubudehe + ubudehe * electricity | cluster, data = join_clean)
summary(model_fe_interaction)





###Fuel annual expenditure#########################################################

# model <- lm(annual_expenditure_dollar ~ ubudehe + electricity, data = join_clean)
# summary(model)
# 
# stargazer(model,
#           type = "html", title = "Table 7: Ubudehe categories and electricity on fuel expenditure", out = "table7.html")

# model_interaction <- lm(annual_expenditure ~ ubudehe + electricity + ubudehe*electricity, data = join_clean)
# summary(model_interaction)
# 
# stargazer(model_interaction,
#           type = "html", title = "Table 6a: Ubudehe categories and electricity on fuel expenditure(interacted)", out = "table6a.html")
# 
# 
# model_fe <- feols(annual_expenditure ~ ubudehe+electricity + ubudehe*electricity|cluster, data = join_clean)
# summary(model_fe)
# 
# modelsummary(model_fe, output = "table5b.html" )
# 
# model_fe_interaction <- feols(electricity ~ ubudehe + ubudehe * electricity | cluster, data = join_clean)
# summary(model_fe_interaction)




#############################################################################
##Expenditure
#####################################


#######Annual expenditure 


################################################################################
##Regression Analysis
###############################################################################

annual_elec_fuel <- lm(all_fuel_exp ~ electricity + urban_rural + paid_work + marital_code, data = join_clean)
candle_model <- lm(candle_exp ~ electricity + urban_rural + paid_work + marital_code, data = join_clean)
kerosene_model <- lm(kerosene_exp ~ electricity + urban_rural + paid_work + marital_code, data = join_clean)


# 
# # Specify the titles for each regression
# fuel_models<- list("expallfuel" = annual_elec_fuel,
#                 "expcandle" = candle_model, 
#                 "expkerosene" = kerosene_model)

# Generate regression tables with titles
stargazer(fuel_models,
          type = "html",
          title = "Table 1: Annual Fuel Expenditure (in dollar) (NAs to 0)",
          out = "table1b_5706.html")


######With fixed effects##########

feols_annual_fuel <- feols(all_fuel_exp ~ electricity + paid_work + marital_code|cluster, data = join_clean, cluster = "cluster")
feols_candle_fuel <- feols(candle_exp ~ electricity + paid_work + marital_code|cluster, data = join_clean, cluster = "cluster")
feols_kerosene_fuel <- feols(kerosene_exp ~ electricity + paid_work + marital_code|cluster, data = join_clean, cluster = "cluster")

models <- list("expallfuel" = feols_annual_fuel ,
               "expcandle" = feols_candle_fuel,
               "expkerosene" = feols_kerosene_fuel)


modelsummary(models, output = "table2b.html",
             title = "Table 2: Annual Fuel Expenditure (in dollar) with Cluster Fixed Effects (NAs to 0)",
             stars = TRUE
             )


############################################################
#Same but adding ubudehe
#################################################################


# join_clean <- join_clean %>% 
#   mutate(annual_expenditure = ifelse(is.na(annual_expenditure),0, annual_expenditure),
#          annual_expenditure_dollar = ifelse(is.na(annual_expenditure_dollar),0, annual_expenditure_dollar))
# 
# candle_data_ubu <- join_clean %>% 
#   filter(type == "Candle")
# 
# kerosene_data_ubu <- join_clean %>% 
#   filter(type == "Kerosene")


######Without Interaction Term

annual_elec_fuel.1 <- lm(all_fuel_exp ~ ubudehe + electricity, data = join_clean)
candle_model.1 <- lm(candle_exp ~ ubudehe + electricity, data = join_clean)
kerosene_model.1 <- lm(kerosene_exp ~ ubudehe + electricity, data =join_clean)

# Specify the titles for each regression
titles <- c("expallfuel", "expcandle", "expkerosene")

# Generate regression tables with titles
stargazer(annual_elec_fuel.1,
          candle_model.1,
          kerosene_model.1,
          type = "html",
          title = "Table 7: Annual Fuel Expenditure (in dollar)(NAs to 0)",
          column.labels = titles,
          out = "table7b.html")













###With interaction term
annual_elec_fuel.1 <- lm(all_fuel_exp ~ ubudehe + electricity + ubudehe*electricity, data = join_clean)
candle_model.1 <- lm(candle_exp ~ ubudehe + electricity + ubudehe*electricity, data = join_clean)
kerosene_model.1 <- lm(kerosene_exp ~ ubudehe + electricity + ubudehe*electricity, data = join_clean)

# Specify the titles for each regression
titles <- c("expallfuel", "expcandle", "expkerosene")

# Generate regression tables with titles
stargazer(annual_elec_fuel.1,
          candle_model.1,
          kerosene_model.1,
          type = "html",
          title = "Table 8: Annual Fuel Expenditure (in dollar)(NAs to 0)",
          column.labels = titles,
          out = "table8b.html")


######With fixed effects##########

feols_annual_fuel.1 <- feols(all_fuel_exp ~ electricity + ubudehe|cluster, data = join_clean, cluster = "cluster")
feols_candle_fuel.1 <- feols(candle_exp ~ electricity + ubudehe|cluster, data = join_clean, cluster = "cluster")
feols_kerosene_fuel.1 <- feols(kerosene_exp ~ electricity + ubudehe|cluster, data = join_clean, cluster = "cluster")

models <- list("expallfuel" = feols_annual_fuel.1 ,
               "expcandle" = feols_candle_fuel.1,
               "expkerosene" = feols_kerosene_fuel.1)


modelsummary(models, output = "table9a.html",
             title = "Table 9: Annual Fuel Expenditure (in dollar) with Cluster Fixed Effects (NAs to 0)",
             stars = TRUE)








#################################################################################
##Running new models aaaaa
##############################################################################
#############################################
#Cleaning the dataset yet once again
############################################


join_clean <- join_clean %>% 
  mutate(no_electricity = ifelse(electricity == 1,0,1))
  
join_clean <- join_clean %>% 
  filter(ubudehe != 888)


write_csv(join_clean, path = here("MTF_join_clean_v1.csv"))  
  
  




################################################################################
#Adding other fuel expenditures
###############################################################################

household_fuel <- sectionI %>% 
  filter(I03 == 1) %>% 
  select(HHID, I02, I13, I13_OTHER, I16) %>% 
  mutate(I02 = forcats::as_factor(I02),
         I13_OTHER = forcats::as_factor(I13_OTHER),
         I13 = forcats:: as_factor(I13))

household_fuel <- household_fuel %>% 
  mutate(I13 = as.factor(I13),
         I13_OTHER = as.factor(I13_OTHER))

household_fuel <- household_fuel %>% 
  filter(!is.na(I16))

household_fuel <- household_fuel %>% 
  mutate()

table(household_fuel$I13)

household_fuel$I13_new <- ifelse(household_fuel$I13 == "Daily", 1,
                                 ifelse(household_fuel$I13 == "Weekly", 7,
                                        ifelse(household_fuel$I13 == "Twice a week", 3.5,
                                               ifelse(household_fuel$I13 == "Monthly", 30,
                                                      ifelse(household_fuel$I13 == "Other, specify", household_fuel$I13, NA)))))
table(household_fuel$I13_OTHER)


# Create a data frame with two columns: I13_OTHER and I13_new_value
mapping_df <- data.frame(
  I13_OTHER = c("3 per year", "21 days", "A year", "after harvest", "every 2 days", "every 4 days", "once in 4 months", "twice a week", "3 months", "every 3 months", "4 months", "after 2 weeks", "every 3 days", "twice in a month", "1,5 months", "3 per month", "after 3 months", "every two months", "in 18 days", "once in 2 days"),
  I13_new_value = c(121.67, 21, 365, 182.5, 2, 4, 120, 3.5, 90, 90, 120, 14, 3, 182.5, 45, 10, 90, 60, 18, 2)
)

# Merge the household_fuel data frame with the mapping_df based on I13_OTHER
household_fuel.1 <- merge(household_fuel, mapping_df, by = "I13_OTHER", all.x = TRUE)

household_fuel.2 <- household_fuel.1 %>% 
  mutate(I13_new = ifelse(!is.na(I13_new_value), I13_new_value, I13_new)) %>% 
  mutate(I13_new = as.numeric(round(I13_new, 1))) %>% 
  rename(frequency = I13_new,
         expenditure = I16,
         fuel_type = I02) %>% 
  select(-I13, -I13_new_value, -I13_OTHER) %>% 
  mutate(annual_expenditure = expenditure*frequency/365) %>% 
  mutate(annual_dollar = annual_expenditure*0.00086)

result_table <- household_fuel.2 %>%
  group_by(fuel_type) %>%
  summarize(count = n())


household_fuel.2 <- household_fuel.2 %>% 
  mutate(charcoal_exp = round(ifelse(fuel_type %in% c("Charcoal",  "Coal Briquette") , annual_dollar, 0),2),
         wood_exp = round(ifelse(fuel_type == "Wood Purchased", annual_dollar, 0),2),
         gas_exp = round(ifelse(fuel_type == "LPG/ cooking gas", annual_dollar, 0),2),
         biomass_exp = round(ifelse(fuel_type == "Crop Residue/ Plant Biomass", annual_dollar, 0),2),
         processed_biomass_exp = round(ifelse(fuel_type == "Pellets/ processed biomass/ wood chips", annual_dollar, 0),2),
         kerosene_exp.2 =  round(ifelse(fuel_type == "Kerosene", annual_dollar, 0),2) )

if (all(!duplicated(household_fuel.2$HHID))) {
  print("yes")
} else {
  print("no")
}

###########Merging it with the master join_clean

##Charcoal
charcoal_exp <- household_fuel.2 %>% 
  filter(fuel_type %in% c("Charcoal", "Coal Briquette")) %>% 
  select(HHID, charcoal_exp)
charcoal_exp_clean <- charcoal_exp %>% 
  group_by(HHID) %>%
  summarize(charcoal_exp = sum(charcoal_exp))


if (all(!duplicated(join_clean$HHID))) {
  print("yes")
} else {
  print("no")
}




join_clean.1 <- join_clean

HHID <- join_clean %>% 
  select(HHID)

fuel_prep_join <- left_join(HHID, charcoal_exp_clean, by = c("HHID" = "HHID"))



test_join_clean <- join_clean %>% 
  select(HHID, charcoal_exp)
