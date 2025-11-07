library(tidyverse)
library(dplyr)
library(here)
library(stringr)
library(haven)
library(lmtest)
library(fixest)
library(kableExtra)
library(stargazer)
library(modelsummary)

getwd()

######################################################################
#Basic Cleaning
#########################################################################

##This is the dta version, with the number
rosterA1 <- read_dta(here("Household Data DTA", "ROSTER_A1.dta"))
sectionA2 <- read_dta(here("Household Data DTA", "SECTION_A2.dta"))
sectionB <- read_dta(here("Household Data DTA", "SECTION_B.dta"))
sectionC1 <- read_dta(here("Household Data DTA", "SECTION_C1.dta"))
sectionC2 <- read_dta(here("Household Data DTA", "SECTION_C2.dta"))
sectionD <- read_dta(here("Household Data DTA", "SECTION_D.dta"))
sectionDE <- read_dta(here("Household Data DTA", "SECTION_DE.dta"))
sectionEF1 <- read_dta(here("Household Data DTA", "SECTION_EF1.dta"))
sectionF1 <- read_dta(here("Household Data DTA", "SECTION_F1.dta"))
sectionF2 <- read_dta(here("Household Data DTA", "SECTION_F2.dta"))
sectionG1 <- read_dta(here("Household Data DTA", "SECTION_G1.dta"))
sectionG2 <- read_dta(here("Household Data DTA", "SECTION_G2.dta"))
sectionH <- read_dta(here("Household Data DTA", "SECTION_H.dta"))
sectionI <- read_dta(here("Household Data DTA", "SECTION_I.dta"))
sectionI2J <- read_dta(here("Household Data DTA", "SECTION_I2J.dta"))
sectionKL <- read_dta(here("Household Data DTA", "SECTION_KL.dta"))
sectionMN <- read_dta(here("Household Data DTA", "SECTION_MN.dta"))

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


join_clean <- read.csv(here("MTF_join_clean_v1.csv"))






###########################################################################
#Cleaning
################################################

join_clean<- join_clean %>% 
  rename(solar = Solar)

join_clean <- join_clean %>% 
  mutate(ubudehe = as.factor(ubudehe))

join_clean<- join_clean %>% 
  rename(mobile_exp = charge_expenditure_dollar)

join_clean <- join_clean %>% 
  select(-province_ID, -district_ID)

join_clean <- join_clean %>% 
  filter(ubudehe != 888)

clean_source <- sectionC1 %>% 
  select(HHID, C171)

clean_source <- clean_source %>% 
  mutate(solar = ifelse(C171 %in% c(4, 5, 6), 1, 0),
         grid = ifelse(C171 %in% c(1, 2), 1, 0),
         no_elec = ifelse(C171 %in% c(7, 8, 111), 1, 0))

###Test of mutual exclusiveness

mutually_exclusive <- all(
  clean_source$solar + clean_source$grid + clean_source$no_elec == 1
)

if (mutually_exclusive) {
  cat("The columns solar, grid, and no_elec are mutually exclusive.\n")
} else {
  cat("The columns solar, grid, and no_elec are not mutually exclusive.\n")
}

clean_source.1 <- clean_source

clean_source <- clean_source.1 %>% 
  select(-C171)

join_clean <- join_clean %>% 
  select(-grid, -no_electricity, -solar)

join_clean <- left_join(join_clean, clean_source, by = c("HHID" = "HHID"))





##########################################################
#Household_fuel
#################################################################

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
  mutate(annual_expenditure = expenditure/frequency*365) %>% 
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





####################################################
#Merging it with the master join_clean
############################################################################

##Charcoal
charcoal_exp <- household_fuel.2 %>% 
  filter(fuel_type %in% c("Charcoal", "Coal Briquette")) %>% 
  select(HHID, charcoal_exp)
charcoal_exp_clean <- charcoal_exp %>% 
  group_by(HHID) %>%
  summarize(charcoal_exp = sum(charcoal_exp))


if (all(!duplicated(charcoal_exp_clean$HHID))) {
  print("yes")
} else {
  print("no")
}

####Join with main dataset
join_clean.1 <- join_clean
join_clean <- left_join(join_clean, charcoal_exp_clean, by = c("HHID" = "HHID"))

if (all(!duplicated(join_clean$HHID))) {
  print("yes")
} else {
  print("no")
}


##wood
wood_exp <- household_fuel.2 %>% 
  filter(fuel_type %in% c("Wood Purchased")) %>% 
  select(HHID, wood_exp)

wood_exp_clean <- wood_exp %>% 
  group_by(HHID) %>%
  summarize(wood_exp = sum(wood_exp))


if (all(!duplicated(wood_exp_clean$HHID))) {
  print("yes")
} else {
  print("no")
}

####Join with main dataset
join_clean <- left_join(join_clean, wood_exp_clean, by = c("HHID" = "HHID"))

if (all(!duplicated(join_clean$HHID))) {
  print("yes")
} else {
  print("no")
}



##gas
gas_exp <- household_fuel.2 %>% 
  filter(fuel_type %in% c("LPG/ cooking gas")) %>% 
  select(HHID, gas_exp)

gas_exp_clean <- gas_exp %>% 
  group_by(HHID) %>%
  summarize(gas_exp = sum(gas_exp))


if (all(!duplicated(gas_exp_clean$HHID))) {
  print("yes")
} else {
  print("no")
}

####Join with main dataset
join_clean <- left_join(join_clean, gas_exp_clean, by = c("HHID" = "HHID"))

if (all(!duplicated(join_clean$HHID))) {
  print("yes")
} else {
  print("no")
}




##biomass

household_fuel.2 <- household_fuel.2 %>% 
  mutate(biomass_exp = biomass_exp + processed_biomass_exp)


biomass_exp <- household_fuel.2 %>% 
  filter(fuel_type %in% c("LPG/ cooking biomass", "Pellets/ processed biomass/ wood chips")) %>% 
  select(HHID, biomass_exp)

biomass_exp_clean <- biomass_exp %>% 
  group_by(HHID) %>%
  summarize(biomass_exp = sum(biomass_exp))


if (all(!duplicated(biomass_exp_clean$HHID))) {
  print("yes")
} else {
  print("no")
}

####Join with main dataset
join_clean <- left_join(join_clean, biomass_exp_clean, by = c("HHID" = "HHID"))

if (all(!duplicated(join_clean$HHID))) {
  print("yes")
} else {
  print("no")
}



##kerosene.2

kerosene_exp.2 <- household_fuel.2 %>% 
  filter(fuel_type %in% c("Kerosene")) %>% 
  select(HHID, kerosene_exp.2)


if (all(!duplicated(kerosene_exp.2$HHID))) {
  print("yes")
} else {
  print("no")
}

####Join with main dataset
join_clean <- left_join(join_clean, kerosene_exp.2, by = c("HHID" = "HHID"))

if (all(!duplicated(join_clean$HHID))) {
  print("yes")
} else {
  print("no")
}


# join_clean <- join_clean.1

####################################################################
##Some cleaning
############################################################################

join_clean <- join_clean %>% 
  rename(kerosene2_exp = kerosene_exp.2) %>% 
  mutate(across(ends_with("_exp"), ~ ifelse(is.na(.), 0, as.numeric(.))))

join_clean <- join_clean %>% 
  mutate(kerosene_exp_all = kerosene_exp + kerosene2_exp) 
 
  
join_clean <- join_clean %>%  
   select(-kerosene_exp, -kerosene2_exp) %>% 
  rename(kerosene_exp = kerosene_exp_all)



write_csv(join_clean, here("MTF_join_clean_fuel.csv"))








#############################################################################
##Regression Analysis
##########################################################################




###With interaction term
annual_elec_fuel <- lm(all_fuel_exp ~ solar + grid + ubudehe + (solar + grid)*ubudehe, data = join_clean)
candle_model <- lm(candle_exp ~ solar + grid + ubudehe + (solar + grid)*ubudehe, data = join_clean)
kerosene_model <- lm(kerosene_exp ~ solar + grid + ubudehe + (solar + grid)*ubudehe, data = join_clean)
charcoal_model <- lm(charcoal_exp ~ solar + grid + ubudehe +  (solar + grid)*ubudehe, data = join_clean)
wood_model <- lm(wood_exp ~ solar + grid + ubudehe + (solar + grid)*ubudehe, data = join_clean)
gas_model <- lm(gas_exp ~ solar + grid + ubudehe + (solar + grid)*ubudehe, data = join_clean)
biomass_model <- lm(biomass_exp ~ solar + grid + ubudehe + (solar + grid)*ubudehe, data = join_clean)




# Specify the titles for each regression
titles <- c("expallfuel", "expcandle", "expkerosene")

# Generate regression tables with titles
stargazer(
          kerosene_model,
          charcoal_model,
          wood_model,
          gas_model,
          biomass_model,
          type = "html",
          title = "Table 13: Annual Fuel Expenditure",
          out = "table13.html")

table(join_clean$ubudehe, join_clean$gas_exp == 0)



###With interaction term and clean cook stove

kerosene_model.1 <- lm(kerosene_exp ~ clean_cooking + ubudehe + clean_cooking*ubudehe, data = join_clean)
charcoal_model.1 <- lm(charcoal_exp ~ clean_cooking + ubudehe +  clean_cooking*ubudehe, data = join_clean)
wood_model.1 <- lm(wood_exp ~ clean_cooking + ubudehe + clean_cooking*ubudehe, data = join_clean)
gas_model.1 <- lm(gas_exp ~ clean_cooking + ubudehe + clean_cooking*ubudehe, data = join_clean)
biomass_model.1 <- lm(biomass_exp ~ clean_cooking + ubudehe + clean_cooking*ubudehe, data = join_clean)

# Generate regression tables with titles
stargazer(
  charcoal_model.1,
  wood_model.1,
  gas_model.1,
  biomass_model.1,
  type = "html",
  title = "Table 14: Fuel Expenditure ~ Clean cooking",
  out = "table14.html")

###With interaction term and clean cook stove

kerosene_model.1 <- lm(kerosene_exp ~ clean_cooking + ubudehe, data = join_clean)
charcoal_model.1 <- lm(charcoal_exp ~ clean_cooking + ubudehe , data = join_clean)
wood_model.1 <- lm(wood_exp ~ clean_cooking + ubudehe , data = join_clean)
gas_model.1 <- lm(gas_exp ~ clean_cooking + ubudehe, data = join_clean)
biomass_model.1 <- lm(biomass_exp ~ clean_cooking + ubudehe , data = join_clean)

# Generate regression tables with titles
stargazer(
  charcoal_model.1,
  wood_model.1,
  gas_model.1,
  biomass_model.1,
  type = "html",
  title = "Table 15: Fuel Expenditure ~ Clean cooking(not interacted)",
  out = "table15.html")


table(join_clean$ubudehe, join_clean$clean_cooking)










###########################################################################################
#Tier Data
######################################################################################



Electricity <- read_dta(here("Household Data DTA", "MTF Rwanda Final Dataset_Electricity.dta"))

##Creat household ID for all of them
Electricity <- Electricity %>%
  mutate(HHID = HI05 * 100 + Cluster * 100 * 1000 + DISTRICT * 100 * 1000 * 100 + PROVINCE)

tier_data <- Electricity %>% 
  select(HHID, elc_aggr_tier, elc_aggr_tier0, elc_aggr_tier1, elc_aggr_tier3, elc_aggr_tier4, elc_aggr_tier5)

tier_data <- tier_data %>% 
  select(HHID, elc_aggr_tier) %>% 
  rename(tier = elc_aggr_tier)



join_clean.2 <- join_clean

join_clean <- left_join(join_clean, tier_data_merge, by = c("HHID" = "HHID"))

join_clean <- join_clean %>% 
  mutate(tier = as.numeric(tier))

join_clean.3 <- join_clean

join_clean <- join_clean %>% 
  filter(ubudehe != 888)

join_clean <- join_clean %>% 
  mutate(tier = tier-1)

##############################################################################
##Regression on this
########################################################################################


tier_model_fe <- feols(tier ~ ubudehe | cluster, data = join_clean)

tier_model_fe

models <- list("Tier (1-5) " = feols(tier ~ ubudehe | cluster, data = join_clean))

modelsummary(models, 
             output = "table16.html",
             title = "Table16: Tier and Ubudehe Categories",
             stars = TRUE)

####Some cleaning between factor + numeric

join_clean <- left_join(join_clean, tier_data_merge, by = c("HHID" = "HHID"))

join_clean <- join_clean %>% 
  rename(tier_num = tier.x,
         tier_fac = tier.y) %>% 
  mutate(tier_fac = as.factor(tier_fac))


models_2 <- list("Grid" = feols(grid ~ tier_fac + ubudehe| cluster, data = join_clean),
                 "Solar" = feols(solar ~ tier_fac + ubudehe| cluster, data = join_clean),
                 "No_elec" = feols(no_elec ~ tier_fac + ubudehe|cluster, data = join_clean))

modelsummary(models_2, 
             output = "table17.html",
             title = "Table17: Tier and Electricity Status",
             stars = TRUE)


