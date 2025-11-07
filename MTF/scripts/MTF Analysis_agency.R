library(tidyverse)
library(dplyr)
library(here)
library(sf)
library(ggplot2)
library(tmap)
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


join_clean <- read.csv(here("MTF_join_clean_v1.csv"))


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




###################################################################################
##Add information on the decision making and agency
########################################################################

solar_agency <- sectionC2 %>%
  select(HHID, C142) %>%
  rename(solar_device = C142) %>%
  distinct(HHID, .keep_all = TRUE)

stove_agency <- sectionG1 %>% 
  select(HHID, G06) %>% 
  rename(obtain_stove = G06) %>% 
  filter(obtain_stove != "")


enterprise_agency <- sectionA2 %>% 
  select(HHID, A27) %>% 
  mutate(A27 = ifelse(A27 == "", NA, A27)) %>% 
  rename(enterprise = A27) %>% 
  filter(!is.na(enterprise))
  
  
roster <- rosterA1 %>% 
  select(HHID, A01, A03, A04, A05) %>% 
  rename(individual_ID = A01,
         sex = A03,
         relationship = A04,
         age = A05)


##GENERATE roster ID###########################################################  
  
roster <- roster %>% 
  mutate(roster_ID = HHID*100 + individual_ID )


solar_agency <- solar_agency %>% 
  mutate(roster_ID = ifelse(nchar(as.character(solar_device)) == 2, HHID * 100 + as.numeric(solar_device), "Join"))

stove_agency <- stove_agency %>% 
  mutate(roster_ID = HHID * 100 + as.numeric(obtain_stove))

enterprise_agency <- enterprise_agency %>% 
  mutate(roster_ID = ifelse(nchar(enterprise) == 2, HHID * 100 + as.numeric(enterprise), "Join"))


###########################################################################
#Join sex
############################################################
roster.1 <- roster %>% 
  select(roster_ID, sex) %>% 
  mutate(sex = forcats::as_factor(sex)) %>% 
  mutate(roster_ID = as.character(roster_ID)) %>% 
  mutate(sex = as.character(sex))


solar_join <- left_join(solar_agency, roster.1, by = c("roster_ID" = "roster_ID")) 

solar_join <-solar_join%>% 
  mutate(sex = ifelse(sex == "join", "Join", sex )) %>% 
  mutate(solar_device = sex) %>% 
  select(-sex, -roster_ID)

#######Stove

stove_agency <- stove_agency %>% 
  mutate(roster_ID = as.factor(roster_ID))

stove_join <- left_join(stove_agency, roster.1, by = c("roster_ID" = "roster_ID")) 

stove_join <-stove_join %>% 
  mutate(obtain_stove =sex) %>% 
  select(-sex, -roster_ID)



###Enterprise

enterprise_agency <- enterprise_agency %>% 
  filter(!is.na(enterprise))

enterprise_join <- left_join(enterprise_agency, roster.1, by = c("roster_ID" = "roster_ID"))


enterprise_join <- enterprise_join %>% 
  mutate(enterprise = ifelse(is.na(sex), "Join", sex)) %>% 
  select(-roster_ID, -sex)


###################################################################
#Join to the master dataset
###################################################################################


join_clean <- left_join(join_clean, enterprise_join, by = c("HHID" = "HHID"))

join_clean <- left_join(join_clean, solar_join, by = c("HHID" = "HHID"))

join_clean <- left_join(join_clean, stove_join, by = c("HHID" = "HHID"))

table(join_clean$solar_device)

####I don't know how this happened, but this fixes it
join_clean <- join_clean %>% 
  mutate(solar_device = ifelse(solar_device == "45190150402", "Female", solar_device))


write_csv(join_clean, path = here("MTF_join_clean_v2.csv"))




















###################################################################################
##Regression
###################################################################

  
model.1 <- lm(all_fuel_exp ~ (solar+grid)*(ubudehe), data = join_clean)
model.1_candle <- lm(candle_exp ~ (solar+grid)*(ubudehe), data = join_clean)
model.1_kerosene <- lm(kerosene_exp ~ (solar+grid)*(ubudehe), data = join_clean)
model.1_mobile <- lm(mobile_exp ~ (solar+grid)*(ubudehe), data = join_clean)

stargazer(model.1,
          model.1_candle,
          model.1_kerosene,
          model.1_mobile, 
          type = "html", title = "Table 12: Annual Fuel Expenditure", out = "table12.html")

model.1a <- lm(all_fuel_exp ~ (ubudehe)*(solar+grid+no_elec), data = join_clean)
model.1a_candle <- lm(candle_exp ~ (ubudehe)*(solar+grid+no_elec), data = join_clean)
model.1a_kerosene <- lm(kerosene_exp ~ (ubudehe)*(solar+grid+no_elec), data = join_clean)
model.1a_mobile <- lm(mobile_exp ~ (ubudehe)*(solar+grid+no_elec), data = join_clean)

stargazer(model.1a,
          model.1a_candle,
          model.1a_kerosene,
          model.1a_mobile, 
          type = "html", title = "Table 12a: Annual Fuel Expenditure", out = "table12a.html")



table(join_clean$ubudehe, join_clean$solar)











