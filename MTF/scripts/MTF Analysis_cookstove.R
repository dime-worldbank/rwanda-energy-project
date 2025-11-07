###############################
#Author: Xiaoming Zhang
#Date: Feb. 12. 2024
#Purpose: run regressions on clean cooking stove related 
###################################


pacman::p_load(tidyverse, dplyr, here, stringr, haven, lmtest, fixest, kableExtra, 
               stargazer, modelsummary)

getwd()


mtf_all <- read.csv(here("outputs", "MTF_join_clean_fuel.csv"))

#Join fuel time----
fuel_time <- sectionKL %>% 
  select(K2A, K2C, HHID)

fuel_time <- fuel_time %>% 
  mutate(fuelprep_w = K2A, 
         fuelprep_m = K2C)

mtf_all <- left_join(mtf_all, fuel_time, by = c("HHID"))



#Run regressions----

mtf_all <- mtf_all %>% 
  mutate(ubudehe = as.factor(ubudehe),
         electricity = as.factor(electricity),
         solar = as.factor(solar),
         clean_cooking = as.factor(clean_cooking))

###clean_cooking----
fuelprep_clean_w <- feols(fuelprep_w ~ ubudehe + clean_cooking | cluster, data = mtf_all)

summary(fuelprep_clean_w)

fuelprep_clean_m <- feols(fuelprep_m ~ ubudehe + clean_cooking | cluster, data = mtf_all)

summary(fuelprep_clean_m)


fuelprep_clean<- list("women " = fuelprep_clean_w,
                      "men" = fuelprep_clean_m)

modelsummary(fuelprep_clean, 
             output = here("outputs", "table19.html"),
             title = "Table19: fuel prep and clean cooking",
             stars = TRUE)

###electricity----
fuelprep_electricity_w <- feols(fuelprep_w ~ ubudehe + electricity | cluster, data = mtf_all)

summary(fuelprep_electricity_w)

fuelprep_electricity_m <- feols(fuelprep_m ~ ubudehe + electricity | cluster, data = mtf_all)

summary(fuelprep_electricity_m)

fuelprep_electricity<- list("women " = fuelprep_electricity_w,
                      "men" = fuelprep_electricity_m)

modelsummary(fuelprep_electricity, 
             output = here("outputs", "table20.html"),
             title = "Table20: fuel prep and electricity",
             stars = TRUE)



###solar----

fuelprep_solar_w <- feols(fuelprep_w ~ ubudehe + solar | cluster, data = mtf_all)

summary(fuelprep_solar_w)

fuelprep_solar_m <- feols(fuelprep_m ~ ubudehe + solar | cluster, data = mtf_all)

summary(fuelprep_solar_m)

fuelprep_solar<- list("women " = fuelprep_solar_w,
                 "men" = fuelprep_solar_m)

modelsummary(fuelprep_solar, 
             output = here("outputs", "table21.html"),
             title = "Table21: fuel prep and solar energy",
             stars = TRUE)


###Combined(controling for electricity, clean_cooking  and solar)----

fuelprep_w <- feols(fuelprep_w ~ ubudehe + solar + electricity + clean_cooking | cluster, data = mtf_all)

summary(fuelprep_w)

fuelprep_m <- feols(fuelprep_m ~ ubudehe + solar + electricity + clean_cooking | cluster, data = mtf_all)

summary(fuelprep_m)

fuelprep <- list("women " = fuelprep_w,
                 "men" = fuelprep_m)

modelsummary(fuelprep, 
             output = here("outputs", "table18.html"),
             title = "Table18: fuel prep and clean cooking",
             stars = TRUE)

#Descriptive statistics:

summary(mtf_all$fuelprep_m)
