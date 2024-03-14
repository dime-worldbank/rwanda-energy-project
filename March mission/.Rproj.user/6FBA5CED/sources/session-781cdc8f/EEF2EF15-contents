####################
#Project name: Data Tidying
#Author: Xiaoming
#Date: 09262023
#Modifying date: 
#########################

library(tidyverse)
library(haven)
library(here)
library(dplyr)

lwh_fup2_assets <- read_dta(here("data", "LWH_FUP2_assets.dta"))
lwh_fup2_households <- read_dta(here( "data", "LWH_FUP2_households.dta"))
lwh_fup2_plots <- read_dta(here( "data", "LWH_FUP2_plots.dta"))


#Households----
####Missing variables----

lwh_fup2_households.1 <- lwh_fup2_households %>% 
  mutate(across(starts_with("INC"), ~case_when(as.numeric(.) == -99 ~ tagged_na("m"),
                                               as.numeric(.) == -88 ~ tagged_na("d"),
                                               as.numeric(.) == -66 ~ tagged_na("r"),
                                               TRUE ~ as.numeric(.))))



na_tag(lwh_fup2_households.1$INC_04)

?across()


#Assets----
####Recode----
lwh_fup2_assets <- lwh_fup2_assets %>% 
  mutate(asset = recode_factor(asset, "1" = "cow", "2" = "sheep"))


#Plots----
####Recode
lwh_fup2_plots$A_CROP
lwh_fup2_plots <- lwh_fup2_plots %>% 
  mutate(
         A_CROP = forcats::as_factor(A_CROP),
         CRP08UA = forcats::as_factor(CRP08UA),
         CRP09UA = forcats::as_factor(CRP09UA)
         )

