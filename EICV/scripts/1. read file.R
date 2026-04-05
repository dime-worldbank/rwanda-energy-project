
#################################################
#Author: Xiaoming zhang
#Date 2026 APril
#Purpose: EICV data on hosuehold fuel ues
#########################################################


pacman::p_load(tidyverse, dplyr, here, sf, 
               stringr, haven, lmtest, fixest, kableExtra,
               ggplot2, readxl, writexl, janitor, randomizr,
               stargazer, modelsummary, googlesheets4)

getwd()