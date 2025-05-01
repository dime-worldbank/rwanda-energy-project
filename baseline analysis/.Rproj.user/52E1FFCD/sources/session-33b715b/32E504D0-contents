##############
#Author: Xiaoming Zhang
#Date: 3.13.2025
#Purpose: Compare EDCL list
#############


pacman::p_load(knitr, stargazer, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr, lfe)
library(googlesheets4)
getwd()


# Import Data ----
dropbox <- 'C:/Users/wb614406/Dropbox'

output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/baseline analysis/output"
)

data_path_1 <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data"
)

karongi_shared <- read_xlsx(path = file.path(data_path_1, "Lot_Karongi_shared.xlsx"), sheet = "household list")
rulindo_shared <- read_xlsx(path = file.path(data_path_1, "Lot_Rulindo_shared.xlsx"), sheet = "household list")
rutsiro_shared <- read_xlsx(path = file.path(data_path_1, "Lot_Rutsiro_shared.xlsx"), sheet = "household list")
rusizi_1_shared <- read_xlsx(path = file.path(data_path_1, "Lot_Rusizi-1_shared.xlsx"), sheet = "household list")
rusizi_2_shared <- read_xlsx(path = file.path(data_path_1, "Lot_Rusizi-2_shared.xlsx"), sheet = "household list")


karongi <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Readyboard by lot","Lot_Karongi.xlsx"), sheet = "household list")
rulindo <- read_xlsx(path = file.path(data_path_1,"Updated scope villages& households","EDCL","Readyboard by lot", "Lot_Rulindo.xlsx"), sheet = "household list")
rutsiro <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Readyboard by lot","Lot_Rutsiro.xlsx"), sheet = "household list")
rusizi_1 <- read_xlsx(path = file.path(data_path_1,"Updated scope villages& households","EDCL","Readyboard by lot", "Lot_Rusizi-1.xlsx"), sheet = "household list")
rusizi_2 <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","EDCL","Readyboard by lot","Lot_Rusizi-2.xlsx"), sheet = "household list")
