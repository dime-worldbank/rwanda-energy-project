##############
#Author: Xiaoming Zhang
#Date: 9.23.2024
#Purpose: Offgrid informations
#############
install.packages("readxlsb")

pacman::p_load(knitr, tidyverse, dplyr, here, sf, ggplot2, readxl, readxlsb, writexl, janitor, randomizr, gtsummary)

getwd()

#Dropbox----


if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

#Method One----
path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/data/data/four_district_2408.xlsx"
)

data_path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/data/data"
)

offgrid_path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/data/Offgrid"
)

output_path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/data/data"
)

scope_path <- file.path(
  DROPBOX,
  "Rwanda Energy/EAQIP/datawork/RCT_data",
  "baseline/data/Updated scope villages& households"
)



#Read file----


four_scope_newly <- read_xlsx(path = file.path(scope_path, "scope_193_0807.xlsx"))
ignite <- read_xlsb(path = file.path(offgrid_path, "IGNITE Impact Analysis of Ubudehe Removal on Customer Engagement and Connections of SHS.xlsb", sheet = "Raw Data"))


















