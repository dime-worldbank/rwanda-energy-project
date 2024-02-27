
##########################
#Author: Xiaoming Zhang
#Date: 02072024
#purpose: joining 2021 observation
############################


pacman::p_load(raster,exactextractr,dplyr, rgdal,here,ggplot2,sf,tidyr,readxl,writexl, stringr, stargazer)

getwd()

#ReadDropbox----


if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX, "Rwanda Energy/datawork/Historical data"
)


# Construct the file path for the raster layer
file_path <- file.path(data_path, paste0("harmonized1992-2020/Harmonized_DN_NTL_2021_simVIIRS.tif"))

# Load the raster layer
night_light <- raster(file_path, 1)

# Extract the mean value and add it as a new column in rwa_df
mean_value <- exact_extract(night_light, rwa_sp, "mean")

rwa_event_new$`2021` <- mean_value


rwa_wide <- rwa_long %>% 
  pivot_wider(
    names_from = Year,
    values_from = Value
  )

?pivot_wider()


rwa_wide$`2021` <- mean_value

rwa_wide <- rwa_wide %>% 
  select(
    -connection_time
  )

write_xlsx(rwa_wide, path = file.path(data_path, "ntl_wide_92_21.xlsx"))




