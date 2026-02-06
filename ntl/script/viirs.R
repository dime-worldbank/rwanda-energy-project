library(raster)
library(exactextractr)
library(dplyr)
library(here)
library(ggplot2)
library(sf)
library(tidyr)
library(readxl)
library(writexl)


if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

historical_data_path <- file.path(
  DROPBOX,
  "Rwanda Energy/EAQIP/datawork/Historical data"
)

#Read shapefile-------

rwa_sp <- st_read(dsn = file.path( historical_data_path, "rwa_villages","Village.shp"))
rwa_sp <- st_transform(rwa_sp, crs = st_crs(4326))
rwa_df <- as.data.frame(rwa_sp)





# until 2306 mean----
night_light.1 <- raster(file.path(historical_data_path, "Nightlight", "data", "gb_viirs_corrected_monthly_start_201401_avg_rad.tif"), 114)
mean <- exact_extract(night_light.1, rwa_sp, "mean")
rwa_df$`2014-01` <- mean

night_light.1 <- raster(here("data", "gb_viirs_corrected_monthly_start_201401_avg_rad.tif"), 114)
mean_test <- exact_extract(night_light.1, rwa_sp, "mean")
rwa_df$`2014-01` <- mean_test


#Loop----

for (layer_number in 1:114) {
  # Construct the file path for the raster layer
  night_light<- raster(file.path(historical_data_path, "Nightlight", "data", "gb_viirs_corrected_monthly_start_201401_avg_rad.tif"), layer_number)
  
  # Extract the mean value and add it as a new column in rwa_df
  mean_value <- exact_extract(night_light, rwa_sp, "mean")
  
  # Create a date starting from "2014-01-01" and add the number of months
  date_start <- as.Date("2014-01-01")
  new_date <- format(seq(date_start, by="months", length.out=layer_number), "%Y-%m")
  
  # Set the column name to the new date
  col_name <- new_date[layer_number]
  rwa_df[[col_name]] <- mean_value
}


##Take yearly----

# Assuming rwa_df contains columns named "2014-01", "2014-02", ..., "2023-12"

# Extract the column names in rwa_df
column_names <- colnames(rwa_df)

# Initialize a data frame to store the yearly means

yearly_means_df <- data.frame(matrix(NA, nrow = nrow(rwa_df), ncol = 0))

# Loop through the years (2014 to 2023)
for (year in 2014:2023) {
  # Extract the columns that belong to the current year
  year_columns <- grep(paste0("^", year, "-"), column_names)
  
  # Calculate the yearly mean for the current year
  yearly_mean <- rowSums(rwa_df[, year_columns]) / length(year_columns)
  
  # Add the yearly mean as a new column in yearly_means_df
  yearly_means_df[[as.character(year)]] <- yearly_mean
}

###Add it to rwa_df----

# Loop through the years (2014 to 2023)
for (year in 2014:2023) {
  # Extract the yearly mean values from yearly_means_df
  yearly_mean_values <- yearly_means_df[[as.character(year)]]
  
  # Add the yearly mean values as new columns in rwa_df
  rwa_df[[as.character(year)]] <- yearly_mean_values
}

View(rwa_df)

rwa_df <- rwa_df %>% 
  st_drop_geometry() %>% 
  select(-geometry)
write_xlsx(rwa_df, path = file.path(historical_data_path, "Nightlight", "data", "viirs_2014-2023(monthly&yearly).xlsx"))


#Yearly data only df ----

rwa_yearly <- rwa_df %>% 
  select(Village_ID, Province, District, Sector, Cell, Name, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`)

write_xlsx(rwa_yearly, path = file.path(historical_data_path, "Nightlight", "data", "viirs_2014-2023(yearly).xlsx"))
