##########################
#Author: Xiaoming Zhang
#Date: 10242024
#purpose: Comparing different nightlight data, choosing the harmonized one, and running Event Study 
############################


pacman::p_load(raster,exactextractr,dplyr, rgdal,here,ggplot2,sf,tidyr,readxl,writexl, stringr, stargazer)
pacman::p_load(fixest, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, plm, haven, stringr, modelsummary, kableExtra, stargazer, lfe, ggfixest, install = TRUE)

getwd()

#ReadDropbox----


if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX, "Rwanda Energy/EAQIP/datawork/Historical data"
)


#read file----

rwa_regress <- read_xlsx(path = file.path(data_path, "rwa_regress_wtax.xlsx"))

rwa_cell <- st_read(dsn = file.path(data_path, "rwa_cell", "Cell.shp"))
rwa_cell <- st_make_valid(rwa_cell)

#0.3 threshold-------
# # Loop over the 114 layers of the raster file
# for (layer_number in 1:114) {
#   
#   # Construct the file path for the raster layer
#   night_light <- raster(file.path(data_path, "Nightlight", "data", "gb_viirs_corrected_monthly_start_201401_avg_rad.tif"), layer_number)
#   
#   # Transform the coordinate reference system of rwa_cell to match the raster
#   rwa_cell <- st_transform(rwa_cell, crs = st_crs(night_light))
#   
#   # Convert raster to points and then to a data frame
#   df <- rasterToPoints(night_light, spatial = TRUE) %>% as.data.frame()
#   
#   # Add binary indicators for nightlight thresholds
#   df <- df %>%
#     mutate(
#       value_0.3 = ifelse(avg_rad >= 0.3, 1, 0)
#     )
#   
#   # Create an sf object from the raster points
#   df_sf <- st_as_sf(df, coords = c("x", "y"), crs = st_crs(night_light))
#   
#   # Perform spatial join with rwa_cell
#   intersect_2 <- st_join(df_sf, rwa_cell, join = st_within, left = FALSE)
#   
#   # Generate dates for the column names
#   date_start <- as.Date("2014-01-01")
#   new_date <- format(seq(date_start, by = "months", length.out = 114), "%Y-%m")
#   
#   # Calculate mean values grouped by Cell_ID
#   intersect <- intersect_2 %>%
#     group_by(Cell_ID) %>%
#     summarise(
#       mean_value = mean(value_0.3, na.rm = TRUE),  # Calculate the mean for the binary indicator
#       .groups = 'drop'  # Drop the grouping structure
#     )
#   
#   # Add the mean values as a new column to the rwa_cell using the date as column name
#   rwa_cell <- rwa_cell %>%
#     left_join(intersect, by = "Cell_ID") %>%
#     mutate(!!new_date[layer_number] := mean_value)  # Dynamically assign the mean value to the correct date column
# }




rwa_df <- data.frame(Cell_ID = rwa_cell$Cell_ID) 

# Loop over the 114 layers of the raster file
for (layer_number in 1:114) {
  # Load the current layer of the raster file
  night_light <- raster(file.path(data_path, "Nightlight", "data", "gb_viirs_corrected_monthly_start_201401_avg_rad.tif"), layer_number)
  
  # Transform the coordinate reference system of rwa_cell to match the raster
  rwa_cell <- st_transform(rwa_cell, crs = st_crs(night_light))
  
  # Calculate the proportion of values where nightlight >= 0.3
  pct_value_0.3 <- exact_extract(night_light, rwa_cell, function(values, coverage_fraction) {
    mean(values >= 0.3) 
  })
  
  # Generate dates for the column names
  date_start <- as.Date("2014-01-01")
  new_date <- format(seq(date_start, by = "months", length.out = 114), "%Y-%m")
  
  # Add the mean values as a new column to the rwa_df using the date as column name
  col_name <- new_date[layer_number]
  rwa_df[[col_name]] <- pct_value_0.3
}

write_xlsx(rwa_df, path = file.path(data_path, "Nightlight", "data",  "viirs(month_0.3.xlsx"))

rwa_save_0.3 <- rwa_df

#Only years----
rwa_df <- read_xlsx( path = file.path(data_path, "Nightlight", "data",  "viirs(month_0.3.xlsx"))

yearly_means_df <- data.frame(matrix(NA, nrow = nrow(rwa_df), ncol = 0))


# Loop through each year (2014 to 2023)
for (year in 2014:2023) {
  # Extract column indices for each month's data within the current year
  year_columns <- grep(paste0("^", year, "-"), names(rwa_df))
  
  # Calculate the mean across the 12 months for each row
  yearly_mean <- rowMeans(rwa_df[, year_columns], na.rm = TRUE)
  
  # Add the yearly mean as a new column in yearly_means_df
  yearly_means_df[[as.character(year)]] <- yearly_mean
}

# Add the yearly means back to rwa_df
rwa_df <- cbind(rwa_df, yearly_means_df)


#Yearly data only df ----

rwa_yearly <- rwa_df %>% 
  select(Cell_ID, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`)

rwa_long <- rwa_yearly %>% 
  pivot_longer(
    cols = `2014`:`2023`,      
    names_to = "Year",         
    values_to = "value_0.3"    
  ) %>% 
  mutate(Cell_ID = as.character(Cell_ID),
         Year = as.numeric(Year)) %>% 
  filter(Year < 2022)



#Event study analysis----

rwa_regress <- read_xlsx(path = file.path(data_path, "rwa_regress_wtax.xlsx"))
rwa_regress <- left_join(rwa_regress, rwa_long, by = c("cell_id" = "Cell_ID", "year" = "Year"))


rwa_regress <- rwa_regress %>% 
  mutate(
    year_first = ifelse(year_first == 2300, 0, year_first),
    value_0.3 = ifelse(is.na(value_0.3), 0, value_0.3)
  )


rwa_event_3 <- rwa_regress %>% subset(year_first %in% c(0, 2015:2017)) %>%
  mutate(elec_m3ormore = ifelse(year_first == 0, 0, year - year_first <= -3) %>% as.numeric,
         elec_m2 = ifelse(year_first == 0, 0, year - year_first == -2) %>% as.numeric,
         elec_0 = ifelse(year_first == 0, 0, year - year_first == 0) %>% as.numeric,
         elec_1 = ifelse(year_first == 0, 0, year - year_first == 1) %>% as.numeric,
         elec_2 = ifelse(year_first == 0, 0, year - year_first == 2) %>% as.numeric,
         elec_3ormore = ifelse(year_first == 0, 0, year - year_first >= 3) %>% as.numeric) %>% 
  filter(year >= 2014)



sum(is.na(rwa_event_3$distr_year))


rwa_event_3 %>% 
  count(cell_id) %>% 
  count(n)

##Ntl----
value_0.3 <- felm(value_0.3  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(value)

value <- felm(value  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(value)

value_distr_0.3 <- felm( value_0.3  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(value_distr_0.3)

value_distr <- felm( value  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(value_distr)


# Create a list of the models
models <- list(value_0.3, value, value_distr_0.3, value_distr)


stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)






##0.5-----



rwa_df <- data.frame(Cell_ID = rwa_cell$Cell_ID) 

# Loop over the 114 layers of the raster file
for (layer_number in 1:114) {
  # Load the current layer of the raster file
  night_light <- raster(file.path(data_path, "Nightlight", "data", "gb_viirs_corrected_monthly_start_201401_avg_rad.tif"), layer_number)
  
  # Transform the coordinate reference system of rwa_cell to match the raster
  rwa_cell <- st_transform(rwa_cell, crs = st_crs(night_light))
  
  # Calculate the proportion of values where nightlight >= 0.5
  pct_value_0.5 <- exact_extract(night_light, rwa_cell, function(values, coverage_fraction) {
    mean(values >= 0.5) 
  })
  
  # Generate dates for the column names
  date_start <- as.Date("2014-01-01")
  new_date <- format(seq(date_start, by = "months", length.out = 114), "%Y-%m")
  
  # Add the mean values as a new column to the rwa_df using the date as column name
  col_name <- new_date[layer_number]
  rwa_df[[col_name]] <- pct_value_0.5
}

write_xlsx(rwa_df, path = file.path(data_path, "Nightlight", "data",  "viirs(month_0.5.xlsx"))

rwa_save <- rwa_df

#Only years----
rwa_df <- read_xlsx(path = file.path(data_path, "Nightlight", "data",  "viirs(month_0.5.xlsx"))
yearly_means_df <- data.frame(matrix(NA, nrow = nrow(rwa_df), ncol = 0))


# Loop through each year (2014 to 2023)
for (year in 2014:2023) {
  # Extract column indices for each month's data within the current year
  year_columns <- grep(paste0("^", year, "-"), names(rwa_df))
  
  # Calculate the mean across the 12 months for each row
  yearly_mean <- rowMeans(rwa_df[, year_columns], na.rm = TRUE)
  
  # Add the yearly mean as a new column in yearly_means_df
  yearly_means_df[[as.character(year)]] <- yearly_mean
}

# Add the yearly means back to rwa_df
rwa_df <- cbind(rwa_df, yearly_means_df)






#Yearly data only df ----

rwa_yearly <- rwa_df %>% 
  select(Cell_ID, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`)

rwa_long <- rwa_yearly %>% 
  pivot_longer(
    cols = `2014`:`2023`,      
    names_to = "Year",         
    values_to = "value_0.5"    
  ) %>% 
  mutate(Cell_ID = as.character(Cell_ID),
         Year = as.numeric(Year)) %>% 
  filter(Year < 2022)



#Event study analysis----

rwa_regress <- read_xlsx(path = file.path(data_path, "rwa_regress_wtax.xlsx"))
rwa_regress <- left_join(rwa_regress, rwa_long, by = c("cell_id" = "Cell_ID", "year" = "Year"))

rwa_regress <- rwa_regress %>% 
  mutate(
    year_first = ifelse(year_first == 2300, 0, year_first)
  ) %>% 
  mutate(
    value_0.5 = ifelse(is.na(value_0.5), 0, value_0.5)
  )

rwa_event_3 <- rwa_regress %>% subset(year_first %in% c(0, 2015:2017)) %>%
  mutate(elec_m3ormore = ifelse(year_first == 0, 0, year - year_first <= -3) %>% as.numeric,
         elec_m2 = ifelse(year_first == 0, 0, year - year_first == -2) %>% as.numeric,
         elec_0 = ifelse(year_first == 0, 0, year - year_first == 0) %>% as.numeric,
         elec_1 = ifelse(year_first == 0, 0, year - year_first == 1) %>% as.numeric,
         elec_2 = ifelse(year_first == 0, 0, year - year_first == 2) %>% as.numeric,
         elec_3ormore = ifelse(year_first == 0, 0, year - year_first >= 3) %>% as.numeric) %>% 
  filter(year >= 2014)


rwa_event_3 %>% 
  count(cell_id) %>% 
  count(n)

##Ntl----
value_0.5 <- felm(value_0.5  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(value_0.5)

value <- felm(value  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(value)

value_distr_0.5 <- felm( value_0.5  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(value_distr_0.5)

value_distr <- felm( value  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(value_distr)


# Create a list of the models
models <- list(value_0.5, value, value_distr_0.5, value_distr)


stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)












##0.6-----



rwa_df <- data.frame(Cell_ID = rwa_cell$Cell_ID) 

# Loop over the 114 layers of the raster file
for (layer_number in 1:114) {
  # Load the current layer of the raster file
  night_light <- raster(file.path(data_path, "Nightlight", "data", "gb_viirs_corrected_monthly_start_201401_avg_rad.tif"), layer_number)
  
  # Transform the coordinate reference system of rwa_cell to match the raster
  rwa_cell <- st_transform(rwa_cell, crs = st_crs(night_light))
  
  # Calculate the proportion of values where nightlight >= 0.6
  pct_value_0.6 <- exact_extract(night_light, rwa_cell, function(values, coverage_fraction) {
    mean(values >= 0.6) 
  })
  
  # Generate dates for the column names
  date_start <- as.Date("2014-01-01")
  new_date <- format(seq(date_start, by = "months", length.out = 114), "%Y-%m")
  
  # Add the mean values as a new column to the rwa_df using the date as column name
  col_name <- new_date[layer_number]
  rwa_df[[col_name]] <- pct_value_0.6
}

write_xlsx(rwa_df, path = file.path(data_path, "Nightlight", "data",  "viirs(month_0.6.xlsx"))

rwa_save_0.6 <- rwa_df

#Only years----
rwa_df <- read_xlsx(path = file.path(data_path, "Nightlight", "data",  "viirs(month_0.6.xlsx"))
yearly_means_df <- data.frame(matrix(NA, nrow = nrow(rwa_df), ncol = 0))


# Loop through each year (2014 to 2023)
for (year in 2014:2023) {
  # Extract column indices for each month's data within the current year
  year_columns <- grep(paste0("^", year, "-"), names(rwa_df))
  
  # Calculate the mean across the 12 months for each row
  yearly_mean <- rowMeans(rwa_df[, year_columns], na.rm = TRUE)
  
  # Add the yearly mean as a new column in yearly_means_df
  yearly_means_df[[as.character(year)]] <- yearly_mean
}

# Add the yearly means back to rwa_df
rwa_df <- cbind(rwa_df, yearly_means_df)






#Yearly data only df ----

rwa_yearly <- rwa_df %>% 
  select(Cell_ID, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`)

rwa_long <- rwa_yearly %>% 
  pivot_longer(
    cols = `2014`:`2023`,      
    names_to = "Year",         
    values_to = "value_0.6"    
  ) %>% 
  mutate(Cell_ID = as.character(Cell_ID),
         Year = as.numeric(Year)) %>% 
  filter(Year < 2022)



#Event study analysis----

rwa_regress <- read_xlsx(path = file.path(data_path, "rwa_regress_wtax.xlsx"))
rwa_regress <- left_join(rwa_regress, rwa_long, by = c("cell_id" = "Cell_ID", "year" = "Year"))

rwa_regress <- rwa_regress %>% 
  mutate(
    year_first = ifelse(year_first == 2300, 0, year_first)
  ) %>% 
  mutate(
    value_0.6 = ifelse(is.na(value_0.6), 0, value_0.6)
  )

rwa_event_3 <- rwa_regress %>% subset(year_first %in% c(0, 2015:2017)) %>%
  mutate(elec_m3ormore = ifelse(year_first == 0, 0, year - year_first <= -3) %>% as.numeric,
         elec_m2 = ifelse(year_first == 0, 0, year - year_first == -2) %>% as.numeric,
         elec_0 = ifelse(year_first == 0, 0, year - year_first == 0) %>% as.numeric,
         elec_1 = ifelse(year_first == 0, 0, year - year_first == 1) %>% as.numeric,
         elec_2 = ifelse(year_first == 0, 0, year - year_first == 2) %>% as.numeric,
         elec_3ormore = ifelse(year_first == 0, 0, year - year_first >= 3) %>% as.numeric) %>% 
  filter(year >= 2014)


rwa_event_3 %>% 
  count(cell_id) %>% 
  count(n)

##Ntl----
value_0.6 <- felm(value_0.6  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(value_0.6)

value <- felm(value  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(value)

value_distr_0.6 <- felm( value_0.6  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(value_distr_0.6)

value_distr <- felm( value  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(value_distr)


# Create a list of the models
models <- list(value_0.6, value, value_distr_0.6, value_distr)


stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)







##0.7-----



rwa_df <- data.frame(Cell_ID = rwa_cell$Cell_ID) 

# Loop over the 114 layers of the raster file
for (layer_number in 1:114) {
  # Load the current layer of the raster file
  night_light <- raster(file.path(data_path, "Nightlight", "data", "gb_viirs_corrected_monthly_start_201401_avg_rad.tif"), layer_number)
  
  # Transform the coordinate reference system of rwa_cell to match the raster
  rwa_cell <- st_transform(rwa_cell, crs = st_crs(night_light))
  
  # Calculate the proportion of values where nightlight >= 0.7
  pct_value_0.7 <- exact_extract(night_light, rwa_cell, function(values, coverage_fraction) {
    mean(values >= 0.7) 
  })
  
  # Generate dates for the column names
  date_start <- as.Date("2014-01-01")
  new_date <- format(seq(date_start, by = "months", length.out = 114), "%Y-%m")
  
  # Add the mean values as a new column to the rwa_df using the date as column name
  col_name <- new_date[layer_number]
  rwa_df[[col_name]] <- pct_value_0.7
}

write_xlsx(rwa_df, path = file.path(data_path, "Nightlight", "data",  "viirs(month_0.7.xlsx"))

rwa_save_0.7 <- rwa_df

#Only years----
rwa_df <- read_xlsx(path = file.path(data_path, "Nightlight", "data",  "viirs(month_0.7.xlsx"))
yearly_means_df <- data.frame(matrix(NA, nrow = nrow(rwa_df), ncol = 0))


# Loop through each year (2014 to 2023)
for (year in 2014:2023) {
  # Extract column indices for each month's data within the current year
  year_columns <- grep(paste0("^", year, "-"), names(rwa_df))
  
  # Calculate the mean across the 12 months for each row
  yearly_mean <- rowMeans(rwa_df[, year_columns], na.rm = TRUE)
  
  # Add the yearly mean as a new column in yearly_means_df
  yearly_means_df[[as.character(year)]] <- yearly_mean
}

# Add the yearly means back to rwa_df
rwa_df <- cbind(rwa_df, yearly_means_df)






#Yearly data only df ----

rwa_yearly <- rwa_df %>% 
  select(Cell_ID, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`)

rwa_long <- rwa_yearly %>% 
  pivot_longer(
    cols = `2014`:`2023`,      
    names_to = "Year",         
    values_to = "value_0.7"    
  ) %>% 
  mutate(Cell_ID = as.character(Cell_ID),
         Year = as.numeric(Year)) %>% 
  filter(Year < 2022)



#Event study analysis----

rwa_regress <- read_xlsx(path = file.path(data_path, "rwa_regress_wtax.xlsx"))
rwa_regress <- left_join(rwa_regress, rwa_long, by = c("cell_id" = "Cell_ID", "year" = "Year"))

rwa_regress <- rwa_regress %>% 
  mutate(
    year_first = ifelse(year_first == 2300, 0, year_first)
  ) %>% 
  mutate(
    value_0.7 = ifelse(is.na(value_0.7), 0, value_0.7)
  )

rwa_event_3 <- rwa_regress %>% subset(year_first %in% c(0, 2015:2017)) %>%
  mutate(elec_m3ormore = ifelse(year_first == 0, 0, year - year_first <= -3) %>% as.numeric,
         elec_m2 = ifelse(year_first == 0, 0, year - year_first == -2) %>% as.numeric,
         elec_0 = ifelse(year_first == 0, 0, year - year_first == 0) %>% as.numeric,
         elec_1 = ifelse(year_first == 0, 0, year - year_first == 1) %>% as.numeric,
         elec_2 = ifelse(year_first == 0, 0, year - year_first == 2) %>% as.numeric,
         elec_3ormore = ifelse(year_first == 0, 0, year - year_first >= 3) %>% as.numeric) %>% 
  filter(year >= 2014)


rwa_event_3 %>% 
  count(cell_id) %>% 
  count(n)

##Ntl----
value_0.7 <- felm(value_0.7  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(value_0.7)

value <- felm(value  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(value)

value_distr_0.7 <- felm( value_0.7  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(value_distr_0.7)

value_distr <- felm( value  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(value_distr)


# Create a list of the models
models <- list(value_0.7, value, value_distr_0.7, value_distr)


stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)


##0.8-----



rwa_df <- data.frame(Cell_ID = rwa_cell$Cell_ID) 

# Loop over the 114 layers of the raster file
for (layer_number in 1:114) {
  # Load the current layer of the raster file
  night_light <- raster(file.path(data_path, "Nightlight", "data", "gb_viirs_corrected_monthly_start_201401_avg_rad.tif"), layer_number)
  
  # Transform the coordinate reference system of rwa_cell to match the raster
  rwa_cell <- st_transform(rwa_cell, crs = st_crs(night_light))
  
  # Calculate the proportion of values where nightlight >= 0.8
  pct_value_0.8 <- exact_extract(night_light, rwa_cell, function(values, coverage_fraction) {
    mean(values >= 0.8) 
  })
  
  # Generate dates for the column names
  date_start <- as.Date("2014-01-01")
  new_date <- format(seq(date_start, by = "months", length.out = 114), "%Y-%m")
  
  # Add the mean values as a new column to the rwa_df using the date as column name
  col_name <- new_date[layer_number]
  rwa_df[[col_name]] <- pct_value_0.8
}

write_xlsx(rwa_df, path = file.path(data_path, "Nightlight", "data",  "viirs(month_0.8.xlsx"))

rwa_save <- rwa_df

#Only years----
rwa_df <- read_xlsx(path = file.path(data_path, "Nightlight", "data",  "viirs(month_0.8.xlsx"))
yearly_means_df <- data.frame(matrix(NA, nrow = nrow(rwa_df), ncol = 0))


# Loop through each year (2014 to 2023)
for (year in 2014:2023) {
  # Extract column indices for each month's data within the current year
  year_columns <- grep(paste0("^", year, "-"), names(rwa_df))
  
  # Calculate the mean across the 12 months for each row
  yearly_mean <- rowMeans(rwa_df[, year_columns], na.rm = TRUE)
  
  # Add the yearly mean as a new column in yearly_means_df
  yearly_means_df[[as.character(year)]] <- yearly_mean
}

# Add the yearly means back to rwa_df
rwa_df <- cbind(rwa_df, yearly_means_df)






#Yearly data only df ----

rwa_yearly <- rwa_df %>% 
  select(Cell_ID, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`)

rwa_long <- rwa_yearly %>% 
  pivot_longer(
    cols = `2014`:`2023`,      
    names_to = "Year",         
    values_to = "value_0.8"    
  ) %>% 
  mutate(Cell_ID = as.character(Cell_ID),
         Year = as.numeric(Year)) %>% 
  filter(Year < 2022)



#Event study analysis----

rwa_regress <- read_xlsx(path = file.path(data_path, "rwa_regress_wtax.xlsx"))
rwa_regress <- left_join(rwa_regress, rwa_long, by = c("cell_id" = "Cell_ID", "year" = "Year"))

rwa_regress <- rwa_regress %>% 
  mutate(
    year_first = ifelse(year_first == 2300, 0, year_first)
  ) %>% 
  mutate(
    value_0.8 = ifelse(is.na(value_0.8), 0, value_0.8)
  )

rwa_event_3 <- rwa_regress %>% subset(year_first %in% c(0, 2015:2017)) %>%
  mutate(elec_m3ormore = ifelse(year_first == 0, 0, year - year_first <= -3) %>% as.numeric,
         elec_m2 = ifelse(year_first == 0, 0, year - year_first == -2) %>% as.numeric,
         elec_0 = ifelse(year_first == 0, 0, year - year_first == 0) %>% as.numeric,
         elec_1 = ifelse(year_first == 0, 0, year - year_first == 1) %>% as.numeric,
         elec_2 = ifelse(year_first == 0, 0, year - year_first == 2) %>% as.numeric,
         elec_3ormore = ifelse(year_first == 0, 0, year - year_first >= 3) %>% as.numeric) %>% 
  filter(year >= 2014)


rwa_event_3 %>% 
  count(cell_id) %>% 
  count(n)

##Ntl----
value_0.8 <- felm(value_0.8  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(value_0.8)

value <- felm(value  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(value)

value_distr_0.8 <- felm( value_0.8  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(value_distr_0.8)

value_distr <- felm( value  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(value_distr)


# Create a list of the models
models <- list(value_0.8, value, value_distr_0.8, value_distr)


stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)


##0.9-----



rwa_df <- data.frame(Cell_ID = rwa_cell$Cell_ID) 

# Loop over the 114 layers of the raster file
for (layer_number in 1:114) {
  # Load the current layer of the raster file
  night_light <- raster(file.path(data_path, "Nightlight", "data", "gb_viirs_corrected_monthly_start_201401_avg_rad.tif"), layer_number)
  
  # Transform the coordinate reference system of rwa_cell to match the raster
  rwa_cell <- st_transform(rwa_cell, crs = st_crs(night_light))
  
  # Calculate the proportion of values where nightlight >= 0.9
  pct_value_0.9 <- exact_extract(night_light, rwa_cell, function(values, coverage_fraction) {
    mean(values >= 0.9) 
  })
  
  # Generate dates for the column names
  date_start <- as.Date("2014-01-01")
  new_date <- format(seq(date_start, by = "months", length.out = 114), "%Y-%m")
  
  # Add the mean values as a new column to the rwa_df using the date as column name
  col_name <- new_date[layer_number]
  rwa_df[[col_name]] <- pct_value_0.9
}

write_xlsx(rwa_df, path = file.path(data_path, "Nightlight", "data",  "viirs(month_0.9.xlsx"))

rwa_save_0.9 <- rwa_df

#Only years----
rwa_df <- read_xlsx(path = file.path(data_path, "Nightlight", "data",  "viirs(month_0.9.xlsx"))
yearly_means_df <- data.frame(matrix(NA, nrow = nrow(rwa_df), ncol = 0))


# Loop through each year (2014 to 2023)
for (year in 2014:2023) {
  # Extract column indices for each month's data within the current year
  year_columns <- grep(paste0("^", year, "-"), names(rwa_df))
  
  # Calculate the mean across the 12 months for each row
  yearly_mean <- rowMeans(rwa_df[, year_columns], na.rm = TRUE)
  
  # Add the yearly mean as a new column in yearly_means_df
  yearly_means_df[[as.character(year)]] <- yearly_mean
}

# Add the yearly means back to rwa_df
rwa_df <- cbind(rwa_df, yearly_means_df)






#Yearly data only df ----

rwa_yearly <- rwa_df %>% 
  select(Cell_ID, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`)

rwa_long <- rwa_yearly %>% 
  pivot_longer(
    cols = `2014`:`2023`,      
    names_to = "Year",         
    values_to = "value_0.9"    
  ) %>% 
  mutate(Cell_ID = as.character(Cell_ID),
         Year = as.numeric(Year)) %>% 
  filter(Year < 2022)



#Event study analysis----

rwa_regress <- read_xlsx(path = file.path(data_path, "rwa_regress_wtax.xlsx"))
rwa_regress <- left_join(rwa_regress, rwa_long, by = c("cell_id" = "Cell_ID", "year" = "Year"))

rwa_regress <- rwa_regress %>% 
  mutate(
    year_first = ifelse(year_first == 2300, 0, year_first)
  ) %>% 
  mutate(
    value_0.9 = ifelse(is.na(value_0.9), 0, value_0.9)
  )

rwa_event_3 <- rwa_regress %>% subset(year_first %in% c(0, 2015:2017)) %>%
  mutate(elec_m3ormore = ifelse(year_first == 0, 0, year - year_first <= -3) %>% as.numeric,
         elec_m2 = ifelse(year_first == 0, 0, year - year_first == -2) %>% as.numeric,
         elec_0 = ifelse(year_first == 0, 0, year - year_first == 0) %>% as.numeric,
         elec_1 = ifelse(year_first == 0, 0, year - year_first == 1) %>% as.numeric,
         elec_2 = ifelse(year_first == 0, 0, year - year_first == 2) %>% as.numeric,
         elec_3ormore = ifelse(year_first == 0, 0, year - year_first >= 3) %>% as.numeric) %>% 
  filter(year >= 2014)


rwa_event_3 %>% 
  count(cell_id) %>% 
  count(n)

##Ntl----
value_0.9 <- felm(value_0.9  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(value_0.9)

value <- felm(value  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(value)

value_distr_0.9 <- felm( value_0.9  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(value_distr_0.9)

value_distr <- felm( value  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(value_distr)


# Create a list of the models
models <- list(value_0.9, value, value_distr_0.9, value_distr)


stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)




##1.0-----



rwa_df <- data.frame(Cell_ID = rwa_cell$Cell_ID) 

# Loop over the 114 layers of the raster file
for (layer_number in 1:114) {
  # Load the current layer of the raster file
  night_light <- raster(file.path(data_path, "Nightlight", "data", "gb_viirs_corrected_monthly_start_201401_avg_rad.tif"), layer_number)
  
  # Transform the coordinate reference system of rwa_cell to match the raster
  rwa_cell <- st_transform(rwa_cell, crs = st_crs(night_light))
  
  # Calculate the proportion of values where nightlight >= 1.0
  pct_value_1.0 <- exact_extract(night_light, rwa_cell, function(values, coverage_fraction) {
    mean(values >= 1.0) 
  })
  
  # Generate dates for the column names
  date_start <- as.Date("2014-01-01")
  new_date <- format(seq(date_start, by = "months", length.out = 114), "%Y-%m")
  
  # Add the mean values as a new column to the rwa_df using the date as column name
  col_name <- new_date[layer_number]
  rwa_df[[col_name]] <- pct_value_1.0
}

write_xlsx(rwa_df, path = file.path(data_path, "Nightlight", "data",  "viirs(month_1.0.xlsx"))

rwa_save_1.0 <- rwa_df

#Only years----
rwa_df <- read_xlsx(path = file.path(data_path, "Nightlight", "data",  "viirs(month_1.0.xlsx"))
yearly_means_df <- data.frame(matrix(NA, nrow = nrow(rwa_df), ncol = 0))


# Loop through each year (2014 to 2023)
for (year in 2014:2023) {
  # Extract column indices for each month's data within the current year
  year_columns <- grep(paste0("^", year, "-"), names(rwa_df))
  
  # Calculate the mean across the 12 months for each row
  yearly_mean <- rowMeans(rwa_df[, year_columns], na.rm = TRUE)
  
  # Add the yearly mean as a new column in yearly_means_df
  yearly_means_df[[as.character(year)]] <- yearly_mean
}

# Add the yearly means back to rwa_df
rwa_df <- cbind(rwa_df, yearly_means_df)






#Yearly data only df ----

rwa_yearly <- rwa_df %>% 
  select(Cell_ID, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`)

rwa_long <- rwa_yearly %>% 
  pivot_longer(
    cols = `2014`:`2023`,      
    names_to = "Year",         
    values_to = "value_1.0"    
  ) %>% 
  mutate(Cell_ID = as.character(Cell_ID),
         Year = as.numeric(Year)) %>% 
  filter(Year < 2022)



#Event study analysis----

rwa_regress <- read_xlsx(path = file.path(data_path, "rwa_regress_wtax.xlsx"))
rwa_regress <- left_join(rwa_regress, rwa_long, by = c("cell_id" = "Cell_ID", "year" = "Year"))

rwa_regress <- rwa_regress %>% 
  mutate(
    year_first = ifelse(year_first == 2300, 0, year_first)
  ) %>% 
  mutate(
    value_1.0 = ifelse(is.na(value_1.0), 0, value_1.0)
  )

rwa_event_3 <- rwa_regress %>% subset(year_first %in% c(0, 2015:2017)) %>%
  mutate(elec_m3ormore = ifelse(year_first == 0, 0, year - year_first <= -3) %>% as.numeric,
         elec_m2 = ifelse(year_first == 0, 0, year - year_first == -2) %>% as.numeric,
         elec_0 = ifelse(year_first == 0, 0, year - year_first == 0) %>% as.numeric,
         elec_1 = ifelse(year_first == 0, 0, year - year_first == 1) %>% as.numeric,
         elec_2 = ifelse(year_first == 0, 0, year - year_first == 2) %>% as.numeric,
         elec_3ormore = ifelse(year_first == 0, 0, year - year_first >= 3) %>% as.numeric) %>% 
  filter(year >= 2014)


rwa_event_3 %>% 
  count(cell_id) %>% 
  count(n)

##Ntl----
value_1.0 <- felm(value_1.0  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(value_1.0)

value <- felm(value  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + year, data = rwa_event_3)
summary(value)

value_distr_1.0 <- felm( value_1.0  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(value_distr_1.0)

value_distr <- felm( value  ~ elec_m3ormore + elec_m2 +elec_0 + elec_1 + elec_2 + elec_3ormore | cell_id + distr_year, data = rwa_event_3)
summary(value_distr)


# Create a list of the models
models <- list(value_1.0, value, value_distr_1.0, value_distr)


stargazer(
  models,
  title = "OLS Regression Results ",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)




# 
# 
# 
# 
# rwa_path <- file.path(
#   data_path, "rwa_villages/Village.shp"
# )
# 
# rwa_sp <- st_read(dsn = rwa_path)
# rwa_sp <- st_make_valid(rwa_sp)
# rwa_sp <- st_transform(rwa_sp, CRS("+init=epsg:4326"))
# 
# 
# 
# rwa_df <- as.data.frame(rwa_sp) %>% 
#   select(-geometry)
# 
# 
# 
# 
# 
# 
# #Harmonized 1992-2020----
# 
# # Data citation: Li, Xuecao; Zhou, Yuyu; zhao, Min; Zhao, Xia (2020): Harmonization of DMSP and VIIRS nighttime
# # light data from 1992-2020 at the global scale. figshare. Dataset.
# # https://doi.org/10.6084/m9.figshare.9828827.v5
# 
# 
# rwa_harmonized <- rwa_df %>% 
#   select(Village_ID, Province, District, Sector, Cell, Name)
# 
# for (year in 1992:2013) {
#   # Construct the file path for the raster layer
#   file_path <- file.path(data_path, paste0("harmonized1992-2020/Harmonized_DN_NTL_", year, "_calDMSP.tif"))
#   
#   # Load the raster layer
#   night_light <- raster(file_path, 1)
#   
#   
#   # Extract the mean value and add it as a new column in rwa_df
#   mean_value <- exact_extract(night_light, rwa_sp, "mean")
#   
#   # Set the column name to the new date (e.g., "layer_1", "layer_2", ...)
#   col_name <-  paste0("year-", year)
#   rwa_harmonized[col_name] <- mean_value
# }
# 
# 
# 
# for (year in 2014:2020) {
#   
#   # Construct the file path for the raster layer
#   file_path <- file.path(data_path, paste0("harmonized1992-2020/Harmonized_DN_NTL_", year, "_simVIIRS.tif"))
#   
#   # Load the raster layer
#   night_light <- raster(file_path, 1)
#  
#   # Extract the mean value and add it as a new column in rwa_df
#   mean_value <- exact_extract(night_light, rwa_sp, "mean")
#   
#   # Set the column name to the new date (e.g., "layer_1", "layer_2", ...)
#   col_name <-  paste0("year-", year)
#   rwa_harmonized[col_name] <- mean_value
# }
# 
# 
# write.csv(rwa_harmonized, file = here("data", "harmonized1992-2020", "harmonized_nightlight_data.csv"), row.names = FALSE)
# 
# 
# 
# #Event study on this harmonized data----
# 
# 
# #Event year----
# 
# MV_2022 <- read_xlsx(path = file.path(data_path, "Rwanda Villages with Existing MV_2022.xlsx"))
# MV_2011 <- read_xlsx(path = file.path(data_path, "Rwanda Villages with Existing MV_2011.xlsx"))
# 
# MV_2022 <- MV_2022 %>% 
#   mutate(connect22 = 1) %>% 
#   select(Village_ID, connect22)
# 
# rwa_event <- left_join(rwa_harmonized, MV_2022, by = c("Village_ID"))
# 
# MV_2011 <- MV_2011 %>% 
#   mutate(connect11 = 1) %>% 
#   select(Village_ID, connect11)
# 
# rwa_event <- left_join(rwa_event, MV_2011, by = c("Village_ID"))
# 
# 
# rwa_event_new <- rwa_event %>% 
#   mutate(connect22 = ifelse(is.na(connect22), 0, connect22)) %>% 
#   mutate(connect11 = ifelse(is.na(connect11), 0, connect11)) 
# 
# rwa_event_new <- rwa_event_new %>% 
#   mutate(
#     connection_time = ifelse(connect11 == 1, "connected 2011", "not connected 2011")
#   )
# 
# 
# 
# 
# 
# 
# table(rwa_event_new$connection_time)
# 
# 
# #Filter out the NAs which are villages that have grid lines in 2011, but not in 2022, noisy
# rwa_event_new  <-rwa_event_new%>%
#   rename_with(~ gsub("^year-", "", .), starts_with("year-")) %>% 
#   filter(!is.na(connection_time))
# 
# 
# 
# 
# #pivot long----
# rwa_event_long <- rwa_event_new %>%
#   pivot_longer(
#     cols = matches("^(19|20)"),  # Use a regular expression to match columns starting with "19" or "20"
#     names_to = "Year",
#     values_to = "Value"
#   )
# 
# 
# # ALL District ES time series ----
# 
# 
# # calculate the means
# cohort_means <- rwa_event_long %>% 
#   group_by(connection_time, Year) %>% 
#   summarize(cohort_mean = mean(Value, na.rm = TRUE)) 
# 
# cohort_means <- cohort_means %>% 
#   ungroup()
# 
# ### plot the means----
# 
# ggplot(data = cohort_means, aes(x = Year, y = cohort_mean, colour = as.factor(connection_time), group = as.factor(connection_time))) + 
#   geom_point() + 
#   geom_line() +
#   labs(x = "Year", y = "Avg Nightlight Value", color = "connection_time") +
#   theme_classic() + 
#   geom_vline(xintercept = c(17,24), alpha = 0.3, linetype = "dashed", size = 0.5, color = "red")+
#   labs(title = "Event Study Time Series Rwanda 1992- 2020")
# 
# 
# #Regression
# 
# 
# connect_2011 <- cohort_means%>% 
#   mutate(Year = as.numeric(Year)) %>% 
#   filter(connection_time == "connected 2011" & Year >=2008 & Year <= 2015)
# 
# connect_2011 <- connect_2011 %>% 
#   mutate(Year = Year-2008)
# 
# 
# model_connect_2011<- lm(as.numeric(cohort_mean) ~ as.numeric(Year), data = connect_2011)
# 
# summary(model_connect_2011)
# 
# 
# 
# unconnect_2011 <- cohort_means%>% 
#   mutate(Year = as.numeric(Year)) %>% 
#   filter(connection_time == "not connected 2011" & Year >=2008 & Year <= 2015)
# 
# unconnect_2011 <- unconnect_2011 %>% 
#   mutate(Year = Year-2008)
# 
# model_unconnect_2011<- lm(as.numeric(cohort_mean) ~ as.numeric(Year), data = unconnect_2011)
# 
# summary(model_unconnect_2011)
# 
# model_rwa <- list(
#   "connected_2011" = model_connect_2011,
#   "not connected_2011" = model_unconnect_2011
# )
# 
# 
# library(stargazer)
# stargazer(
#   model_rwa,
#   type = "html",
#   title = "Table 1: Nightlight trend from 2008 to 2015 in Rwanda",
#   out = "2008-2015.html")
# 
# ###Event study----
# 
# se <- rwa_event_long %>%
#   group_by(connection_time, Year) %>%
#   summarize(var = var(Value, na.rm = TRUE),
#             n = n(),
#             .groups = "drop_last") %>%
#   mutate(se = sqrt(sum(var * (n - 1)) / (sum(n) - 2)) * sum(1 / n))
# 
# 
# #Calculate Mean Difference
# mean_diff <- rwa_event_long %>%
#   group_by(connection_time, Year) %>%
#   summarize(mean_effect = mean(Value, na.rm = TRUE)) %>%
#   spread(connection_time, mean_effect) %>%
#   summarize(diff = `connected 2011` - `not connected 2011`,
#             Year = Year)
# 
# 
# # Combine with Standard Errors
# mean_diff <- mean_diff %>%
#   left_join(se, by = "Year")
# 
# # Create Event Study Plot
# ggplot(mean_diff, aes(x = as.numeric(Year), y = diff)) +
#   geom_line() +
#   geom_point() +
#   geom_errorbar(aes(ymin = diff - 1.96 * se, ymax = diff + 1.96 * se),
#                 colour = "black", width = 0.1, position = position_dodge(0.1)) +
#   geom_vline(xintercept = c(2008,2015), linetype = "dashed") +
#   geom_hline(yintercept = 0.0, linetype = "dashed") +
#   labs(title = "Event Study Plot",
#        x = "Year",
#        y = "Difference and 95% Conf. Int.")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #NOT including Kigali----
# 
# # calculate the means
# cohort_means <- rwa_event_long %>% 
#   filter(!(District %in% c("Gasabo", "Kicukiro", "Nyarugenge"))) %>%
#   group_by(connection_time, Year) %>% 
#   summarize(cohort_mean = mean(Value, na.rm = TRUE)) 
# 
# cohort_means <- cohort_means %>% 
#   ungroup()
# 
# ### plot the means----
# 
# ggplot(data = cohort_means, aes(x = Year, y = cohort_mean, colour = as.factor(connection_time), group = as.factor(connection_time))) + 
#   geom_point() + 
#   geom_line() +
#   labs(x = "Year", y = "Avg Nightlight Value", color = "connection_time") +
#   theme_classic() + 
#   geom_vline(xintercept = c(17,24), alpha = 0.3, linetype = "dashed", size = 0.5, color = "red")+
#   labs(title = "Event Study Time Series Rwanda 1992- 2020")
# 
# 
# #Regression----
# 
# 
# connect_2011 <- cohort_means%>% 
#   mutate(Year = as.numeric(Year)) %>% 
#   filter(connection_time == "connected 2011" & Year >=2008 & Year <= 2015)
# 
# connect_2011 <- connect_2011 %>% 
#   mutate(Year = Year-2008)
# 
# 
# model_connect_2011<- lm(as.numeric(cohort_mean) ~ as.numeric(Year), data = connect_2011)
# 
# summary(model_connect_2011)
# 
# 
# 
# unconnect_2011 <- cohort_means%>% 
#   mutate(Year = as.numeric(Year)) %>% 
#   filter(connection_time == "not connected 2011" & Year >=2008 & Year <= 2015)
# 
# unconnect_2011 <- unconnect_2011 %>% 
#   mutate(Year = Year-2008)
# 
# model_unconnect_2011<- lm(as.numeric(cohort_mean) ~ as.numeric(Year), data = unconnect_2011)
# 
# summary(model_unconnect_2011)
# 
# model_rwa <- list(
#   "connected_2011" = model_connect_2011,
#   "not connected_2011" = model_unconnect_2011
# )
# 
# 
# library(stargazer)
# stargazer(
#   model_rwa,
#   type = "html",
#   title = "Table 2: Nightlight trend from 2008 to 2015 in Rwanda(Outside Kigali)",
#   out = "2008-2015(Outside Kigali).html")
# 
# 
# 
# 
# 
# #All Event study----
# 
# se <- rwa_event_long %>%
#   filter(!(District %in% c("Gasabo", "Kicukiro", "Nyarugenge"))) %>%
#   group_by(connection_time, Year) %>%
#   summarize(var = var(Value, na.rm = TRUE),
#             n = n(),
#             .groups = "drop_last") %>%
#   mutate(se = sqrt(sum(var * (n - 1)) / (sum(n) - 2)) * sum(1 / n))
# 
# 
# #Calculate Mean Difference
# mean_diff <- rwa_event_long %>%
#   filter(!(District %in% c("Gasabo", "Kicukiro", "Nyarugenge"))) %>%
#   group_by(connection_time, Year) %>%
#   summarize(mean_effect = mean(Value, na.rm = TRUE)) %>%
#   spread(connection_time, mean_effect) %>%
#   summarize(diff = `connected 2011` - `not connected 2011`,
#             Year = Year)
# 
# 
# # Combine with Standard Errors
# mean_diff <- mean_diff %>%
#   left_join(se, by = "Year")
# 
# # Create Event Study Plot
# ggplot(mean_diff, aes(x = as.numeric(Year), y = diff)) +
#   geom_line() +
#   geom_point() +
#   geom_errorbar(aes(ymin = diff - 1.96 * se, ymax = diff + 1.96 * se),
#                 colour = "black", width = 0.1, position = position_dodge(0.1)) +
#   geom_vline(xintercept = c(2008,2015), linetype = "dashed") +
#   geom_hline(yintercept = 0.0, linetype = "dashed") +
#   labs(title = "Event Study Plot(Outside)",
#        x = "Year",
#        y = "Difference and 95% Conf. Int.")
# 
# 
# 
# 
# 
# #Read roads data----
# 
# rd <- file.path(data_path, "Classified_Roads_2022/National_Roads.shp")
# national_rd <- st_read(rd)
# 
# str(rwa_sp)
# national_rd <- st_transform(national_rd, crs = st_crs(rwa_sp))
# 
# national_rd <- st_as_sf(national_rd)
# 
# national_rd <- st_zm(national_rd)
# 
# 
# rwa_sp <- st_as_sf(rwa_sp)
# 
# ggplot() + 
#   geom_sf(data = rwa_sp, fill = NA) +
#   geom_sf(data = national_rd, color = "blue") +
#   theme_classic() +
#   labs(title = "Rwanda National Road Network")
# 
# 
# ###overlap with nightlights----
# 
# nightlight <- raster(file.path(data_path, "harmonized1992-2020/Harmonized_DN_NTL_2020_simVIIRS.tif"))
# 
# rwa_sp <- st_transform(rwa_sp, crs = st_crs(national_rd))
# st_crs(rwa_sp)
# 
# 
# rwa_road <- st_intersection(rwa_sp, national_rd)
# 
# rwa_road <- rwa_road %>% 
#   distinct(Village_ID, .keep_all = TRUE)
# 
# write_xlsx(rwa_road, path = file.path("C:/Users/wb614406/Dropbox/Rwanda Energy/datawork/Historical data/Rwanda Villages with National Road.xlsx"))
# 
# rwa_road_join <- rwa_road %>% 
#   st_drop_geometry() %>% 
#   mutate(road = 1) %>% 
#   select(Village_ID, road)  
# 
# #Join with rwa_event_long----
# 
# rwa_event_long <- left_join(rwa_event_long, rwa_road_join, by = c("Village_ID"))
# 
# 
# rwa_event_long <- rwa_event_long %>% 
#   mutate(road = ifelse(is.na(road), 0, road))
# 
# 
# #Join LV in 2022 existing----
# 
# existing_LV22 <- st_read(dsn = file.path("C:/Users/wb614406/Dropbox/Rwanda Energy/datawork/Historical data/Existing Electrical Network_2022/Existing_LVLine.shp"))
# 
# 
# existing_LV22 <- st_transform(existing_LV22, crs = st_crs(rwa_sp))
# 
# LV_villages <- st_intersection(existing_LV22, rwa_sp)
# 
# LV_villages <- LV_villages %>% 
#   distinct(Village_ID, .keep_all = TRUE) %>% 
#   select(Village_ID, Name, Cell, Sector, District, Province)
# 
# write_xlsx(LV_villages, path = file.path("C:/Users/wb614406/Dropbox/Rwanda Energy/datawork/Historical data/Rwanda Villages with Existing LV_2022.xlsx"))
# 
# 
# 
# ###Rejoin back to rwa_event_long 
# 
# lv_join <- LV_villages %>% 
#   select(Village_ID) %>% 
#   mutate(connect22_lv = 1) %>% 
#   st_drop_geometry()
# 
# rwa_event_long <- left_join(rwa_event_long, lv_join, by = c("Village_ID"))
# 
# 
# #rwa_long <- rwa_event_long----
# 
# rwa_long <- rwa_event_long %>% 
#   rename(connect22_mv = connect22,
#          connect11_mv = connect11,
#          connect22_lv = connect22_lv) 
# 
# rwa_long <- rwa_long %>% 
#   mutate(connect22_lv = ifelse(is.na(connect22_lv), 0, connect22_lv))
# 
# 
# ###2022 redo----
# 
# after_2011 <- rwa_long %>% 
#   filter(connect11_mv != 1)
# 
# after_2011 <- after_2011 %>% 
#   mutate(
#     connection_time = ifelse(connect22_lv == 1, "connected 2022", "not connected 2022")
#   )
# 
# cohort_means<- after_2011 %>% 
#   filter(!(District %in% c("Gasabo", "Kicukiro", "Nyarugenge"))) %>%
#   group_by(connection_time, Year, road) %>% 
#   summarize(cohort_mean = mean(Value, na.rm = TRUE)) 
# 
# cohort_means <- cohort_means %>% 
#   ungroup()
# 
# cohort_means <- cohort_means %>% 
#   mutate(connect_road = case_when(
#     connection_time %in% c("connected 2022") & road == 1 ~ "connected 2022-NR1",
#     connection_time %in% c("connected 2022") & road == 0 ~ "connected 2022-NR0",
#     connection_time %in% c("not connected 2022")& road == 1 ~ "not connected 2022-NR1",
#     connection_time %in% c("not connected 2022")& road == 0 ~ "not connected 2022-NR0"
#   ))
# 
# 
# 
# ggplot(data = cohort_means, aes(x = Year, y = cohort_mean,group = as.factor(connect_road))) + 
#   geom_point(aes(color = connect_road)) + 
#   geom_line(aes(linetype = connect_road, color = connect_road)) +  
#   scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed" )) +  # Adjust size for specific groups
#   scale_color_manual(values = c("blue", "blue", "black", "black")) +
#   # Set a default size for all lines
#   theme_classic() + 
#   geom_vline(xintercept = c(17,24), alpha = 0.3, linetype = "solid", size = 0.5, color = "red") +
#   labs(title = "Event Study Time Series Rwanda 1992-2020 (outside Kigali not connected in 2022)") + 
#   labs(x = "Year", y = "Avg Nightlight Value", color = "connect_road") 
# 
# 
# ###2011 redo----
# 
# at_2011 <- rwa_long %>% 
#   mutate(
#     connection_time = ifelse(connect11_mv == 1, "connected 2011", "not connected 2011")
#   )
# 
# 
# cohort_means <- at_2011 %>% 
#   filter(!(District %in% c("Gasabo", "Kicukiro", "Nyarugenge"))) %>%
#   group_by(connection_time, Year, road) %>% 
#   summarize(cohort_mean = mean(Value, na.rm = TRUE)) 
# 
# cohort_means <- cohort_means %>% 
#   ungroup()
# 
# cohort_means <- cohort_means %>% 
#   mutate(connect_road = case_when(
#     connection_time %in% c("connected 2011") & road == 1 ~ "connected 2011-NR1",
#     connection_time %in% c("connected 2011") & road == 0 ~ "connected 2011-NR0",
#     connection_time %in% c("not connected 2011")& road == 1 ~ "not connected 2011-NR1",
#     connection_time %in% c("not connected 2011")& road == 0 ~ "not connected 2011-NR0"
#   ))
# 
# 
# ggplot(data = cohort_means, aes(x = Year, y = cohort_mean,group = as.factor(connect_road))) + 
#   geom_point(aes(color = connect_road)) + 
#   geom_line(aes(linetype = connect_road, color = connect_road)) +  
#   scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed" )) +  # Adjust size for specific groups
#   scale_color_manual(values = c("blue", "blue", "black", "black")) +
#   # Set a default size for all lines
#   theme_classic() + 
#   geom_vline(xintercept = c(17,24), alpha = 0.3, linetype = "solid", size = 0.5, color = "red") +
#   labs(title = "Event Study Time Series Rwanda 1992-2020 (outside Kigali not connected in 2011)") + 
#   labs(x = "Year", y = "Avg Nightlight Value", color = "connect_road") 
# 
# 
# 
# ## No roads,2022----
# 
# #####All villages----
# 
# # calculate the means
# cohort_means <- after_2011 %>% 
#   group_by(connection_time, Year) %>% 
#   summarize(cohort_mean = mean(Value, na.rm = TRUE)) 
# 
# cohort_means <- cohort_means %>% 
#   ungroup()
# 
# ggplot(data = cohort_means, aes(x = Year, y = cohort_mean, colour = as.factor(connection_time), group = as.factor(connection_time))) + 
#   geom_point() + 
#   geom_line() +
#   labs(x = "Year", y = "Avg Nightlight Value", color = "connection_time") +
#   theme_classic() + 
#   geom_vline(xintercept = c(17,24), alpha = 0.3, linetype = "dashed", size = 0.5, color = "red")+
#   labs(title = "Event Study Time Series Rwanda 1992- 2020(not connected in 2011)")
# 
# 
# ##Not including Kigali----
# 
# 
# 
# # calculate the means
# cohort_means <- after_2011 %>% 
#   filter(!(District %in% c("Gasabo", "Kicukiro", "Nyarugenge"))) %>%
#   group_by(connection_time, Year) %>% 
#   summarize(cohort_mean = mean(Value, na.rm = TRUE)) 
# 
# cohort_means <- cohort_means %>% 
#   ungroup()
# 
# ggplot(data = cohort_means, aes(x = Year, y = cohort_mean, colour = as.factor(connection_time), group = as.factor(connection_time))) + 
#   geom_point() + 
#   geom_line() +
#   labs(x = "Year", y = "Avg Nightlight Value", color = "connection_time") +
#   theme_classic() + 
#   geom_vline(xintercept = c(17,24), alpha = 0.3, linetype = "dashed", size = 0.5, color = "red")+
#   labs(title = "Event Study Time Series Rwanda 1992- 2020(outside Kigali not connected in 2011)")
# 
# write_xlsx(rwa_long, path = file.path("C:/Users/wb614406/Dropbox/Rwanda Energy/datawork/Historical data/ntl_long_92_20.xlsx"))
# 
# 
# 
# #Diff in diff----
# 
# 
# ##2011----
# ####For all villages2008-2015----
# 
# rwa_long_did <- rwa_long %>% 
#   filter(!(District %in% c("Gasabo", "Kicukiro", "Nyarugenge")) & Year == c(2008:2015)) %>% 
#   mutate(
#     treated = ifelse(connection_time== "connected 2011", 1, 0),
#     numYear = as.numeric(Year)-2008,
#     Year = as.numeric(Year)
#   )
# 
# 
# ###regression----
# 
# 
# didrwa <- lm(Value ~ numYear*treated + treated + numYear, data = rwa_long_did)
# 
# summary(didrwa)
# 
# stargazer(
#   didrwa,
#   type = "html",
#   title = "Table 3: Difference in differences(2011) 2008-2014",
#   out = "DiD2011.html")
# 
# 
# 
# ####graph----
# 
# 
# # Create a new variable for the predicted values
# rwa_long_did$predicted <- predict(lm(Value ~ treated + Year + treated * Year, data = rwa_long_did))
# lm(Value ~ treated + numYear + treated * numYear, data = rwa_long_did)
# 
# 
# connected_intercept <- subset(rwa_long_did, treated == 1)$predicted[1]
# not_connected_slope <- coef(lm(predicted ~ treated+ Year + treated * Year, 
#                                data = subset(rwa_long_did, treated == 0)))[3]
# 
# # Plot the interaction term
# ggplot(rwa_long_did, aes(x = Year, y = treated * Year, color = connection_time)) +
#   geom_line(aes(y = predicted), linetype = "solid", size = 1) +
#   geom_segment(aes(x = min(Year), y = connected_intercept,
#                    xend = max(Year), yend = connected_intercept + not_connected_slope * (max(Year) - min(Year))),
#                linetype = "dashed", color = "black") +
#   labs(title = "Diff in Diff(2008-2015 outside Kigali)",
#        x = "Year",
#        y = "treated*Year",
#        color = "Group") +
#   theme_minimal()
# 
# 
# 
# 
# ##2022----
# ####For all villages2008-2015----
# 
# rwa_long_did <- rwa_long %>% 
#   filter(!(District %in% c("Gasabo", "Kicukiro", "Nyarugenge")) & Year == c(2008:2015)) %>% 
#   mutate(
#     treated = ifelse(connect22_lv== 1, 1, 0),
#     numYear = as.numeric(Year)-2008,
#     Year = as.numeric(Year)
#   )
# 
# 
# ###regression----
# 
# 
# didrwa <- lm(Value ~ numYear*treated + treated + numYear, data = rwa_long_did)
# 
# summary(didrwa)
# 
# stargazer(
#   didrwa,
#   type = "html",
#   title = "Table 3: Difference in differences(2011) 2008-2014",
#   out = "DiD2011.html")
# 
# 
# 
# ####graph----
# 
# 
# # Create a new variable for the predicted values
# rwa_long_did$predicted <- predict(lm(Value ~ treated + Year + treated * Year, data = rwa_long_did))
# lm(Value ~ treated + numYear + treated * numYear, data = rwa_long_did)
# 
# 
# connected_intercept <- subset(rwa_long_did, treated == 1)$predicted[1]
# not_connected_slope <- coef(lm(predicted ~ treated+ Year + treated * Year, 
#                                data = subset(rwa_long_did, treated == 0)))[3]
# 
# # Plot the interaction term
# ggplot(rwa_long_did, aes(x = Year, y = treated * Year, color = connection_time)) +
#   geom_line(aes(y = predicted), linetype = "solid", size = 1) +
#   geom_segment(aes(x = min(Year), y = connected_intercept,
#                    xend = max(Year), yend = connected_intercept + not_connected_slope * (max(Year) - min(Year))),
#                linetype = "dashed", color = "black") +
#   labs(title = "Diff in Diff(2008-2015 outside Kigali)",
#        x = "Year",
#        y = "treated*Year",
#        color = "Group") +
#   theme_minimal()
