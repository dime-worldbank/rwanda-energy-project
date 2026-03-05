library(raster)
library(exactextractr)
library(dplyr)
library(here)
library(ggplot2)
library(sf)
library(tidyr)
library(readxl)
library(writexl)
library(tidyr)
library(janitor)


if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

historical_data_path <- file.path(
  DROPBOX,
  "Rwanda Energy/EAQIP/datawork/Historical data"
)


expansion_data_path <- file.path(
  DROPBOX,
  "Rwanda Energy/EAQIP/datawork/Historical Expansion"
)

#Read shapefile-------

rwa_sp <- st_read(dsn = file.path( historical_data_path, "rwa_villages","Village.shp"))
rwa_sp <- st_transform(rwa_sp, crs = st_crs(4326))
# rwa_df <- as.data.frame(rwa_sp)




#Winsorize at pixel level-------


# Initialize new dataframe (do NOT overwrite original)

rwa_df_5 <- rwa_sp %>%
  st_drop_geometry()


# Monthly Loop with Pixel-Level Winsorization

for (layer_number in 1:114) {
  
  night_light_5 <- raster(
    file.path(
      historical_data_path,
      "Nightlight",
      "data",
      "gb_viirs_corrected_monthly_start_201401_avg_rad.tif"
    ),
    layer_number
  )
  
  
  # Pixel-level winsorization at 5
  
  night_light_5[night_light_5 < 0] <- 0
  night_light_5[night_light_5 > 5] <- 5
  
  # Extract mean AFTER winsorization
  mean_value_5 <- exact_extract(night_light_5, rwa_sp, "mean")
  
  # Create date column names
  date_start_5 <- as.Date("2014-01-01")
  new_date_5 <- format(
    seq(date_start_5, by = "months", length.out = 114),
    "%Y-%m"
  )
  
  col_name_5 <- new_date_5[layer_number]
  rwa_df_5[[col_name_5]] <- mean_value_5
}

# Compute Yearly Means

column_names_5 <- colnames(rwa_df_5)

yearly_means_df_5 <- data.frame(matrix(NA, nrow = nrow(rwa_df_5), ncol = 0))

for (year in 2014:2023) {
  
  year_columns_5 <- grep(paste0("^", year, "-"), column_names_5)
  
  yearly_mean_5 <- rowMeans(rwa_df_5[, year_columns_5], na.rm = TRUE)
  
  yearly_means_df_5[[as.character(year)]] <- yearly_mean_5
}

# Append yearly columns
for (year in 2014:2023) {
  rwa_df_5[[as.character(year)]] <- yearly_means_df_5[[as.character(year)]]
}

# Save Monthly + Yearly


write_xlsx(
  rwa_df_5,
  path = file.path(
    historical_data_path,
    "Nightlight",
    "data",
    "viirs_2014-2023(monthly&yearly)_5.xlsx"
  )
)


# Yearly-only dataframe


rwa_yearly_5 <- rwa_df_5 %>%
  select(
    Village_ID, Province, District, Sector, Cell, Name,
    `2014`, `2015`, `2016`, `2017`,
    `2018`, `2019`, `2020`, `2021`,
    `2022`, `2023`
  )

write_xlsx(
  rwa_yearly_5,
  path = file.path(
    historical_data_path,
    "Nightlight",
    "data",
    "viirs_2014-2023(yearly)_5.xlsx"
  )
)



#Plot----
# LOAD WINSORIZED YEARLY DATA (_5)

##Read file
rwa_df_5 <- read_xlsx(
  path = file.path(
    historical_data_path,
    "Nightlight",
    "data",
    "viirs_2014-2023(yearly)_5.xlsx"
  )
)

rwa_village <- st_read(dsn = file.path(historical_data_path, "rwa_villages", "Village.shp"))

rwa_district <- st_read(dsn = file.path(rct_data_path, "rwa_district", "District.shp"))
rwa_district <- st_transform(rwa_district, crs = st_crs(rwa_village))

rwa_village <- rwa_village %>%
  clean_names() %>% 
  mutate(village_id = as.character(village_id))

rwa_df_5 <- rwa_df_5 %>%
  mutate(village_id = as.character(Village_ID))


## JOIN 2014 2022 (_5 VERSION)-----

rwa_village_ntl_5 <- rwa_village %>%
  left_join(
    rwa_df_5 %>% select(village_id, `2014`, `2022`),
    by = "village_id"
  ) %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "year",
    values_to = "value"
  )

rwa_village_ntl_5 <- rwa_village_ntl_5 %>%
  mutate(
    value = ifelse(value < 0, 0, value),
    value = ifelse(value >= 5, 5, value),
    value_adj = (value)^(1/4)
  )



##Load elec data

rwa_regress <- read_xlsx(
  path = file.path(expansion_data_path, "outputs", "expansion_join.xlsx")
)

rwa_regress <- rwa_regress %>%
  mutate(
    status = case_when(
      electrified_year == 9999 ~ "never_elec",
      electrified_year <= 2011 ~ "always_elec",
      electrified_year >= 2012 & electrified_year <= 2014 ~ "elec12_14",
      electrified_year >= 2015 & electrified_year <= 2017 ~ "elec15_17",
      electrified_year >= 2018 & electrified_year <= 2020 ~ "elec18_20",
      TRUE ~ "never_elec"
    )
  ) %>% 
  filter(status != "always_elec")

# rwa_regress <- read_xlsx(
#   path = file.path(historical_data_path,  "rwa_regress_villagelevel.xlsx")
# )

village_electrified <- rwa_regress %>% 
  distinct(village_id, .keep_all = TRUE) %>% 
  select(village_id, status)

# join to shapefile
rwa_village_join <- left_join(
  rwa_village,
  village_electrified,
  by = "village_id"
)


# DISTRICTS TO PLOT

district_list <- c("Karongi", "Rutsiro", "Rulindo", "Rusizi")


# LOOP THROUGH DISTRICTS



rwa_village_ntl_5 <- rwa_village_ntl_5 %>%
  left_join(village_electrified, by = "village_id") %>%
  filter(status != "always_elec")
# 
# 



rwa_village_ntl_5 <- rwa_village_ntl_5 %>%
  left_join(village_electrified, by = "village_id") %>%
  filter(status != "always_elec")

#Loop-----



for (dist_name in district_list) {
  
  
  # 1. Filter district boundary
  
  district_boundary <- rwa_district %>%
    filter(District == dist_name)
  
  # 2. Electrification Map
  
  district_village_join <- st_intersection(
    rwa_village_join,
    district_boundary
  )
  
  district_village_join <- district_village_join %>%
    filter(status != "always_elec") %>% 
    mutate(
      status = factor(
        status,
        levels = c(
          "elec12_14",
          "elec15_17",
          "elec18_20",
          "elec21_22",
          "never_elec"
        )
      )
    )
  
  status_colors <- c(
    "elec12_14"   = "#A63603",
    "elec15_17"   = "#D94801",
    "elec18_20"   = "#F16913",
    "elec21_22"   = "#FD8D3C",
    "never_elec"  = "grey80"
  )
  
  p_status <- ggplot(district_village_join) +
    geom_sf(aes(fill = status), color = NA) +
    geom_sf(data = district_boundary, color = "black", fill = NA, size = 0.8) +
    scale_fill_manual(values = status_colors) +
    theme_void() +
    labs(
      fill = "Electrification Status",
      caption = paste0(dist_name, " District: Electrification Status by Village")
    ) +
    theme(
      legend.position = "right",
      plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
      plot.caption.position = "plot"
    )
  
  ggsave(
    filename = file.path(
      historical_data_path,
      "Nightlight",
      "output",
      paste0(dist_name, "_Electrification_Status.png")
    ),
    plot = p_status,
    width = 10,
    height = 8,
    dpi = 400
  )
  
  
  district_ntl_5 <- rwa_village_ntl_5 %>%
    filter(
      district == dist_name,
      year %in% c("2014", "2022"),
      status != "always_elec"
    )
  
  # Fix scale within district (so 2014/2022 comparable)
  max_val <- max(district_ntl_5$value_adj, na.rm = TRUE)
  
  p_ntl_5 <- ggplot(district_ntl_5) +
    geom_sf(aes(fill = value), color = NA) +
    scale_fill_gradient(
      low = "black",
      high = "yellow",
      limits = c(0, max_val)
    ) +
    facet_wrap(~year, ncol = 1) +
    theme_void() +
    theme(
      legend.position = "right",
      strip.text = element_text(size = 16, face = "bold"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    ) +
    labs(title = paste0(dist_name, " Nightlight (2014 vs 2022)"))
  
  ggsave(
    filename = file.path(
      historical_data_path,
      "Nightlight",
      "output",
      paste0(dist_name, "_Nightlight_2014_2022_5.png")
    ),
    plot = p_ntl_5,
    width = 8,
    height = 14,
    dpi = 400
  )
  
  print(paste("Finished:", dist_name))
}


# ===============================
# Pixel-Level Winsorized NTL Plot
# Karongi District
# 2014-01 vs 2022-01
# ===============================

library(raster)
library(sf)
library(dplyr)
library(ggplot2)
# install.packages("viridis")
library(viridis)

# ---- 1. Read VIIRS raster stack ----
viirs_stack <- brick(
  file.path(
    historical_data_path,
    "Nightlight",
    "data",
    "gb_viirs_corrected_monthly_start_201401_avg_rad.tif"
  )
)

# ---- 2. Extract specific months ----
# Layer 1 = 2014-01
# Layer 97 = 2022-01
ntl_2014_01 <- viirs_stack[[1]]
ntl_2022_01 <- viirs_stack[[97]]

# ---- 3. Winsorize globally at 5 ----
winsorize <- function(r) {
  r[r < 0] <- 0
  r[r > 5] <- 5
  return(r)
}

ntl_2014_01 <- winsorize(ntl_2014_01)
ntl_2022_01 <- winsorize(ntl_2022_01)

# ---- 4. Read village shapefile ----
rwa_village <- st_read(
  file.path(historical_data_path, "rwa_villages", "Village.shp")
)
# 
# # Filter Karongi district
# karongi_sf <- rwa_village %>%
#   filter(District == "Karongi")
# 
# # ---- 5. Match CRS ----
# 
# karongi_sf <- st_transform(karongi_sf, crs = crs(ntl_2014_01))
# 
# # ---- 6. Mask raster to Karongi ----
# ntl_2014_karongi <- mask(ntl_2014_01, karongi_sf)
# ntl_2022_karongi <- mask(ntl_2022_01, karongi_sf)
# 
# # ---- 7. Convert to dataframe for ggplot ----
# df_2014 <- as.data.frame(ntl_2014_karongi, xy = TRUE, na.rm = TRUE)
# df_2022 <- as.data.frame(ntl_2022_karongi, xy = TRUE, na.rm = TRUE)
# 
# colnames(df_2014)[3] <- "value"
# colnames(df_2022)[3] <- "value"
# 
# df_2014$year <- "2014-01"
# df_2022$year <- "2022-01"
# 
# df_plot <- bind_rows(df_2014, df_2022)
# 
# # ---- 8. Plot stacked vertically ----
# ggplot(df_plot) +
#   geom_raster(aes(x = x, y = y, fill = value)) +
#   scale_fill_viridis_c(limits = c(0,5)) +
#   facet_wrap(~year, ncol = 1) +
#   coord_equal() +
#   theme_void() +
#   theme(
#     strip.text = element_text(size = 14, face = "bold"),
#     legend.position = "right"
#   )
# 

district_list <- c("Karongi", "Rusizi", "Rulindo", "Rutsiro")

for (dist_name in district_list) {
  
  # ---- Filter district ----
  district_sf <- rwa_village %>%
    filter(District == dist_name) %>% 
    filter(Village_ID %in% rwa_regress$village_id)
  
  # Match CRS
  district_sf <- st_transform(district_sf, crs = crs(ntl_2014_01))

  # ---- Mask raster ----
  ntl_2014_dist <- mask(ntl_2014_01, district_sf)
  ntl_2022_dist <- mask(ntl_2022_01, district_sf)
  
  # ---- Convert to dataframe ----
  df_2014 <- as.data.frame(ntl_2014_dist, xy = TRUE, na.rm = TRUE)
  df_2022 <- as.data.frame(ntl_2022_dist, xy = TRUE, na.rm = TRUE)
  
  colnames(df_2014)[3] <- "value"
  colnames(df_2022)[3] <- "value"
  
  df_2014$year <- "2014-01"
  df_2022$year <- "2022-01"
  
  df_plot <- bind_rows(df_2014, df_2022)
  
  # Ensure ordering
  df_plot$year <- factor(df_plot$year, levels = c("2014-01", "2022-01"))
  
  # ---- Plot ----
  p_ntl_5 <- ggplot(df_plot) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    scale_fill_gradient(
      low = "black",
      high = "yellow",
      limits = c(0, max_val)
    ) +
    facet_wrap(~year, ncol = 1) +
    coord_equal() +
    theme_void() +
    theme(
      strip.text = element_text(size = 16, face = "bold"),
      legend.position = "right",
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    ) +
    labs(title = paste0(dist_name, " Nightlight (Pixel-Level)"))
  
  # ---- Save ----
  ggsave(
    filename = file.path(
      historical_data_path,
      "Nightlight",
      "output",
      paste0(dist_name, "_Nightlight_2014_2022_pixel.png")
    ),
    plot = p_ntl_5,
    width = 8,
    height = 14,
    dpi = 400
  )
  
  print(paste("Finished:", dist_name))
}



#Data--------------


library(raster)
library(sf)
library(writexl)

# ---- 1. Read VIIRS raster stack ----
viirs_stack <- brick(
  file.path(
    historical_data_path,
    "Nightlight",
    "data",
    "gb_viirs_corrected_monthly_start_201401_avg_rad.tif"
  )
)

# # ---- 2. Winsorize entire stack at once ----
# viirs_stack <- calc(viirs_stack, fun = function(x) {
#   x[x < 0] <- 0
#   x[x > 5] <- 5
#   return(x)
# })

# ---- 3. Read village shapefile ----
rwa_village <- st_read(
  file.path(historical_data_path, "rwa_villages", "Village.shp")
)

rwa_village_sp <- st_transform(rwa_village, crs = st_crs(ntl_2014_01))

# ---- 4. Mask entire stack ----
viirs_masked <- mask(viirs_stack, rwa_village_sp)

# ---- 5. Create monthly names (YYYY-MM) ----
n_layers <- nlayers(viirs_masked)

month_seq <- seq(
  as.Date("2014-01-01"),
  by = "month",
  length.out = n_layers
)

month_names <- format(month_seq, "%Y-%m")

names(viirs_masked) <- month_names

# ---- 6. Convert to wide dataframe ----
ntl_wide_df <- as.data.frame(
  viirs_masked,
  xy = TRUE,
  na.rm = TRUE
)

names(ntl_wide_df) <- gsub("^X", "", names(ntl_wide_df))
names(ntl_wide_df) <- gsub("\\.", "_", names(ntl_wide_df))

# Inspect
head(ntl_wide_df)

# Inspect
saveRDS(
  ntl_wide_df,
  file = file.path(
    historical_data_path,
    "Nightlight",
    "nightlight_viirs_pixel.rds"
  )
)

saveRDS(
  ntl_wide_df,
  file = file.path(
    expansion_data_path,"data",
    "Nightlight", "data",
    "nightlight_viirs_pixel.rds"
  )
)

