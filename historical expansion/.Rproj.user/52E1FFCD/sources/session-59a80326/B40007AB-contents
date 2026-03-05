####################################
#Author: Xiaoming Zhang
#Date: Decmber 18th 20205
#Purpose: Read LRCC-DVNL data
#####################################################



pacman::p_load(
  raster,
  exactextractr,
  dplyr,
  here,
  ggplot2,
  tidyverse, 
  sf, gganimate, gifski,
  janitor,
  magick,
  writexl
  
)
#Dropbox path----


if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

rct_data_path <- file.path(
  DROPBOX,
  "Rwanda Energy/EAQIP/datawork/RCT_data",
  "baseline/data/data"
)

historical_data_path <- file.path(
  DROPBOX,
  "Rwanda Energy/EAQIP/datawork/Historical Expansion"
)


#Read 2011 for test--------



#Read files-----

rwa_district <- st_read(dsn = file.path(historical_data_path, "data","rwa_district", "District.shp"))

rwa_villages <- st_read(dsn = file.path(historical_data_path, "data","rwa_villages", "Village.shp"))
rwa_villages <- st_make_valid(rwa_villages)
rwa_villages <- st_zm(rwa_villages)




#1.viirs map of Rwanda-----


##2014----
ntl_2014 <- raster(file.path(historical_data_path,"data", "Nightlight", "data","LRCC-DVNL data","rwa_cropped", "2014.tif"), 1)

plot(ntl_2014)
rwa_district_transformed <- st_transform(rwa_district, crs = st_crs(ntl_2014))

ntl_2014 <- crop(ntl_2014, rwa_district_transformed)
ntl_2014 <- mask(ntl_2014, rwa_district_transformed)

df_2014 <- rasterToPoints(ntl_2014) %>%
  as.data.frame() %>% 
  rename(value = X2014)

plot(ntl_2014)


df_2014_plot <- df_2014 %>%
  mutate(
    value = ifelse(value < 0, 0, value), # Cap values at 2
    value = ifelse(value >= 2, 2, value),
    value = ifelse(value >= 5, 5, value), 
    value_adj = (value)^(1/4)  # Take the 1/4th root of value
  )

df_2014_sf <- st_as_sf(
  df_2014,
  coords = c("x", "y"),
  crs = st_crs(ntl_2014)  # will be aligned below if needed
)

df_2014_sf <- st_transform(df_2014_sf, st_crs(rwa_villages))

df_2014_join<- st_intersection(df_2014_sf, rwa_villages)

df_2014_join <- df_2014_join %>%
  st_drop_geometry() %>%
  clean_names() %>% 
  group_by(village_id) %>%
  summarize(
    ntl_2014 = sum(value, na.rm = TRUE)
  )




p_2014 <- ggplot(data = df_2014_plot) +
  geom_tile(aes(x = x, y = y, fill = value_adj)) +  # Continuous fill based on value_adj
  scale_fill_gradient(
    low = "black", high = "white"  # Continuous gradient from dark to bright yellow
  ) +
  theme_void()+ # Remove background grids and axes
  theme(legend.position = "none")  # remove legend


p_2014




#2.gif----


years <- 2010:2022

output_dir <- file.path(
  historical_data_path,
  "outputs"
)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

#Save one pic per year
for (year in years) {
  
  message("Processing year: ", year)
  
  ntl <- raster(
    file.path(
      historical_data_path,
      "data", "Nightlight", "data", "LRCC-DVNL data", "rwa_cropped",
      paste0(year, ".tif")
    ),
    1
  )
  
  rwa_district_transformed <- st_transform(
    rwa_district,
    crs = st_crs(ntl)
  )
  
  ntl <- crop(ntl, rwa_district_transformed)
  ntl <- mask(ntl, rwa_district_transformed)
  
  df <- rasterToPoints(ntl) %>%
    as.data.frame()
  names(df)[3] <- "value"
  
  df <- df %>%
    mutate(
      value = ifelse(value < 0, 0, value),
      value = ifelse(value >= 5, 5, value),
      value_adj = value^(1/4)
    ) %>%
    filter(!is.na(value_adj))
  
  p <- ggplot(df) +
    geom_tile(aes(x = x, y = y, fill = value_adj)) +
    scale_fill_gradient(low = "black", high = "white") +
    coord_equal(expand = FALSE) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    ggtitle(year)
  
  ggsave(
    filename = file.path(output_dir, "plot", paste0("ntl_", year, ".png")),
    plot = p,
    width = 6,
    height = 6,
    dpi = 300
  )
}

#Animate gif
png_files <- file.path(
  output_dir,
  paste0("ntl_", years, ".png")
)

stopifnot(all(file.exists(png_files)))

image_read(png_files) %>%
  image_animate(fps = 2) %>%
  image_write(
    path = file.path(
      output_dir, "plot",
      "rwanda_nightlights_2010_2022.gif"
    )
  )



#3.join the dataests-------

years <- 2010:2022

compute_village_ntl <- function(year) {
  
  message("Processing year: ", year)
  
  # ---- Read raster
  ntl <- raster(
    file.path(
      historical_data_path,
      "data","Nightlight", "data", "LRCC-DVNL data", "rwa_cropped",
      paste0(year, ".tif")
    ),
    1
  )
  
  # ---- Crop & mask using villages (safer spatially)
  rwa_villages_ntl <- st_transform(rwa_villages, st_crs(ntl))
  ntl <- mask(crop(ntl, rwa_villages_ntl), rwa_villages_ntl)
  
  # ---- Raster
  df <- rasterToPoints(ntl) %>%
    as.data.frame()
  names(df)[3] <- "value"
  
  # ---- Apply SAME transformation logic
  df <- df %>%
    mutate(
      value = ifelse(value < 0, 0, value),
      # value = ifelse(value >= 5, 5, value)
    )
  
  # ---- Points → sf (RASTER CRS!) 
  df_sf <- st_as_sf(
    df,
    coords = c("x", "y"),
    crs = st_crs(ntl)
  )
  
  # ---- Transform points to village CRS
  df_sf <- st_transform(df_sf, st_crs(rwa_villages))
  
  # ---- Spatial join (preferred over st_intersection)
  df_join <- st_join(
    df_sf,
    rwa_villages,
    join = st_intersects,
    left = FALSE
  )
  
  # ---- Aggregate to village level
  df_join %>%
    st_drop_geometry() %>%
    clean_names() %>%
    group_by(village_id) %>%
    summarize(
      !!paste0("ntl_", year) := sum(value, na.rm = TRUE),
      .groups = "drop"
    )
}

# ---- Run for all years
village_ntl_list <- map(years, compute_village_ntl)

# ---- Join into ONE wide table
village_ntl_all <- reduce(
  village_ntl_list,
  full_join,
  by = "village_id"
)

write_xlsx(village_ntl_all, path = file.path(historical_data_path,"data", "Nightlight", "data", "LRCC-DVNL data", "village_ntl(2010-2022).xlsx"))
