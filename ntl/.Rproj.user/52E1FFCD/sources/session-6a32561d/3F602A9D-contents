library(raster)
library(exactextractr)
library(dplyr)
# library(rgdal)
library(here)
library(ggplot2)
library(sf)
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
  "Rwanda Energy/EAQIP/datawork/Historical data"
)


#2010----
#Now it is 2014 new viirs nightlights but kept 2010 for convenience, It's really 2014. Just need to change dataset. 

ntl_2010 <- raster(file.path(historical_data_path, "Nightlight", "data", "rw_viirs_corrected_monthly_start_201401_avg_rad.tif"), 1)

rwa_boundary <- st_read(dsn = file.path(rct_data_path, "rwa_boundary", "RWA_adm0.shp"))
rwa_boundary <- st_transform(rwa_boundary, crs = st_crs(ntl_2010))
rwa_boundary <- rwa_boundary %>% 
  st_make_valid()

rwa_district <- st_read(dsn = file.path(rct_data_path, "rwa_district", "District.shp"))
rwa_district <- st_transform(rwa_district, crs = st_crs(ntl_2010))

rwa_villages <- st_read(dsn = file.path(rct_data_path, "rwa_villages", "Village.shp"))
rwa_villages <- st_zm(rwa_villages)

mv_2011 <- st_read(dsn = file.path(historical_data_path, "Existing Electrical Network_2011", "Existing_MVLine.shp"))
mv_2011 <- st_transform(mv_2011, crs = st_crs(ntl_2010))

hv_2011 <- st_read(dsn = file.path(historical_data_path, "Existing Electrical Network_2011", "Existing_HVLine.shp"))
hv_2011 <- st_transform(hv_2011, crs = st_crs(ntl_2010))

transformer_2011 <- st_read(dsn = file.path(historical_data_path, "Existing Electrical Network_2011", "Existing_Transformer.shp"))
transformer_2011 <- st_transform(transformer_2011, crs = st_crs(transformer_2011))

##Ploting 2011----
ntl_2010 <- crop(ntl_2010, rwa_boundary)
ntl_2010 <- mask(ntl_2010, rwa_boundary)

plot(ntl_2010)

df_2010 <- rasterToPoints(ntl_2010, spatial = TRUE) %>% as.data.frame()
names(df_2010) <- c("value", "x", "y")

summary(df_2010$value)

thresholds <- c(0.01, 0.02, 0.03, 0.05, 0.1, 0.3, 0.5, 1, 2, 7)
output_path <- "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/Historical data/Nightlight/output"

for (threshold in thresholds) {
  # Create a new column based on the current threshold
  df_2010 <- df_2010 %>% 
    mutate(
      value_threshold = ifelse(value >= threshold, 1, 0)
    )
  
  # Plot using the dynamically created column
  p <- ggplot(data = df_2010) +
    geom_tile(aes(x = x, y = y, fill = factor(value_threshold))) +  # Use dynamic threshold column
    geom_sf(aes(color = "MV Line"), data = mv_2011, size = 1, linetype = "solid") +  # MV lines with legend
    geom_sf(aes(color = "HV Line"), data = hv_2011, size = 1, linetype = "solid") +  # HV lines with legend
    geom_sf(aes(color = "Transformer"), data = transformer_2011, size = 0.5) +  # Transformers with legend
    scale_fill_manual(
      values = c("0" = "black", "1" = "yellow"),
      name = paste("Raster value >=", threshold)
    ) +  # Dynamic legend title
    scale_color_manual(
      values = c("MV Line" = "blue", "HV Line" = "red", "Transformer" = "green"),
      name = "Infrastructure"  # Title for the infrastructure legend
    ) +
    theme_void() +  # Remove background grids and axes
    theme(
  
      legend.position = "right",  # Position of the legend
      legend.title = element_text(size = 18),  # Legend title size
      legend.text = element_text(size = 16),  # Legend text size
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Centered title, larger size
      plot.caption = element_text(size = 25)  # Caption size
    ) +
    labs(
      title = paste("Electrification and Voltage Lines Overlay (2014 ntl and 2011 infrastructure) - Threshold:", threshold),
      fill = paste("Raster value >=", threshold),  # Legend title for fill
      caption = "Source: Harmonized Nightlights data 1992-2021"
    )
  
  # Save the plot with a dynamic filename to the specified path
  ggsave(
    filename = file.path(output_path, paste0("overlay_plot_threshold_", threshold, ".png")),
    plot = p,
    width = 10, height = 8, dpi = 300  # Adjust size and resolution if needed
  )
}

#2021----

ntl_2021 <- raster(file.path(historical_data_path, "harmonized1992-2020", "Harmonized_DN_NTL_2021_simVIIRS.tif"), 1)

mv_2022 <- st_read(dsn = file.path(historical_data_path, "Existing Electrical Network_2022", "Existing_MVLine.shp"))
mv_2022 <- st_transform(mv_2022, crs = st_crs(ntl_2021))

hv_2022 <- st_read(dsn = file.path(historical_data_path, "Existing Electrical Network_2022", "Existing_HVLine.shp"))
hv_2022 <- st_transform(hv_2022, crs = st_crs(ntl_2021))

transformer_2022 <- st_read(dsn = file.path(historical_data_path, "Existing Electrical Network_2022", "Existing_Transformer.shp"))
transformer_2022 <- st_transform(transformer_2022, crs = st_crs(ntl_2021))

##Ploting----
ntl_2021 <- crop(ntl_2021, rwa_district)
ntl_2021 <- mask(ntl_2021, rwa_district)

plot(ntl_2021)

df_2021 <- rasterToPoints(ntl_2021, spatial = TRUE) %>% as.data.frame()
names(df_2021) <- c("value", "x", "y")

summary(df_2021$value)


# Thresholds for analysis
thresholds_2022 <- c(7, 8 , 9 , 10, 11, 12)
output_path <- "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/Historical data/Nightlight/output"

# Looping through each threshold to generate the plots
for (threshold in thresholds_2022) {
  # Create a new column based on the current threshold
  df_2021 <- df_2021 %>%
    mutate(value_threshold = ifelse(value >= threshold, 1, 0))
  
  # Plot using the dynamically created column
  p <- ggplot(data = df_2021) +
    geom_tile(aes(x = x, y = y, fill = factor(value_threshold))) +  # Use dynamic threshold column
    geom_sf(aes(color = "MV Line"), data = mv_2022, size = 1, linetype = "solid") +  # MV lines with legend
    geom_sf(aes(color = "HV Line"), data = hv_2022, size = 1, linetype = "solid") +  # HV lines with legend
    geom_sf(aes(color = "Transformer"), data = transformer_2022, size = 0.5) +  # Transformers with legend
    scale_fill_manual(
      values = c("0" = "black", "1" = "yellow"),
      name = paste("Raster value >=", threshold)
    ) +  # Dynamic legend title
    scale_color_manual(
      values = c("MV Line" = "blue", "HV Line" = "red", "Transformer" = "green"),
      name = "Infrastructure"  # Title for the infrastructure legend
    ) +
    theme_void() +  # Remove background grids and axes
    theme(
      legend.position = "right",  # Position of the legend
      legend.title = element_text(size = 18),  # Legend title size
      legend.text = element_text(size = 16),  # Legend text size
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Centered title, larger size
      plot.caption = element_text(size = 25)  # Caption size
    ) +
    labs(
      title = paste("Electrification and Voltage Lines Overlay (2021) - Threshold:", threshold),
      fill = paste("Raster value >=", threshold),  # Legend title for fill
      caption = "Source: Harmonized Nightlights data 1992-2021"
    )
  
  # Save the plot with a dynamic filename to the specified path
  ggsave(
    filename = file.path(output_path, paste0("overlay_plot_2021_threshold_", threshold, ".png")),
    plot = p,
    width = 10, height = 8, dpi = 300  # Adjust size and resolution if needed
  )
}

#Separate districts-----

districts <- c("Burera", "Nyagatare", "Musanze", "Rubavu", "Rusizi", "Ramagana")
thresholds_2022 <- c(1, 7, 8 , 9 , 10, 11, 12)


# Looping through each threshold to generate the plots
for (district in districts) {
  # Filter rwa_district to only include the current district
  district_boundary <- rwa_district %>% filter(District == district)  # Replace 'district_name' with actual column
  
  # Crop and mask nightlights data for the specific district
  ntl_2021_district <- crop(ntl_2021, district_boundary)
  ntl_2021_district <- mask(ntl_2021_district, district_boundary)
  
  # Crop MV, HV, and Transformer data for the district using st_intersection
  mv_2022_district <- st_intersection(mv_2022, district_boundary)  # Crop MV lines to district boundary
  hv_2022_district <- st_intersection(hv_2022, district_boundary)  # Crop HV lines to district boundary
  transformer_2022_district <- st_intersection(transformer_2022, district_boundary)  # Crop Transformers to district boundary
  
  # Convert raster to points for the district
  df_2021 <- rasterToPoints(ntl_2021_district, spatial = TRUE) %>% as.data.frame()
  names(df_2021) <- c("value", "x", "y")
  
  for (threshold in thresholds_2022) {
    # Apply the threshold
    df_2021 <- df_2021 %>%
      mutate(value_threshold = ifelse(value >= threshold, 1, 0))
    
    # Create the plot
    p <- ggplot(data = df_2021) +
      geom_tile(aes(x = x, y = y, fill = factor(value_threshold))) +  # Use dynamic threshold column
      geom_sf(aes(color = "MV Line"), data = mv_2022_district, size = 1, linetype = "solid") +  # MV lines with legend
      geom_sf(aes(color = "HV Line"), data = hv_2022_district, size = 1, linetype = "solid") +  # HV lines with legend
      geom_sf(aes(color = "Transformer"), data = transformer_2022_district, size = 0.5) +  # Transformers with legend
      scale_fill_manual(
        values = c("0" = "black", "1" = "yellow"),
        name = paste("Raster value >=", threshold)
      ) +  # Dynamic legend title
      scale_color_manual(
        values = c("MV Line" = "blue", "HV Line" = "red", "Transformer" = "green"),
        name = "Infrastructure"  # Title for the infrastructure legend
      ) +
      theme_void() +  # Remove background grids and axes
      theme(
        legend.position = "right",  # Position of the legend
        legend.title = element_text(size = 18),  # Legend title size
        legend.text = element_text(size = 16),  # Legend text size
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Centered title, larger size
        plot.caption = element_text(size = 25)  # Caption size
      ) +
      labs(
        title = paste("Electrification and Voltage Lines Overlay (2021)", district, "- Threshold:", threshold),
        fill = paste("Raster value >=", threshold),  # Legend title for fill
        caption = "Source: Harmonized Nightlights data 1992-2021"
      )
    
    # Save the plot with a dynamic filename to the specified path
    ggsave(
      filename = file.path(output_path, paste0("overlay_plot_", district, "_2021_threshold_", threshold, ".png")),
      plot = p,
      width = 10, height = 8, dpi = 300  # Adjust size and resolution if needed
    )
  }
}














#Using VIIRS data---------

##2022---- 
#(using 2021 for convenience )

ntl_2022 <- raster(file.path(historical_data_path, "Nightlight", "data", "rw_viirs_corrected_monthly_start_201401_avg_rad.tif"), 96)

mv_2022 <- st_read(dsn = file.path(historical_data_path, "Existing Electrical Network_2022", "Existing_MVLine.shp"))
mv_2022 <- st_transform(mv_2022, crs = st_crs(ntl_2022))

hv_2022 <- st_read(dsn = file.path(historical_data_path, "Existing Electrical Network_2022", "Existing_HVLine.shp"))
hv_2022 <- st_transform(hv_2022, crs = st_crs(ntl_2022))

transformer_2022 <- st_read(dsn = file.path(historical_data_path, "Existing Electrical Network_2022", "Existing_Transformer.shp"))
transformer_2022 <- st_transform(transformer_2022, crs = st_crs(ntl_2022))

##Ploting----
ntl_2022 <- crop(ntl_2022, rwa_district)
ntl_2022 <- mask(ntl_2022, rwa_district)

plot(ntl_2022)

df_2022 <- rasterToPoints(ntl_2022, spatial = TRUE) %>% as.data.frame()
names(df_2022) <- c("value", "x", "y")

summary(df_2022$value)


# Thresholds for analysis
thresholds_2022 <- c(0.01, 0.02, 0.03, 0.05, 0.1, 0.3, 0.5, 1, 2, 7)
output_path <- "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/Historical data/Nightlight/output"

# Looping through each threshold to generate the plots
for (threshold in thresholds_2022) {
  # Create a new column based on the current threshold
  df_2022 <- df_2022 %>%
    mutate(value_threshold = ifelse(value >= threshold, 1, 0))
  
  # Plot using the dynamically created column
  p <- ggplot(data = df_2022) +
    geom_tile(aes(x = x, y = y, fill = factor(value_threshold))) +  # Use dynamic threshold column
    geom_sf(aes(color = "MV Line"), data = mv_2022, size = 1, linetype = "solid") +  # MV lines with legend
    geom_sf(aes(color = "HV Line"), data = hv_2022, size = 1, linetype = "solid") +  # HV lines with legend
    geom_sf(aes(color = "Transformer"), data = transformer_2022, size = 0.5) +  # Transformers with legend
    scale_fill_manual(
      values = c("0" = "black", "1" = "yellow"),
      name = paste("Raster value >=", threshold)
    ) +  # Dynamic legend title
    scale_color_manual(
      values = c("MV Line" = "blue", "HV Line" = "red", "Transformer" = "green"),
      name = "Infrastructure"  # Title for the infrastructure legend
    ) +
    theme_void() +  # Remove background grids and axes
    theme(
      legend.position = "right",  # Position of the legend
      legend.title = element_text(size = 18),  # Legend title size
      legend.text = element_text(size = 16),  # Legend text size
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Centered title, larger size
      plot.caption = element_text(size = 25)  # Caption size
    ) +
    labs(
      title = paste("Electrification and Voltage Lines Overlay (2022) - Threshold:", threshold),
      fill = paste("Raster value >=", threshold),  # Legend title for fill
      caption = "Source: Harmonized Nightlights data 1992-2022"
    )
  
  # Save the plot with a dynamic filename to the specified path
  ggsave(
    filename = file.path(output_path, paste0("overlay_plot_2022_threshold_", threshold, ".png")),
    plot = p,
    width = 10, height = 8, dpi = 300  # Adjust size and resolution if needed
  )
}


##2014-----



ntl_2014 <- raster(file.path(historical_data_path, "Nightlight", "data", "rw_viirs_corrected_monthly_start_201401_avg_rad.tif"), 1)

rwa_boundary <- st_read(dsn = file.path(rct_data_path, "rwa_boundary", "RWA_adm0.shp"))
rwa_boundary <- st_transform(rwa_boundary, crs = st_crs(ntl_2014))
rwa_boundary <- rwa_boundary %>% 
  st_make_valid()

rwa_district <- st_read(dsn = file.path(rct_data_path, "rwa_district", "District.shp"))
rwa_district <- st_transform(rwa_district, crs = st_crs(ntl_2014))

rwa_villages <- st_read(dsn = file.path(rct_data_path, "rwa_villages", "Village.shp"))
rwa_villages <- st_zm(rwa_villages)

mv_2011 <- st_read(dsn = file.path(historical_data_path, "Existing Electrical Network_2011", "Existing_MVLine.shp"))
mv_2011 <- st_transform(mv_2011, crs = st_crs(ntl_2014))

hv_2011 <- st_read(dsn = file.path(historical_data_path, "Existing Electrical Network_2011", "Existing_HVLine.shp"))
hv_2011 <- st_transform(hv_2011, crs = st_crs(ntl_2014))

transformer_2011 <- st_read(dsn = file.path(historical_data_path, "Existing Electrical Network_2011", "Existing_Transformer.shp"))
transformer_2011 <- st_transform(transformer_2011, crs = st_crs(transformer_2011))

##Ploting 2011----
ntl_2014 <- crop(ntl_2014, rwa_boundary)
ntl_2014 <- mask(ntl_2014, rwa_boundary)

plot(ntl_2014)

df_2014 <- rasterToPoints(ntl_2014, spatial = TRUE) %>% as.data.frame()
names(df_2014) <- c("value", "x", "y")

summary(df_2014$value)

thresholds <- c(0.01, 0.02, 0.03, 0.05, 0.1, 0.3, 0.5, 1, 2, 7)
output_path <- "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/Historical data/Nightlight/output"

for (threshold in thresholds) {
  # Create a new column based on the current threshold
  df_2014 <- df_2014 %>% 
    mutate(
      value_threshold = ifelse(value >= threshold, 1, 0)
    )
  
  # Plot using the dynamically created column
  p <- ggplot(data = df_2014) +
    geom_tile(aes(x = x, y = y, fill = factor(value_threshold))) +  # Use dynamic threshold column
    geom_sf(aes(color = "MV Line"), data = mv_2011, size = 1, linetype = "solid") +  # MV lines with legend
    geom_sf(aes(color = "HV Line"), data = hv_2011, size = 1, linetype = "solid") +  # HV lines with legend
    geom_sf(aes(color = "Transformer"), data = transformer_2011, size = 0.5) +  # Transformers with legend
    scale_fill_manual(
      values = c("0" = "black", "1" = "yellow"),
      name = paste("Raster value >=", threshold)
    ) +  # Dynamic legend title
    scale_color_manual(
      values = c("MV Line" = "blue", "HV Line" = "red", "Transformer" = "green"),
      name = "Infrastructure"  # Title for the infrastructure legend
    ) +
    theme_void() +  # Remove background grids and axes
    theme(
      
      legend.position = "right",  # Position of the legend
      legend.title = element_text(size = 18),  # Legend title size
      legend.text = element_text(size = 16),  # Legend text size
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Centered title, larger size
      plot.caption = element_text(size = 25)  # Caption size
    ) +
    labs(
      title = paste("Electrification and Voltage Lines Overlay (2014 ntl and 2011 infrastructure) - Threshold:", threshold),
      fill = paste("Raster value >=", threshold),  # Legend title for fill
      caption = "Source: Harmonized Nightlights data 1992-2021"
    )
  
  # Save the plot with a dynamic filename to the specified path
  ggsave(
    filename = file.path(output_path, paste0("overlay_plot_2014_threshold_", threshold, ".png")),
    plot = p,
    width = 10, height = 8, dpi = 300  # Adjust size and resolution if needed
  )
}







##2023----
ntl_2023 <- raster(file.path(historical_data_path, "harmonized1992-2020", "rw_viirs_corrected_monthly_start_201401_avg_rad.tif"), 101)
ntl_2023 <- crop(ntl_2023, rwa_boundary)
ntl_2023 <- mask(ntl_2023, rwa_boundary)
plot(ntl_2023)

# df_2023 <- ntl_2023 %>%  mask(rwa_villages)

df_2023 <- rasterToPoints(ntl_2023, spatial = TRUE) %>% as.data.frame()
names(df_2023) <- c("value", "x", "y")


plot_2023 <- ggplot() +
  geom_tile(data = df_2023, 
            aes(x = x, y = y, 
                fill = value )) +  # Use an indicator that's > 0
  # scale_fill_manual(values = c("black", "yellow")) +  # Custom colors for 0 and > 0
  scale_fill_gradient(low = "black", high = "yellow") +  # Adjust colors as needed
  
  # geom_polygon(data = rwa_sp, aes(x = long, y = lat, group = group),
  #              fill = NA, color = "grey") +
  # labs(title = "Rwanda NTL, January 2022 (Any NTL)") +
  coord_quickmap() + 
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none")

plot_2023




##2010----
ntl_2010.1 <- raster(file.path(historical_data_path, "harmonized1992-2020", "F182010.v4d_web.avg_vis.tif"), 1)
rwa_villages <- st_transform(rwa_villages, crs = st_crs(ntl_2010.1))
ntl_2010.1 <- crop(ntl_2010.1, rwa_villages)
df_2010.1 <- ntl_2010.1 %>%  mask(rwa_district)

df_2010.1 <- rasterToPoints(df_2010.1, spatial = TRUE) %>% as.data.frame()
names(df_2010.1) <- c("value", "x", "y")


summary(df_2010.1$value)




plot_2010.1 <- ggplot() +
  geom_tile(data = df_2010.1, 
            aes(x = x, y = y, 
                fill = value )) +  # Use an indicator that's > 0
  # scale_fill_manual(values = c("black", "yellow")) +  # Custom colors for 0 and > 10
  scale_fill_gradient(low = "black", high = "yellow") +  # Adjust colors as needed
  # geom_polygon(data = rwa_sp, aes(x = long, y = lat, group = group),
  #              fill = NA, color = "grey") +
  # labs(title = "Rwanda NTL, January 2022 (Any NTL)") +
  coord_quickmap() + 
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none")

plot_2010.1


##2014----
ntl_2014 <- raster(file.path(historical_data_path, "harmonized1992-2020", "rw_viirs_corrected_monthly_start_201401_avg_rad.tif"), 1)

df_2014 <- ntl_2014 %>%  mask(rwa_villages)

df_2014 <- rasterToPoints(df_2014, spatial = TRUE) %>% as.data.frame()
names(df_2014) <- c("value", "x", "y")

df_2014$value[df_2014$value <= 0] <- 0
df_2014$value_adj <- log(df_2014$value+1)

summary(df_2014$value_adj)

plot_2014 <- ggplot() +
  geom_tile(data = df_2014, 
            aes(x = x, y = y, 
                fill = value_adj)) +  # Use an indicator that's > 0
  # scale_fill_manual(values = c("black", "yellow")) +
  scale_fill_gradient(low = "black", high = "yellow") +  # Adjust colors as needed
  # Custom colors for 0 and > 0
  # geom_polygon(data = rwa_sp, aes(x = long, y = lat, group = group),
  #              fill = NA, color = "grey") +
  # labs(title = "Rwanda NTL, January 2022 (Any NTL)") +
  coord_quickmap() + 
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none")

plot_2014



#Muhanga: Using the old ntl data----

##2010----
ntl_2010.1 <- raster(file.path(historical_data_path, "harmonized1992-2020", "Harmonized_DN_NTL_2010_calDMSP.tif"), 1)
muhanga.1 <- st_transform(muhanga, crs = st_crs(ntl_2010.1))
muhanga_2010.1 <- crop(ntl_2010.1, muhanga.1)
muhanga_2010.1 <- muhanga_2010.1 %>%  mask(muhanga.1)

muhanga_2010.1 <- rasterToPoints(muhanga_2010.1, spatial = TRUE) %>% as.data.frame()
names(muhanga_2010.1) <- c("value", "x", "y")


summary(muhanga_2010.1$value)


plot_2010.1 <- ggplot() +
  geom_tile(data = muhanga_2010.1, 
            aes(x = x, y = y, 
                fill = value )) +  # Use an indicator that's > 0
  scale_fill_gradient(low = "black", high = "yellow") +  # Adjust colors as needed
  geom_sf(data = muhanga,  fill = NA, color = "lightgrey") +
  labs(title = "Muhanga NTL 2010") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none")

plot_2010.1

##2023----
ntl_2023 <- raster(file.path(historical_data_path, "harmonized1992-2020", "Harmonized_DN_NTL_2021_simVIIRS.tif"), 1)
muhanga <- rwa_villages %>% 
  filter(District %in% c("Muhanga"))
muhanga <- st_transform(muhanga, crs = st_crs(ntl_2023))
muhanga <- muhanga %>% st_zm()
ntl_2023 <- crop(ntl_2023, muhanga)
muhanga_2023 <- ntl_2023 %>%  mask(muhanga)

muhanga_2023 <- rasterToPoints(muhanga_2023, spatial = TRUE) %>% as.data.frame()
names(muhanga_2023) <- c("value", "x", "y")

muhanga_2023$value[muhanga_2023$value <= 0] <- 0
muhanga_2023$value[muhanga_2023$value < 1] <- 1


muhanga_2023_plot <- ggplot() +
  geom_tile(data = muhanga_2023, 
            aes(x = x, y = y, 
                fill = value )) +  # Use an indicator that's > 0
  scale_fill_gradient(low = "black", high = "yellow") +  # Adjust colors as needed
  geom_sf(data = muhanga,  fill = NA, color = "lightgrey") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none") + 
  labs(title = "Muhanga District NTL 2021") 


muhanga_2023_plot




