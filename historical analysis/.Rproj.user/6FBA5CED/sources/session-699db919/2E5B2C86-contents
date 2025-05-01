##########################
#Author: Xiaoming Zhang
#Date of last modification: 0923024
#purpose:Year of electrification for each village
############################


#library----

pacman::p_load(fixest, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, plm, haven, stringr, modelsummary, kableExtra, stargazer, lfe, ggfixest, magick)


#read file----

if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX, "Rwanda Energy/EAQIP/datawork/Historical data"
)

rct_data_path <- file.path(
  DROPBOX,
  "Rwanda Energy/EAQIP/datawork/RCT_data",
  "baseline/data/data"
)

rwa_villages <- st_read(dsn = file.path(rct_data_path, "rwa_villages", "Village_area.shp"))
rwa_villages <- st_make_valid(rwa_villages)


rwa_long <- read_xlsx(path = file.path(data_path, "rwa_regress_villagelevel.xlsx"))
usage_id <- read_xlsx(path = file.path(data_path, "usage_id_0416.xlsx"))


usage_year <- usage_id %>% 
  group_by(village_id) %>% 
  summarise(
    year_first = min(year_first)
  )

n_distinct(usage_year$village_id)

rwa_fraction <- left_join(rwa_villages, usage_year, by = c("Village_ID" = "village_id"))


village_number <- rwa_villages %>% 
  group_by(District) %>% 
  summarise(
    n = n()
  )


#Bar plot----

elec_fraction <- rwa_fraction %>% 
  filter(! District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu")) %>% 
  group_by(year_first, District) %>% 
  summarise(
    connected = n()
  ) %>% 
  ungroup()

elec_fraction <- st_drop_geometry(elec_fraction)


elec_fraction_plot <- elec_fraction %>%
  left_join(village_number, by = c("District")) %>%  # Join with village_number by District
  arrange(District, year_first) %>%  # Arrange by District and year_first to ensure cumulative sum works correctly
  group_by(District) %>%  # Group by District
  mutate(connected_cumulative = cumsum(connected)) %>%  # Calculate cumulative sum of connected
  ungroup()  # Remove grouping after the operation


elec_fraction_plot <- elec_fraction %>% 
  st_drop_geometry() %>% 
  mutate(
    fraction = round(connected_cumulative/n,2),
    percentage = round(connected_cumulative/n*100, 2)
  ) %>% 
  filter(!is.na(year_first)) %>% 
  clean_names()

table(elec_fraction_plot$district)

bugesera <- elec_fraction_plot %>% 
  filter(district == "Bugesera")


ggplot(bugesera, aes(x = factor(year_first), y = fraction)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = paste("Fraction of Electrification per Year for", "bugesera"),
    x = "Year",
    y = "Fraction"
  ) +
  theme_minimal()



elec_fraction_plot %>%
  st_drop_geometry() %>%
  clean_names() %>%
  filter(!is.na(year_first)) %>%
  split(.$district) %>%  # Split data by district
  walk(function(df) {
    district_name <- unique(df$district)  # Get the district name
    
    # Generate the bar plot with percentage labels
    plot <- ggplot(df, aes(x = factor(year_first), y = fraction)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5) +  # Add percentage labels
      labs(
        title = paste("Fraction of Electrification per Year for", district_name),
        x = "Year",
        y = "Fraction"
      ) +
      theme_minimal()
    
    # Save the plot to the specified directory
    ggsave(
      filename = file.path(data_path, "elec_bar_plot", paste0(district_name, "_fraction_plot.png")),
      plot = plot,
      width = 8, height = 6
    )
  })




#Map----


###Sector------

rwa_cell <- st_read(dsn = file.path(rct_data_path, "rwa_cell", "Cell.shp"))
rwa_district <- st_read(dsn = file.path(rct_data_path, "rwa_district", "District.shp"))
rwa_district <- st_transform(rwa_district, crs = st_crs(rwa_cell))


rwa_boundary = st_read(dsn = file.path(rct_data_path,"rwa_boundary", "RWA_adm0.shp"))
rwa_boundary = st_transform(rwa_boundary, crs = st_crs(rwa_cell))

rwa_cell <- rwa_cell %>% 
  clean_names() %>% 
  mutate(
    cell_id = as.character(cell_id)
  )



elec_cell <- rwa_regress %>% 
  clean_names() %>% 
  select(year_first, cell_id) %>% 
  distinct(cell_id, .keep_all = TRUE)


elec_cell <- left_join(rwa_cell, elec_cell, by = c("cell_id"))


elec_cell <- elec_cell %>% 
  group_by(cell_id) %>% 
  mutate(
    year_first = min(year_first)
  )

elec_cell <- elec_cell %>% 
  mutate(
    status = case_when(
      year_first <= 2017 & year_first >= 2013 ~ "electrified 2013-2017",
      year_first == 2300 | is.na(year_first) ~ "not yet electrified",
      .default = "other"
    )
  ) %>% 
  distinct(cell_id, .keep_all = TRUE)


elec_cell <- elec_cell %>% 
  st_make_valid()

elec_cell <- st_transform(elec_cell, crs = st_crs(rwa_cell))

elec_cell$status <- factor(elec_cell$status, levels = c("electrified 2013-2017", "other", "not yet electrified"))
# Plot the map with ggplot2, filling by status


ggplot() +
  geom_sf(data = elec_cell, aes(fill = status), color = NA) +  # Remove borders by setting color = NA
  geom_sf(data = rwa_district, color = "white", fill = NA, size = 10) +
  geom_sf(data = rwa_boundary, color = "black", fill = NA, size = 10) +
  scale_fill_viridis_d() +  # Optional: use a colorblind-friendly palette
  theme_void() + 
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),
    legend.margin = margin(r=10),
    plot.title = element_text(hjust = 0.5), 
    plot.margin = margin(t = 20)  
  ) +
  labs(
    title = "Rwanda Cell by Year of Electrification",
    fill = "Electrified Year"
  ) +
  theme(
    legend.position = "right",  # Position the legend on the right
    legend.title = element_text(size = 18),  # Increase legend title font size
    legend.text = element_text(size = 16),  # Increase legend text font size
    plot.title = element_text(size = 18)  # Increase plot title font size
  )



#Investigating usage-----


usage_id <- read_xlsx(path = file.path(data_path, "usage_id_0416.xlsx"))
usage_noid <- read_xlsx(path = file.path(data_path, "usage_noid_0416.xlsx"))
husage_id <- usage_id %>% 
  select(-village_id.x) 



map_output_path <- file.path(data_path, "Electrification map")

##Gisagara-----
usage_gisagara <- usage_id %>% 
  filter(district %in% c("Gisagara"))

gisagara_villages <- rwa_villages %>% 
  filter(District %in% c("Gisagara")) %>% 
  clean_names() %>% 
  select(village_id)


usage_gisagara <- usage_gisagara %>%
  group_by(village_id) %>%
  summarise(
    `2010` = sum(`2010_usage`),
    `2011` = sum(`2011_usage`),
    `2012` = sum(`2012_usage`),
    `2013` = sum(`2013_usage`),
    `2014` = sum(`2014_usage`),
    `2015` = sum(`2015_usage`),
    `2016` = sum(`2016_usage`),
    `2017` = sum(`2017_usage`),
    `2018` = sum(`2018_usage`),
    `2019` = sum(`2019_usage`),
    `2020` = sum(`2020_usage`),
    `2021` = sum(`2021_usage`),
    `2022` = sum(`2022_usage`), 
    year_first = min(year_first)
  ) 

usage_gisagara <- left_join(usage_gisagara, gisagara_villages, by = c("village_id"))
View(usage_gisagara)


usage_gisagara_long <- usage_gisagara %>%
  pivot_longer(cols = `2010`:`2022`, names_to = "year", values_to = "usage") %>% 
  mutate(
    usage_yes = ifelse(usage >0, 1, 0)
  )




  
year_2010 <- usage_gisagara_long %>% 
  filter(year == 2010)

ggplot() +
  geom_sf(data = year_2010, aes(fill = as.factor(usage_yes), geometry = geometry)) +  # Remove borders by setting color = NA
  geom_sf(data = gisagara_villages, color = "black", fill = NA, size = 10) +
  scale_fill_viridis_d() +  # Optional: use a colorblind-friendly palette
  theme_void() + 
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),
    legend.margin = margin(r=10),
    plot.title = element_text(hjust = 0.5), 
    plot.margin = margin(t = 20)  
  ) +
  labs(
    title = "Gisagara Village by Year of Electrification",
    fill = "Usage Observed"
  ) +
  theme(
    legend.position = "right",  # Position the legend on the right
    legend.title = element_text(size = 18),  # Increase legend title font size
    legend.text = element_text(size = 16),  # Increase legend text font size
    plot.title = element_text(size = 18)  # Increase plot title font size
  )



# Loop through the years from 2010 to 2022
for (year in 2010:2021) {
  
  # Filter data for the specific year
  year_data <- usage_gisagara_long %>% 
    filter(year == !!year)
  
  # Generate the plot
  p <- ggplot() +
    geom_sf(data = year_data, aes(fill = as.factor(usage_yes), geometry = geometry)) +
    geom_sf(data = gisagara_villages, color = "black", fill = NA, size = 0.5) +
    scale_fill_viridis_d() +  # Colorblind-friendly palette
    theme_void() + 
    theme(
      plot.background = element_rect(fill = 'white', color = 'white'),
      legend.margin = margin(r = 10),
      plot.title = element_text(hjust = 0.5), 
      plot.margin = margin(t = 20)
    ) +
    labs(
      title = paste("Gisagara Village Electrification -", year),
      fill = "Usage Observed"
    ) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 16),
      plot.title = element_text(size = 18)
    )
  
  # Save each plot as a PNG file
  ggsave(filename = file.path(data_path, "Electrification map", "Gisagara", paste0("gisagara_", year, ".png")), 
         plot = p, 
         width = 10, height = 8, units = "in", dpi = 300)
}


#GIF save
gisagara_map_path <- file.path(data_path, "Electrification map", "Gisagara")

images <- list.files(gisagara_map_path, pattern = "*.png", full.names = TRUE)

img_list <- image_read(images)

gif <- image_animate(img_list, fps = 1)

gif_output_path <- file.path(data_path, "Electrification map", "Gisagara.gif")
image_write(gif, gif_output_path)






##Gicumbi-----
usage_gicumbi <- usage_id %>% 
  filter(district %in% c("Gicumbi"))

gicumbi_villages <- rwa_villages %>% 
  filter(District %in% c("Gicumbi")) %>% 
  clean_names() %>% 
  select(village_id)


usage_gicumbi <- usage_gicumbi %>%
  group_by(village_id) %>%
  summarise(
    `2010` = sum(`2010_usage`),
    `2011` = sum(`2011_usage`),
    `2012` = sum(`2012_usage`),
    `2013` = sum(`2013_usage`),
    `2014` = sum(`2014_usage`),
    `2015` = sum(`2015_usage`),
    `2016` = sum(`2016_usage`),
    `2017` = sum(`2017_usage`),
    `2018` = sum(`2018_usage`),
    `2019` = sum(`2019_usage`),
    `2020` = sum(`2020_usage`),
    `2021` = sum(`2021_usage`),
    `2022` = sum(`2022_usage`), 
    year_first = min(year_first)
  ) 

usage_gicumbi <- left_join(usage_gicumbi, gicumbi_villages, by = c("village_id"))


usage_gicumbi_long <- usage_gicumbi %>%
  pivot_longer(cols = `2010`:`2022`, names_to = "year", values_to = "usage") %>% 
  mutate(
    usage_yes = ifelse(usage >0, 1, 0)
  )


# Loop through the years from 2010 to 2022
for (year in 2010:2021) {
  
  # Filter data for the specific year
  year_data <- usage_gicumbi_long %>% 
    filter(year == !!year)
  
  # Generate the plot
  p <- ggplot() +
    geom_sf(data = year_data, aes(fill = as.factor(usage_yes), geometry = geometry)) +
    geom_sf(data = gicumbi_villages, color = "black", fill = NA, size = 0.5) +
    scale_fill_viridis_d() +  # Colorblind-friendly palette
    theme_void() + 
    theme(
      plot.background = element_rect(fill = 'white', color = 'white'),
      legend.margin = margin(r = 10),
      plot.title = element_text(hjust = 0.5), 
      plot.margin = margin(t = 20)
    ) +
    labs(
      title = paste("gicumbi Village Electrification -", year),
      fill = "Usage Observed"
    ) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 16),
      plot.title = element_text(size = 18)
    )
  
  # Save each plot as a PNG file
  ggsave(filename = file.path(data_path, "Electrification map", "Gicumbi", paste0("gicumbi_", year, ".png")), 
         plot = p, 
         width = 10, height = 8, units = "in", dpi = 300)
}


#GIF save
gicumbi_map_path <- file.path(data_path, "Electrification map", "Gicumbi")

images <- list.files(gicumbi_map_path, pattern = "*.png", full.names = TRUE)

img_list <- image_read(images)

gif <- image_animate(img_list, fps = 1)

gif_output_path <- file.path(data_path, "Electrification map", "Gicumbi.gif")
image_write(gif, gif_output_path)


##Kayonza-----
usage_Kayonza <- usage_id %>% 
  filter(district %in% c("Kayonza"))

Kayonza_villages <- rwa_villages %>% 
  filter(District %in% c("Kayonza")) %>% 
  clean_names() %>% 
  select(village_id)


usage_Kayonza <- usage_Kayonza %>%
  group_by(village_id) %>%
  summarise(
    `2010` = sum(`2010_usage`),
    `2011` = sum(`2011_usage`),
    `2012` = sum(`2012_usage`),
    `2013` = sum(`2013_usage`),
    `2014` = sum(`2014_usage`),
    `2015` = sum(`2015_usage`),
    `2016` = sum(`2016_usage`),
    `2017` = sum(`2017_usage`),
    `2018` = sum(`2018_usage`),
    `2019` = sum(`2019_usage`),
    `2020` = sum(`2020_usage`),
    `2021` = sum(`2021_usage`),
    `2022` = sum(`2022_usage`), 
    year_first = min(year_first)
  ) 

usage_Kayonza <- left_join(usage_Kayonza, Kayonza_villages, by = c("village_id"))


usage_Kayonza_long <- usage_Kayonza %>%
  pivot_longer(cols = `2010`:`2022`, names_to = "year", values_to = "usage") %>% 
  mutate(
    usage_yes = ifelse(usage >0, 1, 0)
  )


# Loop through the years from 2010 to 2022
for (year in 2010:2021) {
  
  # Filter data for the specific year
  year_data <- usage_Kayonza_long %>% 
    filter(year == !!year)
  
  # Generate the plot
  p <- ggplot() +
    geom_sf(data = year_data, aes(fill = as.factor(usage_yes), geometry = geometry)) +
    geom_sf(data = Kayonza_villages, color = "black", fill = NA, size = 0.5) +
    scale_fill_viridis_d() +  # Colorblind-friendly palette
    theme_void() + 
    theme(
      plot.background = element_rect(fill = 'white', color = 'white'),
      legend.margin = margin(r = 10),
      plot.title = element_text(hjust = 0.5), 
      plot.margin = margin(t = 20)
    ) +
    labs(
      title = paste("Kayonza Village Electrification -", year),
      fill = "Usage Observed"
    ) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 16),
      plot.title = element_text(size = 18)
    )
  
  # Save each plot as a PNG file
  ggsave(filename = file.path(data_path, "Electrification map", "Kayonza", paste0("Kayonza_", year, ".png")), 
         plot = p, 
         width = 10, height = 8, units = "in", dpi = 300)
}


#GIF save
Kayonza_map_path <- file.path(data_path, "Electrification map", "Kayonza")

images <- list.files(Kayonza_map_path, pattern = "*.png", full.names = TRUE)

img_list <- image_read(images)

gif <- image_animate(img_list, fps = 1)

gif_output_path <- file.path(data_path, "Electrification map", "Kayonza.gif")
image_write(gif, gif_output_path)



##Rwamagana-----
usage_Rwamagana <- usage_id %>% 
  filter(district %in% c("Rwamagana"))

Rwamagana_villages <- rwa_villages %>% 
  filter(District %in% c("Rwamagana")) %>% 
  clean_names() %>% 
  select(village_id) %>% 
  st_make_valid()


usage_Rwamagana <- usage_Rwamagana %>%
  group_by(village_id) %>%
  summarise(
    `2010` = sum(`2010_usage`),
    `2011` = sum(`2011_usage`),
    `2012` = sum(`2012_usage`),
    `2013` = sum(`2013_usage`),
    `2014` = sum(`2014_usage`),
    `2015` = sum(`2015_usage`),
    `2016` = sum(`2016_usage`),
    `2017` = sum(`2017_usage`),
    `2018` = sum(`2018_usage`),
    `2019` = sum(`2019_usage`),
    `2020` = sum(`2020_usage`),
    `2021` = sum(`2021_usage`),
    `2022` = sum(`2022_usage`), 
    year_first = min(year_first)
  ) 

usage_Rwamagana <- left_join(usage_Rwamagana, Rwamagana_villages, by = c("village_id"))


usage_Rwamagana_long <- usage_Rwamagana %>%
  pivot_longer(cols = `2010`:`2022`, names_to = "year", values_to = "usage") %>% 
  mutate(
    usage_yes = ifelse(usage >0, 1, 0)
  )


# Loop through the years from 2010 to 2022
for (year in 2010:2021) {
  
  # Filter data for the specific year
  year_data <- usage_Rwamagana_long %>% 
    filter(year == !!year)
  
  # Generate the plot
  p <- ggplot() +
    geom_sf(data = year_data, aes(fill = as.factor(usage_yes), geometry = geometry)) +
    geom_sf(data = Rwamagana_villages, color = "black", fill = NA, size = 0.5) +
    scale_fill_viridis_d() +  # Colorblind-friendly palette
    theme_void() + 
    theme(
      plot.background = element_rect(fill = 'white', color = 'white'),
      legend.margin = margin(r = 10),
      plot.title = element_text(hjust = 0.5), 
      plot.margin = margin(t = 20)
    ) +
    labs(
      title = paste("Rwamagana Village Electrification -", year),
      fill = "Usage Observed"
    ) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 16),
      plot.title = element_text(size = 18)
    )
  
  # Save each plot as a PNG file
  ggsave(filename = file.path(data_path, "Electrification map", "Rwamagana", paste0("Rwamagana_", year, ".png")), 
         plot = p, 
         width = 10, height = 8, units = "in", dpi = 300)
}


#GIF save
Rwamagana_map_path <- file.path(data_path, "Electrification map", "Rwamagana")

images <- list.files(Rwamagana_map_path, pattern = "*.png", full.names = TRUE)

img_list <- image_read(images)

gif <- image_animate(img_list, fps = 1)

gif_output_path <- file.path(data_path, "Electrification map", "Rwamagana.gif")
image_write(gif, gif_output_path)



##Join----
usage_check <-usage_id %>% 
  filter(district %in% c("Gicumbi", "Gisagara", "Kayonza", "Rwamagana")) %>% 
  group_by(village_id) %>%
  summarise(
    `2010` = sum(`2010_usage`),
    `2011` = sum(`2011_usage`),
    `2012` = sum(`2012_usage`),
    `2013` = sum(`2013_usage`),
    `2014` = sum(`2014_usage`),
    `2015` = sum(`2015_usage`),
    `2016` = sum(`2016_usage`),
    `2017` = sum(`2017_usage`),
    `2018` = sum(`2018_usage`),
    `2019` = sum(`2019_usage`),
    `2020` = sum(`2020_usage`),
    `2021` = sum(`2021_usage`),
    `2022` = sum(`2022_usage`), 
    year_first = min(year_first)
    ) %>% 
mutate(
    elec_before_11 = ifelse(year_first <= 2011, 1, 0),
    elec12_14 = ifelse( year_first <= 2014 & year_first >= 2012, 1, 0),
    elec15_17 = ifelse(year_first <= 2017 & year_first >= 2015 , 1, 0),
    elec18_20 = ifelse( year_first <= 2020 & year_first >= 2018, 1,0),
    elec21_22 = ifelse(year_first <= 2022 & year_first >= 2021, 1,0)
  ) %>% 
  mutate(
    status = case_when(
      year_first <= 2011 ~ "elec_before_11",
      year_first <= 2014 & year_first >= 2012 ~ "elec12_14",
      year_first <= 2017 & year_first >= 2015 ~ "elec15_17",
      year_first <= 2020 & year_first >= 2018 ~ "elec18_20",
      year_first <= 2022 & year_first >= 2021 ~ "elec21_22"
  ) )
  

write_dta(usage_check, path = file.path(data_path))


#Map on electrification-----------

rwa_village <- st_read(dsn = file.path(rct_data_path, "rwa_villages", "Village.shp"))
rwa_village <- st_make_valid(rwa_village)

rwa_regress_v <- read_xlsx(path = file.path(data_path, "rwa_regress_villagelevel.xlsx"))

rwa_boundary = st_read(dsn = file.path(rct_data_path,"rwa_boundary", "RWA_adm0.shp"))
rwa_boundary = st_transform(rwa_boundary, crs = st_crs(rwa_village))

rwa_map <- rwa_regress_v %>% 
  select(village_id, year , year_first) 

rwa_map <- left_join(rwa_village, rwa_map, by = c("Village_ID" = "village_id"))

rwa_map <- rwa_map %>%
  complete(year, Village_ID, fill = list(year_first = 2300)) 

year_2013 <- rwa_map %>% 
  filter( year == 2013) %>% 
  mutate(
    electrified = ifelse(year_first < 2013, "electrified_before_2013", ifelse(
      year_first == 2013, "electrified_2013", NA
    ))
  )


 ggplot() +
  geom_sf(data = year_2013, aes(fill =electrified, geometry = geometry), color = NA) +
  geom_sf(data = rwa_boundary, color = "black", fill = NA, size = 0.5) +
  scale_fill_viridis_d() +  # Colorblind-friendly palette
  theme_void() + 
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),
    legend.margin = margin(r = 10),
    plot.title = element_text(hjust = 0.5), 
    plot.margin = margin(t = 20)
  ) +
  labs(
    title = "Rwanda Village Electrification 2013",
    fill = "Electrification year"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 18)
  )

 
# Save each plot as a PNG file
ggsave(filename = file.path(data_path, "Electrification map", "Rwamagana", paste0("Rwamagana_", year, ".png")), 
       plot = p, 
       width = 10, height = 8, units = "in", dpi = 300)


years <- 2010:2021

# Loop over each year
for (year in years) {
  
  # Filter data for the current year and update the 'electrified' status
  year_data <- rwa_map %>%
    filter(year == !!year) %>%   # !!year to use the value of the loop variable
    mutate(
      electrified = ifelse(year_first < year, paste0("electrified_before_", year), 
                           ifelse(year_first == year, paste0("electrified_", year), NA))
    )
  
  # Create the plot for the current year
  p <- ggplot() +
    geom_sf(data = year_data, aes(fill = electrified, geometry = geometry), color = NA) +
    geom_sf(data = rwa_boundary, color = "black", fill = NA, size = 0.5) +
    scale_fill_viridis_d() +  # Colorblind-friendly palette
    theme_void() +
    theme(
      plot.background = element_rect(fill = 'white', color = 'white'),
      legend.margin = margin(r = 10),
      plot.title = element_text(hjust = 0.5),
      plot.margin = margin(t = 20)
    ) +
    labs(
      title = paste0("Rwanda Village Electrification ", year),
      fill = "Electrification year"
    ) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 16),
      plot.title = element_text(size = 18)
    )
  
  # Save the plot for the current year
  ggsave(filename = file.path(data_path, "Electrification map", "Year elec", paste0(year, ".png")), 
         plot = p, 
         width = 10, height = 8, units = "in", dpi = 300)
  
}




#GIF save
map_path <- file.path(data_path, "Electrification map", "Year elec")

images <- list.files(map_path, pattern = "*.png", full.names = TRUE)

img_list <- image_read(images)

gif <- image_animate(img_list, fps = 1)

gif_output_path <- file.path(data_path, "Electrification map", "Year_elec.gif")
image_write(gif, gif_output_path)



















