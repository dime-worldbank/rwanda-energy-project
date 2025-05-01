##########################
#Author: Xiaoming Zhang
#Date of last modification: 01142025
#purpose: EC graph
############################


#library----
# install.packages("plm")

pacman::p_load(fixest, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, plm, haven, stringr, modelsummary, kableExtra, stargazer, lfe)

if (Sys.getenv("USERNAME") == "wb614406"){
    DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
  }

data_path <- file.path(
  DROPBOX, "Rwanda Energy/EAQIP/datawork/Historical data"
)

output_path <- file.path(
  DROPBOX, "Rwanda Energy/ASCENT/datawork"
)

#EC 2020----

isic_2020 <- read_xlsx(
  file.path(data_path, "Establishment census", "outputs0605", "2020", "group_long_2020(isic).xlsx"), 
  sheet = "isic_level1"
)

isic_9 <- isic_2020 %>% 
  filter(isic_level1 == 9) %>% 
  mutate(village_id = as.character(village_id)) 


#Join with graph----
rwa_district <- st_read(dsn = file.path(data_path, "rwa_district", "District.shp"))

rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))
rwa_villages <- st_make_valid(rwa_villages)

isic_9 <- left_join( rwa_villages, isic_9, by = c("Village_ID" = "village_id"))

isic_9 <- isic_9 %>% 
  mutate(across(everything(), ~replace_na(.x, 0)))

isic_9 <- as.data.frame(isic_9)



#Cell level----

isic_9_cell <- isic_9 %>% 
  group_by(Cell_ID) %>% 
  summarise(
    n = sum(n)
  ) %>% 
  ungroup() %>% 
  mutate(
    Cell_ID = as.numeric(Cell_ID)
  )

rwa_cell <- st_read(dsn = file.path(data_path, "rwa_cell", "Cell.shp"))
rwa_cell <- st_make_valid(rwa_cell)
rwa_district <- st_transform(rwa_district, crs = st_crs(rwa_cell))
isic_9_cell <- left_join(rwa_cell, isic_9_cell, by = c("Cell_ID" = "Cell_ID"))

#Graph----




#Log graph----

ggplot(data = isic_9_cell) +
  geom_sf(aes(fill = n), color = NA, size = 0) + 
  geom_sf(data = rwa_district, color = "white", size = 2, fill = NA) +
  scale_fill_viridis_c(option = "E", trans = "log10", name = "Number of Establishments") +
  labs(
    title = "Number of Establishments in Rwanda Cells",
    subtitle = "Accommodation and Food Service Activities",
    caption = "Data source: Establishment Census 2020"
  ) +
    theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  
    plot.subtitle = element_text(hjust = 0.5, size = 12),             
    plot.caption = element_text(hjust = 0.5, size = 10),              
    plot.margin = margin(30, 30, 30, 30),                             
    panel.border = element_rect(color = "white", fill = NA, size = 2) 
  )


#Save descent n----
isic_cell_descent <- isic_9_cell%>% 
  as.data.frame() %>% 
  select(
    Prov_ID, Province, Dist_ID, District, Sect_ID, Sector, Cell_ID, Name, n
  ) %>% 
  arrange(desc(n)) %>% 
  rename(num_restaurant = n)

write_xlsx(isic_cell_descent, path = file.path(output_path, "ISIC9_Cell(desc).xlsx"))












#sector level----

isic_9_sector <- isic_9 %>% 
  group_by(Sector_ID) %>% 
  summarise(
    n = sum(n)
  ) %>% 
  ungroup() %>% 
  mutate(
    Sector_ID = as.numeric(Sector_ID)
  )

rwa_sector <- st_read(dsn = file.path(data_path, "rwa_sector", "Sector.shp"))
rwa_sector <- st_make_valid(rwa_sector)

isic_9_sector <- left_join(rwa_sector, isic_9_sector, by = c("Sect_ID" = "Sector_ID"))

#Graph----


#Log graph----

ggplot(data = isic_9_sector) +
  geom_sf(aes(fill = n), color = NA, size = 0) + 
  geom_sf(data = rwa_district, color = "white", size = 2, fill = NA) +
  scale_fill_viridis_c(option = "E", trans = "log10", name = "Number of Establishments") +
  labs(
    title = "Number of Establishments in Rwanda Sector",
    subtitle = "Accommodation and Food Service Activities",
    caption = "Data source: Establishment Census 2020"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  
    plot.subtitle = element_text(hjust = 0.5, size = 12),             
    plot.caption = element_text(hjust = 0.5, size = 10),              
    plot.margin = margin(30, 30, 30, 30),                             
    panel.border = element_rect(color = "white", fill = NA, size = 2) 
  )


#Save descent n----
isic_sector_descent <- isic_9_sector%>% 
  as.data.frame() %>% 
  select(
    Prov_ID, Province, Dist_ID, District, Sect_ID, Name, n
  ) %>% 
  arrange(desc(n)) %>% 
  rename(num_restaurant = n)

write_xlsx(isic_sector_descent, path = file.path(output_path, "ISIC9_Sector(desc).xlsx"))









#Graph.2----

# Calculate centroids and the outer boundary of Rwanda
isic_9_sector <- isic_9_sector %>% 
  mutate(centroid = st_centroid(geometry),
         centroid_coords = st_coordinates(centroid),
         centroid_x = centroid_coords[,1],
         centroid_y = centroid_coords[,2])

# Calculate the centroid of the entire country for reference
rwanda_centroid <- st_centroid(st_union(isic_9_sector$geometry))
rwanda_centroid_coords <- st_coordinates(rwanda_centroid)
rwanda_centroid_x <- rwanda_centroid_coords[1]
rwanda_centroid_y <- rwanda_centroid_coords[2]

# Set the offset for line extension
offset_distance <- 30000  

# Assign each sector an angle based on its geographic position relative to the centroid of Rwanda
isic_9_sector <- isic_9_sector %>%
  mutate(angle = atan2(centroid_y - rwanda_centroid_y, centroid_x - rwanda_centroid_x),
         label_x = centroid_x + offset_distance * cos(angle),
         label_y = centroid_y + offset_distance * sin(angle))

# Plotting with ggplot2 (geographically determined lines spreading evenly)
ggplot(data = isic_9_sector) +
  # Map filled by the number of establishments
  geom_sf(aes(fill = n), color = NA, size = 0) + 
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Number of Establishments (Capped)") +
  
  # Adding lines radiating based on geographic location with evenly spread angles
  geom_segment(aes(x = centroid_x, 
                   y = centroid_y, 
                   xend = label_x, 
                   yend = label_y),
               color = "black", alpha = 0.8, size = 0.5) +
  
  # Adding labels at the end of the lines
  geom_text(aes(x = label_x, 
                y = label_y, 
                label = n),
            size = 3, color = "black", fontface = "bold") +
  
  # Centered title and spacing adjustments
  labs(
    title = "Number of Establishments in Rwanda Sectors",
    subtitle = "Accommodation and Food Service Activities (Capped at 10)",
    caption = "Data source: Establishment Census 2020"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10),
    plot.margin = margin(30, 30, 30, 30)
  )














#Graph.3-----


# Calculate centroids and the outer boundary of Rwanda
isic_9_sector <- isic_9_sector %>% 
  mutate(centroid = st_centroid(geometry),
         centroid_coords = st_coordinates(centroid),
         centroid_x = centroid_coords[,1],
         centroid_y = centroid_coords[,2])

# Calculate the centroid of the entire country for reference
rwanda_centroid <- st_centroid(st_union(isic_9_sector$geometry))
rwanda_centroid_coords <- st_coordinates(rwanda_centroid)
rwanda_centroid_x <- rwanda_centroid_coords[1]
rwanda_centroid_y <- rwanda_centroid_coords[2]

# Set the minimum offset distance (30,000 meters or 30 km)
min_offset_distance <- 30000  

# Calculate the distance of each sector from the country's centroid and adjust line length
isic_9_sector <- isic_9_sector %>%
  mutate(
    # Calculate the distance from Rwanda's centroid to each sector's centroid
    distance_to_centroid = as.numeric(st_distance(st_sfc(centroid, crs = st_crs(isic_9_sector)), 
                                                  st_sfc(rwanda_centroid, crs = st_crs(isic_9_sector)))),
    
    # Ensure the line length is at least 30 km, adjusting dynamically
    scaling_factor = pmax(min_offset_distance / distance_to_centroid, 1),
    
    # Calculate the label positions based on the adjusted scaling factor
    label_x = centroid_x + (centroid_x - rwanda_centroid_x) * scaling_factor,
    label_y = centroid_y + (centroid_y - rwanda_centroid_y) * scaling_factor
  )

# Plotting with ggplot2 (lines extending outward with at least 30 km length)
ggplot(data = isic_9_sector) +
  # Map filled by the number of establishments
  geom_sf(aes(fill = n), color = NA, size = 0) + 
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Number of Establishments (Capped)") +
  
  # Adding lines extending outward from each sector centroid
  geom_segment(aes(x = centroid_x, 
                   y = centroid_y, 
                   xend = label_x, 
                   yend = label_y),
               color = "black", alpha = 0.8, size = 0.5) +
  
  # Adding labels at the end of the lines
  geom_text(aes(x = label_x, 
                y = label_y, 
                label = n),
            size = 3, color = "black", fontface = "bold") +
  
  # Centered title and spacing adjustments
  labs(
    title = "Number of Establishments in Rwanda Sectors",
    subtitle = "Accommodation and Food Service Activities",
    caption = "Data source: Establishment Census 2020"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10),
    plot.margin = margin(30, 30, 30, 30)
  )
