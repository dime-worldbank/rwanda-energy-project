##############
#Author: Xiaoming Zhang
#Date: 3.5.2024
#Purpose: Modify November Mission graph
#############



pacman::p_load(tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr)

getwd()

#Dropbox path----


if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/data/data"
)

#Read popdens----

village_old <- st_read(dsn = file.path(data_path, "villages_old", "villages_boundaries.shp"))

village_old <- village_old %>% 
  mutate(Village_ID = as.character(Code_vill_)) %>% 
  select(-Code_vill_)

village_old <- village_old %>%  st_make_valid()

village_old$area <- st_area(village_old)

village_old <- village_old %>% 
  mutate(area = as.numeric(area),
         popdens = Population/area)

sum(village_old$Population, na.rm = TRUE)

village_old <- village_old %>% 
  mutate(dens_winz = ifelse(popdens > 0.0009718, 0.0009718, popdens) )


gg <- ggplot() +
  geom_sf(data = village_old, mapping = aes(fill = dens_winz), color = NA) +
  scale_fill_gradient(low = "white", high = "black") +
  theme_void() + 
  theme(plot.background = element_rect(fill = 'white', color = 'white') )
theme(legend.position = "none")


village_old <- village_old %>% 
  select(Village_ID, Population, popdens, dens_winz) %>% 
  distinct(Village_ID, .keep_all = TRUE) 

village_old <- village_old %>% 
  st_drop_geometry() 





#Join-----

villages <- left_join(rwa_villages, village_old, by = c("Village_ID"))

sum(is.na(villages$PopDens))

villages <- villages %>% 
  mutate(Population = ifelse(is.na(Population), 0, Population),
         popdens = ifelse(is.na(popdens), 0, popdens),
         dens_winz = ifelse(is.na(dens_winz), 0, dens_winz)
  )


villages <- villages %>% 
  mutate(`population density` = dens_winz)
# 
# villages <- villages %>% 
#   rename(`winzorized population density` = `population density`)
# Create the plot----



base_popdens <- ggplot() +
  geom_sf(data = villages, mapping = aes(fill = `population density`), color = NA) +
  scale_fill_gradient(low = "white", high = "black") +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),
    legend.margin = margin(r=10),
    plot.title = element_text(hjust = 0.5), 
    plot.margin = margin(t = 20)  
  ) +
  theme(legend.position = "right")

base_popdens

#Read national boundary----

historical_data_path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/Historical data"
)

existing_HV_2011 <- st_read(dsn = file.path(historical_data_path, "Existing Electrical Network_2011", "Existing_HVLine.shp"))
existing_MV_2011 <- st_read(dsn = file.path(historical_data_path, "Existing Electrical Network_2011", "Existing_MVLine.shp"))

#2011 graph
grid_popdens_2011 <- base_popdens +
  # geom_sf(data = rwa_boundary, fill = NA, color = "black", linewidth = 0.25) +  
  geom_sf(data = existing_HV_2011, linewidth = 0.25,  color = "red") +
  geom_sf(data = existing_MV_2011, linewidth = 0.25, color = "red") + 
  # scale_color_manual( 
  #   values = c(`HV` =`hv_color1`, `MV`= `mv_color1`),
  #   guide = guide_legend(title.position = "top")
  # ) +
  theme_void() + 
  theme(plot.background = element_rect(fill = 'white', color = 'white') ) 
# labs(title = "Rwanda 2011 Grid Networks & Population Density")

grid_popdens_2011


#2022 graph

grid_popdens_2022 <- base_popdens +
  # geom_sf(data = rwa_boundary, fill = NA, color = "black", linewidth = 0.25) +  
  geom_sf(data = existing_HV, linewidth = 0.25,  color = "red") +
  geom_sf(data = existing_MV, linewidth = 0.25, color = "red") + 
  # scale_color_manual( 
  #   values = c(`HV` =`hv_color1`, `MV`= `mv_color1`),
  #   guide = guide_legend(title.position = "top")
  # ) +
  theme_void() + 
  theme(plot.background = element_rect(fill = 'white', color = 'white') ) 

grid_popdens_2022


#Read NEP ---


nep <- read_xlsx(here("FINAL_LIST_OF_REVISED_NEP_VILLAGES_JULY_2023-PUBLISHED (3).xlsx"), sheet = "Sheet1")

nep <- nep %>% 
  mutate(Village_ID = as.character(Code_vil_1)) %>% 
  rename(nep = `NEP Revision July 2023`) %>% 
  select(Village_ID, nep) 

villages <- left_join(villages, nep, by = c("Village_ID"))

villages <- villages %>% 
  mutate(status = case_when(
    nep %in%c("Fill In", "ge", "GE", "GE_Temporarly SAS") ~ "Grid",
    nep %in% c("SAS" , "SAS 2023", "Microgrid") ~ "Off-grid",
    TRUE ~ NA
  ))


