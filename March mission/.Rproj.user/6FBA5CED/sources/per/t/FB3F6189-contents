##############
#Author: Xiaoming Zhang
#Date: 2.26.2024
#Purpose: join Rutsiro new scope
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


#read files
rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village_area.shp"))

rwa_villages <- st_make_valid(rwa_villages)

###Surveyed----
karongi_LV_surveyed <- st_read(dsn = here(data_path, "Karongi Surveyed 0116", "Surveyed_LV_Lines.shp"))
rulindo_LV_surveyed <- st_read(dsn = here(data_path, "Rulindo Surveyed 0116", "Surveyed_LV_Lines.shp"))
rutsiro_LV_surveyed <- st_read(dsn = here(data_path, "Rutsiro Surveyed 0116", "Surveyed_LV_Lines.shp"))

rulindo_LV_surveyed <- st_transform(rulindo_LV_surveyed, crs =  st_crs(rwa_villages))
karongi_LV_surveyed <- st_transform(karongi_LV_surveyed, crs =  st_crs(rwa_villages))
rutsiro_LV_surveyed <- st_transform(rutsiro_LV_surveyed, crs =  st_crs(rwa_villages))

###Existing----
existing_LV <- st_read(dsn = here(data_path,"Existing Electrical Network_2022", "Existing_LVLine.shp"))
existing_MV <- st_read(dsn = here(data_path,"Existing Electrical Network_2022", "Existing_MVLine.shp"))
existing_HV <- st_read(dsn = here(data_path,"Existing Electrical Network_2022", "Existing_HVLine.shp"))

existing_LV <- st_transform(existing_LV, crs = st_crs(rwa_villages))
existing_MV <- st_transform(existing_MV, crs = st_crs(rwa_villages))
existing_HV <- st_transform(existing_HV, crs = st_crs(rwa_villages))

###meter----
karongi_meter <- st_read(dsn = here(data_path, "eucl meter", "karongi_meter.shp"))
rulindo_meter <- st_read(dsn = here(data_path, "eucl meter", "rulindo_meter.shp"))
rutsiro_meter <- st_read(dsn = here(data_path, "eucl meter", "rutsiro_meter.shp"))
rusizi_meter <- st_read(dsn = here(data_path, "eucl meter", "rusizi_meter.shp"))

karongi_meter <- st_transform(karongi_meter, crs = st_crs(rwa_villages))
rulindo_meter <- st_transform(rulindo_meter, crs = st_crs(rwa_villages))
rutsiro_meter <- st_transform(rutsiro_meter, crs = st_crs(rwa_villages))

#Color----

hv_color <- "orange"
mv_color <- "red"
lv_color <- "green"
meter_color <- "darkgrey"


scope_village <- "orange"
other_village <- "white"



#scope----

four_district_2402 <- read_xlsx(path = file.path(data_path, "four_district_2402.xlsx"))

scope_2402<- four_district_2402 %>% 
  select(village_id, scope_2402)

rwa_villages <- left_join(rwa_villages, scope_2402, by = c("Village_ID" = "village_id"))


#Karongi----

karongi_villages <- rwa_villages %>% 
  filter(District %in% c("Karongi"))

##Existing---
####Existing LV, MV, HV-----

karongi_lv_existing <- st_intersection(karongi_villages, existing_LV)
karongi_mv_existing <- st_intersection(karongi_villages, existing_MV)
karongi_hv_existing <- st_intersection(karongi_villages, existing_HV)

karongi_villages <- karongi_villages %>% 
  mutate(
    electrified = ifelse(Village_ID %in% karongi_lv_existing$Village_ID, 1, 0)
  )



####graph----

karongi_existing <- ggplot() +
  geom_sf(data = karongi_villages, fill = NA) + 
  geom_sf(data = subset(karongi_villages, electrified == 1), aes(fill = "Electrified"), size = 0) +
  scale_fill_manual(
    values = c("Electrified" = "lightblue"),
    name = ""
  )

karongi_existing



karongi_existing_plot <- karongi_existing +
  # geom_sf(data = karongi_meter, aes(color = "Meters"), size = 0.1) +
  # geom_sf(data = karongi_hv_existing, aes(color = "HV"), size = 0.5)+
  geom_sf(data = karongi_mv_existing, aes(color = "MV"), size = 0.5)+
  geom_sf(data = karongi_lv_existing, aes(color = "LV"), size = 1)+
  scale_color_manual(
    values = c(`HV` =`hv_color`, `MV`= `mv_color`, `LV` = `lv_color`, `Meters` = `meter_color`),
    guide = guide_legend(title.position = "top"),
    name = ""
  ) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),
    legend.margin = margin(r=10),
    plot.title = element_text(hjust = 0.5), 
    plot.margin = margin(t = 20)  
  ) +
  labs(title = "Karongi Meter and Grid Network 2022")


karongi_existing_plot








base_karongi <- ggplot() +
  geom_sf(data = karongi_villages, fill = NA) + 
  geom_sf(data = subset(karongi_villages, scope_2402 == 1), aes(fill = "Scope Villages"), size = 0) +
  scale_fill_manual(
    values = c("Scope Villages" = "lightblue"),
    name = ""
  ) + 
  

base_karongi



karongi_plot <- base_karongi +
  geom_sf(data = karongi_meter, aes(color = "Meters"), size = 0.1) +
  geom_sf(data = karongi_hv_existing, aes(color = "HV"), size = 0.5)+
  geom_sf(data = karongi_mv_existing, aes(color = "MV"), size = 0.5)+
  geom_sf(data = karongi_lv_existing, aes(color = "LV"), size = 1)+
  scale_color_manual(
    values = c(`HV` =`hv_color`, `MV`= `mv_color`, `LV` = `lv_color`, `Meters` = `meter_color`),
    guide = guide_legend(title.position = "top"),
    name = ""
  ) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),
    legend.margin = margin(r=10),
    plot.title = element_text(hjust = 0.5), 
    plot.margin = margin(t = 20)  
    ) +
  labs(title = "Karongi Meter and Grid Network 2022")


karongi_plot


karongi_surveyed <- base_karongi +
  geom_sf(data = karongi_LV_surveyed, aes(color = "LV"), size = 1)+
  scale_color_manual(
    values = c(`LV` = `lv_color`),
    guide = guide_legend(title.position = "top"),
    name = ""
  ) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),
    legend.margin = margin(r=10),
    plot.title = element_text(hjust = 0.5), 
    plot.margin = margin(t = 20)  
  ) +
  labs(title = "Karongi scope and surveyed LV lines")

karongi_surveyed





#rutsiro----

rutsiro_villages <- rwa_villages %>% 
  filter(District %in% c("Rutsiro"))

##Existing---
####Existing LV, MV, HV-----

rutsiro_lv_existing <- st_intersection(rutsiro_villages, existing_LV)
rutsiro_mv_existing <- st_intersection(rutsiro_villages, existing_MV)
rutsiro_hv_existing <- st_intersection(rutsiro_villages, existing_HV)



####graph----


base_rutsiro <- ggplot() +
  geom_sf(data = rutsiro_villages, fill = NA) + 
  geom_sf(data = subset(rutsiro_villages, scope_2402 == 1), aes(fill = "Scope Villages"), size = 0) +
  scale_fill_manual(
    values = c("Scope Villages" = "lightblue"),
    name = ""
  )

base_rutsiro



rutsiro_plot <- base_rutsiro +
  geom_sf(data = rutsiro_meter, aes(color = "Meters"), size = 0.1) +
  geom_sf(data = rutsiro_hv_existing, aes(color = "HV"), size = 0.5)+
  geom_sf(data = rutsiro_mv_existing, aes(color = "MV"), size = 0.5)+
  geom_sf(data = rutsiro_lv_existing, aes(color = "LV"), size = 1)+
  scale_color_manual(
    values = c(`HV` =`hv_color`, `MV`= `mv_color`, `LV` = `lv_color`, `Meters` = `meter_color`),
    guide = guide_legend(title.position = "top"),
    name = ""
  ) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),
    legend.margin = margin(r=10),
    plot.title = element_text(hjust = 0.5), 
    plot.margin = margin(t = 20)  
  ) +
  labs(title = "Rutsiro Meter and Grid Network 2022")


rutsiro_plot


rutsiro_surveyed <- base_rutsiro +
  geom_sf(data = rutsiro_LV_surveyed, aes(color = "LV"), size = 1)+
  scale_color_manual(
    values = c(`LV` = `lv_color`),
    guide = guide_legend(title.position = "top"),
    name = ""
  ) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),
    legend.margin = margin(r=10),
    plot.title = element_text(hjust = 0.5), 
    plot.margin = margin(t = 20)  
  ) +
  labs(title = "Rutsiro scope and surveyed LV lines")

rutsiro_surveyed






#rulindo----

rulindo_villages <- rwa_villages %>% 
  filter(District %in% c("Rulindo"))

##Existing---
####Existing LV, MV, HV-----

rulindo_lv_existing <- st_intersection(rulindo_villages, existing_LV)
rulindo_mv_existing <- st_intersection(rulindo_villages, existing_MV)
rulindo_hv_existing <- st_intersection(rulindo_villages, existing_HV)



####graph----


base_rulindo <- ggplot() +
  geom_sf(data = rulindo_villages, fill = NA) + 
  geom_sf(data = subset(rulindo_villages, scope_2402 == 1), aes(fill = "Scope Villages"), size = 0) +
  scale_fill_manual(
    values = c("Scope Villages" = "lightblue"),
    name = ""
  )

base_rulindo



rulindo_plot <- base_rulindo +
  geom_sf(data = rulindo_meter, aes(color = "Meters"), size = 0.1) +
  geom_sf(data = rulindo_hv_existing, aes(color = "HV"), size = 0.5)+
  geom_sf(data = rulindo_mv_existing, aes(color = "MV"), size = 0.5)+
  geom_sf(data = rulindo_lv_existing, aes(color = "LV"), size = 1)+
  scale_color_manual(
    values = c(`HV` =`hv_color`, `MV`= `mv_color`, `LV` = `lv_color`, `Meters` = `meter_color`),
    guide = guide_legend(title.position = "top"),
    name = ""
  ) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),
    legend.margin = margin(r=10),
    plot.title = element_text(hjust = 0.5), 
    plot.margin = margin(t = 20)  
  ) +
  labs(title = "Rulindo Meter and Grid Network 2022")


rulindo_plot


rulindo_surveyed <- base_rulindo +
  geom_sf(data = rulindo_LV_surveyed, aes(color = "LV"), size = 1)+
  scale_color_manual(
    values = c(`LV` = `lv_color`),
    guide = guide_legend(title.position = "top"),
    name = ""
  ) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),
    legend.margin = margin(r=10),
    plot.title = element_text(hjust = 0.5), 
    plot.margin = margin(t = 20)  
  ) +
  labs(title = "Rulindo scope and surveyed LV lines")

rulindo_surveyed




#Draw with partial/newly----


#"newly" is in the randomization scope, "partial" is not----
#Reconstruct status to make sure----

four_district_2402.1 <- four_district_2402 %>% 
  mutate(any_offgrid = case_when(
    et_sum != 0 | priority_0 == 0 ~ "partial",
    .default = "newly"
  ))

four_district_2402.1 <- four_district_2402.1 %>% 
  mutate(
    any_grid = case_when(
      grid_status %in% c("newly") & nep_revision%in% c("GE", "Fill In")  ~ "newly",
      .default = "partial"
    )
  )

four_district_2402.1 <- four_district_2402.1  %>% 
  mutate(status = case_when(
    any_grid == "newly" & any_offgrid == "newly" ~ "newly",
    .default = "partial"
  ))

table(four_district_2402.1$scope_2401)

four_district_2402.1  <- four_district_2402.1 %>% 
  mutate(
    status = ifelse(ubudehe_1 <20, "partial", status)
  )


four_district_2402.1 <- four_district_2402.1 %>% 
  mutate(meter_percent = round(meter_eucl/total_hh, 2))

four_district_2402.1 <- four_district_2402.1 %>% 
  mutate(
    status = ifelse(meter_percent >= 0.3 , "partial", status)
  )

four_district_2402.1 <- four_district_2402.1 %>% 
  mutate(
    status = ifelse(customer <20 & district != "Rusizi", "partial", status)
  )

#Get the scope----


four_scope_2402.1 <- four_district_2402.1 %>% 
  filter( scope_2402 == 1)

table(four_scope_2402.1$status)


status_2402 <- four_scope_2402.1 %>% 
  select(village_id, status)


##Karongi----


karongi_villages <- left_join(karongi_villages, status_2402, by = c("Village_ID" = "village_id"))



status_karongi <- ggplot() +
  geom_sf(data = karongi_villages, fill = NA) + 
  geom_sf(data = subset(karongi_villages, scope_2402 == 1), aes(fill = "Scope Villages"), size = 0) +
  geom_sf(data = subset(karongi_villages, status == "newly"), aes(fill = "Scope villages selected for randomization"), size = 0) +
  
  scale_fill_manual(
    values = c("Scope Villages" = "lightblue", "Scope villages selected for randomization" = "pink"),
    name = ""
  )

status_karongi







karongi_surveyed <- status_karongi +
  geom_sf(data = karongi_LV_surveyed, aes(color = "LV"), size = 1)+
  scale_color_manual(
    values = c(`LV` = `lv_color`),
    guide = guide_legend(title.position = "top"),
    name = ""
  ) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),
    legend.margin = margin(r=10),
    plot.title = element_text(hjust = 0.5), 
    plot.margin = margin(t = 20)  
  ) +
  labs(title = "Karongi scope and surveyed LV lines")

karongi_surveyed


all_karongi <- status_karongi + 
  geom_sf(data = karongi_meter, aes(color = "Meters"), size = 0.1) +
  geom_sf(data = karongi_hv_existing, aes(color = "HV"), size = 0.5)+
  geom_sf(data = karongi_mv_existing, aes(color = "MV"), size = 0.5)+
  geom_sf(data = karongi_lv_existing, aes(color = "LV"), size = 1)+
  scale_color_manual(
    values = c(`HV` =`hv_color`, `MV`= `mv_color`, `LV` = `lv_color`, `Meters` = `meter_color`),
    guide = guide_legend(title.position = "top"),
    name = ""
  ) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),
    legend.margin = margin(r=10),
    plot.title = element_text(hjust = 0.5), 
    plot.margin = margin(t = 20)  
  ) +
  labs(title = "Karongi Meter and Grid Network 2022")

all_karongi



##Rulindo----


rulindo_villages <- left_join(rulindo_villages, status_2402, by = c("Village_ID" = "village_id"))



status_rulindo <- ggplot() +
  geom_sf(data = rulindo_villages, fill = NA) + 
  geom_sf(data = subset(rulindo_villages, scope_2402 == 1), aes(fill = "Scope Villages"), size = 0) +
  geom_sf(data = subset(rulindo_villages, status == "newly"), aes(fill = "Randomization"), size = 0) +
  
  scale_fill_manual(
    values = c("Scope Villages" = "lightblue", "Randomization" = "pink"),
    name = ""
  )

status_rulindo



rulindo_surveyed <- status_rulindo +
  geom_sf(data = rulindo_LV_surveyed, aes(color = "LV"), size = 1)+
  scale_color_manual(
    values = c(`LV` = `lv_color`),
    guide = guide_legend(title.position = "top"),
    name = ""
  ) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),
    legend.margin = margin(r=10),
    plot.title = element_text(hjust = 0.5), 
    plot.margin = margin(t = 20)  
  ) +
  labs(title = "Rulindo scope and surveyed LV lines")

rulindo_surveyed


##rutsiro----


rutsiro_villages <- left_join(rutsiro_villages, status_2402, by = c("Village_ID" = "village_id"))



status_rutsiro <- ggplot() +
  geom_sf(data = rutsiro_villages, fill = NA) + 
  geom_sf(data = subset(rutsiro_villages, scope_2402 == 1), aes(fill = "Scope Villages"), size = 0) +
  geom_sf(data = subset(rutsiro_villages, status == "newly"), aes(fill = "Randomization"), size = 0) +
  
  scale_fill_manual(
    values = c("Scope Villages" = "lightblue", "Randomization" = "pink"),
    name = ""
  )

status_rutsiro



rutsiro_surveyed <- status_rutsiro +
  geom_sf(data = rutsiro_LV_surveyed, aes(color = "LV"), size = 1)+
  scale_color_manual(
    values = c(`LV` = `lv_color`),
    guide = guide_legend(title.position = "top"),
    name = ""
  ) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),
    legend.margin = margin(r=10),
    plot.title = element_text(hjust = 0.5), 
    plot.margin = margin(t = 20)  
  ) +
  labs(title = "Rutsiro scope and surveyed LV lines")

rutsiro_surveyed


#Graph grid lines----


rwa_district <- st_read(dsn = file.path(data_path, "rwa_district", "District.shp"))
district_base <- ggplot() +
  geom_sf(data = rwa_district)

district_base

rwa_villages <- rwa_villages %>% st_make_valid()

village_base <- ggplot()+
  geom_sf(data = rwa_villages)

village_base
hv_color <- "#F8766D"
# blue 
mv_color <- "#619CFF"
lv_color <- "#00BA38"

rwa_plot <- ggplot()+ 
  geom_sf(data = rwa_villages, linewidth = 0.2, col = "grey", fill = NA) +
  geom_sf(data = rwa_district, linewidth = 0.4, col = "black", fill = NA) +
  # scale_fill_manual(values = c(
  #   "scope" = scope_districts,
  #    "not scope" = non_scope
  # )) +   
  geom_sf(data = existing_HV, linewidth = 0.5,  aes(color = "HV")) +
  geom_sf(data = existing_MV, linewidth = 0.5, aes(color = "MV")) +
  geom_sf(data = existing_LV, linewidth = 0.1, aes(color = "LV")) +
  
  # geom_sf(data = existing_substation, size = 0.5, aes(color = "Substation"))+
  # geom_sf(data = existing_transformer, size = 0.3, aes(color = "Transformers"))+
  scale_color_manual(
    values = c(`HV` = hv_color, `MV` = mv_color, `LV` = lv_color),
    guide = guide_legend(title.position = "top")
  ) +
  # theme_void() + 
  # theme(plot.background = element_rect(colour = "white"))+
  labs(title = "Rwanda 2022 Grid Networks")


rwa_plot
