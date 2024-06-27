##############
#Author: Xiaoming Zhang
#Date: 06.17.2024
#Purpose: 
#############


pacman::p_load(tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, stargazer, olsrr)

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

output_path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/outputs"
)


#Read files-----

four_district_2405 <- read_xlsx( path = file.path(data_path, "four_district_2405.xlsx"))

rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))

village_join <- rwa_villages %>% 
  select(Village_ID)

#check scope---

four_district_join <- left_join(four_district_2405, village_join, by = c("village_id" = "Village_ID"))


four_district <- four_district_join %>% 
  mutate(any_offgrid = case_when(
    et_sum != 0 | priority_0 == 0 ~ "partial",
    .default = "newly"
  ))

four_district <- four_district %>% 
  mutate(
    any_grid = case_when(
      grid_status %in% c("newly") & nep_revision%in% c("GE", "Fill In")  ~ "newly",
      .default = "partial"
    )
  )

four_district <- four_district %>% 
  mutate(status = case_when(
    any_grid == "newly" & any_offgrid == "newly" ~ "newly",
    .default = "partial"
  ))


four_district <- four_district %>% 
  mutate(
    status = ifelse(ubudehe_1 < 20, "partial", status)
  )


four_district <- four_district %>% 
  mutate(meter_percent = round(meter_eucl/total_hh, 2))

four_district  <- four_district %>% 
  mutate(
    status = ifelse(meter_percent >= 0.3 , "partial", status)
  )

four_district <- four_district %>% 
  mutate(
    status = ifelse(customer < 20 , "partial", status)
  )


four_scope <- four_district %>% 
  filter( scope_2403 == 1)

table(four_scope$status)

four_scope %>% 
  filter(status == "newly") %>% 
  group_by(district) %>% 
  summarise(
    village_n = n(),
    ubudehe_1 = village_n * 20
  )

#Comparison and discrepencies-----

##Off grid makesure----

four_scope_offgrid <- four_scope %>% 
 group_by(SAS) %>% 
  summarise(
   `no eli & no priority` = sum(priority_0 == 1 & offgrid_et == 0),
   `no eli & priority` = sum(priority_0 == 0 & offgrid_et == 0),
   `eli & no priority` = sum(priority_0 == 1 & offgrid_et == 1),
   `eli & priority` = sum(priority_0 == 0 & offgrid_et == 1)
   
  )



#Filtering out offgrid----

sum(is.na(four_scope$length_lv))

four_scope_grid <- four_scope %>% 
  filter(any_offgrid == "newly") %>% 
  mutate(
    existing_lv = ifelse(length_lv >0, 1, 0)
  )

###meter and LV----

sum(is.na(four_scope_offgrid$meter_eucl))

meter_lv_count <-four_scope_grid %>% 
  group_by(existing_lv) %>% 
  summarise(
    no_meter = sum(meter_eucl == 0),
    yes_meter = sum(meter_eucl != 0)
  )

###Meter and nep----

nep_meter <- four_scope_grid %>% 
  group_by(nep_revision) %>% 
  summarise(
    no_meter = sum(meter_eucl == 0),
    yes_meter = sum(meter_eucl != 0)
  )

###Meter and lv----

lv_meter <- four_scope_grid %>%
  mutate(lv_exist = ifelse(length_lv == 0, 0, 1)) %>% 
  group_by(nep_revision) %>% 
  summarise(
    no_lv = sum(lv_exist == 0),
    yes_lv = sum(lv_exist != 0)
  )

View(lv_meter)


#Graph-----

rulindo_existinglv <- st_read(dsn = file.path(data_path, "rulindo existing", "rulindo_LV.shp"))
rulindo_meter <- st_read(dsn = file.path(data_path, "eucl meter", "rulindo_meter.shp"))
rulindo_villages <- four_district %>% 
  filter( district == "Rulindo")

rulindo_villages <- st_as_sf(rulindo_villages)

rulindo_existinglv <- st_transform(rulindo_existinglv, crs = st_crs(rulindo_villages))
rulindo_meter <- st_transform(rulindo_meter, crs = st_crs(rulindo_villages))

base_rulindo <- ggplot() +
  geom_sf(data = rulindo_villages, fill = NA) + 
  geom_sf(data = subset(rulindo_villages, scope_2403 == 1), aes(fill = "Scope Villages"), size = 0) +
  scale_fill_manual(
    values = c("Scope Villages" = "lightblue"),
    name = ""
  )  

base_rulindo



rulindo_plot <- base_rulindo +
  geom_sf(data = rulindo_meter, aes(color = "Meters"), size = 0.1) +
  # geom_sf(data = rulindo_hv_existing, aes(color = "HV"), size = 0.5)+
  # geom_sf(data = rulindo_mv_existing, aes(color = "MV"), size = 0.5)+
  geom_sf(data = rulindo_existinglv, aes(color = "LV"), size = 1)+
  scale_color_manual(
    values = c( `LV` = "blue", `Meters` = "orange"),
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

#Filter for GE and meter




nep_plot <- ggplot() +
  geom_sf(data = rulindo_villages, fill = NA) + 
  geom_sf(data = subset(rulindo_villages, nep_revision %in% c("Fill In", "GE")),aes(fill = "NEP is grid"), size = 0) +
  scale_fill_manual(
    values = c("NEP is grid" = "lightpink"),
    name = ""
  )  +
  geom_sf(data = rulindo_meter, aes(color = "Meters"), size = 0.5) +
  # geom_sf(data = rulindo_hv_existing, aes(color = "HV"), size = 0.5)+
  # geom_sf(data = rulindo_mv_existing, aes(color = "MV"), size = 0.5)+
  scale_color_manual(
    values = c(  `Meters` = "orange"),
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
  labs(title = "Rulindo Meter and NEP status")


nep_plot


#Rulindo Overall plot----


status_rulindo <- ggplot() +
  geom_sf(data = rulindo_villages, fill = NA) + 
  geom_sf(data = subset(rulindo_villages, scope_2403 == 1), aes(fill = "Scope Villages"), size = 0) +
  geom_sf(data = subset(rulindo_villages, status == "newly"), aes(fill = "Scope villages selected for randomization"), size = 0) +
  
  scale_fill_manual(
    values = c("Scope Villages" = "lightblue", "Scope villages selected for randomization" = "pink"),
    name = ""
  )

status_rulindo


rulindo_existing_plot <- status_rulindo +
  geom_sf(data = rulindo_meter, aes(color = "Meters"), size = 0.1) +
  # geom_sf(data = rulindo_hv_existing, aes(color = "HV"), size = 0.5)+
  geom_sf(data = rulindo_existinglv, aes(color = "LV"), size = 1)+
  scale_color_manual(
    values = c( `LV` = "blue", `Meters` = "orange"),
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
  labs(title = "rulindo Meter and Grid Network 2022")


rulindo_existing_plot

