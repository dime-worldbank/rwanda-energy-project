
#############################################################
#Purpose: Descriptives
#Author:Xiaoming Zhang
#Date: November 3rd 2025
###################################################################




pacman::p_load(knitr, lfe,fixest, modelsummary, stargazer, tidyverse, dplyr, here, sf, haven, ggplot2, readxl,  writexl, janitor, randomizr, RCT, purrr, RODBC, DBI)

getwd()

dropbox <- 'C:/Users/wb614406/Dropbox'

data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Historical Expansion/data"
)

output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Historical Expansion/outputs"
)





#Descriptives-------




#Event study with ISIC restriction----


##ISIC selection------

#utility_long join



ec_2011 <- read_xlsx(path = file.path(data_path, "2011", "group_long_2011(isic).xlsx"))

ec_2011 <- ec_2011 %>% 
  mutate(village_id = as.character(village_id)) %>% 
  rename(num_establishment = n) %>% 
  group_by(village_id, isic_level1) %>% 
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE)) %>% 
  select(village_id, num_establishment, total_employee, isic_level1) %>% 
  mutate(year = 2011) %>% 
  ungroup()

ec_2014 <- read_xlsx(path = file.path(data_path, "2014", "group_long_2014(isic).xlsx"))

ec_2014 <- ec_2014 %>% 
  mutate(village_id = as.character(village_id)) %>% 
  rename(num_establishment = n,
         isic_level1 = isic_level1_main) %>% 
  group_by(village_id, isic_level1) %>% 
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE)) %>% 
  select(village_id, num_establishment, total_employee, isic_level1) %>% 
  mutate(year = 2014) %>% 
  ungroup()


ec_2017 <- read_xlsx(path = file.path(data_path, "2017", "group_long_2017(isic).xlsx"))

ec_2017 <- ec_2017 %>% 
  mutate(village_id = as.character(village_id)) %>% 
  rename(num_establishment = n,
         isic_level1 = isic_1_digit) %>% 
  group_by(village_id, isic_level1) %>% 
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE)) %>% 
  select(village_id, num_establishment, total_employee, isic_level1) %>% 
  mutate(year = 2017) %>% 
  ungroup()

ec_2020 <- read_xlsx(path = file.path(data_path, "2020", "group_long_2020(isic).xlsx"))

ec_2020 <- ec_2020 %>% 
  mutate(village_id = as.character(village_id)) %>% 
  rename(num_establishment = n) %>% 
  group_by(village_id, isic_level1) %>% 
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE)) %>% 
  select(village_id, num_establishment, total_employee, isic_level1) %>% 
  mutate(year = 2020) %>% 
  ungroup()


ec_all <- rbind(ec_2011, ec_2014, ec_2017, ec_2020)



earp_join <- expansion_join%>% 
  select(village_id, earp_mv, earp_lv, cell_id, sector_id, district_id) 






##clean------

earp_did_isic<- left_join( earp_join, ec_all, by = c("village_id"))


earp_did_isic <- earp_did_isic %>%
  complete(year, village_id, isic_level1,
           fill = list(num_establishment = 0, total_employee = 0)) %>% 
  filter(!is.na(year))  


earp <- expansion_join%>% 
  select(village_id, earp_mv, earp_lv, earp_existing_mv, health_center, primary_school, cell_office, 
         secondary_school, sector_district_office,
         industry, market, residential_consumer, non_residential_consumer, imidugudu,  electrified_year) %>% 
  rename(
    mv = earp_mv,
    lv = earp_lv
  )



earp_did_isic <- left_join(earp_did_isic, earp, by = c("village_id"))




earp_did_isic <- earp_did_isic %>% 
  mutate(
    earp_mv = ifelse(is.na(earp_mv), mv, earp_mv),
    earp_lv = ifelse(is.na(earp_lv), lv, earp_lv)
  )%>%
  select(-mv, -lv) %>%
  mutate(
    cell_id    = substr(village_id, 1, 6),
    sector_id  = substr(village_id, 1, 4),
    district_id= substr(village_id, 1, 2)
  ) %>% 
  mutate(
    isic_level1 = as.character(isic_level1),
    `EARP`  = ifelse(earp_lv == 1 | earp_mv == 1, 1, 0)
  ) %>%
  rename(
    isic = isic_level1
  ) %>% 
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0)
  )



# Private sector-------


join_drop13 <- expansion_join_drop %>% 
  select(`EARP`, village_id, cell_id, sector_id, cell_office, health_center, primary_school, secondary_school, 
         sector_district_office, market, industry, residential_consumer, non_residential_consumer, imidugudu, population, popdens, national_rd, dist_na_rd)

earp_p <- earp_did_isic %>%
  mutate(private_sector = ifelse(isic %in% c(9,7,3,19), 1, 0)) %>%
  filter(private_sector == 1) %>%
  group_by(year, village_id) %>%
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE), .groups="drop") %>%
  mutate(total_employee = pmin(total_employee, 100)) %>% 
  left_join(join_drop13, by = c("village_id") )  %>%
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    imidugudu_year = paste0(imidugudu,"_", year),
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  ) %>% 
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0)
  )






#Control regression-----


usage_2019 <- utility_long %>% 
  filter(year == 2019) %>% 
  mutate(
    usage_2019 = usage
  ) %>% 
  select(village_id, usage_2019)


earp_p_2011 <- earp_p %>% 
  filter(year == 2011) %>% 
  select(village_id, num_establishment, total_employee)

expansion_control <- expansion_join%>% 
  filter(electrified_year <= 2019) %>% 
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer)
  ) %>% 
  left_join(usage_2019) %>% 
  left_join(earp_p_2011) %>% 
  mutate(
    usage_2019 = replace_na(usage_2019, 0),
    log_usage_2019 = log1p(usage_2019),
    log_population = log1p(population), 
    log_national = log1p(national_rd),
    log_dist_na = log1p(dist_na_rd)
  )

control_reg <- felm(log_usage_2019 ~ log_population  + log_national + log_dist_na + cell_office + health_center + primary_school + secondary_school + sector_district_office + 
                      industry + market + imidugudu + log1_residential_consumer + log1_non_residential_consumer + num_establishment + total_employee |cell_id, data = expansion_control
                    )

summary(control_reg)


library(stargazer)
# Export regression table
stargazer(
  control_reg,
  type = "latex",    # "text" for console, "latex" for Overleaf
  title = "Regression Results: log_usage_2019",
  dep.var.labels = "Log1(Usage 2019)",
  covariate.labels = c(
    "Log1(Population)",
    "Log1(dist. to nearest national rd)",
    "Log1(dist. to nearest national/district rd)",
    "Cell Office",
    "Health Center",
    "Primary School",
    "Secondary School",
    "Sector/District Office",
    "Industry",
    "Market",
    "Imidugudu",
    "Log1(Residential Consumers + 1)",
    "Log1(Non-Residential Consumers + 1)",
    "2011 num-establishment",
    "2011 total-employee"
  ),
  omit = "cell_id",
  omit.stat = c("f", "ser"),
  digits = 3,
  no.space = TRUE,
  out = file.path(output_path, "regressions", "controls_reg_2019.tex")
)


#Plot--------

library(ggplot2)
library(sf)


# Base plot: administrative cells

p <- ggplot() +
  geom_sf(data = rwa_district, fill = NA, color = "gray80", size = 0.2) +
  
  geom_sf(data = rwa_cell, fill = NA, color = "gray80", size = 0.2) +
  
  # === Grid lines ===
  geom_sf(data = earp_existing_mv_sf, aes(color = "Existing MV line"), 
          size = 2, linetype = "solid", alpha = 0.9) +
  geom_sf(data = earp_planned_mv, aes(color = "Planned MV line"), 
          size = 1, linetype = "dashed", alpha = 0.9) +
  # geom_sf(data = earp_planned_lv, aes(color = "Planned LV line"), 
  #         size = 0.8, linetype = "dashed", alpha = 0.9) +
  
  # === Infrastructure points ===
  # Admin Centres
  geom_sf(data = cell_office_sf, aes(shape = "Cell Office"), 
          fill = "darkgreen", color = "darkgreen", size = 0.5, alpha = 0.9) +
  geom_sf(data = office_sf, aes(shape = "Sector/District Office"), 
          fill = "forestgreen", color = "forestgreen", size = 0.5, alpha = 0.9) +
  
  # Others
  geom_sf(data = industry_sf, aes(shape = "Industry"), 
          fill = "#00BFC4", color = "#00BFC4", size = 0.5, alpha = 0.9) +
  geom_sf(data = primary_school_sf, aes(shape = "Primary School"), 
          fill = "#F8766D", color = "#F8766D", size = 0.5, alpha = 0.9) +
  geom_sf(data = secondary_school_sf, aes(shape = "Secondary School"), 
          fill = "yellow", color = "yellow", size = 0.5, alpha = 0.9) +
  geom_sf(data = health_center_sf, aes(shape = "Health Center"), 
          fill = "purple", color = "purple", size = 0.5, alpha = 0.9) +
  geom_sf(data = market_sf, aes(shape = "Market"), 
          fill = "goldenrod2", color = "goldenrod2", size = 0.5, alpha = 0.9) +
  
  # === Legend setup ===
  scale_color_manual(
    values = c(
      # Grid Lines
      "Existing MV line" = "darkred",
      "Planned MV line"  = "blue"
      # # Admin Centres
      # "Cell Office" = "darkgreen",
      # "Sector/District Office" = "forestgreen",
      # # Others
      # "Industry" = "#00BFC4",
      # "Primary School" = "#F8766D",
      # "Secondary School" = "yellow",
      # "Health Center" = "purple",
      # "Market" = "goldenrod2"
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "Cell Office" = 21,
      "Sector/District Office" = 24,
      "Industry" = 22,
      "Primary School" = 23,
      "Secondary School" = 23,
      "Health Center" = 21,
      "Market" = 24
    ),
    na.translate = FALSE
  ) +
  
  guides(
    # Grid lines legend (only the two MV line types)
    color = guide_legend(
      title = "Grid Lines",
      order = 1,
      override.aes = list(size = 2),
      include = c("Existing MV line", "Planned MV line")
    ),
    # Infrastructure legend (everything else)
    shape = guide_legend(
      title = "Admin Centres & Others",
      order = 2,
      override.aes = list(size = 3, alpha = 0.9)
    )
  ) +
  
  # === Styling ===
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  coord_sf() +
  labs(
    title = "Infrastructure and Grid Network Map",
    subtitle = "Rwanda villages with EARP MV/LV grid lines and key infrastructure",
    caption = "Data: EDCL / DIME Rwanda EAQIP project"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 8)
  ) + theme_void()

p


ggsave(
  filename = file.path(output_path, "plot","infrastructure_grid_network_map.png"),
  plot = p,
  width = 10, height = 8, dpi = 400
)


#Rulindo plot-----


library(ggplot2)
library(sf)
library(dplyr)

# --- 1. Filter to Rulindo district ---------------------------------------
rulindo_dist <- rwa_district %>% filter(District == "Rulindo")
rulindo_cells <- rwa_cell %>% filter(District == "Rulindo")

# Join expansion data and flag EARP

expansion_join_drop13 <- expansion_join %>% 
  mutate(
    `EARP` = ifelse(earp_lv == 1 | earp_mv == 1, 1, 0)
  ) %>% 
  filter(electrified_year > 2013) %>% 
  anti_join(earp_existing_mv, by = c("village_id" = "Village_ID")) %>% 
  filter(! District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu"))

# Get all Rulindo villages (for scope layer)
rulindo_villages <- rwa_villages %>%
  filter(District == "Rulindo") %>%
  left_join(expansion_join_drop13 %>% select(village_id, EARP), by = c("Village_ID" = "village_id"))%>%
  mutate(
    status = case_when(
      EARP == 1 ~ "EARP village",
      EARP == 0 ~ "Non-EARP village",
      TRUE ~ "Electrified before 2013"
    )
  ) %>% 
  left_join(expansion_join %>%  select(village_id, electrified_year), by = c("Village_ID" = "village_id")) %>% 
  mutate(
    electrified_wave = case_when(
      electrified_year == 9999 ~ "Never electrified (reference group)",
      electrified_year <= 2011 ~ "Electrified before 2011",
      electrified_year %in% 2012:2014 ~ "Electrified 2012–2014",
      electrified_year %in% 2015:2017 ~ "Electrified 2015–2017",
      electrified_year > 2017 ~ "Electrified after 2017",
      TRUE ~ NA_character_
    )
  )




#EARP----------
# --- 2. Clip line and point layers to Rulindo boundary --------------------
earp_existing_mv_sf_r <- st_intersection(earp_existing_mv_sf, rulindo_dist)

earp_planned_mv_r     <- st_intersection(earp_planned_mv, rulindo_dist)
earp_planned_lv_r     <- st_intersection(earp_planned_lv, rulindo_dist)

cell_office_sf_r      <- st_intersection(cell_office_sf, rulindo_dist)
office_sf_r           <- st_intersection(office_sf, rulindo_dist)
industry_sf_r         <- st_intersection(industry_sf, rulindo_dist)
primary_school_sf_r   <- st_intersection(primary_school_sf, rulindo_dist)
secondary_school_sf_r <- st_intersection(secondary_school_sf, rulindo_dist)

health_center_sf_r    <- st_intersection(health_center_sf, rulindo_dist)

market_sf_r           <- st_intersection(market_sf, rulindo_dist)


# --- 3. Plot --------------------------------------------------------------
p_rulindo <- ggplot() +
  # --- Shade villages ---
  geom_sf(
    data = rulindo_villages,
    aes(fill = status),
    color = "white",
    size = 0.1
  ) +
  
  # --- Cell and District boundaries ---
  geom_sf(data = rulindo_cells, fill = NA, color = "gray60", size = 0.3) +
  geom_sf(data = rulindo_dist, fill = NA, color = "black", size = 0.6) +
  
  # --- Grid lines ---
  geom_sf(data = earp_existing_mv_sf_r, aes(color = "Existing MV line"),
          size = 1.2, linetype = "solid", alpha = 0.9) +
  geom_sf(data = earp_planned_mv_r, aes(color = "Planned MV line"),
          size = 1, linetype = "dashed", alpha = 0.9) +
  geom_sf(data = earp_planned_lv_r, aes(color = "Planned LV line"),
          size = 1, linetype = "dashed", alpha = 0.9) +
  # --- Infrastructure points ---
  geom_sf(data = cell_office_sf_r, aes(shape = "Cell Office"),
          fill = "darkgreen", color = "darkgreen", size = 1.5, alpha = 0.9) +
  geom_sf(data = office_sf_r, aes(shape = "Sector/District Office"),
          fill = "forestgreen", color = "forestgreen", size = 1.5, alpha = 0.9) +
  # geom_sf(data = industry_sf_r, aes(shape = "Industry"),
  #         fill = "#00BFC4", color = "#00BFC4", size = 1.5, alpha = 0.9) +
  geom_sf(data = primary_school_sf_r, aes(shape = "Primary School"),
          fill = "#F8766D", color = "#F8766D", size = 1.5, alpha = 0.9) +
  geom_sf(data = secondary_school_sf_r, aes(shape = "Secondary School"),
          fill = "yellow", color = "yellow", size = 1.5, alpha = 0.9) +
  geom_sf(data = health_center_sf_r, aes(shape = "Health Center"),
          fill = "purple", color = "purple", size = 1.5, alpha = 0.9) +
  geom_sf(data = market_sf_r, aes(shape = "Market"),
          fill = "goldenrod2", color = "goldenrod2", size = 1.5, alpha = 0.9) +
  
  # --- Color & Fill Scales ---
  scale_fill_manual(
    name = "Village Status",
    values = c(
      "EARP village" = "#F8766D",       # pink
      "Non-EARP village" = "#00BFC4",   # blue
      "Electrified before 2013" = "gray80"         # grey
    )
  ) +
  
  scale_color_manual(
    name = "Grid Lines",
    values = c(
      "Existing MV line" = "darkred",
      "Planned MV line" = "red",
      "Planned LV line" = "blue"
    )
  ) +
  
  scale_shape_manual(
    name = "Infrastructure",
    values = c(
      "Cell Office" = 21,
      "Sector/District Office" = 24,
      "Industry" = 22,
      "Primary School" = 23,
      "Secondary School" = 23,
      "Health Center" = 21,
      "Market" = 24
    )
  ) +
  
  guides(
    fill = guide_legend(order = 1, override.aes = list(size = 5)),
    color = guide_legend(order = 2, override.aes = list(size = 2)),
    shape = guide_legend(order = 3, override.aes = list(size = 3))
  ) +
  
  coord_sf() +
  labs(
    title = "Rulindo District: Electrification and Infrastructure Map"  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 8),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", color = NA)
  )

p_rulindo

# --- 4. Save map output ---------------------------------------------------

ggsave(
  filename = file.path(output_path, "plot", "rulindo_earp_map.png"),
  plot = p_rulindo,
  width = 10,       # in inches
  height = 8,       # in inches
  units = "in",
  dpi = 400,        # high resolution for publication
  scale = 1.2,      # slightly enlarges text/lines for clarity
  bg = "white"      # ensures white background instead of transparent
)





# --- 3. Plot --------------------------------------------------------------



# Infrastructure shapes
infra_shapes <- c(
  "Cell Office"            = 21,
  "Sector/District Office" = 24,
  "Industry"               = 22,
  "Primary School"         = 23,
  "Secondary School"       = 23,
  "Health Center"          = 21,
  "Market"                 = 24
)

p_rulindo <- ggplot() +
  # --- Shade villages by electrification wave ---
  geom_sf(
    data = rulindo_villages,
    aes(fill = electrified_wave),
    color = "white",
    size = 0.1
  ) +
  
  # --- Cell and District boundaries ---
  geom_sf(data = rulindo_cells, fill = NA, color = "gray60", size = 0.3) +
  geom_sf(data = rulindo_dist, fill = NA, color = "black", size = 0.6) +
  
  # --- Grid lines ---
  geom_sf(data = earp_existing_mv_sf_r, aes(color = "Existing MV line"),
          size = 1.2, linetype = "solid", alpha = 0.9) +
  
  # --- Infrastructure points ---
  geom_sf(data = cell_office_sf_r, aes(shape = "Cell Office"),
          fill = "darkgreen", color = "darkgreen", size = 1.5, alpha = 0.9) +
  geom_sf(data = office_sf_r, aes(shape ="Sector/District Office"),
          fill = "forestgreen", color = "forestgreen", size = 1.5, alpha = 0.9) +
  # geom_sf(data = industry_sf_r, aes(shape = "Industry"),
  #         fill = "#00BFC4", color = "#00BFC4", size = 1.5, alpha = 0.9) +
  geom_sf(data = primary_school_sf_r, aes(shape = "Primary School"),
          fill = "#F8766D", color = "#F8766D", size = 1.5, alpha = 0.9) +
  geom_sf(data = secondary_school_sf_r, aes(shape = "Secondary School"),
          fill = "yellow", color = "yellow", size = 1.5, alpha = 0.9) +
  geom_sf(data = health_center_sf_r, aes(shape = "Health Center"),
          fill = "purple", color = "purple", size = 1.5, alpha = 0.9) +
  geom_sf(data = market_sf_r, aes(shape = "Market"),
          fill = "goldenrod2", color = "goldenrod2", size = 1.5, alpha = 0.9) +
  
  # --- Color & Fill Scales ---
  scale_fill_manual(
    name = "Electrification Wave",
    values = c(
      "Never electrified (reference group)" = "#E0E0E0",
      "Electrified before 2011" = "#B2DF8A",
      "Electrified 2012–2014" = "#33A02C",
      "Electrified 2015–2017" = "#1F78B4",
      "Electrified after 2017" = "#A6CEE3"
      # "Cell Office"            = "darkgreen",
      # "Sector/District Office" = "forestgreen",
      # "Industry"               = "#00BFC4",
      # "Primary School"         = "#F8766D",
      # "Secondary School"       = "yellow",
      # "Health Center"          = "purple",
      # "Market"                 = "goldenrod2"
    )
  ) +
  
  scale_color_manual(
    name = "Grid Lines",
    values = c(
      "Existing MV line" = "darkred"
    )
  ) +
  
  scale_shape_manual(
    name = "Infrastructure",
    values = c(
      "Cell Office" = 21,
      "Sector/District Office" = 24,
      "Industry" = 22,
      "Primary School" = 23,
      "Secondary School" = 23,
      "Health Center" = 21,
      "Market" = 24
    )
  ) +
  
  guides(
    fill = guide_legend(order = 1, override.aes = list(size = 5)),
    color = guide_legend(order = 2, override.aes = list(size = 2)),
    shape = guide_legend(order = 3, override.aes = list(size = 3))
  ) +
  
  coord_sf() +
  labs(
    title = "Rulindo District: Electrification and Infrastructure Map",
    subtitle = "Village shading by electrification wave"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 8),
    legend.background = element_blank(),
    legend.key = element_rect(fill = NA, color = "black"), 
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", color = NA)
  )


p_rulindo



# --- Save ----------------------------------------------------
ggsave(
  filename = file.path(output_path, "plot", "rulindo_electrification_year_map.png"),
  plot     = p_rulindo,
  width = 10, height = 8, units = "in",
  dpi = 400, scale = 1.2, bg = "white"
)
