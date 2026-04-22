#######################################
#Purpose: using Pixel
#Author: XIAOMING ZHANG
#Date: March 1st 2026
######################################################
pacman::p_load(knitr, dplyr, lfe,fixest,raster, modelsummary, stargazer, tidyverse, dplyr, here, sf, haven, ggplot2, readxl,  writexl, janitor, randomizr, RCT, purrr, RODBC, DBI)

getwd()

dropbox <- 'C:/Users/wb614406/Dropbox'

data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Historical Expansion/data"
)


historical_data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Historical data"
)

output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Historical Expansion/outputs"
)


#Data read------

##Nightlight-----
viirs_stack <- brick(
  file.path(
    historical_data_path,
    "Nightlight",
    "data",
    "gb_viirs_corrected_monthly_start_201401_avg_rad.tif"
  )
)

ntl_2014_01 <- viirs_stack[[1]]


ntl_pixel <- readRDS(
  file.path(
    data_path,
    "Nightlight",
    "data",
    "nightlight_viirs_pixel.rds"
  )
) 

ntl_pixel_sf <- st_as_sf(
  ntl_pixel,
  coords = c("x", "y"),
  crs = 4326   # WGS84
)

##Rwanda village------
rwa_village <- st_read(
  file.path(historical_data_path, "rwa_villages", "Village.shp")
)

rwa_village <- st_transform(rwa_village, crs = st_crs(ntl_pixel_sf))

rwa_village <- st_make_valid(rwa_village)

##Intersection-----

village_ntl_pixel <- st_join(
  ntl_pixel_sf,
  rwa_village,
  join = st_within   # pixel must fall within village polygon
) %>% 
  rename(village_id = Village_ID)


expansion_join <- read_xlsx(
  file.path(output_path, "expansion_join.xlsx")
) %>% 
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
  dplyr::select(village_id, status, cell_id, sector_id, cell_office, health_center, primary_school, secondary_school, sector_district_office, industry,imidugudu,  market) %>% 
  distinct(village_id, .keep_all = TRUE)




village_ntl <- left_join(village_ntl_pixel,  expansion_join, by = c("village_id"))



#p95_cut <- quantile(ntl_long$ntl, 0.95, na.rm = TRUE)
#p99_cut <- quantile(ntl_long$ntl, 0.99, na.rm = TRUE)


#Data construction-------

ntl_long <- village_ntl %>%
  pivot_longer(
    cols = matches("^(2014|2017|2020)_"),
    names_to = "year_month",
    values_to = "ntl_raw"
  ) %>% 
  mutate(
    ntl = ifelse(ntl_raw <0, 0, ntl_raw),
    ntl_0.6  = ifelse(ntl >= 0.6, 0.6, ntl ),
    ntl_3 = ifelse(ntl >= 3, 3, ntl),
    ntl_15 = ifelse(ntl >= 15, 15, ntl),
    ntl_15_drop = ifelse(ntl >= 15, NA, ntl),
  ) %>% 
  dplyr::select(village_id, year_month,ntl_raw, starts_with("ntl"), status, District, cell_id, sector_id, cell_office, health_center, primary_school, secondary_school, sector_district_office, industry, market, imidugudu, geometry)




#Graph------
village_ntl_analysis <- village_ntl %>% 
  dplyr::select(village_id, status,`2014_01`, `2017_01`, `2020_01`) %>% 
  rename(
    `2014` = `2014_01`,
    `2017` = `2017_01`,
    `2020` = `2020_01`
  )



village_ntl_graph <- village_ntl %>% 
  dplyr::select(
    village_id,
    status,
    starts_with("2014"),
    starts_with("2017"),
    starts_with("2020")
  )

ntl_long_graph <- village_ntl %>%
  pivot_longer(
    cols = matches("^(2014|2017|2020)_"),
    names_to = "year_month",
    values_to = "ntl_raw"
  ) %>% 
  mutate(
    ntl = ifelse(ntl_raw <0, 0, ntl_raw),
    ntl = ifelse(ntl_raw >5, 5, ntl_raw),
    p95 = quantile(ntl_raw, 0.95, na.rm = TRUE),
    p99 = quantile(ntl_raw, 0.99, na.rm = TRUE),
    ntl_p95 = pmin(ntl_raw, p95),               # winsorized at p95
    ntl_p99 = pmin(ntl_raw, p99)
  ) %>% 
  dplyr::select(village_id, year_month,ntl_raw, starts_with("ntl"), status, geometry) %>% 
  filter(status %in% c("elec12_14",  "elec15_17", "never_elec") )

View(ntl_long_graph)

ggplot(ntl_long_graph, aes(x = ntl)) +
  geom_histogram(bins = 80) +
  facet_wrap(~year_month, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Pixel-Level NTL Distribution by Month (win5)",
    x = "NTL value winsorized at 5",
    y = "Count"
  )



ggplot(ntl_long_graph, aes(x = ntl_p99)) +
  geom_histogram(bins = 80) +
  facet_wrap(~year_month, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Pixel-Level NTL Distribution by Month(p99)",
    x = "NTL value pp99",
    y = "Count"
  )


ggplot(ntl_long_graph, aes(x = ntl_p95)) +
  geom_histogram(bins = 80) +
  facet_wrap(~year_month, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Pixel-Level NTL Distribution by Month(p95)",
    x = "NTL value pp95",
    y = "Count"
  )



##Indicator plot------
HIGH_THRESH <- 0.6

# Step 1: Filter shapefile to Rulindo only
# (check exact column/value with names(rwa_village) and unique(rwa_village$District))
rwa_rulindo <- rwa_village %>%
  filter(District == "Rulindo") %>% 
  left_join(expansion_join, by = c("Village_ID" = "village_id")) %>% 
  filter(status %in% c("elec12_14",  "elec15_17", "never_elec") )


# Step 2: Pivot to long and create flags
ntl_flags_rulindo <- village_ntl_graph %>%
  pivot_longer(
    cols = matches("^(2014|2017|2020)_"),
    names_to = "year_month",
    values_to = "ntl_raw"
  ) %>%
  filter(year_month %in% c("2014_01", "2017_01", "2020_01")) %>%  # one per wave
  mutate(
    flag_negative = ntl_raw < 0,
    flag_high     = ntl_raw > HIGH_THRESH,
    flag_type = case_when(
      flag_negative & flag_high ~ "Both",
      flag_negative             ~ "Negative",
      flag_high                 ~ "High (>0.6)",
      TRUE                      ~ "Normal"
    )
  ) %>%
  filter(village_id %in% rwa_rulindo$Village_ID) %>% 
  filter(status %in% c("elec12_14",  "elec15_17", "never_elec") )

View(ntl_flags_rulindo)



## Combined diagnostic ----------------------------
# Create plot
p_flags <- ggplot() +
  geom_sf(data = rwa_rulindo, fill = NA, color = "black", linewidth = 0.1) +
  geom_sf(
    data = filter(ntl_flags_rulindo, flag_type != "Normal"),
    aes(color = flag_type),
    size = 2, alpha = 0.8
  ) +
  scale_color_manual(values = c(
    "Negative" = "blue",
    "High (>0.6)" = "red"
  )) +
  facet_wrap(~year_month) +
  theme_void() +
  labs(
    title = "Rulindo: Flagged Pixels (Negative & High NTL) by Wave",
    color = "Flag type"
  )

# Save to outputs folder
ggsave(
  filename = file.path(output_path, "plot", "rulindo_flagged_pixels_map.png"),
  plot = p_flags,
  width = 12,
  height = 8,
  dpi = 300
)













#Cross-sectional Results-------

##2017-----

library(dplyr)

utility_2017 <- utility_long %>%
  filter(year == 2017)

utility_2020 <- utility_long %>%
  filter(year == 2020)
# Prepare NTL 2017


expansion_join17 <- ntl_long%>%
  filter(year_month == "2017_01") %>%
  filter(! District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu")) %>%
  left_join(utility_2017, by = "village_id") %>%
  filter(status %in% c("elec12_14", "elec15_17", "never_elec")) %>%
  mutate(
    elec12_17 = ifelse(status %in% c("elec12_14", "elec15_17"),1, 0),
    usage = ifelse(is.na(usage), 0, usage),
    usage = ifelse(elec12_17 == 0, 0, usage),
    usage = usage / (100 * 365)
  ) %>%
  rename(
    usage_2017 = usage
  )


#Function------

cross_sectional <- function(lhs_var, data, output_path) {
  
  safe_var <- gsub("\\.", "-", lhs_var)
  safe_var <- gsub("_", "-", safe_var)
  
  # Create log1p variable name
  lhs_log       <- paste0(lhs_var, "_log1p")
  lhs_log_label <- paste0(safe_var, "-log1p")
  
  # Create local copy of data and construct log variable
  data <- data %>%
    mutate(!!lhs_log := log1p(.data[[lhs_var]]))
  
  
  # Pre-build formulas
  infra_fe <- "cell_id + cell_office + health_center + primary_school + secondary_school + sector_district_office + industry + market + imidugudu"
  
  f1 <- as.formula(paste0(lhs_var, " ~ elec12_17 | cell_id | 0 | sector_id"))
  f2 <- as.formula(paste0(lhs_var, " ~ elec12_17 | ", infra_fe, " | 0 | sector_id"))
  f3 <- as.formula(paste0(lhs_log, " ~ elec12_17 | cell_id | 0 | sector_id"))
  f4 <- as.formula(paste0(lhs_log, " ~ elec12_17 | ", infra_fe, " | 0 | sector_id"))
  
  f1_u <- as.formula(paste0(lhs_var, " ~ elec12_17 + usage_2017 | cell_id | 0 | sector_id"))
  f2_u <- as.formula(paste0(lhs_var, " ~ elec12_17 + usage_2017 | ", infra_fe, " | 0 | sector_id"))
  f3_u <- as.formula(paste0(lhs_log, " ~ elec12_17 + usage_2017 | cell_id | 0 | sector_id"))
  f4_u <- as.formula(paste0(lhs_log, " ~ elec12_17 + usage_2017 | ", infra_fe, " | 0 | sector_id"))
  
  # Baseline regressions
  reg1 <- felm(f1, data = data)
  reg2 <- felm(f2, data = data)
  reg3 <- felm(f3, data = data)
  reg4 <- felm(f4, data = data)
  
  stargazer(
    list(reg1, reg2, reg3, reg4),
    type = "latex",
    out = file.path(output_path, "regressions", paste0("elec12_17_", lhs_var, "_cell_viirs.tex")),
    title = paste("Electrification Status and", lhs_var, "in 2017"),
    label = paste0("tab:", safe_var),
    column.labels = c("cell FE", "cell + infra FE", "cell FE", "cell + infra FE"),
    dep.var.labels = c(safe_var, lhs_log_label),
    multicolumn = TRUE,
    covariate.labels = c("Electrified by 2017"),
    keep = c("elec12_17"),
    omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
    keep.stat = c("n", "rsq"),
    header = FALSE, font.size = "small", digits = 4
  )
  
  # With usage
  
  reg1_u <- felm(f1_u, data = data)
  reg2_u <- felm(f2_u, data = data)
  reg3_u <- felm(f3_u, data = data)
  reg4_u <- felm(f4_u, data = data)
  
  stargazer(
    list(reg1_u, reg2_u, reg3_u, reg4_u),
    type = "latex",
    out = file.path(output_path, "regressions", paste0("elec12_17_usage_", lhs_var, "_cell_viirs.tex")),
    title = paste("Electrification, Usage, and", lhs_var, "in 2017"),
    label = paste0("tab:", safe_var, "-usage"),
    column.labels = c("cell FE", "cell + infra FE", "cell FE", "cell + infra FE"),
    dep.var.labels = c(safe_var, lhs_log_label),
    multicolumn = TRUE,
    covariate.labels = c("Electrified by 2017", "Electricity Usage (2017)"),
    keep = c("elec12_17", "usage_2017"),
    omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
    keep.stat = c("n", "rsq"),
    header = FALSE, font.size = "small", digits = 4
  )
}

lhs_vars <- c("ntl_0.6", "ntl_3", "ntl_15", "ntl_15_drop")
for (v in lhs_vars) {
  cross_sectional(v, expansion_join17, output_path)
}

#DID------

#1. Sample restriction----

expansion_join <- read_xlsx(path = file.path(output_path, "expansion_join.xlsx"))


expansion_join_drop15_17 <- expansion_join%>% 
  filter(electrified_year %in% c("2015", "2016", "2017") |electrified_year == "9999" ) %>% 
  mutate(
    `elec15_17` = ifelse(electrified_year %in% c("2015", "2016", "2017"), 1, 0)
  ) %>% 
  #anti_join(earp_existing_mv, by = c("village_id" = "Village_ID")) %>% 
  filter(! District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu")) %>% 
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0),
    consumer = residential_consumer + non_residential_consumer
  ) 


elec15_17_controls <- expansion_join_drop15_17 %>%
  dplyr::select(
    village_id,
    elec15_17,
    cell_id,
    sector_id,
    cell_office,
    health_center,
    primary_school,
    secondary_school,
    sector_district_office,
    industry,
    market,
    imidugudu
  )

# Update ntl_long to include all NTL variants
ntl_long_did <- ntl_long %>%
  dplyr::filter(year_month %in% c("2014_01", "2017_01", "2020_01"))  |> 
  mutate(year = case_when(
    year_month == "2014_01" ~ 2014,
    year_month == "2017_01" ~ 2017,
    year_month == "2020_01" ~ 2020
  ))  %>% 
  dplyr::filter(status %in% c("elec15_17", "never_elec" )) %>% 
  mutate(
    `elec15_17` = ifelse(status %in% c("elec15_17"), 1, 0)
  ) %>% 
  #anti_join(earp_existing_mv, by = c("village_id" = "Village_ID")) %>% 
  dplyr::filter(! District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu")) %>% 
  mutate(
    ntl = ifelse(is.na(ntl), 0 , ntl),
    # Fixed effects
    sector_year = paste0(sector_id, "_", year),
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    imidugudu_year = paste0(imidugudu, "_", year)
  ) %>% 
  mutate(log1_ntl = log1p(ntl)) %>% 
  mutate(year = as.factor(year),
         year = factor(year, levels = c("2014",  "2017", "2020"))
  )


ntl_long_did <- ntl_long_did |>
  mutate(pixel_id = as.character(geometry)) |>
  st_drop_geometry() |>
  complete(pixel_id, year = factor(c("2014", "2017", "2020"), levels = c("2014", "2017", "2020"))) |>
  group_by(pixel_id) |>
  fill(village_id, sector_id, cell_id, District, elec15_17, status,
       cell_office, health_center, primary_school, secondary_school,
       sector_district_office, industry, market, imidugudu,
       .direction = "downup") |>
  ungroup() |>
  mutate(
    ntl = replace_na(ntl, 0),
    log1_ntl = log1p(ntl),
    # Recreate year interaction FEs
    sector_year = paste0(sector_id, "_", year),
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    imidugudu_year = paste0(imidugudu, "_", year)
  )

# Verify balance
ntl_long_did |>
  count(pixel_id) |>
  count(n, name = "n_pixels")

#DID Regressions------

did_regression <- function(lhs_var, data, output_path) {
  
  # Create safe labels for LaTeX
  
  safe_var <- gsub("\\.", "-", lhs_var)
  safe_var <- gsub("_", "-", safe_var)
  
  # Create log1p variable name
  lhs_log       <- paste0(lhs_var, "_log1p")
  lhs_log_label <- paste0(safe_var, "-log1p")
  
  # Create log variable in data
  data <- data %>%
    mutate(!!lhs_log := log1p(.data[[lhs_var]]))
  
  # Build formulas for level regressions
  f_level_1 <- as.formula(paste0(
    lhs_var, " ~ year * elec15_17 | village_id + cell_year | 0 | sector_id"
  ))
  
  f_level_2 <- as.formula(paste0(
    lhs_var, " ~ year * elec15_17 | village_id + cell_year + ",
    "cell_office_year + health_center_year + primary_school_year + ",
    "secondary_school_year + sector_district_office_year + industry_year + ",
    "market_year + imidugudu_year | 0 | sector_id"
  ))
  
  # Build formulas for log regressions
  f_log_1 <- as.formula(paste0(
    lhs_log, " ~ year * elec15_17 | village_id + cell_year | 0 | sector_id"
  ))
  
  f_log_2 <- as.formula(paste0(
    lhs_log, " ~ year * elec15_17 | village_id + cell_year + ",
    "cell_office_year + health_center_year + primary_school_year + ",
    "secondary_school_year + sector_district_office_year + industry_year + ",
    "market_year + imidugudu_year | 0 | sector_id"
  ))
  
  # Run regressions
  reg_level_1 <- felm(f_level_1, data = data)
  reg_level_2 <- felm(f_level_2, data = data)
  reg_log_1   <- felm(f_log_1, data = data)
  reg_log_2   <- felm(f_log_2, data = data)
  
  reg_list <- list(reg_level_1, reg_level_2, reg_log_1, reg_log_2)
  
  # Output file path
  tex_file <- file.path(
    output_path, "regressions",
    paste0("elec15_17_", lhs_var, "_did_pixel.tex")
  )
  
  stargazer(
    reg_list,
    type = "latex",
    out = tex_file,
    title = paste("DID: Electrification and", lhs_var),
    label = paste0("tab:did-", safe_var),
    column.labels = c("cell FE", "cell + infra FE", "cell FE", "cell + infra FE"),
    dep.var.labels = c(safe_var, lhs_log_label),
    multicolumn = TRUE,
    covariate.labels = c("elec15-17 × 2017", "elec15-17 × 2020"),
    keep = c("year2017:elec15_17", "year2020:elec15_17"),
    omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
    keep.stat = c("n", "rsq"),
    add.lines = list(
      c("Village FE", "Yes", "Yes", "Yes", "Yes"),
      c("Cell × Year FE", "Yes", "Yes", "Yes", "Yes"),
      c("Infrastructure × Year FE", "No", "Yes", "No", "Yes"),
      c("Clustered SE (Sector)", "Yes", "Yes", "Yes", "Yes")
    ),
    header = FALSE,
    font.size = "small",
    digits = 4
  )
  
  invisible(reg_list)
}

# Run for all NTL variants
lhs_vars_did <- c("ntl_0.6", "ntl_3", "ntl_15", "ntl_15_drop")
for (v in lhs_vars_did) {
  did_regression(v, ntl_long_did, output_path)
}



# Update ntl_long to include all years 2014-2020-------


ntl_long <- village_ntl %>%
  pivot_longer(
    cols = matches("^(2014|2015|2016|2017|2018|2019|2020)_"),
    names_to = "year_month",
    values_to = "ntl_raw"
  ) %>% 
  mutate(
    ntl = ifelse(ntl_raw <0, 0, ntl_raw),
    ntl_0.6  = ifelse(ntl >= 0.6, 0.6, ntl ),
    ntl_3 = ifelse(ntl >= 3, 3, ntl),
    ntl_15 = ifelse(ntl >= 15, 15, ntl),
    ntl_15_drop = ifelse(ntl >= 15, NA, ntl),
  ) %>% 
  dplyr::select(village_id, year_month,ntl_raw, starts_with("ntl"), status, District, cell_id, sector_id, cell_office, health_center, primary_school, secondary_school, sector_district_office, industry, market, imidugudu, geometry)





ntl_long_did <- ntl_long %>%
  filter(str_detect(year_month, "^(2014|2015|2016|2017|2018|2019|2020)_01$")) |> 
  mutate(year = as.numeric(str_extract(year_month, "^\\d{4}"))) %>% 
  filter(status %in% c("elec15_17", "never_elec")) %>% 
  mutate(
    elec15_17 = ifelse(status == "elec15_17", 1, 0)
  ) %>% 
  filter(!District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu")) %>% 
  mutate(
    ntl = ifelse(is.na(ntl), 0, ntl),
    sector_year = paste0(sector_id, "_", year),
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    imidugudu_year = paste0(imidugudu, "_", year)
  ) %>% 
  mutate(log1_ntl = log1p(ntl)) %>% 
  mutate(
    year = as.factor(year),
    year = factor(year, levels = as.character(2014:2020))
  )

# Balance the panel
ntl_long_did <- ntl_long_did |>
  mutate(pixel_id = as.character(geometry)) |>
  st_drop_geometry() |>
  complete(pixel_id, year = factor(2014:2020, levels = as.character(2014:2020))) |>
  group_by(pixel_id) |>
  fill(village_id, sector_id, cell_id, District, elec15_17, status,
       cell_office, health_center, primary_school, secondary_school,
       sector_district_office, industry, market, imidugudu,
       .direction = "downup") |>
  ungroup() |>
  mutate(
    ntl = replace_na(ntl, 0),
    log1_ntl = log1p(ntl),
    sector_year = paste0(sector_id, "_", year),
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    imidugudu_year = paste0(imidugudu, "_", year)
  )

# Verify balance
ntl_long_did |>
  count(pixel_id) |>
  count(n, name = "n_pixels")

#write one regression with usage control for ntl_0.6
reg_0.6 <- felm(ntl_0.6 ~ year * elec15_17 | village_id + cell_year + cell_office_year + health_center_year + primary_school_year + secondary_school_year + sector_district_office_year + industry_year + market_year + imidugudu_year | 0 | sector_id, data = ntl_long_did)
summary(reg_0.6)

# Updated DID regression function for 2014-2019
did_regression <- function(lhs_var, data, output_path) {
  
  safe_var <- gsub("\\.", "-", lhs_var)
  safe_var <- gsub("_", "-", safe_var)
  
  lhs_log       <- paste0(lhs_var, "_log1p")
  lhs_log_label <- paste0(safe_var, "-log1p")
  
  data <- data %>%
    mutate(!!lhs_log := log1p(.data[[lhs_var]]))
  
  f_level_1 <- as.formula(paste0(
    lhs_var, " ~ year * elec15_17 | village_id + cell_year | 0 | sector_id"
  ))
  
  f_level_2 <- as.formula(paste0(
    lhs_var, " ~ year * elec15_17 | village_id + cell_year + ",
    "cell_office_year + health_center_year + primary_school_year + ",
    "secondary_school_year + sector_district_office_year + industry_year + ",
    "market_year + imidugudu_year | 0 | sector_id"
  ))
  
  f_log_1 <- as.formula(paste0(
    lhs_log, " ~ year * elec15_17 | village_id + cell_year | 0 | sector_id"
  ))
  
  f_log_2 <- as.formula(paste0(
    lhs_log, " ~ year * elec15_17 | village_id + cell_year + ",
    "cell_office_year + health_center_year + primary_school_year + ",
    "secondary_school_year + sector_district_office_year + industry_year + ",
    "market_year + imidugudu_year | 0 | sector_id"
  ))
  
  reg_level_1 <- felm(f_level_1, data = data)
  reg_level_2 <- felm(f_level_2, data = data)
  reg_log_1   <- felm(f_log_1, data = data)
  reg_log_2   <- felm(f_log_2, data = data)
  
  reg_list <- list(reg_level_1, reg_level_2, reg_log_1, reg_log_2)
  
  tex_file <- file.path(
    output_path, "regressions",
    paste0("elec15_17_", lhs_var, "_did_pixel_2014_2020.tex")
  )
  
  
  
  stargazer(
    reg_list,
    type = "latex",
    out = tex_file,
    title = paste("DID: Electrification and", lhs_var, "(2014-2020)"),
    label = paste0("tab:did-", safe_var, "-2014-2020"),
    column.labels = c("cell FE", "cell + infra FE", "cell FE", "cell + infra FE"),
    dep.var.labels = c(safe_var, lhs_log_label),
    multicolumn = TRUE,
    covariate.labels = c(
      "elec15-17 × 2015", "elec15-17 × 2016", "elec15-17 × 2017",
      "elec15-17 × 2018", "elec15-17 × 2019", "elec15-17 × 2020"
    ),
    keep = c("year2015:elec15_17", "year2016:elec15_17", "year2017:elec15_17",
             "year2018:elec15_17", "year2019:elec15_17", "year2020:elec15_17"),
    omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
    keep.stat = c("n", "rsq"),
    add.lines = list(
      c("Village FE", "Yes", "Yes", "Yes", "Yes"),
      c("Cell × Year FE", "Yes", "Yes", "Yes", "Yes"),
      c("Infrastructure × Year FE", "No", "Yes", "No", "Yes"),
      c("Clustered SE (Sector)", "Yes", "Yes", "Yes", "Yes")
    ),
    header = FALSE,
    font.size = "small",
    digits = 4
  )
  
  invisible(reg_list)
}

# Run for all NTL variants
lhs_vars_did <- c("ntl_0.6", "ntl_3", "ntl_15", "ntl_15_drop")
for (v in lhs_vars_did) {
  did_regression(v, ntl_long_did, output_path)
}

# Outside the loop write one regression with usage control for ntl_0.6
did_regression("ntl_0.6", ntl_long_did, output_path)








