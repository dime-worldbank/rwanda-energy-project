#######################################
#Purpose: Using EARP as a control for analysis
#Author: XIAOMING ZHANG
#Date: September 10th 2025
######################################################
pacman::p_load(knitr, lfe,fixest, modelsummary, stargazer, tidyverse, dplyr, here, sf, haven, ggplot2, readxl,  writexl, janitor, randomizr, RCT, purrr, RODBC, DBI)

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

#2011 results-----

# sample: 2011 (dropping Ngororero, Nyabihu, Nyamasheke, and Rubavu)
# regress ntl ~ electrified as of 2011 (electrified as of 2011 = electrified 2011 or before OR has MV line in EARP shapefile)
# 
# regress ntl ~ electricity usage in 2011 + electrified as of 2011 (electrified as of 2011 = electrified 2011 or before OR has MV line in EARP shapefile)

expansion_join <- read_xlsx(path = file.path(output_path, "expansion_join.xlsx"))

View(expansion_join)

utility_2011 <- utility_long %>% 
  filter(year == 2011)


utility_2020 <- utility_long %>% 
  filter(year == 2020) %>% 
  mutate(usage = usage/(100 *365))
  

village_ntl_all <- read_xlsx(path = file.path(path = file.path(historical_data_path, "Nightlight", "data", "LRCC-DVNL data", "village_ntl(2010-2022).xlsx")))


ntl_2011 <- village_ntl_all %>%
  pivot_longer(
    cols = starts_with("ntl_"),
    names_to = "year",
    values_to = "ntl"
  ) %>%
  mutate(
    year = as.integer(gsub("ntl_", "", year))
  ) %>%
  filter(year %in% c(2011)) %>% 
  select(village_id, ntl)

expansion_join11 <- expansion_join %>% 
  filter(! District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu")) %>% 
  mutate(
    `elec11` = ifelse(electrified_year <= 2011|earp_mv == 1, 1, 0)
  ) %>% 
  left_join(utility_2011, by = c("village_id")) %>% 
  left_join(ntl_2011, by = c("village_id")) %>% 
  mutate(usage = ifelse(is.na(usage), 0, usage),
         usage = ifelse(elec11 == 0, 0, usage ),
         ntl = ifelse(is.na(ntl), 0 , ntl),
         ntl = ifelse(ntl >= 5, 5, ntl),
         usage = usage/(100 *365)) %>% 
  rename(usage_2011 = usage,
         ntl_2011 = ntl) %>% 
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0),
    consumer = residential_consumer + non_residential_consumer
  )  %>% 
  mutate(
    ntl_2011_log = log1p(ntl_2011)
  )



##Regressions------
# regress ntl ~ electrified as of 2011 (electrified as of 2011 = electrified 2011 or before OR has MV line in EARP shapefile)
# 
# regress ntl ~ electricity usage in 2011 + electrified as of 2011 (electrified as of 2011 = electrified 2011 or before OR has MV line in EARP shapefile)

###ntl------
ntl_2011_reg.1 = felm(
  ntl_2011 ~ elec11 |
    sector_id |0 | sector_id,
  data = expansion_join11
)

summary(ntl_2011_reg.1)


ntl_2011_reg.2 = felm(
  ntl_2011 ~ elec11 |
    sector_id +
    cell_office + health_center +
    primary_school + secondary_school +
    sector_district_office + industry + market +
    imidugudu |
    0 | sector_id,
  data = expansion_join11
)

summary(ntl_2011_reg.2)

ntl_2011_log_reg.1 = felm(
  ntl_2011_log ~ elec11 |
    sector_id |0 | sector_id,
  data = expansion_join11
)

summary(ntl_2011_log_reg.1)


ntl_2011_log_reg.2 = felm(
  ntl_2011_log ~ elec11 |
    sector_id +
    cell_office + health_center +
    primary_school + secondary_school +
    sector_district_office + industry + market +
    imidugudu |
    0 | sector_id,
  data = expansion_join11
)

summary(ntl_2011_log_reg.2)



tex_file_ntl_2011 <- file.path(
  output_path, "regressions",
  "elec11_ntl_2011.tex"
)

reg_ntl_2011 <- list(
  ntl_2011_reg.1,
  ntl_2011_reg.2,
  ntl_2011_log_reg.1,
  ntl_2011_log_reg.2
)


stargazer(
  reg_ntl_2011,
  type = "latex",
  out = tex_file_ntl_2011,
  title = "Electrification Status and Nightlights in 2011",
  label = "tab:ntl_2011",
  
  column.labels = c(
    "sector FE",
    "sector + infra FE",
    "sector FE",
    "sector + infra FE"
  ),
  
  covariate.labels = c(
    "Electrified by 2011"
  ),
  
  keep = c("elec11"),
  
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  keep.stat = c("n", "rsq"),
  
  add.lines = list(
    c("Sector FE", "Yes", "Yes", "Yes", "Yes"),
    c("Infrastructure Controls", "No", "Yes", "No", "Yes"),
    c("Clustered SE (Sector)", "Yes", "Yes", "Yes", "Yes"),
    c(
      "Usage (2020): mean (sd)",
      as.character(sprintf(
        "%.3f (%.3f)",
        mean(utility_2020$usage[utility_2020$village_id %in% expansion_join11$village_id],
             na.rm = TRUE),
        sd(utility_2020$usage[utility_2020$village_id %in% expansion_join11$village_id],
           na.rm = TRUE)
      )),
      "", "", ""
    )
  ),
  
  header = FALSE,
  font.size = "small",
  digits = 3
)

###usage-----


usage_2011_reg.1 = felm(
  ntl_2011 ~ elec11 + usage_2011 |
    sector_id |0 | sector_id,
  data = expansion_join11
)

summary(usage_2011_reg.1)


usage_2011_reg.2 = felm(
  ntl_2011 ~ elec11 +usage_2011 |
    sector_id +
    cell_office + health_center +
    primary_school + secondary_school +
    sector_district_office + industry + market +
    imidugudu |
    0 | sector_id,
  data = expansion_join11
)

summary(usage_2011_reg.2)




usage_2011_log_reg.1 = felm(
  ntl_2011_log ~ elec11 + usage_2011 |
    sector_id |0 | sector_id,
  data = expansion_join11
)

summary(usage_2011_log_reg.1)


usage_2011_log_reg.2 = felm(
  ntl_2011_log ~ elec11 +usage_2011 |
    sector_id +
    cell_office + health_center +
    primary_school + secondary_school +
    sector_district_office + industry + market +
    imidugudu |
    0 | sector_id,
  data = expansion_join11
)

summary(usage_2011_log_reg.2)

tex_file_ntl_2011_usage <- file.path(
  output_path, "regressions",
  "elec11_usage_ntl_2011.tex"
)

reg_ntl_2011_usage <- list(
  usage_2011_reg.1,
  usage_2011_reg.2,
  usage_2011_log_reg.1,
  usage_2011_log_reg.2
)


stargazer(
  reg_ntl_2011_usage,
  type = "latex",
  out = tex_file_ntl_2011_usage,
  title = "Electrification, Electricity Usage, and Nightlights in 2011",
  label = "tab:ntl_2011_usage",
  
  column.labels = c(
    "sector FE",
    "sector + infra FE",
    "sector FE",
    "sector + infra FE"
  ),
  
  covariate.labels = c(
    "Electrified by 2011",
    "Electricity Usage (2011)"
  ),
  
  keep = c("elec11", "usage_2011"),
  
  digits = 3,   
  
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  keep.stat = c("n", "rsq"),
  
  
  add.lines = list(
    c("Sector FE", "Yes", "Yes", "Yes", "Yes"),
    c("Infrastructure Controls", "No", "Yes", "No", "Yes"),
    c("Clustered SE (Sector)", "Yes", "Yes", "Yes", "Yes"),
    c(
      "Usage (2020): mean (sd)",
      as.character(sprintf(
        "%.3f (%.3f)",
        mean(utility_2020$usage[utility_2020$village_id %in% expansion_join11$village_id],
             na.rm = TRUE),
        sd(utility_2020$usage[utility_2020$village_id %in% expansion_join11$village_id],
           na.rm = TRUE)
      )),
      "", "", ""
    )
  ),
  
  header = FALSE,
  font.size = "small")



#Cell-fe=====


### NTL -----

ntl_2011_reg.1 = felm(
  ntl_2011 ~ elec11 |
    cell_id | 0 | sector_id,
  data = expansion_join11
)

ntl_2011_reg.2 = felm(
  ntl_2011 ~ elec11 |
    cell_id +
    cell_office + health_center +
    primary_school + secondary_school +
    sector_district_office + industry + market +
    imidugudu |
    0 | sector_id,
  data = expansion_join11
)

ntl_2011_log_reg.1 = felm(
  ntl_2011_log ~ elec11 |
    cell_id | 0 | sector_id,
  data = expansion_join11
)

ntl_2011_log_reg.2 = felm(
  ntl_2011_log ~ elec11 |
    cell_id +
    cell_office + health_center +
    primary_school + secondary_school +
    sector_district_office + industry + market +
    imidugudu |
    0 | sector_id,
  data = expansion_join11
)



reg_ntl_2011 <- list(
  ntl_2011_reg.1,
  ntl_2011_reg.2,
  ntl_2011_log_reg.1,
  ntl_2011_log_reg.2
)

tex_file_ntl_2011 <- file.path(
  output_path, "regressions",
  "elec11_ntl_2011_cell.tex"
)

stargazer(
  reg_ntl_2011,
  type = "latex",
  out = tex_file_ntl_2011,
  title = "Electrification Status and Nightlights in 2011",
  label = "tab:ntl_2011",
  
  column.labels = c(
    "cell FE",
    "cell + infra FE",
    "cell FE",
    "cell + infra FE"
  ),
  
  covariate.labels = c("Electrified by 2011"),
  keep = c("elec11"),
  
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  keep.stat = c("n", "rsq"),
  
  add.lines = list(
    c("Cell FE", "Yes", "Yes", "Yes", "Yes"),
    c("Infrastructure Controls", "No", "Yes", "No", "Yes"),
    c("Clustered SE (Sector)", "Yes", "Yes", "Yes", "Yes"),
    c(
      "Usage (2020): mean (sd)",
      sprintf(
        "%.3f (%.3f)",
        mean(utility_2020$usage[
          utility_2020$village_id %in% expansion_join11$village_id
        ], na.rm = TRUE),
        sd(utility_2020$usage[
          utility_2020$village_id %in% expansion_join11$village_id
        ], na.rm = TRUE)
      ),
      "", "", ""
    )
  ),
  
  header = FALSE,
  font.size = "small",
  digits = 3
)


##Usage----

usage_2011_reg.1 = felm(
  ntl_2011 ~ elec11 + usage_2011 |
    cell_id | 0 | sector_id,
  data = expansion_join11
)

usage_2011_reg.2 = felm(
  ntl_2011 ~ elec11 + usage_2011 |
    cell_id +
    cell_office + health_center +
    primary_school + secondary_school +
    sector_district_office + industry + market +
    imidugudu |
    0 | sector_id,
  data = expansion_join11
)

usage_2011_log_reg.1 = felm(
  ntl_2011_log ~ elec11 + usage_2011 |
    cell_id | 0 | sector_id,
  data = expansion_join11
)

usage_2011_log_reg.2 = felm(
  ntl_2011_log ~ elec11 + usage_2011 |
    cell_id +
    cell_office + health_center +
    primary_school + secondary_school +
    sector_district_office + industry + market +
    imidugudu |
    0 | sector_id,
  data = expansion_join11
)


tex_file_ntl_2011_usage <- file.path(
  output_path, "regressions",
  "elec11_usage_ntl_2011_cell.tex"
)

reg_ntl_2011_usage <- list(
  usage_2011_reg.1,
  usage_2011_reg.2,
  usage_2011_log_reg.1,
  usage_2011_log_reg.2
)

stargazer(
  reg_ntl_2011_usage,
  type = "latex",
  out = tex_file_ntl_2011_usage,
  title = "Electrification, Electricity Usage, and Nightlights in 2011",
  label = "tab:ntl_2011_usage",
  
  column.labels = c(
    "cell FE",
    "cell + infra FE",
    "cell FE",
    "cell + infra FE"
  ),
  
  covariate.labels = c(
    "Electrified by 2011",
    "Electricity Usage (2011)"
  ),
  
  keep = c("elec11", "usage_2011"),
  digits = 3,
  
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  keep.stat = c("n", "rsq"),
  
  add.lines = list(
    c("Cell FE", "Yes", "Yes", "Yes", "Yes"),
    c("Infrastructure Controls", "No", "Yes", "No", "Yes"),
    c("Clustered SE (Sector)", "Yes", "Yes", "Yes", "Yes"),
    c(
      "Usage (2020): mean (sd)",
      sprintf(
        "%.3f (%.3f)",
        mean(utility_2020$usage[
          utility_2020$village_id %in% expansion_join11$village_id
        ], na.rm = TRUE),
        sd(utility_2020$usage[
          utility_2020$village_id %in% expansion_join11$village_id
        ], na.rm = TRUE)
      ),
      "", "", ""
    )
  ),
  
  header = FALSE,
  font.size = "small"
)




#Elec12-14---------

#1. Sample restriction----

expansion_join <- read_xlsx(path = file.path(output_path, "expansion_join.xlsx"))

expansion_join_drop12_14 <- expansion_join%>% 
  filter(electrified_year %in% c("2012", "2013", "2014") |electrified_year == "9999" ) %>% 
  mutate(
    `elec12_14` = ifelse(electrified_year %in% c("2012", "2013", "2014"), 1, 0)
  ) %>% 
  anti_join(earp_existing_mv, by = c("village_id" = "Village_ID")) %>% 
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


village_ntl_all <- read_xlsx(path = file.path(path = file.path(historical_data_path,"Nightlight", "data", "LRCC-DVNL data", "village_ntl(2010-2022).xlsx")))


ntl_long <- village_ntl_all %>%
  pivot_longer(
    cols = starts_with("ntl_"),
    names_to = "year",
    values_to = "ntl"
  ) %>%
  mutate(
    year = as.integer(gsub("ntl_", "", year))
  ) %>%
  filter(year %in% c(2011, 2014, 2017, 2020))  %>% 
  mutate(
    ntl = ifelse(is.na(ntl), 0, ntl),
    ntl = ifelse(ntl >= 5, 5, ntl)
  )


#2. DID-------

elec12_14_controls <- expansion_join_drop12_14 %>%
  select(
    village_id,
    elec12_14,
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

ntl_did <- elec12_14_controls %>%
  left_join(ntl_long, by = "village_id") %>%
  complete(
    village_id,
    year = c(2011, 2014, 2017, 2020),
    fill = list(ntl = 0)
  ) %>%
  group_by(village_id) %>%
  fill(
    elec12_14,
    cell_id,
    sector_id,
    cell_office,
    health_center,
    primary_school,
    secondary_school,
    sector_district_office,
    industry,
    market,
    imidugudu,
    .direction = "downup"
  ) %>%
  ungroup() %>% 
  mutate(
    ntl = ifelse(is.na(ntl), 0 , ntl),
    # Fixed effects
    sector_year = paste0(sector_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    imidugudu_year = paste0(imidugudu, "_", year),
    
    # Event dummies
    p_1_2011 = as.integer(year == 2011),
    p0_2014  = as.integer(year == 2014),
    p1_2017  = as.integer(year == 2017),
    p2_2020  = as.integer(year == 2020)
  ) %>% 
    mutate(log1_ntl = log1p(ntl))



#Cross sectional regression-------


# ----------------------------------
# A. Cross-sectional correlation data
# ----------------------------------

# Keep post-electrification years only
ntl_cs <- ntl_did %>%
  filter(year %in% c(2017, 2020)) %>%
  left_join(
    expansion_join_drop12_14 %>%
      select(
        village_id,
        elec12_14,
        any_residential_consumer,
        residential_consumer
      ),
    by = "village_id"
  ) %>%
  mutate(
    log1_usage = log1p(residential_consumer),
    any_elec   = any_residential_consumer
  )


# 3. Regressions: LEVEL nightlights-------

ntl_regs_level <- list(
  
  spec1 = felm(
    ntl ~
      p0_2014 * elec12_14 +
      p1_2017 * elec12_14 +
      p2_2020 * elec12_14 |
      village_id + sector_year |
      0 | sector_id,
    data = ntl_did
  ),
  
  spec2 = felm(
    ntl ~
      p0_2014 * elec12_14 +
      p1_2017 * elec12_14 +
      p2_2020 * elec12_14 |
      village_id + sector_year +
      cell_office_year + health_center_year +
      primary_school_year + secondary_school_year +
      sector_district_office_year + industry_year + market_year +
      imidugudu_year |
      0 | sector_id,
    data = ntl_did
  )
)

# 4. Regressions: LOG nightlights---------

ntl_regs_log <- list(
  
  spec1 = felm(
    log1_ntl ~
      p0_2014 * elec12_14 +
      p1_2017 * elec12_14 +
      p2_2020 * elec12_14 |
      village_id + sector_year |
      0 | sector_id,
    data = ntl_did
  ),

  spec2 = felm(
    log1_ntl ~
      p0_2014 * elec12_14 +
      p1_2017 * elec12_14 +
      p2_2020 * elec12_14 |
      village_id + sector_year +
      cell_office_year + health_center_year +
      primary_school_year + secondary_school_year +
      sector_district_office_year + industry_year + market_year +
      imidugudu_year |
      0 | sector_id,
    data = ntl_did
  )
)


# 5.Stargazer: Nightlights (Levels)--------


tex_file_ntl <- file.path(
  output_path, "regressions", 
  "elec12_14_ntl.tex"
)

reg_ntl <- list(
  ntl_regs_level$spec1,
  ntl_regs_level$spec2,
  ntl_regs_log$spec1,
  ntl_regs_log$spec2
)

stargazer(
  reg_ntl,
  type = "latex",
  out = tex_file_ntl,
  title = "Regression Results: Electrification and Nightlights",
  label = "tab:ntl_12_14",
  column.labels = c(
    "sector FE",
    "sector + infra FE",
    "sector FE",
    "sector + infra FE"
  ),
  covariate.labels = c(
    "elec12–14 × 2014",
    "elec12–14 × 2017",
    "elec12–14 × 2020"
  ),
  keep = c(
    "p0_2014:elec12_14",
    "elec12_14:p1_2017",
    "elec12_14:p2_2020"
  ),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  keep.stat = c("n", "rsq"),
  
  add.lines = list(
    c("Sector FE", "Yes", "Yes", "Yes", "Yes"),
    c("Infrastructure Controls", "No", "Yes", "No", "Yes"),
    c("Clustered SE (Sector)", "Yes", "Yes", "Yes", "Yes"),
    c(
      "Usage (2020): mean (sd)",
      as.character(sprintf(
        "%.3f (%.3f)",
        mean(utility_2020$usage[utility_2020$village_id %in% expansion_join_drop12_14$village_id],
             na.rm = TRUE),
        sd(utility_2020$usage[utility_2020$village_id %in% expansion_join_drop12_14$village_id],
           na.rm = TRUE)
      )),
      "", "", ""
    )
  ),
  header = FALSE,
  font.size = "small",
  digits = 3
)

# 2014 Results-------
village_ntl_all <- read_xlsx(path = file.path(path = file.path(historical_data_path, "Nightlight", "data", "LRCC-DVNL data", "village_ntl(2010-2022).xlsx")))


utility_2014 <- utility_long %>% 
  filter(year == 2014)

# Prepare NTL 2014


ntl_2014 <- village_ntl_all %>%
  pivot_longer(
    cols = starts_with("ntl_"),
    names_to = "year",
    values_to = "ntl"
  ) %>%
  mutate(year = as.integer(gsub("ntl_", "", year))) %>%
  filter(year == 2014) %>% 
  select(village_id, ntl)


# Merge + construct variables


expansion_join14 <- expansion_join %>% 
  filter(!District %in% c(
    "Ngororero", "Nyabihu", "Nyamasheke", "Rubavu"
  )) %>% 
  mutate(
    elec14 = ifelse(electrified_year <= 2014 | earp_mv == 1, 1, 0)
  ) %>% 
  left_join(utility_2014, by = "village_id") %>% 
  left_join(ntl_2014, by = "village_id") %>% 
  mutate(
    usage = ifelse(is.na(usage), 0, usage),
    usage = ifelse(elec14 == 0, 0, usage),
    ntl   = ifelse(is.na(ntl), 0, ntl),
    ntl   = ifelse(ntl >= 5, 5, ntl),
    usage = usage / (100 * 365)
  ) %>% 
  rename(
    usage_2014 = usage,
    ntl_2014   = ntl
  ) %>% 
  mutate(
    ntl_2014_log = log1p(ntl_2014)
  )


# REGRESSIONS: NTL ONLY


ntl_2014_reg.1 <- felm(
  ntl_2014 ~ elec14 |
    sector_id | 0 | sector_id,
  data = expansion_join14
)

ntl_2014_reg.2 <- felm(
  ntl_2014 ~ elec14 |
    sector_id +
    cell_office + health_center +
    primary_school + secondary_school +
    sector_district_office + industry +
    market + imidugudu |
    0 | sector_id,
  data = expansion_join14
)

ntl_2014_log_reg.1 <- felm(
  ntl_2014_log ~ elec14 |
    sector_id | 0 | sector_id,
  data = expansion_join14
)

ntl_2014_log_reg.2 <- felm(
  ntl_2014_log ~ elec14 |
    sector_id +
    cell_office + health_center +
    primary_school + secondary_school +
    sector_district_office + industry +
    market + imidugudu |
    0 | sector_id,
  data = expansion_join14
)

### Stargazer: NTL

tex_file_ntl_2014 <- file.path(
  output_path, "regressions",
  "elec14_ntl_2014.tex"
)

reg_ntl_2014 <- list(
  ntl_2014_reg.1,
  ntl_2014_reg.2,
  ntl_2014_log_reg.1,
  ntl_2014_log_reg.2
)

stargazer(
  reg_ntl_2014,
  type = "latex",
  out = tex_file_ntl_2014,
  title = "Electrification Status and Nightlights in 2014",
  label = "tab:ntl_2014",
  column.labels = c(
    "Sector FE",
    "Sector + Infra FE",
    "Sector FE",
    "Sector + Infra FE"
  ),
  covariate.labels = c("Electrified by 2014"),
  keep = c("elec14"),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  keep.stat = c("n", "rsq"),
  add.lines = list(
    c("Sector FE", "Yes", "Yes", "Yes", "Yes"),
    c("Infrastructure Controls", "No", "Yes", "No", "Yes"),
    c("Clustered SE (Sector)", "Yes", "Yes", "Yes", "Yes"),
    c(
      "Usage (2020): mean (sd)",
      sprintf(
        "%.3f (%.3f)",
        mean(
          utility_2020$usage[
            utility_2020$village_id %in% expansion_join14$village_id
          ],
          na.rm = TRUE
        ),
        sd(
          utility_2020$usage[
            utility_2020$village_id %in% expansion_join14$village_id
          ],
          na.rm = TRUE
        )
      ),
      "", "", ""
    )
  ),
  header = FALSE,
  font.size = "small",
  digits = 3
)

# REGRESSIONS: USAGE + NTL

usage_2014_reg.1 <- felm(
  ntl_2014 ~ elec14 + usage_2014 |
    sector_id | 0 | sector_id,
  data = expansion_join14
)

usage_2014_reg.2 <- felm(
  ntl_2014 ~ elec14 + usage_2014 |
    sector_id +
    cell_office + health_center +
    primary_school + secondary_school +
    sector_district_office + industry +
    market + imidugudu |
    0 | sector_id,
  data = expansion_join14
)

usage_2014_log_reg.1 <- felm(
  ntl_2014_log ~ elec14 + usage_2014 |
    sector_id | 0 | sector_id,
  data = expansion_join14
)

usage_2014_log_reg.2 <- felm(
  ntl_2014_log ~ elec14 + usage_2014 |
    sector_id +
    cell_office + health_center +
    primary_school + secondary_school +
    sector_district_office + industry +
    market + imidugudu |
    0 | sector_id,
  data = expansion_join14
)


# Stargazer: Usage


tex_file_ntl_2014_usage <- file.path(
  output_path, "regressions",
  "elec14_usage_ntl_2014.tex"
)

reg_ntl_2014_usage <- list(
  usage_2014_reg.1,
  usage_2014_reg.2,
  usage_2014_log_reg.1,
  usage_2014_log_reg.2
)

stargazer(
  reg_ntl_2014_usage,
  type = "latex",
  out = tex_file_ntl_2014_usage,
  title = "Electrification, Electricity Usage, and Nightlights in 2014",
  label = "tab:ntl_2014_usage",
  column.labels = c(
    "Sector FE",
    "Sector + Infra FE",
    "Sector FE",
    "Sector + Infra FE"
  ),
  covariate.labels = c(
    "Electrified by 2014",
    "Electricity Usage (2014)"
  ),
  keep = c("elec14", "usage_2014"),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  keep.stat = c("n", "rsq"),
  add.lines = list(
    c("Sector FE", "Yes", "Yes", "Yes", "Yes"),
    c("Infrastructure Controls", "No", "Yes", "No", "Yes"),
    c("Clustered SE (Sector)", "Yes", "Yes", "Yes", "Yes"),
    c(
      "Usage (2020): mean (sd)",
      sprintf(
        "%.3f (%.3f)",
        mean(
          utility_2020$usage[
            utility_2020$village_id %in% expansion_join14$village_id
          ],
          na.rm = TRUE
        ),
        sd(
          utility_2020$usage[
            utility_2020$village_id %in% expansion_join14$village_id
          ],
          na.rm = TRUE
        )
      ),
      "", "", ""
    )
  ),
  header = FALSE,
  font.size = "small",
  digits = 3
)




#Cell-fe=====


### NTL -----

ntl_2014_reg.1 = felm(
  ntl_2014 ~ elec14 |
    cell_id | 0 | sector_id,
  data = expansion_join14
)

ntl_2014_reg.2 = felm(
  ntl_2014 ~ elec14 |
    cell_id +
    cell_office + health_center +
    primary_school + secondary_school +
    sector_district_office + industry + market +
    imidugudu |
    0 | sector_id,
  data = expansion_join14
)

ntl_2014_log_reg.1 = felm(
  ntl_2014_log ~ elec14 |
    cell_id | 0 | sector_id,
  data = expansion_join14
)

ntl_2014_log_reg.2 = felm(
  ntl_2014_log ~ elec14 |
    cell_id +
    cell_office + health_center +
    primary_school + secondary_school +
    sector_district_office + industry + market +
    imidugudu |
    0 | sector_id,
  data = expansion_join14
)



reg_ntl_2014 <- list(
  ntl_2014_reg.1,
  ntl_2014_reg.2,
  ntl_2014_log_reg.1,
  ntl_2014_log_reg.2
)

tex_file_ntl_2014 <- file.path(
  output_path, "regressions",
  "elec14_ntl_2014_cell.tex"
)

stargazer(
  reg_ntl_2014,
  type = "latex",
  out = tex_file_ntl_2014,
  title = "Electrification Status and Nightlights in 2014",
  label = "tab:ntl_2014",
  
  column.labels = c(
    "cell FE",
    "cell + infra FE",
    "cell FE",
    "cell + infra FE"
  ),
  
  covariate.labels = c("Electrified by 2014"),
  keep = c("elec14"),
  
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  keep.stat = c("n", "rsq"),
  
  add.lines = list(
    c("Cell FE", "Yes", "Yes", "Yes", "Yes"),
    c("Infrastructure Controls", "No", "Yes", "No", "Yes"),
    c("Clustered SE (Sector)", "Yes", "Yes", "Yes", "Yes"),
    c(
      "Usage (2020): mean (sd)",
      sprintf(
        "%.3f (%.3f)",
        mean(utility_2020$usage[
          utility_2020$village_id %in% expansion_join14$village_id
        ], na.rm = TRUE),
        sd(utility_2020$usage[
          utility_2020$village_id %in% expansion_join14$village_id
        ], na.rm = TRUE)
      ),
      "", "", ""
    )
  ),
  
  header = FALSE,
  font.size = "small",
  digits = 3
)


##Usage----

usage_2014_reg.1 = felm(
  ntl_2014 ~ elec14 + usage_2014 |
    cell_id | 0 | sector_id,
  data = expansion_join14
)

usage_2014_reg.2 = felm(
  ntl_2014 ~ elec14 + usage_2014 |
    cell_id +
    cell_office + health_center +
    primary_school + secondary_school +
    sector_district_office + industry + market +
    imidugudu |
    0 | sector_id,
  data = expansion_join14
)

usage_2014_log_reg.1 = felm(
  ntl_2014_log ~ elec14 + usage_2014 |
    cell_id | 0 | sector_id,
  data = expansion_join14
)

usage_2014_log_reg.2 = felm(
  ntl_2014_log ~ elec14 + usage_2014 |
    cell_id +
    cell_office + health_center +
    primary_school + secondary_school +
    sector_district_office + industry + market +
    imidugudu |
    0 | sector_id,
  data = expansion_join14
)


tex_file_ntl_2014_usage <- file.path(
  output_path, "regressions",
  "elec14_usage_ntl_2014_cell.tex"
)

reg_ntl_2014_usage <- list(
  usage_2014_reg.1,
  usage_2014_reg.2,
  usage_2014_log_reg.1,
  usage_2014_log_reg.2
)

stargazer(
  reg_ntl_2014_usage,
  type = "latex",
  out = tex_file_ntl_2014_usage,
  title = "Electrification, Electricity Usage, and Nightlights in 2014",
  label = "tab:ntl_2014_usage",
  
  column.labels = c(
    "cell FE",
    "cell + infra FE",
    "cell FE",
    "cell + infra FE"
  ),
  
  covariate.labels = c(
    "Electrified by 2014",
    "Electricity Usage (2014)"
  ),
  
  keep = c("elec14", "usage_2014"),
  digits = 3,
  
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  keep.stat = c("n", "rsq"),
  
  add.lines = list(
    c("Cell FE", "Yes", "Yes", "Yes", "Yes"),
    c("Infrastructure Controls", "No", "Yes", "No", "Yes"),
    c("Clustered SE (Sector)", "Yes", "Yes", "Yes", "Yes"),
    c(
      "Usage (2020): mean (sd)",
      sprintf(
        "%.3f (%.3f)",
        mean(utility_2020$usage[
          utility_2020$village_id %in% expansion_join14$village_id
        ], na.rm = TRUE),
        sd(utility_2020$usage[
          utility_2020$village_id %in% expansion_join14$village_id
        ], na.rm = TRUE)
      ),
      "", "", ""
    )
  ),
  
  header = FALSE,
  font.size = "small"
)






#Elec15-17---------

#1. Sample restriction----

expansion_join <- read_xlsx(path = file.path(output_path, "expansion_join.xlsx"))

expansion_join_drop15_17 <- expansion_join%>% 
  filter(electrified_year %in% c("2015", "2016", "2017") |electrified_year == "9999" ) %>% 
  mutate(
    `elec15_17` = ifelse(electrified_year %in% c("2015", "2016", "2017"), 1, 0)
  ) %>% 
  anti_join(earp_existing_mv, by = c("village_id" = "Village_ID")) %>% 
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


village_ntl_all <- read_xlsx(path = file.path(path = file.path(historical_data_path, "Nightlight", "data", "LRCC-DVNL data", "village_ntl(2010-2022).xlsx")))


ntl_long <- village_ntl_all %>%
  pivot_longer(
    cols = starts_with("ntl_"),
    names_to = "year",
    values_to = "ntl"
  ) %>%
  mutate(
    year = as.integer(gsub("ntl_", "", year))
  ) %>%
  filter(year %in% c(2011, 2014, 2017, 2020)) %>% 
  mutate(
    ntl = ifelse(is.na(ntl), 0, ntl),
    ntl = ifelse(ntl >= 5, 5, ntl)
  )


#2. DID-------

elec15_17_controls <- expansion_join_drop15_17 %>%
  select(
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

ntl_did <- elec15_17_controls %>%
  left_join(ntl_long, by = "village_id") %>%
  complete(
    village_id,
    year = c(2011, 2014, 2017, 2020),
    fill = list(ntl = 0)
  ) %>%
  group_by(village_id) %>%
  fill(
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
    imidugudu,
    .direction = "downup"
  ) %>%
  ungroup() %>% 
  mutate(
    ntl = ifelse(is.na(ntl), 0 , ntl),
    # Fixed effects
    sector_year = paste0(sector_id, "_", year),
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
         year = factor(year, levels = c("2014", "2011", "2017", "2020"))
  )



# 3. Regressions: LEVEL nightlights-------

ntl_regs_level <- list(
  
  spec1 = felm(
    ntl ~
      year * elec15_17|
      village_id + sector_year |
      0 | sector_id,
    data = ntl_did
  ),
  
  spec2 = felm(
    ntl ~
      year * elec15_17 |
      village_id + sector_year +
      cell_office_year + health_center_year +
      primary_school_year + secondary_school_year +
      sector_district_office_year + industry_year + market_year +
      imidugudu_year |
      0 | sector_id,
    data = ntl_did
  )
)

# 4. Regressions: LOG nightlights---------

ntl_regs_log <- list(
  
  spec1 = felm(
    log1_ntl ~
      year * elec15_17  |
      village_id + sector_year |
      0 | sector_id,
    data = ntl_did
  ),
  
  spec2 = felm(
    log1_ntl ~
      year * elec15_17 |
      village_id + sector_year +
      cell_office_year + health_center_year +
      primary_school_year + secondary_school_year +
      sector_district_office_year + industry_year + market_year +
      imidugudu_year |
      0 | sector_id,
    data = ntl_did
  )
)


# 5.Stargazer: Nightlights--------

summary(ntl_regs_level$spec2)


tex_file_ntl <- file.path(
  output_path, "regressions", 
  "elec15_17_ntl.tex"
)

reg_ntl <- list(
  ntl_regs_level$spec1,
  ntl_regs_level$spec2,
  ntl_regs_log$spec1,
  ntl_regs_log$spec2
  
)

stargazer(
  reg_ntl,
  type = "latex",
  out = tex_file_ntl,
  title = "Regression Results: Electrification and Nightlights",
  label = "tab:ntl_level",
  column.labels = c(
    "sector FE",
    "sector + infra FE",
    "sector FE",
    "sector + infra FE"
  ),
  covariate.labels = c(
    "elec15-17 × 2011",
    "elec15-17 × 2017",
    "elec15-17 × 2020"
  ),
  keep = c(
    "year2011:elec15_17",
    "year2017:elec15_17",
    "year2020:elec15_17"
  ),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  keep.stat = c("n", "rsq"),
  
  add.lines = list(
    c("Sector FE", "Yes", "Yes", "Yes", "Yes"),
    c("Infrastructure Controls", "No", "Yes", "No", "Yes"),
    c("Clustered SE (Sector)", "Yes", "Yes", "Yes", "Yes"),
    c(
      "Usage (2020): mean (sd)",
      as.character(sprintf(
        "%.3f (%.3f)",
        mean(utility_2020$usage[utility_2020$village_id %in% expansion_join_drop15_17$village_id],
             na.rm = TRUE),
        sd(utility_2020$usage[utility_2020$village_id %in% expansion_join_drop15_17$village_id],
           na.rm = TRUE)
      )),
      "", "", ""
    )
  ),
  header = FALSE,
  font.size = "small",
  digits = 3
)





#EARP---------

#1. Sample restriction----

expansion_join <- read_xlsx(path = file.path(output_path, "expansion_join.xlsx"))

expansion_join_drop13 <- expansion_join%>% 
  mutate(
    `EARP` = ifelse(earp_lv == 1 | earp_mv == 1, 1, 0)
  ) %>% 
  filter(electrified_year > 2013) %>% 
  anti_join(earp_existing_mv, by = c("village_id" = "Village_ID")) %>% 
  filter(! District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu"))%>% 
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0),
    consumer = residential_consumer + non_residential_consumer,
    `earp`  = ifelse(earp_lv == 1 | earp_mv == 1, 1, 0)
    
  ) 


village_ntl_all <- read_xlsx(path = file.path(path = file.path(historical_data_path, "Nightlight", "data", "LRCC-DVNL data", "village_ntl(2010-2022).xlsx")))


ntl_long <- village_ntl_all %>%
  pivot_longer(
    cols = starts_with("ntl_"),
    names_to = "year",
    values_to = "ntl"
  ) %>%
  mutate(
    year = as.integer(gsub("ntl_", "", year))
  ) %>%
  filter(year %in% c(2011, 2014, 2017, 2020)) %>% 
  mutate(
    ntl = ifelse(is.na(ntl), 0, ntl),
    ntl = ifelse(ntl >= 5, 5, ntl)
  )


#2. DID-------

elecearp_controls <- expansion_join_drop13 %>%
  select(
    village_id,
    earp,
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

ntl_did <- elecearp_controls %>%
  left_join(ntl_long, by = "village_id") %>%
  complete(
    village_id,
    year = c(2011, 2014, 2017, 2020),
    fill = list(ntl = 0)
  ) %>%
  group_by(village_id) %>%
  fill(
    earp,
    cell_id,
    sector_id,
    cell_office,
    health_center,
    primary_school,
    secondary_school,
    sector_district_office,
    industry,
    market,
    imidugudu,
    .direction = "downup"
  ) %>%
  ungroup() %>% 
  mutate(
    ntl = ifelse(is.na(ntl), 0 , ntl),
    # Fixed effects
    sector_year = paste0(sector_id, "_", year),
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
  mutate(year = as.factor(year))


# 3. Regressions: LEVEL nightlights-------

ntl_regs_level <- list(
  
  spec1 = felm(
    ntl ~
      year * earp|
      village_id + sector_year |
      0 | sector_id,
    data = ntl_did
  ),
  
  spec2 = felm(
    ntl ~
      year * earp |
      village_id + sector_year +
      cell_office_year + health_center_year +
      primary_school_year + secondary_school_year +
      sector_district_office_year + industry_year + market_year +
      imidugudu_year |
      0 | sector_id,
    data = ntl_did
  )
)

# 4. Regressions: LOG nightlights---------

ntl_regs_log <- list(
  
  spec1 = felm(
    log1_ntl ~
      year * earp  |
      village_id + sector_year |
      0 | sector_id,
    data = ntl_did
  ),
  
  spec2 = felm(
    log1_ntl ~
      year * earp |
      village_id + sector_year +
      cell_office_year + health_center_year +
      primary_school_year + secondary_school_year +
      sector_district_office_year + industry_year + market_year +
      imidugudu_year |
      0 | sector_id,
    data = ntl_did
  )
)


# 5.Stargazer: Nightlights (Levels)--------

summary(ntl_regs_level$spec2)


tex_file_ntl <- file.path(
  output_path, "regressions", 
  "earp_ntl.tex"
)

reg_ntl <- list(
  ntl_regs_level$spec1,
  ntl_regs_level$spec2,
  ntl_regs_log$spec1,
  ntl_regs_log$spec2
)

stargazer(
  reg_ntl,
  type = "latex",
  out = tex_file_ntl,
  title = "Regression Results: EARP and Nightlights",
  label = "tab:ntl_level",
  column.labels = c(
    "sector FE",
    "sector + infra FE",
    "sector FE",
    "sector + infra FE"
  ),
  covariate.labels = c(
    "earp × 2014",
    "earp × 2017",
    "earp × 2020"
  ),
  keep = c(
    "year2014:earp",
    "year2017:earp",
    "year2020:earp"
  ),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  keep.stat = c("n", "rsq"),
  
  add.lines = list(
    c("Sector FE", "Yes", "Yes", "Yes", "Yes"),
    c("Infrastructure Controls", "No", "Yes", "No", "Yes"),
    c("Clustered SE (Sector)", "Yes", "Yes", "Yes", "Yes"),
    c(
      "Usage (2020): mean (sd)",
      as.character(sprintf(
        "%.3f (%.3f)",
        mean(utility_2020$usage[utility_2020$village_id %in% expansion_join_drop13$village_id],
             na.rm = TRUE),
        sd(utility_2020$usage[utility_2020$village_id %in% expansion_join_drop13$village_id],
           na.rm = TRUE)
      )),
      "", "", ""
    )
  ),
  header = FALSE,
  font.size = "small",
  digits = 3
)





