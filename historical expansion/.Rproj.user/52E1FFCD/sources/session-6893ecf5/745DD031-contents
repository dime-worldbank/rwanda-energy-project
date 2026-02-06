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


expansion_join <- read_xlsx(path = file.path(output_path, "expansion_join.xlsx"))

utility_2020 <- utility_long %>% 
  filter(year == 2020) %>% 
  mutate(usage = usage/(100 *365))


village_ntl_all <- read_xlsx(path = file.path(path = file.path(historical_data_path, "Nightlight", "data", "viirs_2014-2023(monthly&yearly).xlsx"))) %>% 
  rename(village_id = Village_ID)



# 2014 Results-------


utility_2014 <- utility_long %>% 
  filter(year == 2014)

# Prepare NTL 2014


ntl_2014 <- village_ntl_all %>%
  pivot_longer(
    cols = starts_with("20"),
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
  "elec14_ntl_2014_viirs.tex"
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
  "elec14_usage_ntl_2014_viirs.tex"
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
  "elec14_ntl_2014_cell_viirs.tex"
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
  "elec14_usage_ntl_2014_cell_viirs.tex"
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




village_ntl_all <- read_xlsx(path = file.path(path = file.path(historical_data_path, "Nightlight", "data", "viirs_2014-2023(monthly&yearly).xlsx"))) %>% 
  rename(village_id = Village_ID)



ntl_long <- village_ntl_all %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "year",
    values_to = "ntl"
  ) %>%
  mutate(
    year = as.integer(gsub("ntl_", "", year))
  ) %>%
  filter(year %in% c( 2014, 2017, 2020)) %>% 
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
    year = c( 2014, 2017, 2020),
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
         year = factor(year, levels = c("2014",  "2017", "2020"))
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
  "elec15_17_ntl_viirs.tex"
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
  
    "elec15-17 × 2017",
    "elec15-17 × 2020"
  ),
  keep = c(
    
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




