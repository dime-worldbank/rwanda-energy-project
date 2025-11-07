

elec15_17_bt_dta <- elec15_17_did_all %>%
  filter(year == 2014) %>%
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0)
  ) %>% 
  select(num_establishment, total_employee, elec15_17, cell_id, cell_office, health_center,
         primary_school, secondary_school, market, industry, sector_district_office, imidugudu, log1_residential_consumer, log1_non_residential_consumer) 



elec15_17_lm <- felm(elec15_17 ~ cell_office + health_center+ primary_school+ secondary_school+ 
                       market+ industry+ sector_district_office + imidugudu +
                       log1_residential_consumer + log1_non_residential_consumer|cell_id| 0 |cell_id, data = elec15_17_bt_dta )
summary(elec15_17_lm)





elec12_14_bt_dta <- elec12_14_did_all %>%
  filter(year == 2011) %>%
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0)
  ) %>% 
  select(num_establishment, total_employee, elec12_14, cell_id, cell_office, health_center,
         primary_school, secondary_school, market, industry, sector_district_office, imidugudu, log1_residential_consumer, log1_non_residential_consumer) 



elec12_14_lm <- felm(elec12_14 ~ cell_office + health_center+ primary_school+ secondary_school + 
                       market+ industry+ sector_district_office + imidugudu +
                       log1_residential_consumer + log1_non_residential_consumer|cell_id | 0 |cell_id, data = elec12_14_bt_dta )
summary(elec12_14_lm)



earp_bt_dta <- earp_did_all %>%
  filter(year == 2011) %>%
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0)
  ) %>% 
  select(num_establishment, total_employee, EARP, cell_id, cell_office, health_center,
         primary_school, secondary_school, market, industry, sector_district_office, imidugudu, log1_residential_consumer, log1_non_residential_consumer) 



earp_lm <- felm(EARP ~ cell_office + health_center+ primary_school+ secondary_school+ 
                  market+ industry+ sector_district_office + imidugudu +
                  log1_residential_consumer + log1_non_residential_consumer|cell_id | 0 |cell_id, data = earp_bt_dta )
summary(earp_lm)




stargazer(
  earp_lm,
  elec12_14_lm,
  elec15_17_lm,
  type = "latex",
  title = "Infrastructure Correlates of Electrification (Balance Regressions)",
  label = "tab:infra_balance_regs",
  dep.var.labels = c("EARP", "elec12-14", "elec15-17"),
  covariate.labels = c("Cell Office", "Health Center", "Primary School", "Secondary School",
                       "Market", "Industry", "Sector District Office"),
  keep.stat = c("n", "rsq"),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  float.env = "table",
  header = FALSE,
  no.space = TRUE,
  align = TRUE,
  add.lines = list(c("Fixed Effects", "Cell ID", "Cell ID", "Cell ID")),  # <-- added line
  out = file.path(output_path, "regressions", "productive_user_regression.tex") # <-- save
)
