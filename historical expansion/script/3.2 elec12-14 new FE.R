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

output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Historical Expansion/outputs"
)

#Read file-----

#Sample restriction----

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
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0)
  )



lm_1 <- felm(elec12_14 ~  cell_office + health_center + primary_school + 
               secondary_school + sector_district_office + industry + market +
               imidugudu + log1_residential_consumer + log1_non_residential_consumer|cell_id |0| sector_id, 
             data = expansion_join_drop12_14)


summary(lm_1)

expansion_join_drop12_14 <- expansion_join_drop12_14 %>%
  mutate(pred_elec12_14 = fitted(lm_1))






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



elec12_14_join <- expansion_join_drop12_14%>% 
  select(village_id,elec12_14, cell_id, sector_id, district_id) 

elec12_14_did_isic<- left_join( elec12_14_join, ec_all, by = c("village_id"))


elec12_14_did_isic <- elec12_14_did_isic %>%
  complete(year, village_id, isic_level1,
           fill = list(num_establishment = 0, total_employee = 0)) %>% 
  filter(!is.na(year))   %>% 
  filter(!is.na(isic_level1)) %>% 
  select(year, village_id, num_establishment, total_employee, isic_level1)


elec12_14 <- expansion_join_drop12_14%>%
  select(village_id, elec12_14, earp_existing_mv, health_center, primary_school, cell_office, secondary_school, sector_district_office,
         industry, market, residential_consumer, non_residential_consumer, imidugudu) 


elec12_14_did_isic <- left_join(elec12_14_did_isic, elec12_14, by = c("village_id"))



elec12_14_did_isic <- elec12_14_did_isic %>% 
  mutate(
    cell_id    = substr(village_id, 1, 6),
    sector_id  = substr(village_id, 1, 4),
    district_id= substr(village_id, 1, 2)
  ) %>% 
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),

    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  ) %>%
  filter(!is.na(isic_level1)) %>%
  mutate(
    isic_level1 = as.character(isic_level1)  ) %>% 
  rename(
    isic = isic_level1
  )  





# Private sector data clean-------


join_drop12_14 <- expansion_join_drop12_14 %>% 
  select(`elec12_14`, pred_elec12_14,village_id, cell_id, sector_id, cell_office, health_center, primary_school, secondary_school, 
         sector_district_office, market, industry, residential_consumer,  non_residential_consumer, imidugudu)

elec12_14_p <- elec12_14_did_isic %>%
  mutate(private_sector = ifelse(isic %in% c(9,7,3,19), 1, 0)) %>%
  filter(private_sector == 1) %>%
  group_by(year, village_id) %>%
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE), .groups="drop") %>%
  mutate(total_employee = pmin(total_employee, 100)) %>% 
  left_join(join_drop12_14, by = c("village_id") )  %>%
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    imidugudu_year = paste0(imidugudu, "_", year), 
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
  ) %>% 
  mutate(
    log1_num_establishment = log(num_establishment + 1),
    log1_total_employee = log(total_employee + 1)
  )



#Regressions--------
##Levels---------

# 1. Run all regressions (4 specs × 2 outcomes)

run_reg_all <- function(df) {
  list(
    #  Establishments 
    spec1_est = felm(
      num_establishment ~ 
        p0_2014 * elec12_14 + 
        p1_2017 * elec12_14 + 
        p2_2020 * elec12_14 |
        village_id + cell_year | 
        0 | sector_id,
      data = df
    ),
    
    spec2_est = felm(
      num_establishment ~ 
        p0_2014 * elec12_14 + 
        p1_2017 * elec12_14 + 
        p2_2020 * elec12_14 |
        village_id + cell_year + 
        cell_office_year + health_center_year + 
        primary_school_year + secondary_school_year + 
        sector_district_office_year + industry_year + market_year | 
        0 | sector_id,
      data = df
    ),
    
    spec3_est = felm(
      num_establishment ~ 
        p0_2014 * elec12_14 + 
        p1_2017 * elec12_14 + 
        p2_2020 * elec12_14 |
        village_id + cell_year + 
        cell_office_year + health_center_year + 
        primary_school_year + secondary_school_year + 
        sector_district_office_year + industry_year + market_year + 
        imidugudu_year | 
        0 | sector_id,
      data = df
    ),
    
    spec4_est = felm(
      num_establishment ~ 
        p0_2014 * elec12_14 + 
        p1_2017 * elec12_14 + 
        p2_2020 * elec12_14 +
        p0_2014 * log1_residential_consumer + 
        p1_2017 * log1_residential_consumer + 
        p2_2020 * log1_residential_consumer +
        p0_2014 * log1_non_residential_consumer + 
        p1_2017 * log1_non_residential_consumer + 
        p2_2020 * log1_non_residential_consumer |
        village_id + cell_year + 
        cell_office_year + health_center_year + 
        primary_school_year + secondary_school_year + 
        sector_district_office_year + industry_year + market_year + 
        imidugudu_year | 
        0 | sector_id,
      data = df
    ),
    
    #Employees 
    spec1_emp = felm(
      total_employee ~ 
        p0_2014 * elec12_14 + 
        p1_2017 * elec12_14 + 
        p2_2020 * elec12_14 |
        village_id + cell_year | 
        0 | sector_id,
      data = df
    ),
    
    spec2_emp = felm(
      total_employee ~ 
        p0_2014 * elec12_14 + 
        p1_2017 * elec12_14 + 
        p2_2020 * elec12_14 |
        village_id + cell_year + 
        cell_office_year + health_center_year + 
        primary_school_year + secondary_school_year + 
        sector_district_office_year + industry_year + market_year | 
        0 | sector_id,
      data = df
    ),
    
    spec3_emp = felm(
      total_employee ~ 
        p0_2014 * elec12_14 + 
        p1_2017 * elec12_14 + 
        p2_2020 * elec12_14 |
        village_id + cell_year + 
        cell_office_year + health_center_year + 
        primary_school_year + secondary_school_year + 
        sector_district_office_year + industry_year + market_year + 
        imidugudu_year | 
        0 | sector_id,
      data = df
    ),
    
    spec4_emp = felm(
      total_employee ~ 
        p0_2014 * elec12_14 + 
        p1_2017 * elec12_14 + 
        p2_2020 * elec12_14 +
        p0_2014 * log1_residential_consumer + 
        p1_2017 * log1_residential_consumer + 
        p2_2020 * log1_residential_consumer +
        p0_2014 * log1_non_residential_consumer + 
        p1_2017 * log1_non_residential_consumer + 
        p2_2020 * log1_non_residential_consumer |
        village_id + cell_year + 
        cell_office_year + health_center_year + 
        primary_school_year + secondary_school_year + 
        sector_district_office_year + industry_year + market_year + 
        imidugudu_year | 
        0 | sector_id,
      data = df
    )
  )
}


# 2. Run regressions

regs_private_all <- run_reg_all(elec12_14_p) 




# 3. Export raw Stargazer table (8 columns)

keep_terms <- c(
  "p0_2014:elec12_14", "elec12_14:p1_2017", "elec12_14:p2_2020",
  "p0_2014:log1_residential_consumer", "p1_2017:log1_residential_consumer", "p2_2020:log1_residential_consumer",
  "p0_2014:log1_non_residential_consumer", "p1_2017:log1_non_residential_consumer", "p2_2020:log1_non_residential_consumer"
)

tex_file_priv_combined <- file.path(output_path, "regressions", "raw", "elec12_14_Private_combined.tex")

reg <- list(
  regs_private_all$spec1_est,
  regs_private_all$spec1_emp,
  regs_private_all$spec2_est,
  regs_private_all$spec2_emp,
  regs_private_all$spec3_est,
  regs_private_all$spec3_emp,
  regs_private_all$spec4_est,
  regs_private_all$spec4_emp
)

stargazer(
  reg,
  type = "latex",
  out = tex_file_priv_combined,
  title = "Regression Results: Private Sector (Establishments and Employment)",
  label = "tab:private_combined",
  column.labels = c("Spec 1", "Spec 1", "Spec 2", "Spec 2", "Spec 3", "Spec 3", "Spec 4", "Spec 4"),
  covariate.labels = c(
    "elec12-14 × 2014", "elec12-14 × 2017", "elec12-14 × 2020",
    "Log1(Res. Consumer) × 2014", "Log1(Res. Consumer) × 2017", "Log1(Res. Consumer) × 2020",
    "Log1(Non-Res. Consumer) × 2014", "Log1(Non-Res. Consumer) × 2017", "Log1(Non-Res. Consumer) × 2020"
  ),
  keep = keep_terms,
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  keep.stat = c("n", "rsq"),
  header = FALSE,
  font.size = "small",
  digits = 3
)


# 4. Extract coefficient chunks from Stargazer output

extract_terms <- function(tex_file_path) {
  lines <- readLines(tex_file_path)
  
  # Extract a 3-line coefficient block (estimate + SE + blank)
  extract_chunk <- function(pattern) {
    i <- grep(pattern, lines, perl = TRUE)
    if (length(i) == 0) return(character(0))
    chunks <- lapply(i, function(idx) {
      end <- min(idx + 2, length(lines))
      lines[idx:end]
    })
    unlist(chunks)
  }
  
  # --- Group by variable type across all years ---
  elec12_14_terms <- unlist(lapply(c("2014", "2017", "2020"), function(y) 
    extract_chunk(paste0("^\\s*elec12-14\\s*×\\s*", y))
  ))
  
  res_terms <- unlist(lapply(c("2014", "2017", "2020"), function(y)
    extract_chunk(paste0("Log1\\(Res\\. Consumer\\)\\s*×\\s*", y))
  ))
  
  nonres_terms <- unlist(lapply(c("2014", "2017", "2020"), function(y)
    extract_chunk(paste0("Log1\\(Non-Res\\. Consumer\\)\\s*×\\s*", y))
  ))
  
  list(
    p0 = elec12_14_terms,
    p1 = res_terms,
    p2 = nonres_terms,
    obs = lines[grep("^Observations\\s*&", lines)],
    r2  = lines[grep("^R\\$\\^\\{2\\}\\$\\s*&", lines)]
  )
}

extracted_priv_combined <- extract_terms(tex_file_priv_combined)


# 5. Final Combined LaTeX Table (8 columns)

writeLines(c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Regression Results: Private Sector (Establishments and Employment)}",
  "\\label{tab:private_combined}",
  "\\small",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{@{\\extracolsep{5pt}}lcccccccc}",
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  " & \\multicolumn{8}{c}{\\textit{Dependent variable:}} \\\\",
  "\\cline{2-9}",
  "\\\\[-1.8ex] & \\multicolumn{2}{c}{Spec 1: Basic FE} & \\multicolumn{2}{c}{Spec 2: Infra FE} & \\multicolumn{2}{c}{Spec 3: +Imidugudu FE} & \\multicolumn{2}{c}{Spec 4: +Consumer Controls} \\\\",
  "\\\\[-1.8ex] & n-establishment & total-employee & n-establishment & total-employee & n-establishment & total-employee & n-establishment & total-employee \\\\",
  " & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\\\",
  "\\hline \\\\[-1.8ex]",
  
  # ---- Coefficients
  extracted_priv_combined$p0,
  extracted_priv_combined$p1,
  extracted_priv_combined$p2,
  
  "\\hline \\\\[-1.8ex]",
  # ---- Observations and R2
  extracted_priv_combined$obs,
  extracted_priv_combined$r2,
  
  # ---- Updated FE list
  "FE: Village & X & X & X & X & X & X & X & X \\\\",
  "FE: Cell-Year & X & X & X & X & X & X & X & X \\\\",
  "FE: Cell Office-Year &  &  & X & X & X & X & X & X \\\\",
  "FE: Health Center-Year &  &  & X & X & X & X & X & X \\\\",
  "FE: Primary School-Year &  &  & X & X & X & X & X & X \\\\",
  "FE: Secondary School-Year &  &  & X & X & X & X & X & X \\\\",
  "FE: Sector/District Office-Year &  &  & X & X & X & X & X & X \\\\",
  "FE: Industry-Year &  &  & X & X & X & X & X & X \\\\",
  "FE: Market-Year &  &  & X & X & X & X & X & X \\\\",
  "FE: Imidugudu-Year &  &  &  &  & X & X & X & X \\\\",  
  "\\hline",
  "\\end{tabular}",
  "}%",
  "\\end{table}"
), file.path(output_path, "regressions", "elec12_14_Private_combined_imidugudu.tex"))


##Log---------

#1. Run all regressions 
run_reg_all <- function(df) {
  list(
    #  Establishments
    spec1_est = felm(
      log1_num_establishment ~ 
        p0_2014 * elec12_14 + 
        p1_2017 * elec12_14 + 
        p2_2020 * elec12_14 |
        village_id + cell_year | 
        0 | sector_id,
      data = df
    ),
    
    spec2_est = felm(
      log1_num_establishment ~ 
        p0_2014 * elec12_14 + 
        p1_2017 * elec12_14 + 
        p2_2020 * elec12_14 |
        village_id + cell_year + 
        cell_office_year + health_center_year + 
        primary_school_year + secondary_school_year + 
        sector_district_office_year + industry_year + market_year | 
        0 | sector_id,
      data = df
    ),
    
    spec3_est = felm(
      log1_num_establishment ~ 
        p0_2014 * elec12_14 + 
        p1_2017 * elec12_14 + 
        p2_2020 * elec12_14 |
        village_id + cell_year + 
        cell_office_year + health_center_year + 
        primary_school_year + secondary_school_year + 
        sector_district_office_year + industry_year + market_year + 
        imidugudu_year | 
        0 | sector_id,
      data = df
    ),
    
    spec4_est = felm(
      log1_num_establishment ~ 
        p0_2014 * elec12_14 + 
        p1_2017 * elec12_14 + 
        p2_2020 * elec12_14 +
        p0_2014 * log1_residential_consumer + 
        p1_2017 * log1_residential_consumer + 
        p2_2020 * log1_residential_consumer +
        p0_2014 * log1_non_residential_consumer + 
        p1_2017 * log1_non_residential_consumer + 
        p2_2020 * log1_non_residential_consumer |
        village_id + cell_year + 
        cell_office_year + health_center_year + 
        primary_school_year + secondary_school_year + 
        sector_district_office_year + industry_year + market_year + 
        imidugudu_year | 
        0 | sector_id,
      data = df
    ),
    
    # Employees 
    spec1_emp = felm(
      log1_total_employee ~ 
        p0_2014 * elec12_14 + 
        p1_2017 * elec12_14 + 
        p2_2020 * elec12_14 |
        village_id + cell_year | 
        0 | sector_id,
      data = df
    ),
    
    spec2_emp = felm(
      log1_total_employee ~ 
        p0_2014 * elec12_14 + 
        p1_2017 * elec12_14 + 
        p2_2020 * elec12_14 |
        village_id + cell_year + 
        cell_office_year + health_center_year + 
        primary_school_year + secondary_school_year + 
        sector_district_office_year + industry_year + market_year | 
        0 | sector_id,
      data = df
    ),
    
    spec3_emp = felm(
      log1_total_employee ~ 
        p0_2014 * elec12_14 + 
        p1_2017 * elec12_14 + 
        p2_2020 * elec12_14 |
        village_id + cell_year + 
        cell_office_year + health_center_year + 
        primary_school_year + secondary_school_year + 
        sector_district_office_year + industry_year + market_year + 
        imidugudu_year | 
        0 | sector_id,
      data = df
    ),
    
    spec4_emp = felm(
      log1_total_employee ~ 
        p0_2014 * elec12_14 + 
        p1_2017 * elec12_14 + 
        p2_2020 * elec12_14 +
        p0_2014 * log1_residential_consumer + 
        p1_2017 * log1_residential_consumer + 
        p2_2020 * log1_residential_consumer +
        p0_2014 * log1_non_residential_consumer + 
        p1_2017 * log1_non_residential_consumer + 
        p2_2020 * log1_non_residential_consumer |
        village_id + cell_year + 
        cell_office_year + health_center_year + 
        primary_school_year + secondary_school_year + 
        sector_district_office_year + industry_year + market_year + 
        imidugudu_year | 
        0 | sector_id,
      data = df
    )
  )
}

#2. Run regressions 

regs_private_all <- run_reg_all(elec12_14_p)


#3. Export raw Stargazer table (8 columns) 

keep_terms <- c(
  "p0_2014:elec12_14", "elec12_14:p1_2017", "elec12_14:p2_2020",
  "p0_2014:log1_residential_consumer", "p1_2017:log1_residential_consumer", "p2_2020:log1_residential_consumer",
  "p0_2014:log1_non_residential_consumer", "p1_2017:log1_non_residential_consumer", "p2_2020:log1_non_residential_consumer"
)

tex_file_priv_combined <- file.path(output_path, "regressions", "raw", "elec12_14_Private_combined_log.tex")

reg <- list(
  regs_private_all$spec1_est,
  regs_private_all$spec1_emp,
  regs_private_all$spec2_est,
  regs_private_all$spec2_emp,
  regs_private_all$spec3_est,
  regs_private_all$spec3_emp,
  regs_private_all$spec4_est,
  regs_private_all$spec4_emp
)

stargazer(
  reg,
  type = "latex",
  out = tex_file_priv_combined,
  title = "Regression Results: Private Sector (Log of Establishments and Employees)",
  label = "tab:private_combined_log",
  column.labels = c("Spec 1", "Spec 1", "Spec 2", "Spec 2", "Spec 3", "Spec 3", "Spec 4", "Spec 4"),
  covariate.labels = c(
    "elec12-14 × 2014", "elec12-14 × 2017", "elec12-14 × 2020",
    "Log1(Res. Consumer) × 2014", "Log1(Res. Consumer) × 2017", "Log1(Res. Consumer) × 2020",
    "Log1(Non-Res. Consumer) × 2014", "Log1(Non-Res. Consumer) × 2017", "Log1(Non-Res. Consumer) × 2020"
  ),
  keep = keep_terms,
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  keep.stat = c("n", "rsq"),
  header = FALSE,
  font.size = "small",
  digits = 3
)


#4. Extract coefficient chunks from Stargazer output

extract_terms <- function(tex_file_path) {
  lines <- readLines(tex_file_path)
  
  # Extract 3-line coefficient block
  extract_chunk <- function(pattern) {
    i <- grep(pattern, lines, perl = TRUE)
    if (length(i) == 0) return(character(0))
    chunks <- lapply(i, function(idx) {
      end <- min(idx + 2, length(lines))
      lines[idx:end]
    })
    unlist(chunks)
  }
  
  # Group by variable type
  elec12_14_terms <- unlist(lapply(c("2014", "2017", "2020"), function(y) 
    extract_chunk(paste0("^\\s*elec12-14\\s*×\\s*", y))
  ))
  
  res_terms <- unlist(lapply(c("2014", "2017", "2020"), function(y)
    extract_chunk(paste0("Log1\\(Res\\. Consumer\\)\\s*×\\s*", y))
  ))
  
  nonres_terms <- unlist(lapply(c("2014", "2017", "2020"), function(y)
    extract_chunk(paste0("Log1\\(Non-Res\\. Consumer\\)\\s*×\\s*", y))
  ))
  
  list(
    p0 = elec12_14_terms,
    p1 = res_terms,
    p2 = nonres_terms,
    obs = lines[grep("^Observations\\s*&", lines)],
    r2  = lines[grep("^R\\$\\^\\{2\\}\\$\\s*&", lines)]
  )
}

extracted_priv_combined <- extract_terms(tex_file_priv_combined)


# 5. Final Combined LaTeX Table (8 columns)
writeLines(c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Regression Results: Private Sector (Log of Establishments and Employees)}",
  "\\label{tab:private_combined_log}",
  "\\small",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{@{\\extracolsep{5pt}}lcccccccc}",
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  " & \\multicolumn{8}{c}{\\textit{Dependent variable:}} \\\\",
  "\\cline{2-9}",
  "\\\\[-1.8ex] & \\multicolumn{2}{c}{Spec 1: Basic FE} & \\multicolumn{2}{c}{Spec 2: Infra FE} & \\multicolumn{2}{c}{Spec 3: +Imidugudu FE} & \\multicolumn{2}{c}{Spec 4: +Consumer Controls} \\\\",
  "\\\\[-1.8ex] & log1(n-establishments) & log1(total-employee) & log1(n-establishments) & log1(total-employee) & log1(n-establishments) & log1(total-employee) & log1(n-establishments) & log1(total-employee) \\\\",
  " & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\\\",
  "\\hline \\\\[-1.8ex]",
  
  # ---- Coefficients
  extracted_priv_combined$p0,
  extracted_priv_combined$p1,
  extracted_priv_combined$p2,
  
  "\\hline \\\\[-1.8ex]",
  # ---- Observations and R2
  extracted_priv_combined$obs,
  extracted_priv_combined$r2,
  
  # ---- Fixed Effects List
  "FE: Village & X & X & X & X & X & X & X & X \\\\",
  "FE: Cell-Year & X & X & X & X & X & X & X & X \\\\",
  "FE: Cell Office-Year &  &  & X & X & X & X & X & X \\\\",
  "FE: Health Center-Year &  &  & X & X & X & X & X & X \\\\",
  "FE: Primary School-Year &  &  & X & X & X & X & X & X \\\\",
  "FE: Secondary School-Year &  &  & X & X & X & X & X & X \\\\",
  "FE: Sector/District Office-Year &  &  & X & X & X & X & X & X \\\\",
  "FE: Industry-Year &  &  & X & X & X & X & X & X \\\\",
  "FE: Market-Year &  &  & X & X & X & X & X & X \\\\",
  "FE: Imidugudu-Year &  &  &  &  & X & X & X & X \\\\",  
  "\\hline",
  "\\end{tabular}",
  "}%",
  "\\end{table}"
), file.path(output_path, "regressions", "elec12_14_Private_combined_log_imidugudu.tex"))





#Predicted 12-14-----

elec12_14_p <- elec12_14_p %>% 
  group_by(village_id) %>%
  mutate(
    bl_num_establishment = num_establishment[year == 2011][1],
    bl_total_employee = total_employee[year == 2011][1]
  ) %>%
  ungroup() %>% 
  mutate(elec12_14xpred_elec12_14 = elec12_14 * pred_elec12_14) %>% 
  mutate(
    year = factor(year, levels = c(2011, 2014, 2017, 2020))
  ) %>% 
  mutate(
    cell_year = paste0(cell_id, "_", year)
  )  %>% 
  mutate(
    log1_num_establishment = log(num_establishment + 1),
    log1_total_employee = log(total_employee + 1)
  )


# 1. Run all regressions -------------------------------------

run_prediction_regs <- function(df) {
  list(
    pred_num_est_1 = felm(num_establishment ~ year + elec12_14 * year + pred_elec12_14 * year |
                            cell_year | 0 | sector_id, data = df),
    
    pred_num_est_2 = felm(num_establishment ~ year + elec12_14 * year + pred_elec12_14 * year + 
                            bl_num_establishment * year |
                            cell_year | 0 | sector_id, data = df),
    
    pred_log_num_est_1 = felm(log1_num_establishment ~  year + elec12_14 * year + pred_elec12_14 * year |
                                cell_year | 0 | sector_id, data = df),
    
    pred_log_num_est_2 = felm(log1_num_establishment ~ year + elec12_14 * year + pred_elec12_14 * year + 
                                bl_num_establishment * year |
                                cell_year | 0 | sector_id, data = df),
    
    pred_total_emp_1 = felm(total_employee ~ year + elec12_14 * year + pred_elec12_14 * year |
                              cell_year | 0 | sector_id, data = df),
    
    pred_total_emp_2 = felm(total_employee ~ year + elec12_14 * year + pred_elec12_14 * year + 
                              bl_total_employee * year |
                              cell_year | 0 | sector_id, data = df),
    
    pred_log_total_emp_1 = felm(log1_total_employee ~ year + elec12_14 * year + pred_elec12_14 * year |
                                  cell_year | 0 | sector_id, data = df),
    
    pred_log_total_emp_2 = felm(log1_total_employee ~ year + elec12_14 * year + pred_elec12_14 * year + 
                                  bl_total_employee * year |
                                  cell_year | 0 | sector_id, data = df)
  )
}

regs_pred_all <- run_prediction_regs(elec12_14_p)




# 2. Export raw Stargazer .tex --------------------------------

tex_file_pred_combined <- file.path(output_path, "regressions", "raw", "prediction_combined.tex")

reg <- list(
  regs_pred_all$pred_num_est_1,
  regs_pred_all$pred_total_emp_1,
  regs_pred_all$pred_num_est_2,
  regs_pred_all$pred_total_emp_2,
  regs_pred_all$pred_log_num_est_1,
  regs_pred_all$pred_log_total_emp_1,
  regs_pred_all$pred_log_num_est_2,
  regs_pred_all$pred_log_total_emp_2
)

stargazer(
  reg,
  type = "latex",
  out = tex_file_pred_combined,
  title = "Regression Results: Prediction × Electrification Interactions",
  label = "tab:prediction_combined",
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  keep.stat = c("n", "rsq"),
  header = FALSE,
  font.size = "small",
  digits = 3
)


# 3. Extract all coefficient lines -----------------------------
extract_all_terms <- function(tex_file_path) {
    lines <- readLines(tex_file_path)
    
    # Helper: extract coefficient + SE (+ optional blank)
    extract_chunk <- function(pattern) {
      i <- grep(pattern, lines, perl = TRUE)
      if (length(i) == 0) return(character(0))
      chunks <- lapply(i, function(idx) {
        end <- min(idx + 2, length(lines))
        lines[idx:end]
      })
      unlist(chunks)
    }
    
    # =============================
    # ---- Coefficient groups -----
    # =============================
    
    # Base terms (must include LaTeX backslashes!)
    elec12_14_base        <- extract_chunk("^\\s*elec12\\\\_14\\b")
    pred_elec12_14_base   <- extract_chunk("^\\s*pred\\\\_elec12\\\\_14\\b")
    bl_num_est            <- extract_chunk("^\\s*bl\\\\_num\\\\_establishment\\b")
    bl_total_emp          <- extract_chunk("^\\s*bl\\\\_total\\\\_employee\\b")
    
    # Year interactions
    extract_year_terms <- function(var) {
      unlist(lapply(c("2014", "2017", "2020"), function(y) {
        pattern <- paste0("^\\s*year", y, ":", var, "\\b")
        extract_chunk(pattern)
      }))
    }
    
    year_elec12_14        <- extract_year_terms("elec12\\\\_14")
    year_pred_elec12_14   <- extract_year_terms("pred\\\\_elec12\\\\_14")
    year_bl_num_est       <- extract_year_terms("bl\\\\_num\\\\_establishment")
    year_bl_total_emp     <- extract_year_terms("bl\\\\_total\\\\_employee")
    
    # Observations and R² lines
    obs_lines <- lines[grep("^Observations\\s*&", lines)]
    r2_lines  <- lines[grep("^R\\$\\^\\{2\\}\\$\\s*&", lines)]
    
    # Return all groups
    list(
      elec12_14 = elec12_14_base,
      pred_elec12_14 = pred_elec12_14_base,
      bl_num_establishment = bl_num_est,
      bl_total_employee = bl_total_emp,
      year_elec12_14 = year_elec12_14,
      year_pred_elec12_14 = year_pred_elec12_14,
      year_bl_num_establishment = year_bl_num_est,
      year_bl_total_employee = year_bl_total_emp,
      obs = obs_lines,
      r2 = r2_lines
    )
  }
  

extracted_pred_combined <- extract_all_terms(tex_file_pred_combined)


# 4. Final LaTeX table (8 columns, all covariates, FE marked) --

writeLines(c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Regression Results: Prediction × Electrification Interactions}",
  "\\label{tab:prediction_combined}",
  "\\small",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{@{\\extracolsep{5pt}}lcccccccc}",
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  " & \\multicolumn{8}{c}{\\textit{Dependent variable:}} \\\\",
  "\\cline{2-9}",
  "\\\\[-1.8ex] & \\multicolumn{2}{c}{Level Spec 1} & \\multicolumn{2}{c}{Level Spec 2 (w/ Baseline)} & \\multicolumn{2}{c}{Log Spec 1} & \\multicolumn{2}{c}{Log Spec 2 (w/ Baseline)} \\\\",
  "\\\\[-1.8ex] & Num. Est. & Total Emp. & Num. Est. & Total Emp. & log1(Est.) & log1(Emp.) & log1(Est.) & log1(Emp.) \\\\",
  " & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\\\",
  "\\hline \\\\[-1.8ex]",
  
  # ---- Coefficients (all covariates)
  extracted_pred_combined$elec12_14,
  extracted_pred_combined$pred_elec12_14,
  extracted_pred_combined$year_elec12_14,
  extracted_pred_combined$year_pred_elec12_14,
  extracted_pred_combined$bl_num_establishment,
  extracted_pred_combined$year_bl_num_establishment,
  extracted_pred_combined$bl_total_employee,
  extracted_pred_combined$year_bl_total_employee,
  
  "\\hline \\\\[-1.8ex]",
  # ---- Observations and R2
  extracted_pred_combined$obs,
  extracted_pred_combined$r2,
  
  # ---- Fixed Effects Notes
  "FE: Cell-Year & X & X & X & X & X & X & X & X \\\\",
  "\\hline",
  "\\end{tabular}",
  "}%",
  "\\end{table}"
), file.path(output_path, "regressions", "elec12_14_prediction_combined.tex"))



#ANCOVA regression------
# 
# ~ treat + time + treat*time + Ptreat + Ptreat*time + X + X*time
# ~ treat + time + treat*time + Ptreat + Ptreat*time + X + X*time + X*treat + X*treat*time

elec12_14_p <- elec12_14_p %>% 
  mutate(
    log1_bl_num_establishment = log(bl_num_establishment + 1),
    log1_bl_total_employee = log(bl_total_employee + 1),
    
  )







run_ancova_regs <- function(df) {
  list(
    ancova_num_est = felm(
      num_establishment ~ elec12_14 * year + pred_elec12_14 * year +
        bl_num_establishment * year + bl_num_establishment * elec12_14 * year |
        cell_year | 0 | sector_id, data = df),
    
    ancova_total_emp = felm(
      total_employee ~ elec12_14 * year + pred_elec12_14 * year +
        bl_total_employee * year + bl_total_employee * elec12_14 * year |
        cell_year | 0 | sector_id, data = df),
    
    ancova_log_num_est = felm(
      log1_num_establishment ~ elec12_14 * year + pred_elec12_14 * year +
        log1_bl_num_establishment * year + log1_bl_num_establishment* elec12_14 * year |
        cell_year | 0 | sector_id, data = df),
    
    ancova_log_total_emp = felm(
      log1_total_employee ~ elec12_14 * year + pred_elec12_14 * year +
        log1_bl_total_employee * year + log1_bl_total_employee * elec12_14 * year |
        cell_year | 0 | sector_id, data = df)
  )
}

regs_ancova_all <- run_ancova_regs(elec12_14_p)



# 2. Export raw Stargazer .tex --------------------------------

tex_file_ancova_combined <- file.path(output_path, "regressions", "raw", "ancova_combined.tex")

reg <- list(
  regs_ancova_all$ancova_num_est,
  regs_ancova_all$ancova_total_emp,
  regs_ancova_all$ancova_log_num_est,
  regs_ancova_all$ancova_log_total_emp
)

stargazer(
  reg,
  type = "latex",
  out = tex_file_ancova_combined,
  title = "ANCOVA Regression Results: Electrification × Prediction × Baseline Interactions",
  label = "tab:ancova_combined",
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  keep.stat = c("n", "rsq"),
  header = FALSE,
  font.size = "small",
  digits = 3
)



# 3. Extract all coefficient lines -----------------------------

extract_all_terms <- function(tex_file_path) {
  lines <- readLines(tex_file_path)
  
  elec12_14_base        <- lines[grep("^\\s*elec12\\\\_14\\b", lines)]
  pred_elec12_14_base   <- lines[grep("^\\s*pred\\\\_elec12\\\\_14\\b", lines)]
  bl_num_est            <- lines[grep("^\\s*bl\\\\_num\\\\_establishment\\b", lines)]
  bl_total_emp          <- lines[grep("^\\s*bl\\\\_total\\\\_employee\\b", lines)]
  
  year_elec12_14        <- lines[grep("^\\s*year(2014|2017|2020):elec12\\\\_14", lines)]
  year_pred_elec12_14   <- lines[grep("^\\s*year(2014|2017|2020):pred\\\\_elec12\\\\_14", lines)]
  year_bl_num_est       <- lines[grep("^\\s*year(2014|2017|2020):bl\\\\_num\\\\_establishment", lines)]
  year_bl_total_emp     <- lines[grep("^\\s*year(2014|2017|2020):bl\\\\_total\\\\_employee", lines)]
  
  triple_bl_num_est     <- lines[grep("^\\s*bl\\\\_num\\\\_establishment:elec12\\\\_14:year(2014|2017|2020)", lines)]
  triple_bl_total_emp   <- lines[grep("^\\s*bl\\\\_total\\\\_employee:elec12\\\\_14:year(2014|2017|2020)", lines)]
  
  obs_lines <- lines[grep("^Observations\\s*&", lines)]
  r2_lines  <- lines[grep("^R\\$\\^\\{2\\}\\$\\s*&", lines)]
  
  list(
    elec12_14 = elec12_14_base,
    pred_elec12_14 = pred_elec12_14_base,
    bl_num_establishment = bl_num_est,
    bl_total_employee = bl_total_emp,
    year_elec12_14 = year_elec12_14,
    year_pred_elec12_14 = year_pred_elec12_14,
    year_bl_num_establishment = year_bl_num_est,
    year_bl_total_employee = year_bl_total_emp,
    triple_bl_num_establishment = triple_bl_num_est,
    triple_bl_total_employee = triple_bl_total_emp,
    obs = obs_lines,
    r2 = r2_lines
  )
}

extracted_ancova_combined <- extract_all_terms(tex_file_ancova_combined)



# 4. Final LaTeX table (4 columns, all covariates, FE marked) --

writeLines(c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{ANCOVA Regression Results: Electrification × Prediction × Baseline Interactions}",
  "\\label{tab:ancova_combined}",
  "\\small",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{@{\\extracolsep{5pt}}lcccc}",
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  " & Num. Est. & Total Emp. & log1(Est.) & log1(Emp.) \\\\",
  " & (1) & (2) & (3) & (4) \\\\",
  "\\hline \\\\[-1.8ex]",
  
  extracted_ancova_combined$elec12_14,
  extracted_ancova_combined$pred_elec12_14,
  extracted_ancova_combined$year_elec12_14,
  extracted_ancova_combined$year_pred_elec12_14,
  extracted_ancova_combined$bl_num_establishment,
  extracted_ancova_combined$year_bl_num_establishment,
  extracted_ancova_combined$bl_total_employee,
  extracted_ancova_combined$year_bl_total_employee,
  extracted_ancova_combined$triple_bl_num_establishment,
  extracted_ancova_combined$triple_bl_total_employee,
  
  "\\hline \\\\[-1.8ex]",
  extracted_ancova_combined$obs,
  extracted_ancova_combined$r2,
  "FE: Cell-Year & X & X & X & X \\\\",
  "\\hline",
  "\\end{tabular}",
  "}%",
  "\\end{table}"
), file.path(output_path, "regressions", "elec12_14_ancova_combined.tex"))