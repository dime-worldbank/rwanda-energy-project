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
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0)
  )



lm_1 <- felm(elec15_17 ~  cell_office + health_center + primary_school + 
               secondary_school + sector_district_office + industry + market +
               imidugudu + log1_residential_consumer + log1_non_residential_consumer|cell_id |0| sector_id, 
             data = expansion_join_drop15_17)


summary(lm_1)

expansion_join_drop15_17 <- expansion_join_drop15_17 %>%
  mutate(pred_elec15_17 = fitted(lm_1))






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



elec15_17_join <- expansion_join_drop15_17%>% 
  select(village_id,elec15_17, cell_id, sector_id, district_id) 

elec15_17_did_isic<- left_join( elec15_17_join, ec_all, by = c("village_id"))


elec15_17_did_isic <- elec15_17_did_isic %>%
  complete(year, village_id, isic_level1,
           fill = list(num_establishment = 0, total_employee = 0)) %>% 
  filter(!is.na(year))   %>% 
  filter(!is.na(isic_level1)) %>% 
  select(year, village_id, num_establishment, total_employee, isic_level1)


elec15_17 <- expansion_join_drop15_17%>%
  select(village_id, elec15_17, earp_existing_mv, health_center, primary_school, cell_office, secondary_school, sector_district_office,
         industry, market, residential_consumer, non_residential_consumer, imidugudu) 


elec15_17_did_isic <- left_join(elec15_17_did_isic, elec15_17, by = c("village_id"))



elec15_17_did_isic <- elec15_17_did_isic %>% 
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
    
    p_2_2011 = ifelse(year == 2011, 1, 0),
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


join_drop15_17 <- expansion_join_drop15_17 %>% 
  select(`elec15_17`, pred_elec15_17,village_id, cell_id, sector_id, cell_office, health_center, primary_school, secondary_school, 
         sector_district_office, market, industry, residential_consumer,  non_residential_consumer, imidugudu)

elec15_17_p <- elec15_17_did_isic %>%
  mutate(private_sector = ifelse(isic %in% c(9,7,3,19), 1, 0)) %>%
  filter(private_sector == 1) %>%
  group_by(year, village_id) %>%
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE), .groups="drop") %>%
  mutate(total_employee = pmin(total_employee, 100)) %>% 
  left_join(join_drop15_17, by = c("village_id") )  %>%
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
    p_2_2011 = ifelse(year == 2011, 1, 0),
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
        p_2_2011 * elec15_17 + 
        p1_2017 * elec15_17 + 
        p2_2020 * elec15_17 |
        village_id + cell_year | 
        0 | sector_id,
      data = df
    ),
    
    spec2_est = felm(
      num_establishment ~ 
        p_2_2011 * elec15_17 + 
        p1_2017 * elec15_17 + 
        p2_2020 * elec15_17 |
        village_id + cell_year + 
        cell_office_year + health_center_year + 
        primary_school_year + secondary_school_year + 
        sector_district_office_year + industry_year + market_year | 
        0 | sector_id,
      data = df
    ),
    
    spec3_est = felm(
      num_establishment ~ 
        p_2_2011 * elec15_17 + 
        p1_2017 * elec15_17 + 
        p2_2020 * elec15_17 |
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
        p_2_2011 * elec15_17 + 
        p1_2017 * elec15_17 + 
        p2_2020 * elec15_17 +
        p_2_2011 * log1_residential_consumer + 
        p1_2017 * log1_residential_consumer + 
        p2_2020 * log1_residential_consumer +
        p_2_2011 * log1_non_residential_consumer + 
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
        p_2_2011 * elec15_17 + 
        p1_2017 * elec15_17 + 
        p2_2020 * elec15_17 |
        village_id + cell_year | 
        0 | sector_id,
      data = df
    ),
    
    spec2_emp = felm(
      total_employee ~ 
        p_2_2011 * elec15_17 + 
        p1_2017 * elec15_17 + 
        p2_2020 * elec15_17 |
        village_id + cell_year + 
        cell_office_year + health_center_year + 
        primary_school_year + secondary_school_year + 
        sector_district_office_year + industry_year + market_year | 
        0 | sector_id,
      data = df
    ),
    
    spec3_emp = felm(
      total_employee ~ 
        p_2_2011 * elec15_17 + 
        p1_2017 * elec15_17 + 
        p2_2020 * elec15_17 |
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
        p_2_2011 * elec15_17 + 
        p1_2017 * elec15_17 + 
        p2_2020 * elec15_17 +
        p_2_2011 * log1_residential_consumer + 
        p1_2017 * log1_residential_consumer + 
        p2_2020 * log1_residential_consumer +
        p_2_2011 * log1_non_residential_consumer + 
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

regs_private_all <- run_reg_all(elec15_17_p) 




# 3. Export raw Stargazer table (8 columns)

keep_terms <- c(
  "p_2_2011:elec15_17", "elec15_17:p1_2017", "elec15_17:p2_2020",
  "p_2_2011:log1_residential_consumer", "p1_2017:log1_residential_consumer", "p2_2020:log1_residential_consumer",
  "p_2_2011:log1_non_residential_consumer", "p1_2017:log1_non_residential_consumer", "p2_2020:log1_non_residential_consumer"
)

tex_file_priv_combined <- file.path(output_path, "regressions", "raw", "elec15_17_Private_combined.tex")

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
    "elec15-17 × 2011", "elec15-17 × 2017", "elec15-17 × 2020",
    "Log1(Res. Consumer) × 2011", "Log1(Res. Consumer) × 2017", "Log1(Res. Consumer) × 2020",
    "Log1(Non-Res. Consumer) × 2011", "Log1(Non-Res. Consumer) × 2017", "Log1(Non-Res. Consumer) × 2020"
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
  elec15_17_terms <- unlist(lapply(c("2011", "2017", "2020"), function(y) 
    extract_chunk(paste0("^\\s*elec15-17\\s*×\\s*", y))
  ))
  
  res_terms <- unlist(lapply(c("2011", "2017", "2020"), function(y)
    extract_chunk(paste0("Log1\\(Res\\. Consumer\\)\\s*×\\s*", y))
  ))
  
  nonres_terms <- unlist(lapply(c("2011", "2017", "2020"), function(y)
    extract_chunk(paste0("Log1\\(Non-Res\\. Consumer\\)\\s*×\\s*", y))
  ))
  
  list(
    p0 = elec15_17_terms,
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
), file.path(output_path, "regressions", "elec15_17_Private_combined_imidugudu.tex"))


##Log---------

#1. Run all regressions 
run_reg_all <- function(df) {
  list(
    #  Establishments
    spec1_est = felm(
      log1_num_establishment ~ 
        p_2_2011 * elec15_17 + 
        p1_2017 * elec15_17 + 
        p2_2020 * elec15_17 |
        village_id + cell_year | 
        0 | sector_id,
      data = df
    ),
    
    spec2_est = felm(
      log1_num_establishment ~ 
        p_2_2011 * elec15_17 + 
        p1_2017 * elec15_17 + 
        p2_2020 * elec15_17 |
        village_id + cell_year + 
        cell_office_year + health_center_year + 
        primary_school_year + secondary_school_year + 
        sector_district_office_year + industry_year + market_year | 
        0 | sector_id,
      data = df
    ),
    
    spec3_est = felm(
      log1_num_establishment ~ 
        p_2_2011 * elec15_17 + 
        p1_2017 * elec15_17 + 
        p2_2020 * elec15_17 |
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
        p_2_2011 * elec15_17 + 
        p1_2017 * elec15_17 + 
        p2_2020 * elec15_17 +
        p_2_2011 * log1_residential_consumer + 
        p1_2017 * log1_residential_consumer + 
        p2_2020 * log1_residential_consumer +
        p_2_2011 * log1_non_residential_consumer + 
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
        p_2_2011 * elec15_17 + 
        p1_2017 * elec15_17 + 
        p2_2020 * elec15_17 |
        village_id + cell_year | 
        0 | sector_id,
      data = df
    ),
    
    spec2_emp = felm(
      log1_total_employee ~ 
        p_2_2011 * elec15_17 + 
        p1_2017 * elec15_17 + 
        p2_2020 * elec15_17 |
        village_id + cell_year + 
        cell_office_year + health_center_year + 
        primary_school_year + secondary_school_year + 
        sector_district_office_year + industry_year + market_year | 
        0 | sector_id,
      data = df
    ),
    
    spec3_emp = felm(
      log1_total_employee ~ 
        p_2_2011 * elec15_17 + 
        p1_2017 * elec15_17 + 
        p2_2020 * elec15_17 |
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
        p_2_2011 * elec15_17 + 
        p1_2017 * elec15_17 + 
        p2_2020 * elec15_17 +
        p_2_2011 * log1_residential_consumer + 
        p1_2017 * log1_residential_consumer + 
        p2_2020 * log1_residential_consumer +
        p_2_2011 * log1_non_residential_consumer + 
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

regs_private_all <- run_reg_all(elec15_17_p)


#3. Export raw Stargazer table (8 columns) 

keep_terms <- c(
  "p_2_2011:elec15_17", "elec15_17:p1_2017", "elec15_17:p2_2020",
  "p_2_2011:log1_residential_consumer", "p1_2017:log1_residential_consumer", "p2_2020:log1_residential_consumer",
  "p_2_2011:log1_non_residential_consumer", "p1_2017:log1_non_residential_consumer", "p2_2020:log1_non_residential_consumer"
)

tex_file_priv_combined <- file.path(output_path, "regressions", "raw", "elec15_17_Private_combined_log.tex")

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
    "elec15-17 × 2011", "elec15-17 × 2017", "elec15-17 × 2020",
    "Log1(Res. Consumer) × 2011", "Log1(Res. Consumer) × 2017", "Log1(Res. Consumer) × 2020",
    "Log1(Non-Res. Consumer) × 2011", "Log1(Non-Res. Consumer) × 2017", "Log1(Non-Res. Consumer) × 2020"
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
  elec15_17_terms <- unlist(lapply(c("2011", "2017", "2020"), function(y) 
    extract_chunk(paste0("^\\s*elec15-17\\s*×\\s*", y))
  ))
  
  res_terms <- unlist(lapply(c("2011", "2017", "2020"), function(y)
    extract_chunk(paste0("Log1\\(Res\\. Consumer\\)\\s*×\\s*", y))
  ))
  
  nonres_terms <- unlist(lapply(c("2011", "2017", "2020"), function(y)
    extract_chunk(paste0("Log1\\(Non-Res\\. Consumer\\)\\s*×\\s*", y))
  ))
  
  list(
    p0 = elec15_17_terms,
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
), file.path(output_path, "regressions", "elec15_17_Private_combined_log_imidugudu.tex"))





#Predicted 15-17-----

elec15_17_p <- elec15_17_p %>% 
  group_by(village_id) %>%
  mutate(
    bl_num_establishment = num_establishment[year == 2011][1],
    bl_total_employee = total_employee[year == 2011][1]
  ) %>%
  ungroup() %>% 
  mutate(elec15_17xpred_elec15_17 = elec15_17 * pred_elec15_17) %>% 
  mutate(
    year = factor(year, levels = c(2014, 2011, 2017, 2020))
  ) %>% 
  mutate(
    cell_year = paste0(cell_id, "_", year)
  ) %>% 
  mutate(
    log1_num_establishment = log(num_establishment + 1),
    log1_total_employee = log(total_employee + 1)
  )



# 1. Run all regressions -------------------------------------

run_prediction_regs <- function(df) {
  list(
    pred_num_est_1 = felm(num_establishment ~ year + elec15_17 * year + pred_elec15_17 * year |
                            cell_year | 0 | sector_id, data = df),
    
    pred_num_est_2 = felm(num_establishment ~ year + elec15_17 * year + pred_elec15_17 * year + 
                            bl_num_establishment * year |
                            cell_year | 0 | sector_id, data = df),
    
    pred_log_num_est_1 = felm(log1_num_establishment ~ year + elec15_17 * year + pred_elec15_17 * year |
                                cell_year | 0 | sector_id, data = df),
    
    pred_log_num_est_2 = felm(log1_num_establishment ~ year + elec15_17 * year + pred_elec15_17 * year + 
                                bl_num_establishment * year |
                                cell_year | 0 | sector_id, data = df),
    
    pred_total_emp_1 = felm(total_employee ~ year + elec15_17 * year + pred_elec15_17 * year |
                              cell_year | 0 | sector_id, data = df),
    
    pred_total_emp_2 = felm(total_employee ~ year + elec15_17 * year + pred_elec15_17 * year + 
                              bl_total_employee * year |
                              cell_year | 0 | sector_id, data = df),
    
    pred_log_total_emp_1 = felm(log1_total_employee ~ year + elec15_17 * year + pred_elec15_17 * year |
                                  cell_year | 0 | sector_id, data = df),
    
    pred_log_total_emp_2 = felm(log1_total_employee ~ year + elec15_17 * year + pred_elec15_17 * year + 
                                  bl_total_employee * year |
                                  cell_year | 0 | sector_id, data = df)
  )
}

regs_pred_all <- run_prediction_regs(elec15_17_p)


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
  elec15_17_base        <- extract_chunk("^\\s*elec15\\\\_17\\b")
  pred_elec15_17_base   <- extract_chunk("^\\s*pred\\\\_elec15\\\\_17\\b")
  bl_num_est            <- extract_chunk("^\\s*bl\\\\_num\\\\_establishment\\b")
  bl_total_emp          <- extract_chunk("^\\s*bl\\\\_total\\\\_employee\\b")
  
  # Year interactions
  extract_year_terms <- function(var) {
    unlist(lapply(c("2011", "2017", "2020"), function(y) {
      pattern <- paste0("^\\s*year", y, ":", var, "\\b")
      extract_chunk(pattern)
    }))
  }
  
  year_elec15_17        <- extract_year_terms("elec15\\\\_17")
  year_pred_elec15_17   <- extract_year_terms("pred\\\\_elec15\\\\_17")
  year_bl_num_est       <- extract_year_terms("bl\\\\_num\\\\_establishment")
  year_bl_total_emp     <- extract_year_terms("bl\\\\_total\\\\_employee")
  
  # Observations and R² lines
  obs_lines <- lines[grep("^Observations\\s*&", lines)]
  r2_lines  <- lines[grep("^R\\$\\^\\{2\\}\\$\\s*&", lines)]
  
  # Return all groups
  list(
    elec15_17 = elec15_17_base,
    pred_elec15_17 = pred_elec15_17_base,
    bl_num_establishment = bl_num_est,
    bl_total_employee = bl_total_emp,
    year_elec15_17 = year_elec15_17,
    year_pred_elec15_17 = year_pred_elec15_17,
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
  extracted_pred_combined$elec15_17,
  extracted_pred_combined$pred_elec15_17,
  extracted_pred_combined$year_elec15_17,
  extracted_pred_combined$year_pred_elec15_17,
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
), file.path(output_path, "regressions", "elec15_17_prediction_combined.tex"))

