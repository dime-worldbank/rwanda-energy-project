#######################################
#Purpose: Using EARP as a control for analysis
#Author: XIAOMING ZHANG
#Date: October 9th 2025
######################################################




pacman::p_load(knitr, lfe,fixest, modelsummary, stargazer, kableExtra,tidyverse, dplyr, here, sf, haven, ggplot2, readxl,  writexl, janitor, randomizr, RCT, purrr, RODBC, DBI)

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


#Sample restriction----

expansion_join_drop13 <- expansion_join %>% 
  mutate(
    `EARP` = ifelse(earp_lv == 1 | earp_mv == 1, 1, 0)
  ) %>% 
  filter(electrified_year > 2013) %>% 
  anti_join(earp_existing_mv, by = c("village_id" = "Village_ID")) %>% 
  filter(! District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu"))
# %>%
#   filter(electrified_year == 9999)



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



earp_join <- expansion_join_drop13%>% 
  select(village_id, earp_mv, earp_lv, cell_id, sector_id, district_id) 






##clean------

earp_did_isic<- left_join( earp_join, ec_all, by = c("village_id"))


earp_did_isic <- earp_did_isic %>%
  complete(year, village_id, isic_level1,
           fill = list(num_establishment = 0, total_employee = 0)) %>% 
  filter(!is.na(year))  


earp <- expansion_join_drop13%>% 
  select(village_id, earp_mv, earp_lv, earp_existing_mv, health_center, primary_school, cell_office, secondary_school, sector_district_office,
         industry, market, residential_consumer, non_residential_consumer, imidugudu) %>% 
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


join_drop13 <- expansion_join_drop13 %>% 
  select(`EARP`, village_id, cell_id, sector_id, cell_office, health_center, primary_school, secondary_school, 
         sector_district_office, market, industry, residential_consumer, non_residential_consumer, imidugudu)

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


#Runreg on control======









#Run reg------
run_reg_all <- function(df) {
  list(
    spec1_est = felm(
      num_establishment ~ 
        p0_2014 * EARP + 
        p1_2017 * EARP + 
        p2_2020 * EARP |
        village_id + cell_year + cell_office_year + health_center_year + 
        primary_school_year + secondary_school_year + sector_district_office_year + 
        industry_year + market_year + imidugudu_year | 
        0 | 
        sector_id,
      data = df
    ),
    
    spec2_est = felm(
      num_establishment ~ 
        p0_2014 * EARP + 
        p1_2017 * EARP + 
        p2_2020 * EARP +
        p0_2014 * log1_residential_consumer + 
        p1_2017 * log1_residential_consumer + 
        p2_2020 * log1_residential_consumer +
        p0_2014 * log1_non_residential_consumer + 
        p1_2017 * log1_non_residential_consumer + 
        p2_2020 * log1_non_residential_consumer |
        village_id + cell_year + cell_office_year + health_center_year + 
        primary_school_year + secondary_school_year + sector_district_office_year + 
        industry_year + market_year+ imidugudu_year | 
        0 | 
        sector_id,
      data = df
    ),
    
    spec1_emp = felm(
      total_employee ~ 
        p0_2014 * EARP + 
        p1_2017 * EARP + 
        p2_2020 * EARP |
        village_id + cell_year + cell_office_year + health_center_year + 
        primary_school_year + secondary_school_year + sector_district_office_year + 
        industry_year + market_year+ imidugudu_year | 
        0 | 
        sector_id,
      data = df
    ),
    
   
    spec2_emp = felm(
      total_employee ~ 
        p0_2014 * EARP + 
        p1_2017 * EARP + 
        p2_2020 * EARP +
        p0_2014 * log1_residential_consumer + 
        p1_2017 * log1_residential_consumer + 
        p2_2020 * log1_residential_consumer +
        p0_2014 * log1_non_residential_consumer + 
        p1_2017 * log1_non_residential_consumer + 
        p2_2020 * log1_non_residential_consumer |
        village_id + cell_year + cell_office_year + health_center_year + 
        primary_school_year + secondary_school_year + sector_district_office_year + 
        industry_year + market_year+ imidugudu_year | 
        0 | 
        sector_id,
      data = df
    )
  )
}



  
# 2. Run and export combined table-----

regs_private_all <- run_reg_all(earp_p)
# lapply(regs_private_all, summary)

# Terms to keep
keep_terms <- c(
  "p0_2014:EARP", "EARP:p1_2017", "EARP:p2_2020",
  "p0_2014:log1_residential_consumer", "p1_2017:log1_residential_consumer", "p2_2020:log1_residential_consumer",
  "p0_2014:log1_non_residential_consumer", "p1_2017:log1_non_residential_consumer", "p2_2020:log1_non_residential_consumer"
)


#Export--------

tex_file_priv_combined <- file.path(output_path,"regressions", "raw", "EARP_Private_combined.tex")

reg <- list(
  regs_private_all$spec1_est,
  regs_private_all$spec1_emp,
  regs_private_all$spec2_est,
  regs_private_all$spec2_emp
)

stargazer(
  reg,
  type = "latex",
  out = tex_file_priv_combined,
  title = "Regression Results: Private Sector (Establishments and Employment)",
  label = "tab:private_combined",
  # dep.var.labels = c("num_establishment", "total_employee"),
  column.labels = c("Spec 1", "Spec 1", "Spec 2", "Spec 2"),
  covariate.labels = c(
    "EARP × 2014", "EARP × 2017", "EARP × 2020",
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



##Extract terms---------
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
  earp_terms <- unlist(lapply(c("2014", "2017", "2020"), function(y) 
    extract_chunk(paste0("^\\s*EARP\\s*×\\s*", y))
  ))
  
  res_terms <- unlist(lapply(c("2014", "2017", "2020"), function(y)
    extract_chunk(paste0("Log1\\(Res\\. Consumer\\)\\s*×\\s*", y))
  ))
  
  nonres_terms <- unlist(lapply(c("2014", "2017", "2020"), function(y)
    extract_chunk(paste0("Log1\\(Non-Res\\. Consumer\\)\\s*×\\s*", y))
  ))
  
  list(
    p0 = earp_terms,
    p1 = res_terms,
    p2 = nonres_terms,
    obs = lines[grep("^Observations\\s*&", lines)],
    r2  = lines[grep("^R\\$\\^\\{2\\}\\$\\s*&", lines)]
  )
}

extracted_priv_combined <- extract_terms(tex_file_priv_combined)


# 4. Build final combined LaTeX table------

writeLines(c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Regression Results: Private Sector (Establishments and Employment)}",
  "\\label{tab:private_combined}",
  "\\small",
  "\\resizebox{\\textwidth}{!}{%",  # <— Shrink table to 75%
  "\\begin{tabular}{@{\\extracolsep{5pt}}lcccc}",
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  " & \\multicolumn{4}{c}{\\textit{Dependent variable:}} \\\\",
  "\\cline{2-5}",
  "\\\\[-1.8ex] & \\multicolumn{2}{c}{Infra FE only} & \\multicolumn{2}{c}{Infra FE + Consumer Controls} \\\\",
  "\\\\[-1.8ex] & Number of Establishments & Total Employees & Number of Establishments & Total Employees \\\\",
  " & (1) & (2) & (3) & (4) \\\\",
  "\\hline \\\\[-1.8ex]",
  
  # ---- Coefficients
  extracted_priv_combined$p0,
  extracted_priv_combined$p1,
  extracted_priv_combined$p2,
  
  "\\hline \\\\[-1.8ex]",
  # ---- Observations and R2
  extracted_priv_combined$obs,
  extracted_priv_combined$r2,
  
  # ---- FE list
  "FE: Village & X & X & X & X \\\\",
  "FE: Cell\\_Year & X & X & X & X \\\\",
  "FE: Cell Office\\_Year & X & X & X & X \\\\",
  "FE: Health Center\\_Year & X & X & X & X \\\\",
  "FE: Primary School\\_Year & X & X & X & X \\\\",
  "FE: Secondary School\\_Year & X & X & X & X \\\\",
  "FE: District Office\\_Year & X & X & X & X \\\\",
  "FE: Industry\\_Year & X & X & X & X \\\\",
  "FE: Market\\_Year & X & X & X & X \\\\",
  "FE: Imidugudu\\_Year & X & X & X & X \\\\",
  "\\hline",
  "\\end{tabular}",
  "}%",  # <— close resizebox
  "\\end{table}"
), file.path(output_path, "regressions", "EARP_Private_combined_imidugudu.tex"))
