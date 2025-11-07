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
  filter(electrified_year %in% c("2012", "2013", "2014") |electrified_year > 2020 ) %>% 
  mutate(
    `elec12_14` = ifelse(electrified_year %in% c("2012", "2013", "2014"), 1, 0)
  ) %>% 
  anti_join(earp_existing_mv, by = c("village_id" = "Village_ID")) %>% 
  filter(! District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu")) 






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
    priority_year = paste0(priority, "-", year), 
    
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



#ISIC all-------



elec12_14_did_all <- elec12_14_did_isic %>%
  group_by(village_id, year)%>%
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE))  %>% 
  left_join(elec12_14) %>%
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
    priority_year = paste0(priority, "-", year), 
    
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0),
  ) %>% 
  mutate(
    total_employee = pmin(total_employee, 100),
    num_establishment = pmin(num_establishment, 10)
  )  









#Selected FE------------

run_reg_est <- function(df) {
  list(
    # num_establishment regressions
    num_est1 = felm(num_establishment ~ p0_2014*`elec12_14` + p1_2017*`elec12_14` + p2_2020*`elec12_14`
                    | village_id + cell_year | 0 | sector_id, data = df),
    
    num_est2 = felm(num_establishment ~ p0_2014*`elec12_14` + p1_2017*`elec12_14` + p2_2020*`elec12_14`
                    | village_id + cell_year + cell_office_year | 0 | sector_id, data = df),
    
    num_est3 = felm(num_establishment ~ p0_2014*`elec12_14` + p1_2017*`elec12_14` + p2_2020*`elec12_14`
                    | village_id + cell_year + cell_office_year + health_center_year | 0 | sector_id, data = df),
    
    num_est4 = felm(num_establishment ~ p0_2014*`elec12_14` + p1_2017*`elec12_14` + p2_2020*`elec12_14`
                    | village_id + cell_year + cell_office_year + health_center_year + primary_school_year | 0 | sector_id, data = df),
    
    num_est5 = felm(num_establishment ~ p0_2014*`elec12_14` + p1_2017*`elec12_14` + p2_2020*`elec12_14`
                    | village_id + cell_year + cell_office_year + health_center_year + primary_school_year + secondary_school_year | 0 | sector_id, data = df),
    
    num_est6 = felm(num_establishment ~ p0_2014*`elec12_14` + p1_2017*`elec12_14` + p2_2020*`elec12_14`
                    | village_id + cell_year + cell_office_year + health_center_year + primary_school_year + secondary_school_year + sector_district_office_year | 0 | sector_id, data = df),
    
    num_est7 = felm(num_establishment ~ p0_2014*`elec12_14` + p1_2017*`elec12_14` + p2_2020*`elec12_14`
                    | village_id + cell_year + cell_office_year + health_center_year + primary_school_year + secondary_school_year + sector_district_office_year + industry_year | 0 | sector_id, data = df),
    
    num_est8 = felm(num_establishment ~ p0_2014*`elec12_14` + p1_2017*`elec12_14` + p2_2020*`elec12_14`
                    | village_id + cell_year + cell_office_year + health_center_year + primary_school_year + secondary_school_year + sector_district_office_year + industry_year + market_year | 0 | sector_id, data = df)
    
    
  )
}



run_reg_emp <- function(df) {
  list(
    
    # total_employee regressions
    emp1 = felm(total_employee ~ p0_2014*`elec12_14` + p1_2017*`elec12_14` + p2_2020*`elec12_14`
                | village_id + cell_year | 0 | sector_id, data = df),
    
    emp2 = felm(total_employee ~ p0_2014*`elec12_14` + p1_2017*`elec12_14` + p2_2020*`elec12_14`
                | village_id + cell_year + cell_office_year | 0 | sector_id, data = df),
    
    emp3 = felm(total_employee ~ p0_2014*`elec12_14` + p1_2017*`elec12_14` + p2_2020*`elec12_14`
                | village_id + cell_year + cell_office_year + health_center_year | 0 | sector_id, data = df),
    
    emp4 = felm(total_employee ~ p0_2014*`elec12_14` + p1_2017*`elec12_14` + p2_2020*`elec12_14`
                | village_id + cell_year + cell_office_year + health_center_year + primary_school_year | 0 | sector_id, data = df),
    
    emp5 = felm(total_employee ~ p0_2014*`elec12_14` + p1_2017*`elec12_14` + p2_2020*`elec12_14`
                | village_id + cell_year + cell_office_year + health_center_year + primary_school_year + secondary_school_year | 0 | sector_id, data = df),
    
    emp6 = felm(total_employee ~ p0_2014*`elec12_14` + p1_2017*`elec12_14` + p2_2020*`elec12_14`
                | village_id + cell_year + cell_office_year + health_center_year + primary_school_year + secondary_school_year + sector_district_office_year | 0 | sector_id, data = df),
    
    emp7 = felm(total_employee ~ p0_2014*`elec12_14` + p1_2017*`elec12_14` + p2_2020*`elec12_14`
                | village_id + cell_year + cell_office_year + health_center_year + primary_school_year + secondary_school_year + sector_district_office_year + industry_year | 0 | sector_id, data = df),
    
    emp8 = felm(total_employee ~ p0_2014*`elec12_14` + p1_2017*`elec12_14` + p2_2020*`elec12_14`
                | village_id + cell_year + cell_office_year + health_center_year + primary_school_year + secondary_school_year + sector_district_office_year + industry_year + market_year | 0 | sector_id, data = df)
  )
}





# ISIC16 (Education)-------

elec12_14_did_16 <- elec12_14_did_isic %>% 
  filter(isic == 16) %>% 
  mutate(total_employee = pmin(total_employee, 100))

# Run regressions separately
regs_isic16_est <- run_reg_est(elec12_14_did_16)
regs_isic16_emp <- run_reg_emp(elec12_14_did_16)

# Export temporary stargazer outputs for extraction
tex_file_est <- file.path(output_path, "regressions", "raw", "elec12_14_ISIC16_est_raw.tex")
tex_file_emp <- file.path(output_path, "regressions", "raw","elec12_14_ISIC16_emp_raw.tex")

stargazer(
  regs_isic16_est,
  type = "latex",
  out = tex_file_est,
  keep = c("p0_2014:elec12_14", "elec12_14:p1_2017", "elec12_14:p2_2020"),
  keep.stat = c("n", "rsq"),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  header = FALSE
)

stargazer(
  regs_isic16_emp,
  type = "latex",
  out = tex_file_emp,
  keep = c("p0_2014:elec12_14", "elec12_14:p1_2017", "elec12_14:p2_2020"),
  keep.stat = c("n", "rsq"),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  header = FALSE
)


## Extractor------

extract_terms <- function(tex_file_path) {
  lines <- readLines(tex_file_path)
  
  extract_chunk_by_year <- function(year_string) {
    i <- grep(year_string, lines, perl = TRUE)
    if (length(i) == 0) return(rep("", 3))
    start <- i[1]
    end <- min(start + 2, length(lines))
    return(lines[start:end])
  }
  
  list(
    p0  = extract_chunk_by_year("2014"),
    p1  = extract_chunk_by_year("2017"),
    p2  = extract_chunk_by_year("2020"),
    obs = lines[grep("^Observations\\s*&", lines)],
    r2  = lines[grep("^R\\$\\^\\{2\\}\\$\\s*&", lines)]
  )
}

extracted_est <- extract_terms(tex_file_est)
extracted_emp <- extract_terms(tex_file_emp)





## Custom LaTeX table: Establishments-------

mean_line <- paste(
  "Mean &",
  sprintf("%.3f", compute_mean(elec12_14_did_16, "num_establishment")), 
  "& \\multicolumn{7}{c}{} \\\\"
)

writeLines(c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Regression Results: num\\_establishment (ISIC16-Education)}",
  "\\label{tab:isic16_est}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{@{\\extracolsep{5pt}}lcccccccc}",
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  " & FE1 & FE2 & FE3 & FE4 & FE5 & FE6 & FE7 & FE8 \\\\",
  "\\hline \\\\[-1.8ex]",
  extracted_est$p0,
  extracted_est$p1,
  extracted_est$p2,
  "\\hline \\\\[-1.8ex]",
  extracted_est$obs,
  extracted_est$r2,
  "FE:Village & X & X & X & X & X & X & X & X \\\\",
  "FE:Cell\\_Year & X & X & X & X & X & X & X & X \\\\",
  "FE:Cell Office\\_Year &  & X & X & X & X & X & X & X \\\\",
  "FE:Health Center\\_Year &  &  & X & X & X & X & X & X \\\\",
  "FE:Primary School\\_Year &  &  &  & X & X & X & X & X \\\\",
  "FE:Secondary School\\_Year &  &  &  &  & X & X & X & X \\\\",
  "FE:District Office\\_Year &  &  &  &  &  & X & X & X \\\\",
  "FE:Industry\\_Year &  &  &  &  &  &  & X & X \\\\",
  "FE:Market\\_Year &  &  &  &  &  &  &  & X \\\\",
  mean_line,
  "\\hline \\hline \\\\[-1.8ex]",
  "\\textit{Note:} & \\multicolumn{8}{r}{$^{*}$p$<0.1$; $^{**}$p$<0.05$; $^{***}$p$<0.01$} \\\\",
  "\\end{tabular}",
  "}%",
  "\\end{table}"
), file.path(output_path, "regressions", "elec12_14_ISIC16_est.tex"))

## Custom LaTeX table: Employees----------

mean_line <- paste(
  "Mean &",
  sprintf("%.3f", compute_mean(elec12_14_did_16, "total_employee")), 
  "& \\multicolumn{7}{c}{} \\\\"
)
writeLines(c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Regression Results: total\\_employee (ISIC16-Education)}",
  "\\label{tab:isic16_emp}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{@{\\extracolsep{5pt}}lcccccccc}",
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  " & FE1 & FE2 & FE3 & FE4 & FE5 & FE6 & FE7 & FE8 \\\\",
  "\\hline \\\\[-1.8ex]",
  extracted_emp$p0,
  extracted_emp$p1,
  extracted_emp$p2,
  "\\hline \\\\[-1.8ex]",
  extracted_emp$obs,
  extracted_emp$r2,
  "FE:Village & X & X & X & X & X & X & X & X \\\\",
  "FE:Cell\\_Year & X & X & X & X & X & X & X & X \\\\",
  "FE:Cell Office\\_Year &  & X & X & X & X & X & X & X \\\\",
  "FE:Health Center\\_Year &  &  & X & X & X & X & X & X \\\\",
  "FE:Primary School\\_Year &  &  &  & X & X & X & X & X \\\\",
  "FE:Secondary School\\_Year &  &  &  &  & X & X & X & X \\\\",
  "FE:District Office\\_Year &  &  &  &  &  & X & X & X \\\\",
  "FE:Industry\\_Year &  &  &  &  &  &  & X & X \\\\",
  "FE:Market\\_Year &  &  &  &  &  &  &  & X \\\\",
  mean_line, 
  "\\hline \\hline \\\\[-1.8ex]",
  "\\textit{Note:} & \\multicolumn{8}{r}{$^{*}$p$<0.1$; $^{**}$p$<0.05$; $^{***}$p$<0.01$} \\\\",
  "\\end{tabular}",
  "}%",
  "\\end{table}"
), file.path(output_path, "regressions", "elec12_14_ISIC16_emp.tex"))





# Private sector--------


join_drop13 <- expansion_join_drop12_14 %>% 
  select(elec12_14, village_id, cell_id, sector_id, cell_office, health_center, primary_school, secondary_school, 
         sector_district_office, market, industry, priority)

elec12_14_p <- elec12_14_did_isic %>%
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
    priority_year = paste0(priority, "-", year), 
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )


regs_private_est <- run_reg_est(elec12_14_p)
regs_private_emp <- run_reg_emp(elec12_14_p)

# Export raw stargazer files
tex_file_priv_est <- file.path(output_path, "regressions","raw", "elec12_14_Private_est_raw.tex")
tex_file_priv_emp <- file.path(output_path, "regressions","raw", "elec12_14_Private_emp_raw.tex")

stargazer(
  regs_private_est,
  type = "latex",
  out = tex_file_priv_est,
  keep = c("p0_2014:elec12_14", "elec12_14:p1_2017", "elec12_14:p2_2020"),
  keep.stat = c("n", "rsq"),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  header = FALSE
)

stargazer(
  regs_private_emp,
  type = "latex",
  out = tex_file_priv_emp,
  keep = c("p0_2014:elec12_14", "elec12_14:p1_2017", "elec12_14:p2_2020"),
  keep.stat = c("n", "rsq"),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  header = FALSE
)

# Extract results
extracted_priv_est <- extract_terms(tex_file_priv_est)
extracted_priv_emp <- extract_terms(tex_file_priv_emp)

# Final Private tables

mean_line <- paste(
  "Mean &",
  sprintf("%.3f", compute_mean(elec12_14_p, "num_establishment")), 
  "& \\multicolumn{7}{c}{} \\\\"
)

writeLines(c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Regression Results: num\\_establishment (Private sector)}",
  "\\label{tab:private_est}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccccccc}",
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  " & FE1 & FE2 & FE3 & FE4 & FE5 & FE6 & FE7 & FE8 \\\\",
  "\\hline \\\\[-1.8ex]",
  extracted_priv_est$p0,
  extracted_priv_est$p1,
  extracted_priv_est$p2,
  "\\hline \\\\[-1.8ex]",
  extracted_priv_est$obs,
  extracted_priv_est$r2,
  "FE:Village & X & X & X & X & X & X & X & X \\\\",
  "FE:Cell\\_Year & X & X & X & X & X & X & X & X \\\\",
  "FE:Cell Office\\_Year &  & X & X & X & X & X & X & X \\\\",
  "FE:Health Center\\_Year &  &  & X & X & X & X & X & X \\\\",
  "FE:Primary School\\_Year &  &  &  & X & X & X & X & X \\\\",
  "FE:Secondary School\\_Year &  &  &  &  & X & X & X & X \\\\",
  "FE:District Office\\_Year &  &  &  &  &  & X & X & X \\\\",
  "FE:Industry\\_Year &  &  &  &  &  &  & X & X \\\\",
  "FE:Market\\_Year &  &  &  &  &  &  &  & X \\\\",
  mean_line,
  "\\hline",
  "\\end{tabular}",
  "}%",
  "\\end{table}"
), file.path(output_path, "regressions", "elec12_14_Private_est.tex"))


mean_line <- paste(
  "Mean &",
  sprintf("%.3f", compute_mean(elec12_14_p, "total_employee")), 
  "& \\multicolumn{7}{c}{} \\\\"
)
writeLines(c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Regression Results: total\\_employee (Private sector)}",
  "\\label{tab:private_emp}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccccccc}",
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  " & FE1 & FE2 & FE3 & FE4 & FE5 & FE6 & FE7 & FE8 \\\\",
  "\\hline \\\\[-1.8ex]",
  extracted_priv_emp$p0,
  extracted_priv_emp$p1,
  extracted_priv_emp$p2,
  "\\hline \\\\[-1.8ex]",
  extracted_priv_emp$obs,
  extracted_priv_emp$r2,
  "FE:Village & X & X & X & X & X & X & X & X \\\\",
  "FE:Cell\\_Year & X & X & X & X & X & X & X & X \\\\",
  "FE:Cell Office\\_Year &  & X & X & X & X & X & X & X \\\\",
  "FE:Health Center\\_Year &  &  & X & X & X & X & X & X \\\\",
  "FE:Primary School\\_Year &  &  &  & X & X & X & X & X \\\\",
  "FE:Secondary School\\_Year &  &  &  &  & X & X & X & X \\\\",
  "FE:District Office\\_Year &  &  &  &  &  & X & X & X \\\\",
  "FE:Industry\\_Year &  &  &  &  &  &  & X & X \\\\",
  "FE:Market\\_Year &  &  &  &  &  &  &  & X \\\\",
  mean_line,
  "\\hline",
  "\\end{tabular}",
  "}%",
  "\\end{table}"
), file.path(output_path, "regressions", "elec12_14_Private_emp.tex"))


# Public sector-------

elec12_14_pu <- elec12_14_did_isic %>%
  mutate(private_sector = ifelse(isic %in% c(9,7,3,19), 1, 0)) %>%
  filter(private_sector == 0) %>%
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
    priority_year = paste0(priority, "-", year), 
    
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )

regs_public_est <- run_reg_est(elec12_14_pu)
regs_public_emp <- run_reg_emp(elec12_14_pu)

# Export raw stargazer
tex_file_pub_est <- file.path(output_path, "regressions","raw", "elec12_14_Public_est_raw.tex")
tex_file_pub_emp <- file.path(output_path, "regressions", "raw","elec12_14_Public_emp_raw.tex")

stargazer(
  regs_public_est,
  type = "latex",
  out = tex_file_pub_est,
  keep = c("p0_2014:elec12_14", "elec12_14:p1_2017", "elec12_14:p2_2020"),
  keep.stat = c("n", "rsq"),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  header = FALSE
)

stargazer(
  regs_public_emp,
  type = "latex",
  out = tex_file_pub_emp,
  keep = c("p0_2014:elec12_14", "elec12_14:p1_2017", "elec12_14:p2_2020"),
  keep.stat = c("n", "rsq"),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  header = FALSE
)

# Extract results
extracted_pub_est <- extract_terms(tex_file_pub_est)
extracted_pub_emp <- extract_terms(tex_file_pub_emp)

mean_line <- paste(
  "Mean &",
  sprintf("%.3f", compute_mean(elec12_14_pu, "num_establishment")), 
  "& \\multicolumn{7}{c}{} \\\\"
)
# Final Public tables
writeLines(c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Regression Results: num\\_establishment (Public sector)}",
  "\\label{tab:public_est}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccccccc}",
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  " & FE1 & FE2 & FE3 & FE4 & FE5 & FE6 & FE7 & FE8 \\\\",
  "\\hline \\\\[-1.8ex]",
  extracted_pub_est$p0,
  extracted_pub_est$p1,
  extracted_pub_est$p2,
  "\\hline \\\\[-1.8ex]",
  extracted_pub_est$obs,
  extracted_pub_est$r2,
  "FE:Village & X & X & X & X & X & X & X & X \\\\",
  "FE:Cell\\_Year & X & X & X & X & X & X & X & X \\\\",
  "FE:Cell Office\\_Year &  & X & X & X & X & X & X & X \\\\",
  "FE:Health Center\\_Year &  &  & X & X & X & X & X & X \\\\",
  "FE:Primary School\\_Year &  &  &  & X & X & X & X & X \\\\",
  "FE:Secondary School\\_Year &  &  &  &  & X & X & X & X \\\\",
  "FE:District Office\\_Year &  &  &  &  &  & X & X & X \\\\",
  "FE:Industry\\_Year &  &  &  &  &  &  & X & X \\\\",
  "FE:Market\\_Year &  &  &  &  &  &  &  & X \\\\",
  mean_line,
  "\\hline",
  "\\end{tabular}",
  "}%",
  "\\end{table}"
), file.path(output_path, "regressions", "elec12_14_Public_est.tex"))



mean_line <- paste(
  "Mean &",
  sprintf("%.3f", compute_mean(elec12_14_pu, "total_employee")), 
  "& \\multicolumn{7}{c}{} \\\\"
)
writeLines(c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Regression Results: total\\_employee (Public sector)}",
  "\\label{tab:public_emp}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccccccc}",
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  " & FE1 & FE2 & FE3 & FE4 & FE5 & FE6 & FE7 & FE8 \\\\",
  "\\hline \\\\[-1.8ex]",
  extracted_pub_emp$p0,
  extracted_pub_emp$p1,
  extracted_pub_emp$p2,
  "\\hline \\\\[-1.8ex]",
  extracted_pub_emp$obs,
  extracted_pub_emp$r2,
  "FE:Village & X & X & X & X & X & X & X & X \\\\",
  "FE:Cell\\_Year & X & X & X & X & X & X & X & X \\\\",
  "FE:Cell Office\\_Year &  & X & X & X & X & X & X & X \\\\",
  "FE:Health Center\\_Year &  &  & X & X & X & X & X & X \\\\",
  "FE:Primary School\\_Year &  &  &  & X & X & X & X & X \\\\",
  "FE:Secondary School\\_Year &  &  &  &  & X & X & X & X \\\\",
  "FE:District Office\\_Year &  &  &  &  &  & X & X & X \\\\",
  "FE:Industry\\_Year &  &  &  &  &  &  & X & X \\\\",
  "FE:Market\\_Year &  &  &  &  &  &  &  & X \\\\",
  mean_line,
  "\\hline",
  "\\end{tabular}",
  "}%",
  "\\end{table}"
), file.path(output_path, "regressions", "elec12_14_Public_emp.tex"))





#Add priority as FE------

private_priority_est <- felm(num_establishment ~ p0_2014*`elec12_14` + p1_2017*`elec12_14` + p2_2020*`elec12_14`
                             | village_id + cell_year + cell_office_year + health_center_year + 
                               primary_school_year + secondary_school_year + sector_district_office_year + 
                               industry_year + market_year + priority_year | 0 | sector_id, data = elec12_14_p)

summary(private_priority_est)

private_priority_emp <- felm(total_employee ~ p0_2014*`elec12_14` + p1_2017*`elec12_14` + p2_2020*`elec12_14`
                             | village_id + cell_year + cell_office_year + health_center_year + 
                               primary_school_year + secondary_school_year + sector_district_office_year + 
                               industry_year + market_year + priority_year | 0 | sector_id, data = elec12_14_p)

summary(private_priority_emp)




public_priority_est<- felm(num_establishment ~ p0_2014*`elec12_14` + p1_2017*`elec12_14` + p2_2020*`elec12_14`
                           | village_id + cell_year + cell_office_year + health_center_year + 
                             primary_school_year + secondary_school_year + sector_district_office_year + 
                             industry_year + market_year + priority_year | 0 | sector_id, data = elec12_14_pu)

summary(public_priority_est)



public_priority_emp<- felm(total_employee ~ p0_2014*`elec12_14` + p1_2017*`elec12_14` + p2_2020*`elec12_14`
                           | village_id + cell_year + cell_office_year + health_center_year + 
                             primary_school_year + secondary_school_year + sector_district_office_year + 
                             industry_year + market_year + priority_year | 0 | sector_id, data = elec12_14_pu)

summary(public_priority_emp)



models <- list(
  private_priority_est = private_priority_est,
  private_priority_emp = private_priority_emp,
  public_priority_est  = public_priority_est,
  public_priority_emp  = public_priority_emp
)



stargazer(
  models,
  type = "latex",
  out = file.path(output_path, "regressions", "elec12_14_priority.tex"),
  title = "Regression Results: elec12-14 with Priority Fixed Effects",
  label = "tab:priority_fe",
  dep.var.labels = c("Num. Establishments", "Total Employees", "Num. Establishments", "Total Employees"),
  column.labels = c("Private (Est.)", "Private (Emp.)", "Public (Est.)", "Public (Emp.)"),
  covariate.labels = c("elec12-14 × 2014", "elec12-14 × 2017", "elec12-14 × 2020"),
  keep = c("p0_2014:elec12_14", "elec12_14:p1_2017", "elec12_14:p2_2020"),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  keep.stat = c("n", "rsq"),
  float = TRUE,
  float.env = "table",
  font.size = "small",
  header = FALSE,
  digits = 3,
  no.space = TRUE,
  add.lines = list(
    c("FE:Village", "X", "X", "X", "X"),
    c("FE:Cell-Year", "X", "X", "X", "X"),
    c("FE:All infra-Year", "X", "X", "X", "X"),
    c("FE:Priority-Year", "X", "X", "X", "X"),
    c("Cluster:Sector", "X", "X", "X", "X")
  )
)
