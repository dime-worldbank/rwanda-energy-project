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

electrification_status_year <- read_xlsx(path = file.path(output_path, "electrification_status_year.xlsx"))

#earp_existing_mv----

earp_existing_mv <- st_read(dsn = file.path(data_path, "shapefiles", "earp_existingMV.shp"))

earp_existing_mv <- st_transform(earp_existing_mv, crs = st_crs(rwa_villages))

earp_existing_mv <- st_intersection(earp_existing_mv, rwa_villages)

earp_existing_mv <- earp_existing_mv %>% 
  distinct(Village_ID) %>% 
  mutate(
    earp_existing_mv = 1
  )


#Sample restriction----

expansion_join_drop13 <- expansion_join %>% 
  mutate(
    `EARP` = ifelse(earp_lv == 1 | earp_mv == 1, 1, 0)
  ) %>% 
  filter(electrified_year > 2013) %>% 
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



earp_join <- expansion_join_drop13%>% 
  select(village_id, earp_mv, earp_lv, cell_id, sector_id, district_id) 




##clean------

earp_did_isic<- left_join( earp_join, ec_all, by = c("village_id"))


earp_did_isic <- earp_did_isic %>%
  complete(year, village_id, isic_level1,
           fill = list(num_establishment = 0, total_employee = 0)) %>% 
  filter(!is.na(year))  


earp <- expansion_join_drop13%>% 
  select(village_id, earp_mv, earp_lv, earp_existing_mv, health_center, primary_school, cell_office) %>% 
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
  )

#First batch of regression--------
anyNA(earp_did_isic)

##ISIC ALL------

earp_did_all <- earp_did_isic %>%
  group_by(village_id, year)%>%
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE))

anyNA(earp_did_all)
earp_did_all <- left_join(earp_did_all, earp) %>%
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
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0),
    `EARP`  = ifelse(lv == 1 | mv == 1, 1, 0)
  ) %>% 
  mutate(
    total_employee = pmin(total_employee, 100)   
  ) 

num_establishment<- felm(num_establishment ~ p0_2014*`EARP` + p1_2017*`EARP` + p2_2020*`EARP`|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_all)

summary(num_establishment)

total_employee <- felm (total_employee ~ p0_2014*`EARP` + p1_2017*`EARP` + p2_2020*`EARP`|village_id + cell_year + cell_office_year + health_center_year + primary_school_year |0|sector_id, data = earp_did_all)
summary(total_employee)




##ISIC 19-----


earp_did_19 <- earp_did_isic %>% 
  filter(isic == 19)  %>% 
  mutate(
    total_employee = pmin(total_employee, 100)   
  ) 


num_establishment_19 <- felm(num_establishment ~ p0_2014*`EARP` + p1_2017*`EARP` + p2_2020*`EARP`|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_19)
summary(num_establishment_19)

total_employee_19 <- felm (total_employee ~ p0_2014*`EARP` + p1_2017*`EARP` + p2_2020*`EARP`|village_id + cell_year + cell_office_year + health_center_year + primary_school_year |0|sector_id, data = earp_did_19)
summary(total_employee_19)



##ISIC 3-----

earp_did_3 <- earp_did_isic %>% 
  filter(isic == 3) %>% 
  mutate(
    total_employee = pmin(total_employee, 100)   
  ) 

mean(earp_did_3$num_establishment)
mean(earp_did_3$total_employee)

num_establishment_3 <- felm(num_establishment ~ p0_2014*`EARP` + p1_2017*`EARP` + p2_2020*`EARP`|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_3)
summary(num_establishment_3)

total_employee_3 <- felm (total_employee ~ p0_2014*`EARP` + p1_2017*`EARP` + p2_2020*`EARP`|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_3)
summary(total_employee_3)


##ISIC 7-----
earp_did_7 <- earp_did_isic %>% 
  filter(isic == 7) %>% 
  mutate(
    total_employee = pmin(total_employee, 100)   
  ) 

mean(earp_did_7$num_establishment)
mean(earp_did_7$total_employee)

num_establishment_7 <- felm(num_establishment ~ p0_2014*`EARP` + p1_2017*`EARP` + p2_2020*`EARP`+ p0_2014*`EARP` + p1_2017*`EARP` + p2_2020*`EARP`|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_7)
summary(num_establishment_7)

total_employee_7 <- felm (total_employee ~ p0_2014*`EARP` + p1_2017*`EARP` + p2_2020*`EARP`+ p0_2014*`EARP` + p1_2017*`EARP` + p2_2020*`EARP`|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_7)
summary(total_employee_7)









# Extract mean------
compute_mean <- function(df, var, na.rm = TRUE) {
  stopifnot(var %in% names(df))
  x <- df[[var]]
  if (!is.numeric(x)) x <- as.numeric(x)
  mean(x, na.rm = na.rm)
}

mean_1 <- paste("Mean &", paste(sprintf("%.3f", c(
  compute_mean(earp_did_all,"num_establishment"),
  compute_mean(earp_did_all,"total_employee"),
  compute_mean(earp_did_3,"num_establishment"),
  compute_mean(earp_did_3,"total_employee"),
  compute_mean(earp_did_7,"num_establishment"),
  compute_mean(earp_did_7,"total_employee"),
  compute_mean(earp_did_19,"num_establishment"),
  compute_mean(earp_did_19,"total_employee"))), collapse=" & "), "\\\\")




#Extract regression output
# Step 1: Define the regression list
regs <- list(
  "num_establishment" = num_establishment,
  "total_employee" = total_employee,
  "establishment_isic3" = num_establishment_3,
  "employee_isic_3" = total_employee_3,
  "establishment_isic7" = num_establishment_7,
  "employee_isic_7" = total_employee_7,
  "establishment_isic19" = num_establishment_19,
  "employee_isic_19" = total_employee_19
)


#Step 2: extract the line
# Step 1: Generate .tex file
tex_file <- file.path(output_path, "event_study.tex")

stargazer(
  regs,
  type = "latex",
  out = tex_file,
  keep = c("p0_2014:EARP", "EARP:p1_2017", "EARP:p2_2020"),
  keep.stat = c("n", "rsq"),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  header = FALSE
)

# Step 2: Define helper function for fuzzy extraction (multi-row match)
extract_terms <- function(tex_file_path) {
  lines <- readLines(tex_file_path)
  
  extract_chunk_by_year <- function(year_string) {
    i <- grep(year_string, lines, perl = TRUE)
    if (length(i) == 0) return(rep("", 3))
    
    # Safely extract 3 lines
    start <- i[1]
    end <- min(start + 2, length(lines))  # Ensure we don’t go out of bounds
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

# Step 4: Run the extractor
extracted_terms_1 <- extract_terms(tex_file)


# Step 4: Write final LaTeX table
writeLines(c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Regression Results}",
  "\\label{}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{@{\\extracolsep{5pt}}lcccccccc}",
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  " & \\multicolumn{8}{c}{\\textit{Dependent variable:}} \\\\",
  "\\cline{2-9}",
  "\\\\[-1.8ex] & \\multicolumn{2}{c}{All ISIC} & \\multicolumn{2}{c}{ISIC3-Manufacture} & \\multicolumn{2}{c}{ISIC7-Wholesale} & \\multicolumn{2}{c}{ISIC19-Barbershop} \\\\",
  "\\\\[-1.8ex] & Establishment & Employee & Establishment & Employee & Establishment & Employee & Establishment & Employee \\\\",
  "\\\\[-1.8ex] & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\\\",
  "\\hline \\\\[-1.8ex]",
  extracted_terms_1$p0,
  extracted_terms_1$p1,
  extracted_terms_1$p2,
  "\\hline \\\\[-1.8ex]",
  extracted_terms$obs,
  extracted_terms$r2,
  "FE:Village & X & X & X & X & X & X & X & X \\\\",
  "FE:Cell\\_Year & X & X & X & X & X & X & X & X \\\\",
  "FE:Cell Offices\\_Year & X & X & X & X & X & X & X & X \\\\",
  "FE:Health Center\\_Year & X & X & X & X & X & X & X & X \\\\",
  "FE:Primary School\\_Year & X & X & X & X & X & X & X & X \\\\",
  "\\hline",
  mean_1,
  "\\hline \\hline \\\\[-1.8ex]",
  "\\textit{Note:} & \\multicolumn{8}{r}{$^{*}$p$<0.1$; $^{**}$p$<0.05$; $^{***}$p$<0.01$} \\\\",
  "\\end{tabular}",
  "}%",
  "\\end{table}"), file.path(output_path, "EARP_ISIC_1.tex"))













#Second batch of regression----

##ISIC 16----
earp_did_16 <- earp_did_isic %>% 
  filter(isic == 16) %>% 
  mutate(
    total_employee = pmin(total_employee, 100)   
  ) 

mean(earp_did_16$num_establishment)
mean(earp_did_16$total_employee)

num_establishment_16 <- felm(num_establishment ~ p0_2014*`EARP` + p1_2017*`EARP` + p2_2020*`EARP`|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_16)
summary(num_establishment_16)

total_employee_16 <- felm (total_employee ~ p0_2014*`EARP` + p1_2017*`EARP` + p2_2020*`EARP`|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_16)
summary(total_employee_16)


##ISIC 17----
earp_did_17 <- earp_did_isic %>% 
  filter(isic == 17) %>% 
  mutate(
    total_employee = pmin(total_employee, 100)   
  ) 

mean(earp_did_17$num_establishment)
mean(earp_did_17$total_employee)

num_establishment_17 <- felm(num_establishment ~ p0_2014*`EARP` + p1_2017*`EARP` + p2_2020*`EARP`|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_17)
summary(num_establishment_17)

total_employee_17 <- felm (total_employee ~ p0_2014*`EARP` + p1_2017*`EARP` + p2_2020*`EARP`|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_17)
summary(total_employee_17)



##ISIC 9----
earp_did_9 <- earp_did_isic %>% 
  filter(isic == 9) %>% 
  mutate(
    total_employee = pmin(total_employee, 100)   
  ) 

mean(earp_did_9$num_establishment)
mean(earp_did_9$total_employee)

num_establishment_9 <- felm(num_establishment ~ p0_2014*`EARP` + p1_2017*`EARP` + p2_2020*`EARP`|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_9)
summary(num_establishment_9)

total_employee_9 <- felm (total_employee ~ p0_2014*`EARP` + p1_2017*`EARP` + p2_2020*`EARP`|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_did_9)
summary(total_employee_9)




# Step 1: Define second batch regression list
regs <- list(
  "num_establishment"     = num_establishment,
  "total_employee"        = total_employee,
  "establishment_isic16"  = num_establishment_16,
  "employee_isic_16"      = total_employee_16,
  "establishment_isic17"  = num_establishment_17,
  "employee_isic_17"      = total_employee_17,
  "establishment_isic9"   = num_establishment_9,
  "employee_isic_9"       = total_employee_9
)

# Step 2: Export stargazer LaTeX output
tex_file <- file.path(output_path, "event_study.tex")

stargazer(
  regs,
  type = "latex",
  out = tex_file,
  keep = c("p0_2014:EARP", "EARP:p1_2017", "EARP:p2_2020"),
  keep.stat = c("n", "rsq"),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  header = FALSE
)

# Step 5: Run extractor
extracted_terms_2 <- extract_terms(tex_file)

# Step 6: Compute means
mean_2 <- paste("Mean &", paste(sprintf("%.3f", c(
  compute_mean(earp_did_all,    "num_establishment"),
  compute_mean(earp_did_all,    "total_employee"),
  compute_mean(earp_did_16, "num_establishment"),
  compute_mean(earp_did_16, "total_employee"),
  compute_mean(earp_did_17, "num_establishment"),
  compute_mean(earp_did_17, "total_employee"),
  compute_mean(earp_did_9,  "num_establishment"),
  compute_mean(earp_did_9,  "total_employee")
  )), collapse=" & "), "\\\\")





# Step 7: Write final LaTeX output
writeLines(c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Regression Results}",
  "\\label{}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{@{\\extracolsep{5pt}}lcccccccc}",
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  " & \\multicolumn{8}{c}{\\textit{Dependent variable:}} \\\\",
  "\\cline{2-9}",
  "\\\\[-1.8ex] & \\multicolumn{2}{c}{All ISIC} & \\multicolumn{2}{c}{ISIC16-Education} & \\multicolumn{2}{c}{ISIC17-Social Work} & \\multicolumn{2}{c}{ISIC9-Accomodation} \\\\",
  "\\\\[-1.8ex] & Establishment & Employee & Establishment & Employee & Establishment & Employee & Establishment & Employee \\\\",
  "\\\\[-1.8ex] & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\\\",
  "\\hline \\\\[-1.8ex]",
  extracted_terms_2$p0,
  extracted_terms_2$p1,
  extracted_terms_2$p2,
  "\\hline \\\\[-1.8ex]",
  extracted_terms_2$obs,
  extracted_terms_2$r2,
  "FE:Village & X & X & X & X & X & X & X & X \\\\",
  "FE:Cell\\_Year & X & X & X & X & X & X & X & X \\\\",
  "FE:Cell Offices\\_Year & X & X & X & X & X & X & X & X \\\\",
  "FE:Health Center\\_Year & X & X & X & X & X & X & X & X \\\\",
  "FE:Primary School\\_Year & X & X & X & X & X & X & X & X \\\\",
  "\\hline",
  mean_2,
  "\\hline \\hline \\\\[-1.8ex]",
  "\\textit{Note:} & \\multicolumn{8}{r}{$^{*}$p$<0.1$; $^{**}$p$<0.05$; $^{***}$p$<0.01$} \\\\",
  "\\end{tabular}",
  "}%",
  "\\end{table}"
) , file.path(output_path, "EARP_ISIC_2.tex"))














#EARP Public private sector-----

join_drop13 <- expansion_join_drop13 %>% 
  select(`EARP`, village_id, cell_id, sector_id, cell_office, health_center, primary_school)


earp_private <- earp_did_isic %>% 
  mutate(
    private_sector = ifelse(isic %in% c(9,7,3,19), 1, 0)
  ) 

earp_p <- earp_private %>% 
  filter(private_sector == 1) %>%
  group_by(year, village_id, private_sector) %>%
  summarise(
    num_establishment = sum(num_establishment, na.rm = TRUE),
    total_employee = sum(total_employee, na.rm = TRUE)
  ) %>%
  mutate(
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  ) %>%
  left_join(join_drop13, by = c("village_id") )  %>%
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year)
  ) %>% 
  mutate(
    total_employee = pmin(total_employee, 100)   
  ) 




num_establishment_p <- felm(num_establishment ~ p0_2014*`EARP` + p1_2017*`EARP` + p2_2020*`EARP`|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_p)
summary(num_establishment_p)

total_employee_p <- felm (total_employee ~ p0_2014*`EARP` + p1_2017*`EARP` + p2_2020*`EARP`|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_p)
summary(total_employee_p)

earp_pu <- earp_private %>% 
  filter(private_sector == 0) %>%
  group_by(year, village_id, private_sector) %>%
  summarise(
    num_establishment = sum(num_establishment, na.rm = TRUE),
    total_employee = sum(total_employee, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  ) %>%
  left_join(join_drop13, by = c("village_id") )  %>%
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year)
  ) %>% 
  mutate(
    total_employee = pmin(total_employee, 100)   
  ) 

num_establishment_pu <- felm(num_establishment ~ p0_2014*`EARP` + p1_2017*`EARP` + p2_2020*`EARP`|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_pu)
summary(num_establishment_pu)

total_employee_pu <- felm (total_employee ~ p0_2014*`EARP` + p1_2017*`EARP` + p2_2020*`EARP`|village_id + cell_year + cell_office_year + health_center_year + primary_school_year|0|sector_id, data = earp_pu)
summary(total_employee_pu)




# Step 1: Define the regression list
regs3 <- list(
  "num_establishment" = num_establishment,
  "total_employee" = total_employee,
  "num_establishment_private" = num_establishment_p,
  "total_employee_private" = total_employee_p,
  "num_establishment_public" = num_establishment_pu,
  "total_employee_public" = total_employee_pu
)

# Step 2: Generate .tex file for stargazer output
tex_file <- file.path(output_path, "event_study.tex")

stargazer(
  regs3,
  type = "latex",
  out = tex_file,
  keep = c("p0_2014:EARP", "EARP:p1_2017", "EARP:p2_2020"),
  keep.stat = c("n", "rsq"),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  header = FALSE
)


# Step 5: Extract content
extracted_terms3 <- extract_terms(tex_file)

mean_3 <- paste("Mean &", paste(sprintf("%.3f", c(
  compute_mean(earp_did_all,    "num_establishment"),
  compute_mean(earp_did_all,    "total_employee"),
  compute_mean(earp_p,    "num_establishment"),
  compute_mean(earp_p,    "total_employee"),
  compute_mean(earp_pu, "num_establishment"),
  compute_mean(earp_pu, "total_employee")
)), collapse=" & "), "\\\\")

writeLines(c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Regression Results}",
  "\\label{}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{@{\\extracolsep{5pt}}lcccccc}",
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  " & \\multicolumn{6}{c}{\\textit{Dependent variable:}} \\\\",
  "\\cline{2-7}",
  "\\\\[-1.8ex] & \\multicolumn{2}{c}{All ISIC} & \\multicolumn{2}{c}{ISIC-Private Sector} & \\multicolumn{2}{c}{ISIC-Public Sector} \\\\",
  "\\\\[-1.8ex] & num\\_establishment & total\\_employee & num\\_establishment & total\\_employee & num\\_establishment & total\\_employee \\\\",
  "\\\\[-1.8ex] & (1) & (2) & (3) & (4) & (5) & (6) \\\\",
  "\\hline \\\\[-1.8ex]",
  extracted_terms3$p0,
  extracted_terms3$p1,
  extracted_terms3$p2,
  "\\hline \\\\[-1.8ex]",
  extracted_terms3$obs,
  extracted_terms3$r2,
  "FE:Village & X & X & X & X & X & X \\\\",
  "FE:Cell\\_Year & X & X & X & X & X & X \\\\",
  "FE:Cell Offices\\_Year & X & X & X & X & X & X \\\\",
  "FE:Health Center\\_Year & X & X & X & X & X & X \\\\",
  "FE:Primary School\\_Year & X & X & X & X & X & X \\\\",
  "\\hline",
  mean_3,
  "\\hline \\hline \\\\[-1.8ex]",
  "\\textit{Note:} & \\multicolumn{6}{r}{$^{*}$p$<0.1$; $^{**}$p$<0.05$; $^{***}$p$<0.01$} \\\\",
  "\\end{tabular}",
  "}%",
  "\\end{table}"
),  file.path(output_path, "EARP_ISIC_Private.tex"))
