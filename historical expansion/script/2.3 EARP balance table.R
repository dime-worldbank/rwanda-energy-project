#######################################
#Purpose:EARP balance table
#Author: XIAOMING ZHANG
#Date: September 10th 2025
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

#Data clean-----



earp_did_all <- earp_did_isic %>%
  group_by(village_id, year)%>%
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE))  %>% 
  left_join( earp) %>%
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
    p2_2020 = ifelse(year == 2020, 1, 0),
    `EARP`  = ifelse(lv == 1 | mv == 1, 1, 0)
  ) %>% 
  mutate(
    total_employee = pmin(total_employee, 100)  ,
    num_establishment = pmin(num_establishment, 10)
  ) 


#=== Run balance tables ===

library(dplyr)
library(fixest)
library(broom)

balance_table <- function(data, treatment_vars, cluster, fe = NULL,
                          outcomes = c("num_establishment", "total_employee"),
                          extract_var = "EARP") {
  
  cluster_var <- enquo(cluster)
  cluster_name <- quo_name(cluster_var)
  
  rhs_string <- paste(treatment_vars, collapse = " + ")
  fe_string  <- if (!is.null(fe)) paste(fe, collapse = " + ") else "0"
  
  # helper function for significance stars
  starify <- function(p) {
    if (p < 0.001) return("***")
    else if (p >= 0.001 & p < 0.01) return("**")
    else if (p >= 0.01 & p < 0.05) return("*")
    else if (p >= 0.05 & p < 0.1) return(".")
    else return("")
  }
  
  results <- list()
  
  for (outcome in outcomes) {
    
    # summary stats
    summ_stats <- data %>%
      group_by(.data[[extract_var]]) %>%
      summarise(
        mean = mean(.data[[outcome]]),
        sd   = sd(.data[[outcome]]),
        n    = n(),
        .groups = "drop"
      )
    
    # regression
    fml <- as.formula(paste0(outcome, " ~ ", rhs_string, " | ", fe_string, " | 0 | ", cluster_name))
    reg <- felm(fml, data = data)
    reg_summary <- summary(reg)
    
    coef_row <- reg_summary$coefficients[extract_var, ]
    diff_est <- coef_row["Estimate"]
    diff_se  <- coef_row["Cluster s.e."]
    p_value  <- coef_row["Pr(>|t|)"]
    stars    <- starify(p_value)
    
    results[[length(results) + 1]] <- tibble(
      outcome = outcome,
      control = sprintf("%.2f (%.2f)",
                        summ_stats$mean[summ_stats[[extract_var]] == 0],
                        summ_stats$sd[summ_stats[[extract_var]] == 0]),
      treated = sprintf("%.2f (%.2f)",
                        summ_stats$mean[summ_stats[[extract_var]] == 1],
                        summ_stats$sd[summ_stats[[extract_var]] == 1]),
      N_control = summ_stats$n[summ_stats[[extract_var]] == 0],
      N_treated = summ_stats$n[summ_stats[[extract_var]] == 1],
      diff = sprintf("%.2f (%.2f)%s", diff_est, diff_se, stars),
      p_value = sprintf("%.4f", p_value)
    )
  }
  
  bind_rows(results)
}


# 
balance_allfe <- balance_table(
  data = earp_bt_dta,
  treatment_vars = c("EARP", "log1_residential_consumer", "log1_non_residential_consumer"),
  cluster = cell_id,
  fe = c("cell_id", "cell_office", "health_center", "primary_school",
         "secondary_school", "sector_district_office", "industry", "market", "imidugudu"),
  outcomes = c("num_establishment", "total_employee"),
  extract_var = "EARP"
)
# 
# balance <- felm(num_establishment ~ EARP + log_residential_consumer + log_non_residential_consumer
#                 + cell_office +health_center + primary_school + secondary_school + sector_district_office +
#                   industry + market|
#                   cell_id |0|cell_id, data = earp_bt_dta )
# 
# summary(balance)


# 
# #Try -------
# 
# 
# 
# # Step 1: Summarise means, sds, counts for control/treatment
# summ_stats <- earp_bt_dta %>%
#   group_by(EARP) %>%
#   summarise(
#     mean = mean(total_employee, na.rm = TRUE),
#     sd = sd(total_employee, na.rm = TRUE),
#     n = sum(!is.na(total_employee)),
#     .groups = "drop"
#   )
# 
# # Step 2: Run the regression
# reg <- felm(total_employee ~ EARP + log1_residential_consumer + log1_non_residential_consumer |
#               cell_id |
#               0 |
#               cell_id + cell_office + health_center + primary_school +
#               secondary_school + sector_district_office + industry + market,
#             data = earp_bt_dta)
# 
# # Step 3: Extract coefficient for EARP
# reg_summary <- summary(reg)
# coef_matrix <- reg_summary$coefficients
# earp_row <- coef_matrix["EARP", ]
# diff_est <- earp_row["Estimate"]
# diff_se  <- earp_row["Cluster s.e."]
# stars    <- reg_summary$signif["EARP"]
# 
# # Step 4: Put everything in a tibble
# balance_df <- tibble(
#   outcome = "total_employee",
#   control = sprintf("%.2f (%.2f)", summ_stats$mean[summ_stats$EARP == 0], summ_stats$sd[summ_stats$EARP == 0]),
#   treated = sprintf("%.2f (%.2f)", summ_stats$mean[summ_stats$EARP == 1], summ_stats$sd[summ_stats$EARP == 1]),
#   N_control = summ_stats$n[summ_stats$EARP == 0],
#   N_treated = summ_stats$n[summ_stats$EARP == 1],
#   diff = sprintf("%.2f (%.2f)%s", diff_est, diff_se, stars)
# )
# 
# balance_df
# 















#Balance table on earp_all------

earp_bt_dta <- earp_did_all %>%
  filter(year == 2011) %>%
  select(num_establishment, total_employee, EARP, cell_id, cell_office, health_center,
         primary_school, secondary_school, market, industry, sector_district_office, residential_consumer, non_residential_consumer, imidugudu) %>% 
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0)
  )
  



# 
# earp_lm <- felm(EARP ~   cell_office + health_center+ primary_school+ secondary_school+ 
#                   market+ industry+ sector_district_office + any_residential_consumer + any_non_residential_consumer +
#                   log_residential_consumer + log_non_residential_consumer|cell_id | 0 |cell_id, data = earp_bt_dta )
# summary(earp_lm)
# 
# stargazer(
#   earp_lm,
#   type = "text",
#   title = "Regression of EARP on Infrastructure and Consumers",
#   dep.var.labels = "EARP (Binary)",
#   covariate.labels = c(
#     "Cell Office",
#     "Health Center",
#     "Primary School",
#     "Secondary School",
#     "Market",
#     "Industry",
#     "Sector/District Office"
#   ),
#   omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
#   keep.stat = c("n", "rsq"),
#   digits = 3,
#   no.space = TRUE,
#   add.lines = list(
#     c("FE: Cell ID", "X"),
#     c("Cluster: Cell ID", "X")
#   )
# )

balance_none <- balance_table(
  data = earp_did_all %>% filter(year == 2011),
  treatment = c("EARP"),
  cluster = cell_id
)


balance_cellid <- balance_table(
  data = earp_did_all %>% filter(year == 2011),
  treatment = c("EARP"), 
  cluster = cell_id,
  fe = c("cell_id", "cell_office", "health_center",
         "primary_school", "secondary_school",  "sector_district_office",
         "industry", "market", "imidugudu")
)

balance_allfe <- balance_table(
  data = earp_bt_dta,
  treatment = c("EARP", "log1_residential_consumer", "log1_non_residential_consumer"),
  cluster = cell_id,
  fe = c("cell_id", "cell_office", "health_center",
         "primary_school", "secondary_school",  "sector_district_office",
         "industry", "market", "imidugudu")
)

# === Combine and format for LaTeX ===

combined_diff <- balance_none %>%
  select(outcome, N_control, N_treated, control, treated, diff_none = diff) %>%
  left_join(balance_cellid %>% select(outcome, diff_cellid = diff), by = "outcome") %>%
  left_join(balance_allfe %>% select(outcome, diff_allfe = diff), by = "outcome") %>%
  rename(
    `Control (Mean (SD))` = control,
    `Treated (Mean (SD))` = treated,
    `Diff (No FE)` = diff_none,
    `Diff (Infra FE)` = diff_cellid,
    `Diff (Infra + Demo FE)` = diff_allfe
  ) 

# === Export to LaTeX ===
kable_output <- combined_diff %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    align = "lcccccc", # adjust if you add/remove columns
    caption = "EARP on 2011 establishment census output",
    label = "tab:balance_fe_compare"
  ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down", "scale_down"), full_width = FALSE)

# Save to file
writeLines(kable_output,
           file.path(output_path, "balance_table", "balance_table_earp_fe.tex"))




#Private sector-------


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
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )%>% 
  mutate(
    total_employee = pmin(total_employee, 100)  ,
    num_establishment = pmin(num_establishment, 10)
  )  %>% 
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0)
  )







balance_none_p <- balance_table(
  data = earp_p %>% filter(year == 2011),
  treatment = c("EARP"),
  cluster = cell_id
)


balance_cellid_p <- balance_table(
  data = earp_p %>% filter(year == 2011),
  treatment = c("EARP"),
  cluster = cell_id,
  fe = c("cell_id", "cell_office", "health_center",
         "primary_school", "secondary_school",  "sector_district_office",
         "industry", "market")
)

balance_allfe_p <- balance_table(
  data = earp_p %>% filter(year == 2011),
  treatment = c("EARP", "log1_residential_consumer", "log1_non_residential_consumer"),
  cluster = cell_id,
  fe = c("cell_id", "cell_office", "health_center",
         "primary_school", "secondary_school",  "sector_district_office",
         "industry", "market", "residential_consumer", "non_residential_consumer", "imidugudu")
)

# === Combine and format for LaTeX ===

combined_diff_p <- balance_none_p %>%
  select(outcome, N_control, N_treated, control, treated, diff_none = diff) %>%
  left_join(balance_cellid_p %>% select(outcome, diff_cellid = diff), by = "outcome") %>%
  left_join(balance_allfe_p %>% select(outcome, diff_allfe = diff), by = "outcome") %>%
  rename(
    `Control (Mean (SD))` = control,
    `Treated (Mean (SD))` = treated,
    `Diff (No FE)` = diff_none,
    `Diff (Infra FE)` = diff_cellid,
    `Diff (Infra + Demo FE)` = diff_allfe
  )

# === Export to LaTeX ===
kable_output <- combined_diff_p %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    align = "lcccccc", # adjust if you add/remove columns
    caption = "EARP on 2011 establishment census private sector output",
    label = "tab:balance_fe_compare"
  ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"), full_width = FALSE)

# Save to file
writeLines(kable_output,
           file.path(output_path, "balance_table", "balance_table_earp_p.tex"))


#Public sector------


earp_pu <- earp_did_isic %>%
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
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )%>% 
  mutate(
    total_employee = pmin(total_employee, 100)  ,
    num_establishment = pmin(num_establishment, 10)
  ) %>% 
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0)
  )




balance_none_pu <- balance_table(
  data = earp_pu %>% filter(year == 2011),
  treatment = c("EARP"),
  cluster = cell_id
)


balance_cellid_pu <- balance_table(
  data = earp_pu %>% filter(year == 2011),
  treatment = c("EARP"),
  cluster = cell_id,
  fe = c("cell_id", "cell_office", "health_center",
         "primary_school", "secondary_school",  "sector_district_office",
         "industry", "market")
)

balance_allfe_pu <- balance_table(
  data = earp_pu %>% filter(year == 2011),
  treatment = c("EARP", "log1_residential_consumer", "log1_non_residential_consumer"),
  cluster = cell_id,
  fe = c("cell_id", "cell_office", "health_center",
         "primary_school", "secondary_school",  "sector_district_office",
         "industry", "market", "residential_consumer", "non_residential_consumer", "imidugudu")
)

# === Combine and format for LaTeX ===

combined_diff_pu <- balance_none_pu %>%
  select(outcome, N_control, N_treated, control, treated, diff_none = diff) %>%
  left_join(balance_cellid_pu %>% select(outcome, diff_cellid = diff), by = "outcome") %>%
  left_join(balance_allfe_pu %>% select(outcome, diff_allfe = diff), by = "outcome") %>%
  rename(
    `Control (Mean (SD))` = control,
    `Treated (Mean (SD))` = treated,
    `Diff (No FE)` = diff_none,
    `Diff (Infra FE)` = diff_cellid,
    `Diff (Infra + Demo FE)` = diff_allfe
  )

# === Export to LaTeX ===
kable_output <- combined_diff_pu %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    align = "lcccccc", # adjust if you add/remove columns
    caption = "EARP on 2011 establishment census public sector output",
    label = "tab:balance_fe_compare"
  ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"), full_width = FALSE)

# Save to file
writeLines(kable_output,
           file.path(output_path, "balance_table", "balance_table_earp_pu.tex"))



#Combined------

# Tag each set (all, private, public) before combining
combined_diff <- combined_diff %>%
  mutate(outcome = paste0(outcome, "_all"))

combined_diff_p <- combined_diff_p %>%
  mutate(outcome = paste0(outcome, "_private"))

combined_diff_pu <- combined_diff_pu %>%
  mutate(outcome = paste0(outcome, "_public"))

# Stack them
combined_all_private_public <- bind_rows(
  combined_diff,
  combined_diff_p,
  combined_diff_pu
)

# Export to LaTeX
kable_output <- combined_all_private_public %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    align = "lcccccc",
    caption = "EARP on 2011 establishment census",
    label = "tab:balance_all_private_public"
  ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"), full_width = FALSE)

writeLines(
  kable_output,
  file.path(output_path, "balance_table", "balance_table_earp_all.tex")
)

